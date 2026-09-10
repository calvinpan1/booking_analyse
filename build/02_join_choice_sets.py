"""Join the converted events to the choice-set roster and drop off-protocol rows.

Usage: python 02_join_choice_sets.py INPUTS_DIR CHOICE_SETS_JSON
Reads INPUTS_DIR/extension_converted.csv; writes INPUTS_DIR/extension_joined_raw.csv
(full outer join, every row kept, `_join_status` labels) and
INPUTS_DIR/extension_joined.csv (matched, unmatched-but-on-target and
non-joinable-by-design rows only).
"""
import json
import re
import sys
import unicodedata
from collections import Counter
from pathlib import Path
from urllib.parse import parse_qs, urlparse

import pandas as pd

inputs_dir, choice_sets_json = Path(sys.argv[1]), Path(sys.argv[2])
df = pd.read_csv(inputs_dir / "extension_converted.csv", dtype=str)
with open(choice_sets_json, encoding="utf-8") as f:
    raw_cs = json.load(f)


def derive_city_key(text):
    if not isinstance(text, str) or not text:
        return ""
    text = "".join(c for c in unicodedata.normalize("NFKD", text) if not unicodedata.combining(c))
    return re.sub(r"[^a-z0-9]+", "-", text.lower()).strip("-")


DEST_ID_TO_CITY = {-1407760: "annecy", -1408052: "arcachon", -1416533: "cannes", -1435362: "la-ciotat",
                   -1446308: "le-treport", -1454990: "nice", -1470743: "sete"}


def city_from_url(url):
    if not isinstance(url, str) or not url:
        return ""
    try:
        qs = parse_qs(urlparse(url).query)
    except ValueError:
        return ""
    if qs.get("ss", [None])[0]:
        return derive_city_key(qs["ss"][0])
    try:
        return DEST_ID_TO_CITY.get(int(qs.get("dest_id", [None])[0]), "")
    except (TypeError, ValueError):
        return ""


HOTEL_SLUG_RE = re.compile(r"/hotel/[^/]+/([^.]+)")


def slug_from_url(url):
    if not isinstance(url, str) or not url:
        return ""
    m = HOTEL_SLUG_RE.search(urlparse(url).path)
    return m.group(1) if m else ""


df["_city_key"] = df["url"].apply(city_from_url)

# Backfill the property from the URL on hotel-detail pages (no card in the DOM there).
PROPERTY_AWARE_TYPES = {"click", "hover", "viewport", "reserve"}
needs = df["targetPropertyId"].isna() & df["type"].isin(PROPERTY_AWARE_TYPES)
slugs = df.loc[needs, "url"].apply(slug_from_url)
df.loc[needs, "targetPropertyId"] = slugs.replace("", pd.NA)
df["_targetPropertyId_backfilled"] = False
df.loc[needs & slugs.ne(""), "_targetPropertyId_backfilled"] = True

# A reserve fired from the results page: code it as the last detail page viewed that weekend.
df["timestamp"] = pd.to_numeric(df["timestamp"], errors="coerce")
df["_url_slug"] = df["url"].apply(slug_from_url)
viewed = df[df["_url_slug"] != ""].sort_values(["participant_code", "cell_index", "timestamp"])
fallback = (df["type"] == "reserve") & df["targetPropertyId"].isna()
df["_targetPropertyId_fallback_last_viewed"] = False
for idx in df.index[fallback]:
    prior = viewed[(viewed["participant_code"] == df.at[idx, "participant_code"])
                   & (viewed["cell_index"] == df.at[idx, "cell_index"])
                   & (viewed["timestamp"] <= df.at[idx, "timestamp"])]
    if not prior.empty:
        df.at[idx, "targetPropertyId"] = prior.iloc[-1]["_url_slug"]
        df.at[idx, "_targetPropertyId_fallback_last_viewed"] = True
n_unresolved = int((fallback & df["targetPropertyId"].isna()).sum())
if n_unresolved:
    print(f"  ! {n_unresolved} reserve event(s) could not be attributed to a property")
df.drop(columns=["_url_slug"], inplace=True)

# Flatten {city -> choice_sets/substitute_pools -> [properties]}.
rows = []
for city, sets in raw_cs.items():
    groups = ([(k, v, False) for k, v in sets.get("choice_sets", {}).items()]
              + [(k, v, True) for k, v in sets.get("substitute_pools", {}).items()]
              if "choice_sets" in sets or "substitute_pools" in sets
              else [(k, v, False) for k, v in sets.items()])
    for set_name, props, is_sub in groups:
        for p in props:
            rows.append({**p, "_city": city, "_city_key": p.get("city_slug") or derive_city_key(p.get("city_search") or city),
                         "_choice_set": set_name, "_is_substitute": is_sub})
cs = pd.DataFrame(rows)

merged = pd.merge(df, cs, left_on=["targetPropertyId", "_city_key"], right_on=["property_slug", "_city_key"],
                  how="outer", suffixes=("", "_choiceset"), indicator=True)
merged["_join_status"] = merged["_merge"].map({"both": "matched", "left_only": "master (extension) only",
                                               "right_only": "child (choice set) only"}).astype(object)
merged.drop(columns=["_merge"], inplace=True)

NOT_JOINABLE_TYPES = {"visibility", "scroll", "tab_focus", "tab_navigation", "manip_check", "page_view",
                      "page_ready", "health", "preload", "page_snapshot"}
not_joinable = merged["type"].isin(NOT_JOINABLE_TYPES)
merged.loc[not_joinable, "_join_status"] = "Event TYPE has no property slug, can't be joined"
no_slug = merged["targetPropertyId"].isna() & ~not_joinable & (merged["_join_status"] != "child (choice set) only")
merged.loc[no_slug, "_join_status"] = "Event has no property slug, can't be joined"
merged.loc[merged["isTargetSample"] == "False", "_join_status"] = "Should not exist."

# Cross-tab navigation: oTree / other allowed site / forbidden site.
OTHER_ALLOWED_HOSTS = ["localhost", "127.0.0.1", "google.com", "google.fr", "wikipedia.org", "wikimedia.org",
                       "sncf.com", "sncf-connect.com", "sncf", "openstreetmap.org"]


def host_matches(host, root):
    return bool(host) and bool(root) and (host == root or host.endswith("." + root))


def host_of(url):
    try:
        return (urlparse(url).hostname or "") if isinstance(url, str) else ""
    except ValueError:
        return ""


cross_tab = merged["type"].isin({"tab_focus", "tab_navigation"})
hosts = merged["url"].apply(host_of)
otree_path = re.compile(r"/p/[^/]+/")
candidates = []
for url, host in zip(merged.loc[cross_tab, "url"], hosts[cross_tab]):
    if not host or host_matches(host, "booking.com") or any(host_matches(host, a) for a in OTHER_ALLOWED_HOSTS):
        continue
    if otree_path.search(urlparse(url).path or ""):
        labels = host.split(".")
        candidates.append(".".join(labels[-2:]) if len(labels) >= 2 else host)
OTREE_ROOT = Counter(candidates).most_common(1)[0][0] if candidates else ""
is_booking = hosts.apply(lambda h: host_matches(h, "booking.com"))
is_otree = hosts.apply(lambda h: host_matches(h, OTREE_ROOT)) if OTREE_ROOT else pd.Series(False, index=merged.index)
is_allowed = hosts.apply(lambda h: any(host_matches(h, a) for a in OTHER_ALLOWED_HOSTS))
merged.loc[cross_tab & ~is_booking & is_otree, "_join_status"] = "OTree"
merged.loc[cross_tab & ~is_booking & ~is_otree & is_allowed, "_join_status"] = "Autres sites permis que booking.com"
merged.loc[cross_tab & ~is_booking & ~is_otree & ~is_allowed, "_join_status"] = "Sites interdites"


def dom_visibility(row):
    if row["type"] in ("hover", "click", "reserve"):
        return "visible (interaction implies the element was rendered)"
    if row["type"] == "viewport":
        return {"True": "visible (in viewport)",
                "False": "not in viewport (off-screen or CSS-hidden — can't distinguish)"}.get(row["isIntersecting"], "")
    return ""


merged["_dom_visibility"] = merged.apply(dom_visibility, axis=1)

COLS_FIRST = ["participant_code", "_join_status", "_dom_visibility", "timestamp", "datetime_local", "elapsed", "type",
              "cell_index", "cell_city_key", "cell_checkin", "cell_list_index", "cell_thumb",
              "targetPropertyId", "_targetPropertyId_backfilled", "property_slug", "_city_key", "_city",
              "_choice_set", "_is_substitute", "property_id", "cluster", "preferred", "dest_id", "city_search",
              "kind", "isTargetSample", "durationMs", "trigger", "ratio", "isIntersecting", "setupMs", "afterPreload",
              "scrollDepthPercent", "state", "source", "tab_event", "tabId", "windowId", "displayedPrice",
              "selectedRooms", "cellIndex", "page", "ok", "thumb", "visiblePouceCount", "totalPouceCount",
              "shownCardCount", "targetTestId", "values"]
COLS_LAST = ["targetText", "metadata", "referrer", "visiblePropertyIds", "failures", "checks", "url"]
cols = list(merged.columns)
middle = sorted(c for c in cols if c not in COLS_FIRST and c not in COLS_LAST)
merged = merged[[c for c in COLS_FIRST if c in cols] + middle + [c for c in COLS_LAST if c in cols]]
merged.to_csv(inputs_dir / "extension_joined_raw.csv", index=False, encoding="utf-8")

KEEP = {"matched", "Event TYPE has no property slug, can't be joined",
        "Event has no property slug, can't be joined", "master (extension) only"}
clean = merged[merged["_join_status"].isin(KEEP)]
clean.to_csv(inputs_dir / "extension_joined.csv", index=False, encoding="utf-8")

n_violations = int((merged["_join_status"] == "Should not exist.").sum())
if n_violations:
    print(f"  ! {n_violations} event(s) on booking.com pages outside the assigned design (protocol violation)")
print(f"{len(clean)} of {len(merged)} rows kept -> extension_joined.csv")
