# AUTHOR: Calvin Pan
# DATE CREATED: 9 June 2026
# DATE LAST MODIFIED: 15 June 2026
# PURPOSE: Join extension_converted.csv (from outputs/) with a choice-sets JSON
#   (from config/, or inputs/, e.g. choice_sets.json) on the primary key
#   (property_slug, city) / (targetPropertyId, city), validate the merge, and
#   annotate rows. The choice-sets JSON is flattened from
#   {city → choice_set_N → [properties]} into a flat table before the join.
# OUTPUTS: extension_joined.csv (in outputs/)
#   Full outer join of both tables, plus a `_join_status` column.

import sys
import json
import re
import unicodedata
from pathlib import Path
from urllib.parse import urlparse, parse_qs
import pandas as pd

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

BASE_DIR    = Path(__file__).parent.parent          # analyse/
PLUGIN_DIR  = BASE_DIR.parent                       # booking_plugin/
INPUTS_DIR  = BASE_DIR / "inputs"
OUTPUTS_DIR = BASE_DIR / "outputs"
CONFIG_CHOICESET_JSON = PLUGIN_DIR / "config" / "choice_sets.json"

EXTENSION_CSV = OUTPUTS_DIR / "extension_converted.csv"
OUTPUT_CSV_RAW = OUTPUTS_DIR / "extension_joined_raw.csv"

# ---------------------------------------------------------------------------
# Prompt for the choice-sets JSON filename — same numbered-list / filename /
# Enter-for-most-recent selection logic as 00a_tracking_converter.py.
# ---------------------------------------------------------------------------

def prompt_for_json_choice(candidates: list, prompt_label: str = "Which file to use?") -> Path:
    if len(candidates) == 1:
        return candidates[0]

    print("JSON files found:")
    for i, p in enumerate(candidates, start=1):
        print(f"  [{i}] {p.name}")
    print("  Press Enter for the most recent.")

    choice = input(f"{prompt_label} [1-{len(candidates)}]: ").strip()
    if choice == "":
        return candidates[0]  # most recently modified
    if choice.isdigit() and 1 <= int(choice) <= len(candidates):
        return candidates[int(choice) - 1]

    # Allow typing the filename directly (with or without .json)
    by_name = {p.name: p for p in candidates}
    if choice in by_name:
        return by_name[choice]
    if not choice.endswith(".json") and f"{choice}.json" in by_name:
        return by_name[f"{choice}.json"]

    sys.exit(f"Invalid choice: {choice!r}")


if len(sys.argv) > 1:
    json_name = sys.argv[1]
    if not json_name.endswith(".json"):
        json_name += ".json"
    CHOICESET_JSON = INPUTS_DIR / json_name
else:
    use_config = input(
        f"Utiliser le fichier choice-sets de config/ ({CONFIG_CHOICESET_JSON}) ? [O/n]: "
    ).strip().lower()
    if use_config in ("", "o", "oui", "y", "yes"):
        if not CONFIG_CHOICESET_JSON.exists():
            sys.exit(f"Fichier introuvable : {CONFIG_CHOICESET_JSON}")
        CHOICESET_JSON = CONFIG_CHOICESET_JSON
    else:
        candidates = sorted(INPUTS_DIR.glob("*.json"), key=lambda p: p.stat().st_mtime, reverse=True)
        if not candidates:
            sys.exit(f"No JSON files found in {INPUTS_DIR}. Put your choice-sets file there or pass a path as argument.")
        CHOICESET_JSON = prompt_for_json_choice(candidates, "Choice-sets JSON to use?")

# ---------------------------------------------------------------------------
# Prompt for the oTree host's root domain — used to label tab_focus /
# tab_navigation rows that point at the lab's oTree instance (e.g. a subject
# coming back from a weekend/city selection page). This is lab-specific and
# not hardcoded anywhere in the extension, so it can't be inferred reliably.
# ---------------------------------------------------------------------------

print(
    "\nNom de domaine racine de l'hôte oTree (ex: univ-paris1.fr).\n"
    "  Pour le trouver : ouvrez extension_converted.csv (sortie de 00a) et repérez\n"
    "  une ligne de type tab_navigation ou tab_focus dont l'URL contient '/p/<id_sujet>/'\n"
    "  (le chemin oTree) — le nom de domaine de cette URL est l'hôte oTree.\n"
    "  Vous pouvez aussi demander l'URL d'accès oTree à l'administrateur de la session.\n"
    "  Laissez vide pour ne pas distinguer les événements oTree des autres sites permis."
)
OTREE_ROOT = input("Domaine racine oTree : ").strip().lower()

# ---------------------------------------------------------------------------
# Load
# ---------------------------------------------------------------------------

if not EXTENSION_CSV.exists():
    sys.exit(f"extension_converted.csv not found at {EXTENSION_CSV}. Run 00a first.")

if not CHOICESET_JSON.exists():
    sys.exit(f"Choice-sets file not found: {CHOICESET_JSON}")

print(f"Loading events:    {EXTENSION_CSV}")
print(f"Loading choiceset: {CHOICESET_JSON}")

df_events = pd.read_csv(EXTENSION_CSV, dtype=str)

with open(CHOICESET_JSON, encoding="utf-8") as f:
    raw_cs = json.load(f)

# ---------------------------------------------------------------------------
# City key helper — a property_slug alone is NOT a safe primary key: the same
# slug can exist in two different cities' choice sets (e.g. "cute-hotel-duplex"
# in both Nice and Le Treport). We disambiguate by also joining on city, derived
# the same way as the extension's deriveCityKey() (src/shared/choice-sets.ts):
# lowercase, strip accents, non-alnum runs → single dash, trim dashes.
# ---------------------------------------------------------------------------

def derive_city_key(text) -> str:
    if not text or not isinstance(text, str):
        return ""
    text = unicodedata.normalize("NFKD", text)
    text = "".join(c for c in text if not unicodedata.combining(c))
    text = text.lower()
    text = re.sub(r"[^a-z0-9]+", "-", text)
    return text.strip("-")


# Mirrors CITY_META in src/shared/choice-sets.ts — used as a fallback when the
# URL has no `ss` param (hotel-detail pages carry only `dest_id`, no search box).
DEST_ID_TO_CITY = {
    -1407760: "annecy",
    -1408052: "arcachon",
    -1416533: "cannes",
    -1435362: "la-ciotat",
    -1446308: "le-treport",
    -1454990: "nice",
    -1470743: "sete",
}


def extract_city_from_url(url) -> str:
    """
    Resolve a Booking URL to a normalized city key.
    Search-results pages carry `ss` (search box text); hotel-detail pages have
    no `ss` but carry `dest_id`, resolved via DEST_ID_TO_CITY.
    """
    if not isinstance(url, str) or not url:
        return ""
    try:
        qs = parse_qs(urlparse(url).query)
    except ValueError:
        return ""

    ss = qs.get("ss", [None])[0]
    if ss:
        return derive_city_key(ss)

    dest_id = qs.get("dest_id", [None])[0]
    if dest_id is not None:
        try:
            return DEST_ID_TO_CITY.get(int(dest_id), "")
        except ValueError:
            return ""

    return ""


df_events["_city_key"] = df_events["url"].apply(extract_city_from_url)

# ---------------------------------------------------------------------------
# Backfill targetPropertyId from the URL for hotel-detail-page events that
# have no `property-card` ancestor to extract it from — not just reserve /
# more_info, but ANY click/hover with no property reference (reviews, share
# button, FAQ accordion, map POI, language selector, ...): the detail page
# shows a single hotel full-screen, no card markup anywhere on it; cf.
# observer.ts's `target.closest('[data-testid="property-card"]')`, which is a
# search-results-page concept that simply doesn't exist on a hotel page. The
# slug is the last path segment before the locale suffix:
# /hotel/fr/<slug>.fr.html — same regex the extension itself uses in
# matchCellBySlug() (src/shared/choice-sets.ts). We don't restrict by `kind`
# or `type`: the regex only ever matches on a genuine hotel-detail URL, so it
# naturally leaves search-results-page clicks (list/map toggle, header nav,
# filters, ...) alone without forcing a match.
# ---------------------------------------------------------------------------

HOTEL_SLUG_RE = re.compile(r"/hotel/[^/]+/([^.]+)")


def slug_from_url(url) -> str:
    if not isinstance(url, str) or not url:
        return ""
    m = HOTEL_SLUG_RE.search(urlparse(url).path)
    return m.group(1) if m else ""


# Restricted to types whose schema can carry a targetPropertyId (click, hover,
# viewport, reserve) — scroll/visibility/page_view/... never reference a
# single property by design, even when they happen to fire on a hotel page.
PROPERTY_AWARE_TYPES = {"click", "hover", "viewport", "reserve"}

needs_backfill = df_events["targetPropertyId"].isna() & df_events["type"].isin(PROPERTY_AWARE_TYPES)
backfilled_slugs = df_events.loc[needs_backfill, "url"].apply(slug_from_url)
df_events.loc[needs_backfill, "targetPropertyId"] = backfilled_slugs.replace("", pd.NA)
df_events["_targetPropertyId_backfilled"] = False
df_events.loc[needs_backfill & backfilled_slugs.ne(""), "_targetPropertyId_backfilled"] = True

n_backfilled = int(df_events["_targetPropertyId_backfilled"].sum())
if n_backfilled:
    print(f"Backfilled targetPropertyId from URL for {n_backfilled} row(s) with no "
          f"property-card in the DOM (hotel-detail-page events: reserve, more_info, "
          f"reviews, share, FAQ, map POI, ...)")

# ---------------------------------------------------------------------------
# Flatten {city → choice_set_N → [properties]} → flat DataFrame
# Adds _city, _city_key, _choice_set and _is_substitute columns to track origin.
#
# Supports two JSON shapes:
#   - legacy/flat:   {city: {choice_set_N: [properties]}}
#   - with-substitutes (nested): {city: {"choice_sets": {choice_set_N: [...]},
#                                          "substitute_pools": {pool_name: [...]}}}
# ---------------------------------------------------------------------------

def iter_property_sets(sets: dict):
    """Yield (set_name, properties, is_substitute) for one city's blob,
    regardless of which of the two JSON shapes above it uses."""
    if "choice_sets" in sets or "substitute_pools" in sets:
        for set_name, properties in sets.get("choice_sets", {}).items():
            yield set_name, properties, False
        for pool_name, properties in sets.get("substitute_pools", {}).items():
            yield pool_name, properties, True
    else:
        for set_name, properties in sets.items():
            yield set_name, properties, False


rows = []
for city, sets in raw_cs.items():
    for set_name, properties, is_substitute in iter_property_sets(sets):
        for prop in properties:
            city_key = prop.get("city_slug") or derive_city_key(prop.get("city_search") or city)
            rows.append({
                **prop,
                "_city": city,
                "_city_key": city_key,
                "_choice_set": set_name,
                "_is_substitute": is_substitute,
            })

df_choiceset = pd.DataFrame(rows)
n_substitutes = int(df_choiceset["_is_substitute"].sum())
print(f"  Flattened choice-sets: {len(df_choiceset)} rows "
      f"({df_choiceset['property_slug'].nunique()} unique slugs, "
      f"{len(raw_cs)} cities, "
      f"{df_choiceset['_choice_set'].nunique()} sets/pools per city, "
      f"{n_substitutes} substitute-pool row(s))")

# ---------------------------------------------------------------------------
# Sanity checks
# ---------------------------------------------------------------------------

if "targetPropertyId" not in df_events.columns:
    sys.exit("extension_converted.csv has no 'targetPropertyId' column.")
if "property_slug" not in df_choiceset.columns:
    sys.exit(f"{CHOICESET_JSON.name} properties have no 'property_slug' field.")

dup_keys = df_choiceset[["property_slug", "_city_key"]].dropna().duplicated()
if dup_keys.any():
    print(f"Warning: {dup_keys.sum()} duplicate (property_slug, city) pair(s) in the "
          "choice-sets JSON — matched event rows will be duplicated accordingly.")

cross_city_dups = (
    df_choiceset.dropna(subset=["property_slug"])
    .groupby("property_slug")["_city_key"]
    .nunique()
)
n_cross_city = (cross_city_dups > 1).sum()
if n_cross_city:
    print(f"Note: {n_cross_city} property_slug(s) appear under more than one city "
          "in the choice-sets JSON — disambiguated by joining on (slug, city).")

overlapping = sorted(set(df_events.columns) & set(df_choiceset.columns) - {"property_slug", "_city_key"})
if overlapping:
    print(f"Note: overlapping columns will get a _choiceset suffix: {overlapping}")

# ---------------------------------------------------------------------------
# Full outer join: (targetPropertyId, _city_key) (events) ↔ (property_slug, _city_key)
# (choiceset). Joining on city as well as slug avoids cross-city slug collisions.
# ---------------------------------------------------------------------------

df_merged = pd.merge(
    df_events,
    df_choiceset,
    left_on=["targetPropertyId", "_city_key"],
    right_on=["property_slug", "_city_key"],
    how="outer",
    suffixes=("", "_choiceset"),
    indicator=True,
)

df_merged["_join_status"] = df_merged["_merge"].map(
    {
        "both":       "matched",
        "left_only":  "master (extension) only",
        "right_only": "child (choice set) only",
    }
).astype(object)
df_merged.drop(columns=["_merge"], inplace=True)

# ---------------------------------------------------------------------------
# Event types that never carry a property reference (no targetPropertyId field
# in their schema — cf. src/shared/types.ts) can never join, by construction.
# Flag them with a dedicated status instead of the generic "master only" label,
# so they're not counted as join failures in the validation matrix below.
# ---------------------------------------------------------------------------
inp
NOT_JOINABLE_TYPES = {
    "visibility", "scroll", "tab_focus", "tab_navigation", "manip_check",
    "page_view", "page_ready", "health", "preload", "page_snapshot",
}

not_joinable_mask = df_merged["type"].isin(NOT_JOINABLE_TYPES)
df_merged.loc[not_joinable_mask, "_join_status"] = "Event TYPE has no property slug, can't be joined"

# ---------------------------------------------------------------------------
# Instance-level absence — distinct from the type-level case above. These are
# click/hover/viewport/reserve rows (types that CAN carry a targetPropertyId)
# where THIS particular row simply has none — even after the URL backfill
# above (e.g. a click on the "Liste"/"Mosaïque" view toggle, header nav, or
# any UI element with no property-card ancestor and no hotel-detail URL to
# backfill from). Distinguished from "master (extension) only", which is
# reserved for rows that DO have a targetPropertyId but it doesn't match any
# choice-set hotel — a genuine join failure, not an absent slug.
# ---------------------------------------------------------------------------

# Excludes right-only rows ("child (choice set) only" — a choice-set hotel
# with NO matching event at all, not an event missing its slug; every column
# from df_events, including `type`, is NaN for these by construction of the
# outer join).
no_slug_instance_mask = (
    df_merged["targetPropertyId"].isna()
    & ~not_joinable_mask
    & (df_merged["_join_status"] != "child (choice set) only")
)
df_merged.loc[no_slug_instance_mask, "_join_status"] = "Event has no property slug, can't be joined"

# ---------------------------------------------------------------------------
# Protocol violations — `isTargetSample` is computed live by the extension
# itself (isTargetSample() in src/shared/config.ts = resolveSlotForUrl() !=
# null): it is True only when the URL's (city, checkin) matches one of the
# subject's 12 assigned design cells. False means the subject reached a
# booking.com page OUTSIDE their assigned cities/weekends — e.g. searching a
# city never assigned to them (England, ...), or the same city on a date that
# isn't one of their 4 weekends. By design this should never happen (the
# StimulusFilter and the navigation guard exist specifically to prevent it);
# any such row is the result of an extension malfunction or a manual escape.
# Overrides ANY previous status (incl. the two "no property slug" statuses)
# for ALL booking.com event types — it never applies to cross-tab navigation
# (tab_focus/tab_navigation have no isTargetSample field, untouched here).
# ---------------------------------------------------------------------------

should_not_exist_mask = df_merged["isTargetSample"] == "False"
df_merged.loc[should_not_exist_mask, "_join_status"] = "Should not exist."

# ---------------------------------------------------------------------------
# Cross-tab navigation (tab_focus / tab_navigation) labeling — these are the
# only event types that can point OUTSIDE booking.com (chrome.tabs.onActivated /
# onUpdated track every tab, cf. src/background/service-worker.ts E14). Split
# the generic "no property slug" status into oTree / other allowed site /
# forbidden site (incl. the blocked.html redirect screen and browser-internal
# pages), mirroring the extension's own ALLOWED_HOSTS allowlist.
# ---------------------------------------------------------------------------

# Mirrors ALLOWED_HOSTS in src/background/service-worker.ts, minus booking.com
# (booking.com navigation keeps the default "no property slug" status — it's
# the expected/on-target site, not an "other allowed site").
OTHER_ALLOWED_HOSTS = [
    "localhost", "127.0.0.1",
    "google.com", "google.fr",
    "wikipedia.org", "wikimedia.org",
    "sncf.com", "sncf-connect.com", "sncf",
    "openstreetmap.org",
]


def host_matches(host: str, root: str) -> bool:
    return bool(host) and bool(root) and (host == root or host.endswith("." + root))


def host_of(url) -> str:
    if not isinstance(url, str) or not url:
        return ""
    try:
        return urlparse(url).hostname or ""
    except ValueError:
        return ""


CROSS_TAB_TYPES = {"tab_focus", "tab_navigation"}
cross_tab_mask = df_merged["type"].isin(CROSS_TAB_TYPES)
hosts = df_merged["url"].apply(host_of)

is_booking = hosts.apply(lambda h: host_matches(h, "booking.com"))
is_otree = hosts.apply(lambda h: host_matches(h, OTREE_ROOT)) if OTREE_ROOT else pd.Series(False, index=df_merged.index)
is_other_allowed = hosts.apply(lambda h: any(host_matches(h, allowed) for allowed in OTHER_ALLOWED_HOSTS))

otree_mask          = cross_tab_mask & ~is_booking & is_otree
other_allowed_mask  = cross_tab_mask & ~is_booking & ~is_otree & is_other_allowed
forbidden_mask      = cross_tab_mask & ~is_booking & ~is_otree & ~is_other_allowed

df_merged.loc[otree_mask, "_join_status"] = "OTree"
df_merged.loc[other_allowed_mask, "_join_status"] = "Autres sites permis que booking.com"
df_merged.loc[forbidden_mask, "_join_status"] = "Sites interdites"

# ---------------------------------------------------------------------------
# DOM-visibility signal — distinguishes, among property-referencing events
# that fail to join a choice-set hotel ("master only"), genuinely visible
# interactions from ones that may have happened on an off-screen/CSS-hidden
# card. The StimulusFilter hides non-whitelisted cards with
# `display:none !important` (src/content/stimulus-filter.ts) — a display:none
# element is not rendered, so the browser can never dispatch a mouseover/click
# on it. Therefore hover/click/reserve events are ALWAYS proof the element was
# actually visible at the time (real interaction, e.g. a "similar hotels"
# widget on a hotel-detail page, or off-script navigation) — never CSS-hidden.
# For viewport events, `isIntersecting` (from a real IntersectionObserver)
# tells us directly whether the card was on-screen at that moment.
# ---------------------------------------------------------------------------

def dom_visibility(row) -> str:
    if row["type"] in ("hover", "click", "reserve"):
        return "visible (interaction implies the element was rendered)"
    if row["type"] == "viewport":
        if row["isIntersecting"] == "True":
            return "visible (in viewport)"
        if row["isIntersecting"] == "False":
            return "not in viewport (off-screen or CSS-hidden — can't distinguish)"
    return ""

df_merged["_dom_visibility"] = df_merged.apply(dom_visibility, axis=1)

# ---------------------------------------------------------------------------
# Validation: 2×2 matrix — restricted to event types that CAN carry a property
# reference (targetPropertyId), so the match rate isn't diluted by types that
# never join by design.
# ---------------------------------------------------------------------------

def print_validation_matrix(df, n_ev_total, title) -> None:
    n_matched    = (df["_join_status"] == "matched").sum()
    n_left_only  = (df["_join_status"] == "master (extension) only").sum()
    n_right_only = (df["_join_status"] == "child (choice set) only").sum()
    W = 16

    print()
    print(f"  {title}")
    print(f"  {'':30s}  {'In choiceset':>{W}}  {'Not in choiceset':>{W}}  {'Total':>{W}}")
    print(f"  {'─'*30}  {'─'*W}  {'─'*W}  {'─'*W}")
    print(f"  {'In extension_converted':30s}  {n_matched:>{W},}  {n_left_only:>{W},}  {n_ev_total:>{W},}")
    print(f"  {'Not in extension_converted':30s}  {n_right_only:>{W},}  {'—':>{W}}  {n_right_only:>{W},}")
    print(f"  {'─'*30}  {'─'*W}  {'─'*W}  {'─'*W}")
    print(f"  {'Total':30s}  {n_matched + n_right_only:>{W},}  {n_left_only:>{W},}  {len(df):>{W},}")
    print()

    match_pct = 100 * n_matched / n_ev_total if n_ev_total else 0
    print(f"  {n_matched:,} of {n_ev_total:,} event rows matched ({match_pct:.1f}%)")
    if n_left_only:
        print(f"  {n_left_only:,} event row(s) unmatched  → _join_status = master (extension) only")
    if n_right_only:
        print(f"  {n_right_only:,} choiceset row(s) unmatched → _join_status = child (choice set) only")


df_joinable = df_merged[~not_joinable_mask]
n_not_joinable = not_joinable_mask.sum()
n_ev = len(df_events) - df_events["type"].isin(NOT_JOINABLE_TYPES).sum()

print()
print(f"  ({n_not_joinable:,} event row(s) excluded from the matrix below — "
      f"event type has no property field by design)")
print_validation_matrix(df_joinable, n_ev, "Join validation (rows) — joinable event types only, raw")

print()
print("  All events by _join_status (raw):")
for status, n in df_merged["_join_status"].value_counts().items():
    print(f"    {status:60s} {n:6,d}")

# ---------------------------------------------------------------------------
# Column ordering: compact/analytical columns first, long/blob columns last
# ---------------------------------------------------------------------------

COLS_FIRST = [
    # subject identity (from extension_converted.csv's participant_code, set in 00a)
    "participant_code",
    # join status + identity
    "_join_status",
    "_dom_visibility",
    # timing
    "timestamp", "datetime_local", "elapsed",
    # event type
    "type",
    # cell/weekend identity (set in 00a, present on every event regardless of type)
    "cell_index", "cell_city_key", "cell_checkin", "cell_list_index", "cell_thumb",
    # property (join key + choice-set fields)
    "targetPropertyId", "_targetPropertyId_backfilled", "property_slug",
    "_city_key", "_city", "_choice_set", "_is_substitute",
    "property_id", "cluster", "preferred", "dest_id", "city_search",
    # interaction
    "kind", "isTargetSample",
    "durationMs", "trigger",
    "ratio", "isIntersecting",
    # page_ready
    "setupMs", "afterPreload",
    # scroll
    "scrollDepthPercent",
    # visibility / tab
    "state", "source",
    "tab_event", "tabId", "windowId",
    # reserve
    "displayedPrice", "selectedRooms", "cellIndex",
    # health / manip_check
    "page", "ok", "thumb",
    "visiblePouceCount", "totalPouceCount", "shownCardCount",
    # click
    "targetTestId",
    # filter
    "values",
]

# Long / blobby columns go at the end
COLS_LAST = [
    "targetText", "metadata",
    "referrer",
    "visiblePropertyIds",
    "failures", "checks",
    "url",
]

# Anything not explicitly placed goes in the middle (alphabetical)
all_cols = list(df_merged.columns)
middle = sorted(c for c in all_cols if c not in COLS_FIRST and c not in COLS_LAST)
ordered = (
    [c for c in COLS_FIRST if c in all_cols]
    + [c for c in middle if c not in COLS_FIRST and c not in COLS_LAST]
    + [c for c in COLS_LAST if c in all_cols]
)

df_merged = df_merged[ordered]

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

OUTPUTS_DIR.mkdir(parents=True, exist_ok=True)

df_merged.to_csv(OUTPUT_CSV_RAW, index=False, encoding="utf-8")
print(f"\nSaved {len(df_merged):,} rows → {OUTPUT_CSV_RAW}")

# ---------------------------------------------------------------------------
# Final, clean output: keep rows that are either a genuine match ("matched"),
# non-joinable by design at the type level ("Event TYPE has no property
# slug..."), non-joinable at the instance level ("Event has no property
# slug..." — e.g. clicks with no property-card ancestor and no hotel-detail
# URL to backfill from), or unmatched but still a real on-target interaction
# ("master (extension) only" — a targetPropertyId that doesn't match any
# choice-set hotel). Every other status (child only, OTree / other allowed
# site / forbidden site, protocol violations) is noise relative to the
# choice-set join and is dropped here — it stays fully visible in
# extension_joined_raw.csv for audit.
# ---------------------------------------------------------------------------

KEEP_STATUSES = {
    "matched",
    "Event TYPE has no property slug, can't be joined",
    "Event has no property slug, can't be joined",
    "master (extension) only",
}

OUTPUT_CSV = OUTPUTS_DIR / "extension_joined.csv"

df_clean = pd.read_csv(OUTPUT_CSV_RAW, dtype=str)

keep_mask = df_clean["_join_status"].isin(KEEP_STATUSES)

n_before = len(df_clean)
dropped_counts = df_clean.loc[~keep_mask, "_join_status"].value_counts()
df_clean = df_clean[keep_mask]
n_dropped = n_before - len(df_clean)

print(f"\nDropped {n_dropped:,} row(s) not in {sorted(KEEP_STATUSES)}:")
for status, n in dropped_counts.items():
    print(f"    {status:60s} {n:6,d}")

clean_joinable = df_clean[~df_clean["type"].isin(NOT_JOINABLE_TYPES)]
n_ev_clean = len(df_clean) - df_clean["type"].isin(NOT_JOINABLE_TYPES).sum()

print_validation_matrix(
    clean_joinable, n_ev_clean,
    "Join validation (rows) — joinable event types only, final",
)

df_clean.to_csv(OUTPUT_CSV, index=False, encoding="utf-8")
print(f"\nSaved {len(df_clean):,} rows → {OUTPUT_CSV}")

# ---------------------------------------------------------------------------
# Loud final warning: rows labeled "Should not exist." mean the subject reached
# a booking.com page outside their assigned (city, weekend) design — a
# protocol violation that should be impossible if the extension worked
# perfectly. Surface this prominently so it can't be missed at the bottom of
# a long run.
# ---------------------------------------------------------------------------

n_violations = int((df_merged["_join_status"] == "Should not exist.").sum())
if n_violations:
    violation_rows = df_merged[df_merged["_join_status"] == "Should not exist."]
    by_type = violation_rows["type"].value_counts()
    by_city = violation_rows["_city_key"].value_counts() if "_city_key" in violation_rows.columns else None

    banner = "!" * 78
    print(f"\n{banner}")
    print(f"!!  ALERTE : {n_violations:,} événement(s) « Should not exist. »")
    print( "!!  Le sujet a atteint des pages booking.com HORS de son design assigné")
    print( "!!  (ville/week-end non attribué) — normalement impossible si l'extension")
    print( "!!  fonctionne correctement. Vérifier le déroulement de la session.")
    print(f"{banner}")
    print("  Par type d'événement :")
    for t, n in by_type.items():
        print(f"    {t:18s} {n:5,d}")
    if by_city is not None:
        print("  Par ville (_city_key) :")
        for c, n in by_city.items():
            print(f"    {c or '(inconnue)':18s} {n:5,d}")
    print(banner)
