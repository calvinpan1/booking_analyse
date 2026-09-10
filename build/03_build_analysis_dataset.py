"""Build the listing-level analysis dataset (subject x city x weekend x listing).

Usage: python 03_build_analysis_dataset.py RAW_DIR INPUTS_DIR CHOICE_SETS_JSON
Reads INPUTS_DIR/extension_converted.csv and extension_joined.csv, the oTree wide
export (RAW_DIR/**/all_apps_wide*.csv) and PageTimes export (RAW_DIR/**/PageTimes*.csv);
writes INPUTS_DIR/analysis_dataset.csv.
"""
import json
import re
import sys
import unicodedata
from pathlib import Path
from urllib.parse import urlparse

import numpy as np
import pandas as pd

raw_dir, inputs_dir, choice_sets_json = Path(sys.argv[1]), Path(sys.argv[2]), Path(sys.argv[3])


def newest(pattern):
    files = sorted(raw_dir.rglob(pattern), key=lambda p: p.stat().st_mtime, reverse=True)
    return files[0] if files else None


otree_csv = newest("all_apps_wide*.csv")
pagetimes_csv = newest("PageTimes*.csv") or newest("pagetimes*.csv")
if otree_csv is None:
    sys.exit(f"No all_apps_wide*.csv under {raw_dir}")
if pagetimes_csv is None:
    print("  ! No PageTimes*.csv found: session durations will be missing")

df_otree = pd.read_csv(otree_csv, dtype=str, encoding="utf-8-sig")
df_conv = pd.read_csv(inputs_dir / "extension_converted.csv", dtype=str)
df_joined = pd.read_csv(inputs_dir / "extension_joined.csv", dtype=str)
df_pt = pd.read_csv(pagetimes_csv, dtype=str) if pagetimes_csv else None
with open(choice_sets_json, encoding="utf-8") as f:
    raw_cs = json.load(f)


def derive_city_key(text):
    if not isinstance(text, str) or not text:
        return ""
    text = "".join(c for c in unicodedata.normalize("NFKD", text) if not unicodedata.combining(c))
    return re.sub(r"[^a-z0-9]+", "-", text.lower()).strip("-")


# Roster: canonical sets plus substitute pools (a substitute has no fixed choice_set).
rows = []
for city, sets in raw_cs.items():
    for set_name, props in (sets["choice_sets"] if "choice_sets" in sets else sets).items():
        for p in props:
            rows.append({**p, "city_slug": p.get("city_slug") or derive_city_key(city), "choice_set": set_name, "_is_substitute": False})
    for _pool, props in sets.get("substitute_pools", {}).items():
        for p in props:
            rows.append({**p, "city_slug": p.get("city_slug") or derive_city_key(city), "choice_set": None, "_is_substitute": True})
df_cs = pd.DataFrame(rows)
df_canonical = df_cs[~df_cs["_is_substitute"]]
prop_lookup = df_cs.drop_duplicates(subset=["city_slug", "property_slug"])

# A. Weekend roster per subject, from the cell tags every event carries.
roster_cols = ["participant_code", "cell_index", "cell_city_key", "cell_list_index", "cell_checkin", "cell_thumb"]
roster = df_conv[roster_cols].dropna(subset=["cell_index"]).drop_duplicates()
roster["cell_index"] = roster["cell_index"].astype(int)
roster["cell_list_index"] = roster["cell_list_index"].astype(int)
roster["choice_set"] = "choice_set_" + (roster["cell_list_index"] + 1).astype(str)
n_expected = roster["participant_code"].nunique() * 12
if len(roster) != n_expected:
    print(f"  ! {len(roster)} tracked weekends for {roster['participant_code'].nunique()} subjects (expected {n_expected})")


# B. Listings actually displayed: the last preload event's effectiveWhitelist; canonical roster as fallback.
def parse_list(s):
    try:
        return json.loads(s) if isinstance(s, str) and s.strip() else []
    except json.JSONDecodeError:
        return []


preload = df_conv[df_conv["type"] == "preload"].copy()
preload["cell_index"] = pd.to_numeric(preload["cell_index"], errors="coerce")
preload["timestamp"] = pd.to_numeric(preload["timestamp"], errors="coerce")
preload = preload.dropna(subset=["cell_index", "participant_code"])
preload["cell_index"] = preload["cell_index"].astype(int)
preload_last = (preload.sort_values(["participant_code", "cell_index", "timestamp"])
                .drop_duplicates(subset=["participant_code", "cell_index"], keep="last"))
displayed = []
for _, r in preload_last.iterrows():
    slugs = parse_list(r.get("effectiveWhitelist"))
    replaced_by = {s["substituteSlug"]: s["missingSlug"] for s in parse_list(r.get("substitutions"))
                   if s.get("reason") == "selected" and s.get("substituteSlug")}
    for slug in slugs:
        displayed.append({"participant_code": r["participant_code"], "cell_index": r["cell_index"],
                          "property_slug": slug, "replaced_property_slug": replaced_by.get(slug)})
df_displayed = pd.DataFrame(displayed, columns=["participant_code", "cell_index", "property_slug", "replaced_property_slug"])
has_preload = roster.apply(lambda r: (r["participant_code"], r["cell_index"]) in
                           set(zip(df_displayed["participant_code"], df_displayed["cell_index"])), axis=1)
if (~has_preload).sum():
    print(f"  ! {(~has_preload).sum()} weekend(s) without a preload event: canonical roster assumed")
skel_cols = ["participant_code", "cell_index", "cell_city_key", "cell_list_index", "cell_checkin", "cell_thumb",
             "choice_set", "property_slug", "replaced_property_slug"]
skel_tracked = roster[has_preload].merge(df_displayed, on=["participant_code", "cell_index"], how="left")
skel_fallback = roster[~has_preload].merge(df_canonical, left_on=["cell_city_key", "choice_set"],
                                            right_on=["city_slug", "choice_set"], how="left")
skel_fallback["replaced_property_slug"] = pd.NA
skeleton = (pd.concat([skel_tracked[skel_cols], skel_fallback[skel_cols]], ignore_index=True)
            .merge(prop_lookup.drop(columns=["choice_set"]), left_on=["cell_city_key", "property_slug"],
                   right_on=["city_slug", "property_slug"], how="left")
            .rename(columns={"_is_substitute": "is_substitute_listing"}))
skeleton["is_substitute_listing"] = skeleton["is_substitute_listing"].fillna(False)

# C. Event-derived measures.
ev = df_joined[df_joined["_join_status"] != "child (choice set) only"].copy()
ev["timestamp"] = pd.to_numeric(ev["timestamp"], errors="coerce")
ev["cell_index"] = pd.to_numeric(ev["cell_index"], errors="coerce")
ev = ev.dropna(subset=["timestamp", "cell_index"])
ev["cell_index"] = ev["cell_index"].astype(int)
ev = ev.sort_values(["participant_code", "cell_index", "timestamp"])

HOTEL_SLUG_RE = re.compile(r"/hotel/[^/]+/([^.]+)")


def slug_from_url(url):
    if not isinstance(url, str) or not url:
        return ""
    m = HOTEL_SLUG_RE.search(urlparse(url).path)
    return m.group(1) if m else ""


is_pv = ev["type"] == "page_view"
ev["_detail_slug"] = ""
ev.loc[is_pv, "_detail_slug"] = ev.loc[is_pv, "url"].apply(slug_from_url)
ev["_is_detail_page_view"] = is_pv & (ev["_detail_slug"] != "")
ev["_eff_slug"] = ev["property_slug"].fillna("")
ev.loc[ev["_is_detail_page_view"], "_eff_slug"] = ev.loc[ev["_is_detail_page_view"], "_detail_slug"]

# Dwell on a detail page ends at the next event pointing away from it (another slug, the
# page's own visibility=hidden, or the reserve click). Booking opens detail pages in new tabs.
ev["_ev_slug"] = ev["url"].apply(slug_from_url)
grp = ev.groupby(["participant_code", "cell_index"])
codes, levels = pd.factorize(ev["_detail_slug"].where(ev["_detail_slug"] != ""))
codes = pd.Series(codes, index=ev.index).where(lambda c: c >= 0)
last = codes.groupby([ev["participant_code"], ev["cell_index"]]).ffill()
ev["_last_detail_slug"] = pd.Series(np.where(last.notna(), np.asarray(levels)[last.fillna(0).astype(int)], ""), index=ev.index)
state = ev["state"].astype(str) if "state" in ev.columns else pd.Series("", index=ev.index)
same = (ev["_ev_slug"] != "") & (ev["_ev_slug"] == ev["_last_detail_slug"])
leave = ((ev["type"].isin(["page_view", "tab_navigation", "tab_focus"]) & ~same)
         | ((ev["type"] == "visibility") & (state == "hidden") & same)
         | (ev["type"] == "reserve"))
ev["_leave_ts"] = ev["timestamp"].where(leave)
ev["_next_ts"] = grp["_leave_ts"].transform(lambda s: s.shift(-1).bfill())
ev["_dwell_s"] = (ev["_next_ts"] - ev["timestamp"]) / 1000.0
time_on_listing = (ev[ev["_is_detail_page_view"]].groupby(["participant_code", "cell_index", "_eff_slug"])["_dwell_s"].sum()
                   .reset_index().rename(columns={"_eff_slug": "property_slug", "_dwell_s": "time_on_listing_page_seconds"}))

prop_rows = ev[ev["property_slug"].notna() & (ev["property_slug"] != "")]
clicks = (prop_rows[prop_rows["type"] == "click"].groupby(["participant_code", "cell_index", "property_slug"])
          .size().reset_index(name="listing_n_clicks"))
chosen = prop_rows[prop_rows["type"] == "reserve"][["participant_code", "cell_index", "property_slug"]].drop_duplicates()
chosen["listing_chosen"] = True

# Loading time: sum of preload passes; the first preload event is the "cards ready" anchor.
preload_ev = (ev[ev["type"] == "preload"].sort_values(["participant_code", "cell_index", "timestamp"])
              .drop_duplicates(subset=["participant_code", "cell_index"], keep="first").copy())


def total_loading_ms(row):
    details = row.get("passDetails")
    if isinstance(details, str) and details.strip():
        try:
            return float(sum(d["elapsedMs"] for d in json.loads(details)))
        except (json.JSONDecodeError, KeyError, TypeError):
            pass
    return float(row["elapsedMs"]) if pd.notna(row.get("elapsedMs")) else float("nan")


preload_ev["loading_time_seconds"] = preload_ev.apply(total_loading_ms, axis=1) / 1000.0
loading_time = preload_ev[["participant_code", "cell_index", "loading_time_seconds"]]

# Decision time: cards ready -> reserve; first interaction (else first page view) when no preload.
key = ["participant_code", "cell_index"]
cards_ready = preload_ev.set_index(key)["timestamp"].rename("_ready")
reserve_ts = ev[ev["type"] == "reserve"].groupby(key)["timestamp"].max().rename("_reserve")
first_inter = ev[ev["type"].isin(["viewport", "hover", "scroll", "click"])].groupby(key)["timestamp"].min().rename("_inter")
first_pv = ev[ev["type"] == "page_view"].groupby(key)["timestamp"].min().rename("_pv")
dt = pd.concat([cards_ready, reserve_ts, first_inter, first_pv], axis=1).reset_index()
fallback = dt["_inter"].fillna(dt["_pv"])
dt["decision_time_imputed"] = dt["_ready"].isna() & fallback.notna() & dt["_reserve"].notna()
dt["decision_time_seconds"] = (dt["_reserve"] - dt["_ready"].fillna(fallback)) / 1000.0
decision_time = dt[key + ["decision_time_seconds", "decision_time_imputed"]]
page_visits = ev[ev["_is_detail_page_view"]].groupby(key).size().reset_index(name="property_page_visits_number")

# D. Assemble.
df = (skeleton.merge(clicks, on=key + ["property_slug"], how="left")
      .merge(chosen, on=key + ["property_slug"], how="left")
      .merge(time_on_listing, on=key + ["property_slug"], how="left")
      .merge(decision_time, on=key, how="left")
      .merge(page_visits, on=key, how="left")
      .merge(loading_time, on=key, how="left"))
df["listing_n_clicks"] = df["listing_n_clicks"].fillna(0).astype(int)
df["listing_clicked"] = df["listing_n_clicks"] > 0
df["listing_chosen"] = df["listing_chosen"].eq(True)
df["property_page_visits_number"] = df["property_page_visits_number"].fillna(0).astype(int)
df = df.merge(df.groupby(key)["property_slug"].nunique().rename("n_hotels_in_choice_set"), on=key, how="left")
df = df.rename(columns={"preferred": "would_be_cued"})
df["cued_weekend"] = df["cell_thumb"] == "P"
df["cued_listing"] = df["cued_weekend"] & (df["would_be_cued"] == True)  # noqa: E712

# E. City-level preference consistency from the clusters of the chosen listings.
chosen_listings = df[df["listing_chosen"]][["participant_code", "cell_city_key", "cell_index", "cell_thumb", "cluster"]]


def consistency(g):
    not_cued = g.loc[g["cell_thumb"] == "A", "cluster"].dropna().tolist()
    cued = g.loc[g["cell_thumb"] == "P", "cluster"].dropna().tolist()
    pref = not_cued[0] == not_cued[1] if len(not_cued) == 2 else pd.NA
    cued_ok = all(c == not_cued[0] for c in cued) if pref is True and cued else pd.NA
    return pd.Series({"preference_consistency": pref, "cued_choice_preference_consistency": cued_ok})


df = df.merge(chosen_listings.groupby(["participant_code", "cell_city_key"]).apply(consistency, include_groups=False).reset_index(),
              on=["participant_code", "cell_city_key"], how="left")

# F0. Session durations from PageTimes (completion times: arrival at a page = completion of the one before).
if df_pt is not None:
    pt = df_pt.copy()
    for c in ["epoch_time_completed", "round_number", "page_index"]:
        pt[c] = pd.to_numeric(pt[c], errors="coerce")
    pt = pt.dropna(subset=["epoch_time_completed", "page_index"])
    span = pt.groupby("participant_code")["epoch_time_completed"].agg(_first="min", _last="max").reset_index()
    span["whole_experiment_time_seconds"] = span["_last"] - span["_first"]

    def before(page_idx, name):
        out = []
        for pcode, idx in page_idx.items():
            prior = pt[(pt["participant_code"] == pcode) & (pt["page_index"] < idx)]
            out.append({"participant_code": pcode, name: prior["epoch_time_completed"].max() if not prior.empty else pd.NA})
        return pd.DataFrame(out, columns=["participant_code", name])

    round1 = pt[(pt["app_name"] == "choice_task_block") & (pt["round_number"] == 1)].groupby("participant_code")["page_index"].min()
    postexp = pt[pt["app_name"] == "postexperiment_block"].groupby("participant_code")["page_index"].min()
    conv_ts = df_conv.copy()
    conv_ts["timestamp"] = pd.to_numeric(conv_ts["timestamp"], errors="coerce")
    last_click = (conv_ts[conv_ts["type"] == "click"].groupby("participant_code")["timestamp"].max() / 1000.0).rename("_last_click").reset_index()
    times = (span[["participant_code", "whole_experiment_time_seconds"]]
             .merge(before(round1, "_w1"), on="participant_code", how="left")
             .merge(before(postexp, "_post"), on="participant_code", how="left")
             .merge(last_click, on="participant_code", how="left"))
    times["choice_task_time_seconds"] = times[["_last_click", "_post"]].min(axis=1, skipna=True) - times["_w1"]
    times = times[["participant_code", "whole_experiment_time_seconds", "choice_task_time_seconds"]]
else:
    times = pd.DataFrame(columns=["participant_code", "whole_experiment_time_seconds", "choice_task_time_seconds"])
df = df.merge(times, on="participant_code", how="left")

# F. Subject-level variables from the oTree export (only these columns are carried over).
subject_cols = {
    "participant.code": "participant_code",
    "participant.clutter_treatment": "clutter_treatment",
    "postexperiment_block.1.player.noticed_thumb": "cue_recognition",
    "postexperiment_block.1.player.noticed_checkmark": "noticed_decoy_checkmark",
    "postexperiment_block.1.player.noticed_badge": "noticed_decoy_badge",
    "postexperiment_block.1.player.visual_complexity": "visual_complexity",
    "postexperiment_block.1.player.nasa_tlx_mental": "nasa_tlx_mental",
    "postexperiment_block.1.player.nasa_tlx_physical": "nasa_tlx_physical",
    "postexperiment_block.1.player.nasa_tlx_temporal": "nasa_tlx_temporal",
    "postexperiment_block.1.player.nasa_tlx_performance": "nasa_tlx_performance",
    "postexperiment_block.1.player.nasa_tlx_effort": "nasa_tlx_effort",
    "postexperiment_block.1.player.nasa_tlx_frustration": "nasa_tlx_frustration",
    "instructions_block.1.player.failed_comprehension_prize": "failed_comprehension_prize",
    "instructions_block.1.player.failed_comprehension_choice_city_weekend": "failed_comprehension_choice_city_weekend",
    "instructions_block.1.player.failed_comprehension_no_cancellation": "failed_comprehension_no_cancellation",
    "postexperiment_block.1.player.gender": "gender",
    "postexperiment_block.1.player.age": "age",
    "postexperiment_block.1.player.student_status": "student_status",
    "postexperiment_block.1.player.household_structure": "household_structure",
    "postexperiment_block.1.player.paris_resident": "paris_resident",
    "postexperiment_block.1.player.booking_familiarity": "booking_familiarity",
    "postexperiment_block.1.player.belief_thumb_quality": "belief_thumb_quality",
    "pretask_block.1.player.accommodation_preference_open": "accommodation_preference_open",
    "postexperiment_block.1.player.choice_process_open": "choice_process_open",
    "postexperiment_block.1.player.belief_thumb_meaning_open": "belief_thumb_meaning_open",
    "postexperiment_block.1.player.thumb_use_open": "thumb_use_open",
    "postexperiment_block.1.player.feedback_open": "feedback_open",
}
missing = [c for c in subject_cols if c not in df_otree.columns]
if missing:
    sys.exit(f"Missing oTree column(s): {missing}")
subj = df_otree[list(subject_cols)].rename(columns=subject_cols)
subj["clutter_high"] = subj["clutter_treatment"] == "O"
df = df.merge(subj, on="participant_code", how="left")
n_unmatched = df.loc[df["clutter_treatment"].isna(), "participant_code"].nunique()
if n_unmatched:
    print(f"  ! {n_unmatched} tracked subject(s) absent from {otree_csv.name}")

# G. Cross-check the tracked chosen cluster against oTree's own record.
n_checked = n_mismatch = 0
for r in range(1, 13):
    col = f"choice_task_block.{r}.player.choice_cluster"
    if col not in df_otree.columns:
        continue
    o = df_otree[["participant.code", col]].rename(columns={"participant.code": "participant_code", col: "_o"})
    o["cell_index"] = r
    cmp = o.merge(chosen_listings.rename(columns={"cluster": "_t"})[["participant_code", "cell_index", "_t"]],
                  on=key, how="inner").dropna(subset=["_o", "_t"])
    n_checked += len(cmp)
    n_mismatch += int((cmp["_o"].astype(float).astype(int) != cmp["_t"].astype(int)).sum())
if n_mismatch:
    print(f"  ! {n_mismatch} of {n_checked} chosen clusters disagree between oTree and the tracked reserve event")

df = df.rename(columns={"cell_city_key": "city", "cell_index": "weekend_number_global", "cell_checkin": "checkin"})
COLUMNS = ["participant_code", "city", "weekend_number_global", "property_slug",
           "clutter_treatment", "clutter_high", "cue_recognition",
           "nasa_tlx_mental", "nasa_tlx_physical", "nasa_tlx_temporal", "nasa_tlx_performance", "nasa_tlx_effort",
           "nasa_tlx_frustration", "visual_complexity", "whole_experiment_time_seconds", "choice_task_time_seconds",
           "gender", "age", "student_status", "household_structure", "paris_resident", "booking_familiarity",
           "belief_thumb_quality", "noticed_decoy_checkmark", "noticed_decoy_badge",
           "accommodation_preference_open", "choice_process_open", "belief_thumb_meaning_open", "thumb_use_open",
           "feedback_open", "failed_comprehension_prize", "failed_comprehension_choice_city_weekend",
           "failed_comprehension_no_cancellation", "preference_consistency", "cued_choice_preference_consistency",
           "n_hotels_in_choice_set", "property_page_visits_number", "decision_time_seconds", "decision_time_imputed",
           "loading_time_seconds", "cued_weekend", "listing_chosen", "listing_clicked", "listing_n_clicks",
           "time_on_listing_page_seconds", "cluster", "cued_listing", "checkin", "choice_set", "would_be_cued",
           "is_substitute_listing", "replaced_property_slug"]
df = df[[c for c in COLUMNS if c in df.columns]].sort_values(["participant_code", "city", "weekend_number_global", "property_slug"])
df.to_csv(inputs_dir / "analysis_dataset.csv", index=False, encoding="utf-8")
print(f"{len(df)} rows, {df['participant_code'].nunique()} subjects -> analysis_dataset.csv")
