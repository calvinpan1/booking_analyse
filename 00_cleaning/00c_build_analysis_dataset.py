# AUTHOR: Calvin Pan
# DATE CREATED: 23 June 2026
# DATE LAST MODIFIED: 8 July 2026
# PURPOSE: Build the final analysis dataset for the pre-registration
#   (Documents/20260618_Booking_preregistration.pdf): one row per
#   subject × city × weekend × listing, joining the oTree wide export
#   (post-experiment questionnaire, treatment assignment) with
#   extension_joined.csv (from 00b — tracked browsing behaviour) and
#   config/choice_sets_with_substitutes.json (the canonical 9-listing roster
#   of each city × choice_set, PLUS the substitute_pools the extension draws
#   from live, in-session, whenever an original listing's card never loads —
#   cf. resolveVisibleSubstitutes() in booking_plugin/src/shared/choice-sets.ts.
#   The listing set actually shown to a subject in a given weekend is read
#   from the tracked `preload` event's effectiveWhitelist field, not assumed
#   to be the static choice_set_N roster.
# OUTPUTS: analysis_dataset.csv (in outputs/)
#   60 subjects × 3 cities × 4 weekends × 9 listings = 6,480 rows (108/subject),
#   per the pre-registration's observation structure.

import sys
import json
import re
import unicodedata
from pathlib import Path
from urllib.parse import urlparse
import pandas as pd

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

BASE_DIR    = Path(__file__).parent.parent          # analyse/
PLUGIN_DIR  = BASE_DIR.parent                       # booking_plugin/
INPUTS_DIR  = BASE_DIR / "inputs"
OUTPUTS_DIR = BASE_DIR / "outputs"
CONFIG_CHOICESET_JSON = PLUGIN_DIR /"booking_plugin" / "config" / "choice_sets_with_substitutes.json"

EXTENSION_CONVERTED_CSV = OUTPUTS_DIR / "extension_converted.csv"  # from 00a (every event, unfiltered)
EXTENSION_JOINED_CSV    = OUTPUTS_DIR / "extension_joined.csv"     # from 00b (events ↔ choice-set join)
OUTPUT_CSV               = OUTPUTS_DIR / "analysis_dataset.csv"

# City slug helper — mirrors 00b's derive_city_key(): lowercase, strip accents,
# non-alnum runs → single dash. Used as a fallback ONLY for properties with no
# city_slug field of their own; every property in choice_sets_with_substitutes.json
# already carries one (e.g. "sete", "la-ciotat"), which is preferred since a
# hardcoded display-name→slug map would silently go stale if a city is renamed,
# added, or removed in the JSON (as happened here: Sète/La Ciotat replaced
# Annecy/Nice in the config the analysis pipeline actually runs against).
def derive_city_key(text) -> str:
    if not text or not isinstance(text, str):
        return ""
    text = unicodedata.normalize("NFKD", text)
    text = "".join(c for c in text if not unicodedata.combining(c))
    text = text.lower()
    text = re.sub(r"[^a-z0-9]+", "-", text)
    return text.strip("-")

# ---------------------------------------------------------------------------
# Prompt for the oTree wide-export CSV — same numbered-list / filename /
# Enter-for-most-recent selection logic as 00a/00b.
# ---------------------------------------------------------------------------

def prompt_for_file_choice(candidates: list, prompt_label: str = "Which file to use?") -> Path:
    if len(candidates) == 1:
        return candidates[0]

    print("CSV files found in inputs/:")
    for i, p in enumerate(candidates, start=1):
        print(f"  [{i}] {p.name}")
    print("  Press Enter for the most recent.")

    choice = input(f"{prompt_label} [1-{len(candidates)}]: ").strip()
    if choice == "":
        return candidates[0]  # most recently modified
    if choice.isdigit() and 1 <= int(choice) <= len(candidates):
        return candidates[int(choice) - 1]

    by_name = {p.name: p for p in candidates}
    if choice in by_name:
        return by_name[choice]
    if not choice.endswith(".csv") and f"{choice}.csv" in by_name:
        return by_name[f"{choice}.csv"]

    sys.exit(f"Invalid choice: {choice!r}")


if len(sys.argv) > 1:
    csv_name = sys.argv[1]
    if not csv_name.endswith(".csv"):
        csv_name += ".csv"
    OTREE_CSV = INPUTS_DIR / csv_name
else:
    candidates = sorted(INPUTS_DIR.glob("*.csv"), key=lambda p: p.stat().st_mtime, reverse=True)
    if not candidates:
        sys.exit(f"No CSV files found in {INPUTS_DIR}. Put the oTree wide export there or pass a path as argument.")
    OTREE_CSV = prompt_for_file_choice(candidates, "Fichier oTree (all_apps_wide) à utiliser ?")

print(f"oTree input: {OTREE_CSV}")

# ---------------------------------------------------------------------------
# Prompt for the oTree PageTimes export (session_code/participant_code/
# app_name/page_name/round_number/epoch_time_completed — per-page timestamps,
# recorded even for subjects who never finished). Optional: older runs may not
# have this file, in which case whole_experiment_time_seconds and
# choice_task_time_seconds are left blank rather than failing the whole script.
# ---------------------------------------------------------------------------

pagetimes_candidates = sorted(
    (p for p in INPUTS_DIR.glob("*.csv") if p.name.lower().startswith("pagetimes")),
    key=lambda p: p.stat().st_mtime, reverse=True,
)
if len(sys.argv) > 2:
    pt_name = sys.argv[2]
    if not pt_name.endswith(".csv"):
        pt_name += ".csv"
    PAGETIMES_CSV = INPUTS_DIR / pt_name
elif pagetimes_candidates:
    PAGETIMES_CSV = prompt_for_file_choice(pagetimes_candidates, "Fichier oTree PageTimes à utiliser ?")
else:
    PAGETIMES_CSV = None
    print("  ! No PageTimes-*.csv found in inputs/ — whole_experiment_time_seconds and "
          "choice_task_time_seconds will be left blank.")

if PAGETIMES_CSV:
    print(f"PageTimes input: {PAGETIMES_CSV}")

if not EXTENSION_JOINED_CSV.exists():
    sys.exit(f"{EXTENSION_JOINED_CSV} not found. Run 00b first.")
if not EXTENSION_CONVERTED_CSV.exists():
    sys.exit(f"{EXTENSION_CONVERTED_CSV} not found. Run 00a first.")

# ---------------------------------------------------------------------------
# Load everything
# ---------------------------------------------------------------------------

df_otree     = pd.read_csv(OTREE_CSV, dtype=str, encoding="utf-8-sig")
df_conv      = pd.read_csv(EXTENSION_CONVERTED_CSV, dtype=str)
df_joined    = pd.read_csv(EXTENSION_JOINED_CSV, dtype=str)
df_pagetimes = pd.read_csv(PAGETIMES_CSV, dtype=str) if PAGETIMES_CSV else None

with open(CONFIG_CHOICESET_JSON, encoding="utf-8") as f:
    raw_cs = json.load(f)

# Flatten {city display name → choice_set_N → [properties]} the same way 00b
# does. City slug comes from each property's own `city_slug` field (present
# on every property in choice_sets_with_substitutes.json), falling back to a
# derived slug of the display-name key only if that field is somehow absent —
# NOT from a hardcoded display-name→slug map, which previously went stale
# (Sète/La Ciotat replaced Annecy/Nice in this config at some point) and
# silently produced empty listing rows for the un-mapped cities. The JSON
# nests each city under {"choice_sets": {choice_set_N: [...]}, "substitute_pools":
# {...}}. Unlike the earlier version of this script, substitute_pools is KEPT
# (not dropped): the extension substitutes a same-cluster, same-preferred
# property from that pool live, in-session, whenever an original listing's
# card never loads in the visible SERP DOM (resolveVisibleSubstitutes(),
# booking_plugin/src/shared/choice-sets.ts) — the subject sees and can choose
# the substitute instead of the original, so its attributes (cluster,
# preferred, property_id, ...) must be resolvable too, or a substituted
# listing's row below would be missing them. A substitute isn't tied to one
# choice_set_N in the JSON (it can fill in for whichever weekend needs it),
# so `choice_set` is left blank for these rows — the weekend it actually
# ended up in is resolved per-subject in PART B, from the tracked event data,
# not from this static roster.
choiceset_rows = []
for city_display, sets in raw_cs.items():
    choice_sets = sets["choice_sets"] if "choice_sets" in sets else sets
    for set_name, properties in choice_sets.items():
        for prop in properties:
            city_slug = prop.get("city_slug") or derive_city_key(city_display)
            choiceset_rows.append({**prop, "city_slug": city_slug, "choice_set": set_name, "_is_substitute": False})
    for pool_name, properties in sets.get("substitute_pools", {}).items():
        for prop in properties:
            city_slug = prop.get("city_slug") or derive_city_key(city_display)
            choiceset_rows.append({**prop, "city_slug": city_slug, "choice_set": None, "_is_substitute": True})
df_choicesets = pd.DataFrame(choiceset_rows)

n_substitute_props = int(df_choicesets["_is_substitute"].sum())
df_canonical = df_choicesets[~df_choicesets["_is_substitute"]]
print(f"Choice-sets roster: {len(df_canonical)} canonical listings "
      f"({df_canonical['city_slug'].nunique()} cities × "
      f"{df_canonical['choice_set'].nunique()} weekends × 9 listings), "
      f"plus {n_substitute_props} substitute-pool propert{'y' if n_substitute_props == 1 else 'ies'}")

# Lookup table keyed by (city_slug, property_slug), spanning both buckets —
# used below to resolve whichever slug actually ended up displayed to a
# subject, canonical or substitute alike.
prop_lookup = df_choicesets.drop_duplicates(subset=["city_slug", "property_slug"])

# ---------------------------------------------------------------------------
# PART A: Per-subject weekend roster — every (participant_code, cell_index)
# combo actually observed in the tracking data, with its city/listIndex/
# checkin/thumb. Built from extension_converted.csv (NOT the joined file)
# because cell_* columns are attached to every event regardless of whether
# that event matched a choice-set listing — this is the most complete source
# for "which weekend is this".
# ---------------------------------------------------------------------------

roster_cols = ["participant_code", "cell_index", "cell_city_key", "cell_list_index", "cell_checkin", "cell_thumb"]
df_roster = df_conv[roster_cols].dropna(subset=["cell_index"]).drop_duplicates()
df_roster["cell_index"] = df_roster["cell_index"].astype(int)
df_roster["cell_list_index"] = df_roster["cell_list_index"].astype(int)
df_roster["choice_set"] = "choice_set_" + (df_roster["cell_list_index"] + 1).astype(str)

n_expected_cells = df_roster["participant_code"].nunique() * 12
print(f"Weekend roster: {len(df_roster)} (subject × weekend) cell(s) "
      f"for {df_roster['participant_code'].nunique()} subject(s) "
      f"(expected {n_expected_cells} if every subject has all 12 weekends tracked)")
if len(df_roster) != n_expected_cells:
    print("  ! Some subjects are missing weekends in the tracking data (no event at all for that cell).")

# ---------------------------------------------------------------------------
# PART B: Listing-level skeleton — cross the per-subject weekend roster with
# the listings ACTUALLY displayed that weekend, so every listing gets a row
# even if the subject never interacted with it (no viewport/click/hover/
# reserve event at all). "Actually displayed" is not always the canonical
# choice_set_N roster (see note above df_choicesets) — the `preload` event's
# `effectiveWhitelist` field is the authoritative record of the 9 (or fewer)
# cards a subject actually had for that weekend, cf. runPreloadCycle() in
# booking_plugin/src/content/index.ts. We fall back to the canonical roster
# only for weekends with no preload event at all (e.g. dropped by the
# extension/tracker), so every cell still gets a full roster.
# ---------------------------------------------------------------------------

def _parse_json_list(s):
    if isinstance(s, str) and s.strip():
        try:
            return json.loads(s)
        except json.JSONDecodeError:
            return []
    return []

preload_ev = df_conv[df_conv["type"] == "preload"].copy()
preload_ev["cell_index"] = pd.to_numeric(preload_ev["cell_index"], errors="coerce")
preload_ev["timestamp"] = pd.to_numeric(preload_ev["timestamp"], errors="coerce")
preload_ev = preload_ev.dropna(subset=["cell_index", "participant_code"])
preload_ev["cell_index"] = preload_ev["cell_index"].astype(int)
# A subject can revisit the search page within the same weekend, firing more
# than one preload event for the same cell — the LAST one reflects what was
# actually left on screen for the rest of that weekend.
preload_ev = preload_ev.sort_values(["participant_code", "cell_index", "timestamp"])
preload_last = preload_ev.drop_duplicates(subset=["participant_code", "cell_index"], keep="last")

displayed_rows = []
for _, r in preload_last.iterrows():
    slugs = _parse_json_list(r.get("effectiveWhitelist"))
    if not slugs:
        continue
    replaced_by = {
        s["substituteSlug"]: s["missingSlug"]
        for s in _parse_json_list(r.get("substitutions"))
        if s.get("reason") == "selected" and s.get("substituteSlug")
    }
    for slug in slugs:
        displayed_rows.append({
            "participant_code": r["participant_code"],
            "cell_index": r["cell_index"],
            "property_slug": slug,
            "replaced_property_slug": replaced_by.get(slug),
        })
df_displayed = pd.DataFrame(
    displayed_rows, columns=["participant_code", "cell_index", "property_slug", "replaced_property_slug"]
)

cells_with_preload = set(zip(df_displayed["participant_code"], df_displayed["cell_index"]))
has_preload = df_roster.apply(lambda r: (r["participant_code"], r["cell_index"]) in cells_with_preload, axis=1)

roster_tracked = df_roster[has_preload]
roster_fallback = df_roster[~has_preload]
if len(roster_fallback):
    print(f"  ! {len(roster_fallback)} weekend(s) with no `preload` event tracked — falling back to the "
          f"canonical choice-set roster for these (actual substitutions, if any, unknown).")

skeleton_cols = [
    "participant_code", "cell_index", "cell_city_key", "cell_list_index",
    "cell_checkin", "cell_thumb", "choice_set", "property_slug", "replaced_property_slug",
]

skeleton_tracked = roster_tracked.merge(df_displayed, on=["participant_code", "cell_index"], how="left")

skeleton_fallback = roster_fallback.merge(
    df_canonical,
    left_on=["cell_city_key", "choice_set"],
    right_on=["city_slug", "choice_set"],
    how="left",
)
skeleton_fallback["replaced_property_slug"] = pd.NA

skeleton_ids = pd.concat(
    [skeleton_tracked[skeleton_cols], skeleton_fallback[skeleton_cols]], ignore_index=True
)

# Attach property attributes (cluster, preferred, property_id, price_total, ...)
# for whichever slug ended up actually displayed — canonical or substitute alike.
df_skeleton = skeleton_ids.merge(
    prop_lookup.drop(columns=["choice_set"]),
    left_on=["cell_city_key", "property_slug"],
    right_on=["city_slug", "property_slug"],
    how="left",
)
df_skeleton = df_skeleton.rename(columns={"_is_substitute": "is_substitute_listing"})
df_skeleton["is_substitute_listing"] = df_skeleton["is_substitute_listing"].fillna(False)

n_expected_rows = df_roster["participant_code"].nunique() * 3 * 4 * 9
print(f"Listing-level skeleton: {len(df_skeleton)} rows "
      f"(expected {n_expected_rows} = subjects × 3 cities × 4 weekends × 9 listings); "
      f"{int(df_skeleton['is_substitute_listing'].sum())} substitute-listing row(s)")

# ---------------------------------------------------------------------------
# PART C: Event-derived measures. Work off extension_joined.csv, restricted to
# rows that DO carry a real event (drop "child (choice set) only" rows, which
# are choice-set listings with no event at all — already represented in the
# skeleton above with zero/NaN measures by construction of the left-merge).
# ---------------------------------------------------------------------------

df_ev = df_joined[df_joined["_join_status"] != "child (choice set) only"].copy()
df_ev["timestamp"] = pd.to_numeric(df_ev["timestamp"], errors="coerce")
df_ev["cell_index"] = pd.to_numeric(df_ev["cell_index"], errors="coerce")
df_ev = df_ev.dropna(subset=["timestamp", "cell_index"])
df_ev["cell_index"] = df_ev["cell_index"].astype(int)
df_ev = df_ev.sort_values(["participant_code", "cell_index", "timestamp"])

# --- Detail-page page_view detection (page_view's targetPropertyId is NOT
# backfilled from the URL in 00b — only click/hover/viewport/reserve are —
# so we extract the slug straight from the URL here, the same regex 00b uses
# for its own backfill step). ---
HOTEL_SLUG_RE = re.compile(r"/hotel/[^/]+/([^.]+)")

def slug_from_url(url) -> str:
    if not isinstance(url, str) or not url:
        return ""
    m = HOTEL_SLUG_RE.search(urlparse(url).path)
    return m.group(1) if m else ""

is_page_view = df_ev["type"] == "page_view"
df_ev["_detail_slug"] = ""
df_ev.loc[is_page_view, "_detail_slug"] = df_ev.loc[is_page_view, "url"].apply(slug_from_url)
df_ev["_is_detail_page_view"] = is_page_view & (df_ev["_detail_slug"] != "")

# Effective property_slug for aggregation: the choiceset-joined slug for
# click/hover/viewport/reserve, OR the URL-derived slug for detail page_views.
df_ev["_eff_slug"] = df_ev["property_slug"].fillna("")
df_ev.loc[df_ev["_is_detail_page_view"], "_eff_slug"] = df_ev.loc[df_ev["_is_detail_page_view"], "_detail_slug"]

# --- Time on listing page: duration until the NEXT event in the same cell
# session (any type — leaving the detail page, whatever the destination,
# ends the clock on that page view). Last event of a cell has no "next" event
# to bound it, so its dwell time is unknown (NaN), not zero. ---
df_ev["_next_ts"] = df_ev.groupby(["participant_code", "cell_index"])["timestamp"].shift(-1)
df_ev["_dwell_s"] = (df_ev["_next_ts"] - df_ev["timestamp"]) / 1000.0

time_on_listing = (
    df_ev[df_ev["_is_detail_page_view"]]
    .groupby(["participant_code", "cell_index", "_eff_slug"])["_dwell_s"]
    .sum()
    .reset_index()
    .rename(columns={"_eff_slug": "property_slug", "_dwell_s": "time_on_listing_page_seconds"})
)

# --- Clicks / chosen, per (subject, weekend, listing) — restricted to rows
# with a real property_slug from the choice-set join (click/hover/viewport/
# reserve), independent of the page_view-only detail-slug extraction above.
# ---
prop_rows = df_ev[df_ev["property_slug"].notna() & (df_ev["property_slug"] != "")].copy()

clicks = (
    prop_rows[prop_rows["type"] == "click"]
    .groupby(["participant_code", "cell_index", "property_slug"])
    .size()
    .reset_index(name="listing_n_clicks")
)

chosen = (
    prop_rows[prop_rows["type"] == "reserve"]
    [["participant_code", "cell_index", "property_slug"]]
    .drop_duplicates()
)
chosen["listing_chosen"] = True

# --- Weekend-level: decision time (first page_view → reserve, in seconds)
# and property page visits number (count of detail-page page_views). ---
first_page_view = (
    df_ev[df_ev["type"] == "page_view"]
    .groupby(["participant_code", "cell_index"])["timestamp"].min()
    .rename("_first_pv_ts")
)
reserve_ts = (
    df_ev[df_ev["type"] == "reserve"]
    .groupby(["participant_code", "cell_index"])["timestamp"].max()
    .rename("_reserve_ts")
)
decision_time = pd.concat([first_page_view, reserve_ts], axis=1).reset_index()
decision_time["decision_time_seconds"] = (decision_time["_reserve_ts"] - decision_time["_first_pv_ts"]) / 1000.0
decision_time = decision_time[["participant_code", "cell_index", "decision_time_seconds"]]

property_page_visits = (
    df_ev[df_ev["_is_detail_page_view"]]
    .groupby(["participant_code", "cell_index"])
    .size()
    .reset_index(name="property_page_visits_number")
)

# --- Loading time: how long Booking took to load the search results down to
# the design's 9-listing choice set for this weekend, from the `preload` event
# (flattened PreloadOutcome, cf. src/content/preload.ts). Each preload PASS
# resets its own internal `start = Date.now()`, so when substitute retries
# happened (passes > 1), the top-level `elapsedMs` reflects only the LAST
# pass — the true total curtain duration is the sum of passDetails[].elapsedMs.
# Single-pass successes have no passDetails, so elapsedMs alone IS the total
# there. If several preload events land in the same weekend (e.g. the subject
# revisited the search page), take the chronologically FIRST one. ---
preload_events = (
    df_ev[df_ev["type"] == "preload"]
    .sort_values(["participant_code", "cell_index", "timestamp"])
    .drop_duplicates(subset=["participant_code", "cell_index"], keep="first")
    .copy()
)

def total_loading_ms(row) -> float:
    pass_details = row.get("passDetails")
    if isinstance(pass_details, str) and pass_details.strip():
        try:
            details = json.loads(pass_details)
            return float(sum(d["elapsedMs"] for d in details))
        except (json.JSONDecodeError, KeyError, TypeError):
            pass
    elapsed = row.get("elapsedMs")
    return float(elapsed) if pd.notna(elapsed) else float("nan")

preload_events["loading_time_seconds"] = preload_events.apply(total_loading_ms, axis=1) / 1000.0
loading_time = preload_events[["participant_code", "cell_index", "loading_time_seconds"]]

# ---------------------------------------------------------------------------
# PART D: Assemble the listing-level table
# ---------------------------------------------------------------------------

df = df_skeleton.merge(clicks, on=["participant_code", "cell_index", "property_slug"], how="left")
df = df.merge(chosen, on=["participant_code", "cell_index", "property_slug"], how="left")
df = df.merge(time_on_listing, on=["participant_code", "cell_index", "property_slug"], how="left")
df = df.merge(decision_time, on=["participant_code", "cell_index"], how="left")
df = df.merge(property_page_visits, on=["participant_code", "cell_index"], how="left")
df = df.merge(loading_time, on=["participant_code", "cell_index"], how="left")

df["listing_n_clicks"] = df["listing_n_clicks"].fillna(0).astype(int)
df["listing_clicked"] = df["listing_n_clicks"] > 0
df["listing_chosen"] = df["listing_chosen"].eq(True)  # NaN (never reserved) → False
df["property_page_visits_number"] = df["property_page_visits_number"].fillna(0).astype(int)

n_hotels = df.groupby(["participant_code", "cell_index"])["property_slug"].nunique().rename("n_hotels_in_choice_set")
df = df.merge(n_hotels, on=["participant_code", "cell_index"], how="left")

df = df.rename(columns={"preferred": "would_be_cued"})

df["cued_weekend"] = df["cell_thumb"] == "P"
df["cued_listing"] = df["cued_weekend"] & (df["would_be_cued"] == True)  # noqa: E712 — would_be_cued is a real bool column

# ---------------------------------------------------------------------------
# PART E: City-level welfare measures — preference consistency & cued
# choice-preference consistency, computed from the cluster of the CHOSEN
# listing in each weekend (cell), using cell_thumb to split cued vs not-cued.
# ---------------------------------------------------------------------------

chosen_listings = df[df["listing_chosen"]][["participant_code", "cell_city_key", "cell_index", "cell_thumb", "cluster"]]

def consistency_for_city(group: pd.DataFrame) -> pd.Series:
    # Raw (not deduplicated) per-weekend cluster of the chosen listing — the
    # design has exactly 2 not-cued and 2 cued weekends per city, so each list
    # should have length 2 when both reserve choices were tracked.
    not_cued_vals = group.loc[group["cell_thumb"] == "A", "cluster"].dropna().tolist()
    cued_vals     = group.loc[group["cell_thumb"] == "P", "cluster"].dropna().tolist()

    if len(not_cued_vals) == 2:
        pref_consistency = not_cued_vals[0] == not_cued_vals[1]
    else:
        pref_consistency = pd.NA  # missing one of the two not-cued choices

    cued_consistency = pd.NA
    if pref_consistency is True and cued_vals:
        consistent_cluster = not_cued_vals[0]
        cued_consistency = all(c == consistent_cluster for c in cued_vals)

    return pd.Series({
        "preference_consistency": pref_consistency,
        "cued_choice_preference_consistency": cued_consistency,
    })

city_consistency = (
    chosen_listings.groupby(["participant_code", "cell_city_key"])
    .apply(consistency_for_city, include_groups=False)
    .reset_index()
)

df = df.merge(city_consistency, on=["participant_code", "cell_city_key"], how="left")

# ---------------------------------------------------------------------------
# PART F0: Whole-experiment & choice-task timing, from oTree's PageTimes
# export. epoch_time_completed is when a page was COMPLETED (i.e. left), not
# when it was arrived at — so "begin weekend 1" / "reach the questionnaire" is
# the completion time of whatever page came immediately BEFORE the round-1
# ChoiceTaskLoop / first postexperiment_block row, not that row's own
# timestamp. Kept in oTree's own time domain (seconds) throughout, except for
# the "last click" boundary of choice_task_time, which comes from the
# extension's click events (ms epoch, converted to seconds) — both are Unix
# epoch, so this is valid as long as the lab machine and subject browser
# clocks are roughly in sync, the same assumption the rest of this pipeline
# already relies on. PageTimes records every page reached even for subjects
# who never finished, so this works for incomplete sessions too.
# ---------------------------------------------------------------------------

def _timestamp_before(grouped, page_idx_series, out_col):
    """For each (participant_code -> page_index) pair, the latest
    epoch_time_completed among that subject's PageTimes rows strictly before
    page_index — i.e. the arrival time at that page."""
    rows = []
    for pcode, idx in page_idx_series.items():
        g = grouped.get_group(pcode)
        prior = g[g["page_index"] < idx]
        ts = prior["epoch_time_completed"].max() if not prior.empty else pd.NA
        rows.append({"participant_code": pcode, out_col: ts})
    return pd.DataFrame(rows)


if df_pagetimes is not None:
    df_pt = df_pagetimes.copy()
    df_pt["epoch_time_completed"] = pd.to_numeric(df_pt["epoch_time_completed"], errors="coerce")
    df_pt["round_number"] = pd.to_numeric(df_pt["round_number"], errors="coerce")
    df_pt["page_index"] = pd.to_numeric(df_pt["page_index"], errors="coerce")
    df_pt = df_pt.dropna(subset=["epoch_time_completed", "page_index"])

    # Whole experiment time: span from the first recorded page (InitializeParticipant)
    # to the last recorded page, per subject — works for incomplete sessions:
    # an abandoned subject just has fewer rows, and "last" is wherever they stopped.
    experiment_span = (
        df_pt.groupby("participant_code")["epoch_time_completed"]
        .agg(_first_page_ts="min", _last_page_ts="max")
        .reset_index()
    )
    experiment_span["whole_experiment_time_seconds"] = (
        experiment_span["_last_page_ts"] - experiment_span["_first_page_ts"]
    )

    grouped_pt = df_pt.groupby("participant_code")

    round1_idx = (
        df_pt[(df_pt["app_name"] == "choice_task_block") & (df_pt["round_number"] == 1)]
        .groupby("participant_code")["page_index"].min()
    )
    begin_weekend1 = _timestamp_before(grouped_pt, round1_idx, "_weekend1_start_ts")

    postexp_idx = (
        df_pt[df_pt["app_name"] == "postexperiment_block"]
        .groupby("participant_code")["page_index"].min()
    )
    postexp_start = _timestamp_before(grouped_pt, postexp_idx, "_postexp_start_ts")

    # Last interface interaction (click) per subject, from the extension's own
    # click events (ms epoch) — converted to seconds to match PageTimes.
    df_conv_ts = df_conv.copy()
    df_conv_ts["timestamp"] = pd.to_numeric(df_conv_ts["timestamp"], errors="coerce")
    last_click = (
        df_conv_ts[df_conv_ts["type"] == "click"]
        .groupby("participant_code")["timestamp"].max() / 1000.0
    ).rename("_last_click_ts").reset_index()

    df_experiment_times = (
        experiment_span[["participant_code", "whole_experiment_time_seconds"]]
        .merge(begin_weekend1, on="participant_code", how="left")
        .merge(postexp_start, on="participant_code", how="left")
        .merge(last_click, on="participant_code", how="left")
    )

    # choice_task end = the EARLIEST of (last click, arrival at postexperiment_block)
    # that's actually available — a subject who never clicked has no
    # _last_click_ts, one who never reached the questionnaire has no
    # _postexp_start_ts; min(skipna=True) uses whichever bound(s) exist.
    df_experiment_times["_choice_task_end_ts"] = df_experiment_times[
        ["_last_click_ts", "_postexp_start_ts"]
    ].min(axis=1, skipna=True)

    df_experiment_times["choice_task_time_seconds"] = (
        df_experiment_times["_choice_task_end_ts"] - df_experiment_times["_weekend1_start_ts"]
    )

    df_experiment_times = df_experiment_times[
        ["participant_code", "whole_experiment_time_seconds", "choice_task_time_seconds"]
    ]
else:
    df_experiment_times = pd.DataFrame(
        columns=["participant_code", "whole_experiment_time_seconds", "choice_task_time_seconds"]
    )

df = df.merge(df_experiment_times, on="participant_code", how="left")

# ---------------------------------------------------------------------------
# PART F: Subject-level variables from the oTree wide export.
# ---------------------------------------------------------------------------

subject_cols = {
    "participant.code":                                          "participant_code",
    "participant.clutter_treatment":                              "clutter_treatment",
    "postexperiment_block.1.player.noticed_thumb":                "cue_recognition",
    "postexperiment_block.1.player.visual_complexity":            "visual_complexity",
    "postexperiment_block.1.player.nasa_tlx_mental":              "nasa_tlx_mental",
    "postexperiment_block.1.player.nasa_tlx_physical":            "nasa_tlx_physical",
    "postexperiment_block.1.player.nasa_tlx_temporal":            "nasa_tlx_temporal",
    "postexperiment_block.1.player.nasa_tlx_performance":         "nasa_tlx_performance",
    "postexperiment_block.1.player.nasa_tlx_effort":              "nasa_tlx_effort",
    "postexperiment_block.1.player.nasa_tlx_frustration":         "nasa_tlx_frustration",
    "instructions_block.1.player.failed_comprehension_prize":                       "failed_comprehension_prize",
    "instructions_block.1.player.failed_comprehension_choice_city_weekend":         "failed_comprehension_choice_city_weekend",
    "instructions_block.1.player.failed_comprehension_no_cancellation":             "failed_comprehension_no_cancellation",
}
missing_otree_cols = [c for c in subject_cols if c not in df_otree.columns]
if missing_otree_cols:
    sys.exit(f"Missing expected oTree column(s): {missing_otree_cols}")

df_subject = df_otree[list(subject_cols)].rename(columns=subject_cols)
df_subject["clutter_high"] = df_subject["clutter_treatment"] == "O"  # 'O' = Original/high clutter, 'N' = less clutter

df = df.merge(df_subject, on="participant_code", how="left")

n_unmatched_subjects = df.loc[df["clutter_treatment"].isna(), "participant_code"].nunique()
if n_unmatched_subjects:
    print(f"  ! {n_unmatched_subjects} subject(s) in the tracking data have no matching row in {OTREE_CSV.name}.")

# ---------------------------------------------------------------------------
# PART G: QA cross-check against oTree's own (choice_cluster, choice_preferred)
# — oTree never stores the chosen property_slug (per the pre-registration
# note), but it DOES store the cluster of the chosen listing and whether that
# listing was the cluster's "preferred" one. Compare against the slug-level
# answer we derived from the tracked `reserve` event, as a sanity check on
# the whole pipeline (mismatches suggest a tracking gap, not necessarily a
# wrong answer — the reserve event is the authoritative source here, since
# oTree alone cannot resolve the slug).
# ---------------------------------------------------------------------------

n_checked = 0
n_mismatch = 0
for round_n in range(1, 13):
    cluster_col = f"choice_task_block.{round_n}.player.choice_cluster"
    preferred_col = f"choice_task_block.{round_n}.player.choice_preferred"
    if cluster_col not in df_otree.columns:
        continue
    otree_round = df_otree[["participant.code", cluster_col, preferred_col]].rename(columns={
        "participant.code": "participant_code",
        cluster_col: "_otree_cluster",
        preferred_col: "_otree_preferred",
    })
    otree_round["cell_index"] = round_n

    tracked = chosen_listings.rename(columns={"cluster": "_tracked_cluster"})[
        ["participant_code", "cell_index", "_tracked_cluster"]
    ]
    cmp = otree_round.merge(tracked, on=["participant_code", "cell_index"], how="inner")
    cmp = cmp.dropna(subset=["_otree_cluster", "_tracked_cluster"])
    if cmp.empty:
        continue
    n_checked += len(cmp)
    n_mismatch += (cmp["_otree_cluster"].astype(float).astype(int) != cmp["_tracked_cluster"].astype(int)).sum()

if n_checked:
    print(f"QA — oTree choice_cluster vs tracked reserve cluster: "
          f"{n_checked - n_mismatch}/{n_checked} match ({n_mismatch} mismatch(es)).")

# ---------------------------------------------------------------------------
# Final column selection & ordering
# ---------------------------------------------------------------------------

df = df.rename(columns={
    "cell_city_key": "city",
    "cell_index": "weekend_number_global",
    "cell_checkin": "checkin",
})

COLUMNS = [
    # identity
    "participant_code", "city", "weekend_number_global", "property_slug",
    # subject level
    "clutter_treatment", "clutter_high", "cue_recognition",
    "nasa_tlx_mental", "nasa_tlx_physical", "nasa_tlx_temporal",
    "nasa_tlx_performance", "nasa_tlx_effort", "nasa_tlx_frustration",
    "visual_complexity",
    "whole_experiment_time_seconds", "choice_task_time_seconds",
    # city level
    "preference_consistency", "cued_choice_preference_consistency",
    # weekend level
    "n_hotels_in_choice_set", "property_page_visits_number",
    "decision_time_seconds", "loading_time_seconds", "cued_weekend",
    # listing level
    "listing_chosen", "listing_clicked", "listing_n_clicks",
    "time_on_listing_page_seconds", "cluster", "cued_listing",
    # reference (not pre-registered DVs, kept for QA / debugging)
    "checkin", "choice_set", "would_be_cued",
    "is_substitute_listing", "replaced_property_slug",
]
df = df[[c for c in COLUMNS if c in df.columns]]
df = df.sort_values(["participant_code", "city", "weekend_number_global", "property_slug"])

OUTPUTS_DIR.mkdir(parents=True, exist_ok=True)
df.to_csv(OUTPUT_CSV, index=False, encoding="utf-8")

print(f"\nSaved {len(df):,} rows → {OUTPUT_CSV}")
print(f"  {df['participant_code'].nunique()} subjects × "
      f"{df.groupby('participant_code')['city'].nunique().mean():.1f} cities × "
      f"{df.groupby('participant_code')['weekend_number_global'].nunique().mean():.1f} weekends × "
      f"{df.groupby(['participant_code','weekend_number_global'])['property_slug'].nunique().mean():.1f} listings (means)")
