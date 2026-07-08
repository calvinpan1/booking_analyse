# AUTHOR: Calvin Pan
# PURPOSE: For each participant x weekend, how many distinct hotels actually
#   showed up in the choice set? Read straight from the raw per-cell tracking
#   JSONs exported by the extension (inputs/<participant_code>/cellNN_*.json),
#   not from the already-derived analysis_dataset.csv — this is an independent
#   check of the same n_hotels_in_choice_set logic 00c uses (the `preload`
#   event's `effectiveWhitelist` field is the authoritative record of what a
#   subject's search page actually displayed that weekend; cf.
#   runPreloadCycle() in booking_plugin/src/content/index.ts). If a cell has
#   more than one preload event (subject revisited the search page), the LAST
#   one by timestamp is what was actually left on screen.
#   Also checks how many weekends had a substitute hotel actually used — the
#   same preload event's `substitutions` field lists every substitute-pool
#   candidate the extension tried for a missing original listing; only entries
#   with reason == "selected" ended up displayed to the subject (cf. 00c's own
#   `replaced_by` logic and resolveVisibleSubstitutes() in
#   booking_plugin/src/shared/choice-sets.ts) — "no-visible-substitute" means
#   the attempt failed and nothing was shown in that slot.
# OUTPUTS: choice_set_size_histogram_python.png,
#   substitute_weekend_histogram_python.png (in outputs/)

import json
from pathlib import Path

import pandas as pd
import matplotlib.pyplot as plt

BASE_DIR = Path(__file__).parent.parent  # booking_analyse/
INPUTS_DIR = BASE_DIR / "inputs"
OUTPUTS_DIR = BASE_DIR / "outputs"

participant_dirs = sorted(
    d for d in INPUTS_DIR.iterdir()
    if d.is_dir() and d.name != "merged"
)

rows = []
n_no_preload = 0

for pdir in participant_dirs:
    for cell_file in sorted(pdir.glob("cell*.json")):
        with open(cell_file, encoding="utf-8") as f:
            data = json.load(f)

        participant_code = data["participant"].get("participantId", pdir.name)
        cell_meta = data["cell"]

        preloads = [e for e in data["events"] if e.get("type") == "preload"]
        if not preloads:
            n_no_preload += 1
            print(f"  ! No preload event for {participant_code} / {cell_file.name}")
            continue

        last_preload = max(preloads, key=lambda e: e.get("timestamp", 0))
        whitelist = last_preload.get("effectiveWhitelist") or []
        n_hotels = len(set(whitelist))

        substitutions = last_preload.get("substitutions") or []
        n_substitutes = sum(1 for s in substitutions if s.get("reason") == "selected")

        rows.append({
            "participant_code": participant_code,
            "cell_index": cell_meta.get("cellIndex"),
            "city": cell_meta.get("cityKey"),
            "checkin": cell_meta.get("checkin"),
            "n_hotels": n_hotels,
            "n_substitutes": n_substitutes,
        })

df = pd.DataFrame(rows)

print(f"\nParticipant x weekend observations: {len(df)}")
if n_no_preload:
    print(f"  ({n_no_preload} weekend(s) skipped — no preload event tracked)")

print("\nDistribution of number of hotels shown per weekend:")
print(df["n_hotels"].value_counts().sort_index())

n_total = len(df)
n_nine = (df["n_hotels"] == 9).sum()
pct_nine = 100 * n_nine / n_total

print(f"\n{n_nine} of {n_total} weekends ({pct_nine:.1f}%) showed exactly 9 hotels.")

fig, ax = plt.subplots(figsize=(7, 5))
bins = range(df["n_hotels"].min(), df["n_hotels"].max() + 2)
ax.hist(df["n_hotels"], bins=bins, align="left", color="steelblue", edgecolor="white")
ax.set_xlabel("Number of distinct hotels in choice set")
ax.set_ylabel("Number of participant x weekend observations")
ax.set_title(
    "Number of hotels shown per participant x weekend\n"
    f"{pct_nine:.1f}% of weekends showed all 9 hotels",
    fontsize=12,
)
ax.set_xticks(sorted(df["n_hotels"].unique()))
fig.tight_layout()

OUTPUTS_DIR.mkdir(parents=True, exist_ok=True)
out_path = OUTPUTS_DIR / "choice_set_size_histogram_python.png"
fig.savefig(out_path, dpi=150)
print(f"\nSaved histogram → {out_path}")

# ---------------------------------------------------------------------------
# Substitute-hotel usage
# ---------------------------------------------------------------------------

n_weekends_with_sub = (df["n_substitutes"] > 0).sum()
pct_with_sub = 100 * n_weekends_with_sub / n_total

print(f"\n{n_weekends_with_sub} of {n_total} weekends ({pct_with_sub:.1f}%) "
      f"had at least one substitute hotel used.")

sub_weekends = df[df["n_substitutes"] > 0]
print("\nAmong weekends with a substitute, distribution of # of substitutes used:")
print(sub_weekends["n_substitutes"].value_counts().sort_index())

fig2, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))

ax1.bar(
    ["No substitute", "Has substitute"],
    [n_total - n_weekends_with_sub, n_weekends_with_sub],
    color=["steelblue", "indianred"],
)
ax1.set_ylabel("Number of participant x weekend observations")
ax1.set_title(
    "Weekends with a substitute hotel used\n"
    f"{pct_with_sub:.1f}% of weekends ({n_weekends_with_sub}/{n_total})",
    fontsize=11,
)

bins2 = range(sub_weekends["n_substitutes"].min(), sub_weekends["n_substitutes"].max() + 2)
ax2.hist(sub_weekends["n_substitutes"], bins=bins2, align="left", color="indianred", edgecolor="white")
ax2.set_xlabel("Number of substitute hotels used")
ax2.set_ylabel("Number of participant x weekend observations")
ax2.set_title(
    "Among weekends with a substitute,\n# of substitutes used",
    fontsize=11,
)
ax2.set_xticks(sorted(sub_weekends["n_substitutes"].unique()))

fig2.tight_layout()
out_path2 = OUTPUTS_DIR / "substitute_weekend_histogram_python.png"
fig2.savefig(out_path2, dpi=150)
print(f"\nSaved histogram → {out_path2}")
