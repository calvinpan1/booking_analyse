"""Export the questionnaire's open-text answers as one anonymised text file.

Purpose: let the researcher screen the free-text answers for anything
sensitive before deciding whether the file may be read by an assistant.
The file is written next to the other pipeline outputs on the SSD (never in
the Nextcloud-synced tree) and contains no identifier: each open field is a
separate block, and within a block the answers are shuffled with a fresh,
unrecorded random seed, so answers cannot be linked to each other across
fields nor to a participant.

Input : the oTree wide export (all_apps_wide_*.csv) in the SSD raw folder,
        same lookup as 00c.
Output: <target_dir>/open_fields_anonymized.txt
Run   : python 00_cleaning/00d_export_open_fields.py
"""

import secrets
import random
import sys
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
from _ssd_paths import resolve_dirs  # noqa: E402

BASE_DIR = Path(__file__).resolve().parents[1]
INPUTS_DIR, TARGET_DIR = resolve_dirs(BASE_DIR)

# Open fields of the post-experiment questionnaire (postexperiment_block/
# __init__.py), with the question as shown to participants.
OPEN_FIELDS = {
    "pretask_block.1.player.accommodation_preference_open":
        "Quels sont les principaux critères que vous prenez en compte quand vous réservez un logement de vacances ?",
    "postexperiment_block.1.player.choice_process_open":
        "Comment avez-vous procédé pour choisir les logements sur le site ?",
    "postexperiment_block.1.player.belief_thumb_meaning_open":
        "Selon vous, que signifie l'icône pouce ?",
    "postexperiment_block.1.player.thumb_use_open":
        "Si vous aviez remarqué l'icône pouce durant l'expérience, vous en êtes-vous servi, et si oui, comment ?",
    "postexperiment_block.1.player.feedback_open":
        "Souhaitez-vous nous faire part de commentaires ou de suggestions concernant cette expérience ?",
}

candidates = sorted(
    (p for p in INPUTS_DIR.glob("*.csv") if p.name.lower().startswith("all_apps_wide")),
    key=lambda p: p.stat().st_mtime, reverse=True,
)
if not candidates:
    sys.exit(f"No all_apps_wide_*.csv found in {INPUTS_DIR}")
OTREE_CSV = candidates[0]

df = pd.read_csv(OTREE_CSV, dtype=str, encoding="utf-8-sig")
missing = [c for c in OPEN_FIELDS if c not in df.columns]
if missing:
    sys.exit(f"Columns not found in {OTREE_CSV.name}: {missing}")

rng = random.Random(secrets.randbits(64))  # unrecorded seed: the order cannot be reproduced
out_path = TARGET_DIR / "open_fields_anonymized.txt"
TARGET_DIR.mkdir(parents=True, exist_ok=True)
with open(out_path, "w", encoding="utf-8", newline="\n") as f:
    f.write("Open-text answers from the post-experiment questionnaire, one block per field.\n"
            "Within a block, answers are in an independent random order and carry no identifier.\n")
    for col, question in OPEN_FIELDS.items():
        answers = [a.strip() for a in df[col].dropna().astype(str) if a.strip()]
        rng.shuffle(answers)
        f.write(f"\n{'=' * 78}\n{col.split('.')[-1]} -- {question}\n"
                f"{len(answers)} non-empty answer(s)\n{'=' * 78}\n")
        for i, a in enumerate(answers, 1):
            f.write(f"\n[{i}] {a}\n")

print(f"Wrote {out_path}")
