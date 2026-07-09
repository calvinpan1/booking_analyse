# booking_analyse

Analyse des données générées par l'expérience Booking.com : ce dépôt prend les
exports bruts de l'extension Chrome et d'oTree, les nettoie, les joint, et
produit un dataset final (`outputs/analysis_dataset.csv`) prêt pour les tests
d'hypothèses du pré-enregistrement.

## Set up — how `inputs/` must be organized

Everything the pipeline reads lives in `inputs/`. Before running anything, lay
it out like this:

```
inputs/
├── <participant_code_1>/            # one folder per participant
│   ├── cell01_<city>_<checkin>.json
│   ├── cell01_<city>_<checkin>.html
│   ├── cell02_<city>_<checkin>.json
│   ├── cell02_<city>_<checkin>.html
│   └── ...                          # up to cell12
├── <participant_code_2>/
│   └── ...
├── PageTimes-<date>.csv             # oTree PageTimes export
├── <session>_all_apps_wide_<date>.csv   # oTree wide export
└── choice_sets_with_substitutes.json     # OPTIONAL — see below
```

- **Individual participant folders (top level)** — one folder per subject,
  named with that subject's `participant_code`, containing the raw per-cell
  JSON (+ matching HTML) exports dumped by the Booking.com extension — one
  `cellNN_<city>_<checkin>.json` per weekend the subject saw (up to 12). Don't
  rename or restructure these; 00a discovers every such folder automatically
  and merges its 12 files into a single session JSON under `inputs/merged/`
  the first time it runs.
- **A CSV starting with `PageTime`, at the top level** — oTree's PageTimes
  export (`session_code, participant_code, app_name, page_name,
  round_number, epoch_time_completed, ...`). It records the completion
  timestamp of every page a subject reached, including subjects who never
  finished. 00c uses it to compute `whole_experiment_time_seconds` and
  `choice_task_time_seconds`. Optional: if it's missing, those two columns
  are just left blank in the final dataset.
- **The oTree file, at the top level** — oTree's "all apps wide" session
  export (one row per participant, e.g.
  `session_2_all_apps_wide_<date>.csv`), containing treatment assignment
  (`participant.clutter_treatment`), the post-experiment questionnaire
  (NASA-TLX, cue recognition, visual complexity), and each round's
  `choice_cluster` / `choice_preferred`. Any `.csv` in `inputs/` that doesn't
  start with `PageTimes` is treated as a candidate for this file.
- **A choice-set file, at the top level (optional)** — a JSON with the
  canonical 9-listing roster per city × choice_set, plus substitute pools
  (e.g. `choice_sets_with_substitutes.json`). Only 00b actually needs a
  choice-set file: it will offer to use the canonical copy already committed
  in `booking_plugin/config/choice_sets_with_substitutes.json` instead, so
  dropping one in `inputs/` is only necessary if you want to join against a
  different/older choice-set version. (00c always reads the
  `booking_plugin/config/` copy directly — it does not look in `inputs/`.)

None of the scripts need to be told which file is which beyond this — they
scan `inputs/` and prompt you (or take a filename as a CLI argument) when more
than one candidate exists.

## Requirements

- **Python 3** with `pandas` and `matplotlib` (`pip install pandas
  matplotlib`).
- **R** with the packages `fs`, `readr`, `dplyr`, `ggplot2`, `fixest`,
  `scales` (and optionally `rstudioapi`, used only to detect the script's own
  path when run inside RStudio).

## Pipeline — run in this order

Run everything from the `booking_analyse/` directory (or use full paths —
each script locates `inputs/`/`outputs/` relative to its own file, not your
shell's working directory).

### 00_cleaning — build the analysis dataset

**1. `00a_tracking_converter.py`**
Converts every participant's raw extension JSON exports into one flat CSV of
events. Auto-merges any raw per-cell export folders it finds in `inputs/`
into `inputs/merged/` first, drops non-interaction `scraped_content` events,
and sorts every participant's events by timestamp.
- Input: participant folders in `inputs/` (or already-merged files in
  `inputs/merged/`)
- Output: `extension_converted.csv`, plus the merged JSONs in `inputs/merged/`
- Run: `python3 00_cleaning/00a_tracking_converter.py`

**2. `00b_choiceset_joining.py`**
Joins `extension_converted.csv` against the choice-set JSON on
`(targetPropertyId, city)`, backfills missing property IDs from the URL,
flags protocol violations (pages outside the subject's assigned design) and
cross-tab navigation (oTree vs. other allowed sites vs. forbidden sites), and
prints a join-validation matrix.
- Input: `extension_converted.csv` (from 00a), a choice-set JSON (prompted —
  either `booking_plugin/config/choice_sets_with_substitutes.json` or a file
  in `inputs/`)
- Output: `extension_joined_raw.csv` (full outer join, everything kept, for
  audit), `extension_joined.csv` (cleaned — noise statuses dropped)
- Run: `python3 00_cleaning/00b_choiceset_joining.py [choiceset_filename]`
  (interactive prompts for the choice-set file and the oTree host domain if
  no argument is given)

**3. `00c_build_analysis_dataset.py`**
Builds the final listing-level dataset: one row per subject × city ×
weekend × listing, combining the tracked behavioural measures (clicks, time
on listing page, decision time, loading time, ...) with the oTree wide export
(treatment, questionnaire) and the choice-set roster (including
in-session substitutions).
- Input: `extension_converted.csv` (from 00a), `extension_joined.csv` (from
  00b), the oTree wide CSV and PageTimes CSV (both prompted from `inputs/`),
  `booking_plugin/config/choice_sets_with_substitutes.json`
- Output: `analysis_dataset.csv`
- Run: `python3 00_cleaning/00c_build_analysis_dataset.py [otree_csv_name] [pagetimes_csv_name]`
  (interactive prompts if arguments are omitted)

### 01_analysis — checks and hypothesis tests

**4a. `01b_choicesetnumbers.py`** (independent data-quality check, Python)
Reads the raw per-cell JSONs directly (not `analysis_dataset.csv`) to
double-check, per participant × weekend, how many distinct hotels were
actually shown and how often a substitute hotel had to be used.
- Input: participant folders in `inputs/`
- Output: `choice_set_size_histogram_python.png`,
  `substitute_weekend_histogram_python.png`
- Run: `python3 01_analysis/01b_choicesetnumbers.py`

**4b. `01b_choicesetnumbers.R`** (same check, R, off the built dataset)
Same question as 4a, but computed from `analysis_dataset.csv`'s
`n_hotels_in_choice_set` column instead of re-reading the raw JSONs.
- Input: `analysis_dataset.csv` (from 00c)
- Output: `choice_set_size_histogram.png`
- Run: `Rscript 01_analysis/01b_choicesetnumbers.R`

**5. `01_analysis.rmd`**
The main analysis notebook: applies the pre-registration's sample exclusions,
reports descriptives (choice completeness, loading time), and runs every
pre-registered hypothesis test (H1.1, H1.2.1/H1.2.2, H2.1.1–H2.1.3, H2.2,
H2.3.1–H2.3.3, H3.1, H3.2) plus the manipulation checks.
- Input: `analysis_dataset.csv` (from 00c)
- Output: knitted report (console/inline plots); run interactively in
  RStudio, or render headless
- Run: open in RStudio and "Run All", or
  `Rscript -e "rmarkdown::render('01_analysis/01_analysis.rmd')"`
