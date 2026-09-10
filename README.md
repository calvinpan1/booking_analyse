# Booking in the dark -- replication package

Reproduces every number and figure of the paper from the raw exports of the
experiment: the browser extension's per-cell JSON files, the oTree "all apps
wide" and PageTimes exports, and the choice-set file.

## Layout

```
main.R          the only file that knows where anything lives; runs everything
build/          stage 1, raw/ -> inputs/   (Python cleaning, one R step)
analysis/       stage 2, inputs/ -> outputs/ (R)
tools/          not run by main.R: synthetic fixture, checks against the paper
raw/            raw exports (see below) + choice_sets_with_substitutes.json
inputs/         intermediate, participant-level files (never edit by hand)
outputs/        values_BookingAnalysis.tex and figures/ for the paper
```

## Requirements

- R with `dplyr`, `readr`, `ggplot2`, `fixest`, `scales`.
- Python 3 with `pandas` and `numpy`.

## Running

```
Rscript main.R              # everything
Rscript main.R build        # stage 1 only
Rscript main.R analyse      # stage 2 only
Rscript main.R analysis/02_hypotheses.R   # one script, paths already set
```

`raw/` must contain, at any depth: one folder per participant holding the
`cellNN_<city>_<checkin>.json` exports, one `all_apps_wide*.csv` and one
`PageTimes*.csv` (the most recent of each is used), and
`choice_sets_with_substitutes.json`. Paths can be overridden with the
environment variables `BOOKING_RAW`, `BOOKING_INPUTS`, `BOOKING_OUTPUTS`,
`BOOKING_PAPER` (an Overleaf clone to copy outputs into; unset = no copy) and
`BOOKING_PYTHON` (interpreter, default `python3`, `python` on Windows).

While the package lives in the synced project folder, `main.R` reads `raw/` and
writes `inputs/` under `SSD_DATA_ROOT/Booking_in_the_dark/` when that
variable is set, so participant-level files stay on the SSD; the block doing
this is marked INTERIM and is to be deleted once the package moves there.

## Scripts

| Script | Reads | Writes |
|---|---|---|
| `build/01_convert_tracking.py` | participant folders | `inputs/merged/`, `inputs/extension_converted.csv` |
| `build/02_join_choice_sets.py` | converted events, choice sets | `inputs/extension_joined_raw.csv`, `inputs/extension_joined.csv` |
| `build/03_build_analysis_dataset.py` | joined events, oTree, PageTimes | `inputs/analysis_dataset.csv` (subject x city x weekend x listing) |
| `build/04_tracking_measures.R` | event files | `inputs/subject_tracking.csv`, `inputs/tooltip_cells.csv` |
| `analysis/01_sample.R` | analysis dataset | `inputs/analysis_sample.csv` (preregistered exclusions), descriptives |
| `analysis/02_hypotheses.R` | analysis sample | H1, H2, H3, manipulation check, preference-measure validity |
| `analysis/03_figures.R` | analysis sample | the three figures and their sample sizes |
| `analysis/04_exploratory.R` | analysis sample, tracking measures | belief, decision time, interface use, coded open answers |

Each analysis script writes `outputs/values/values_<script>.tex`; `main.R`
concatenates them into `outputs/values_BookingAnalysis.tex` after every run.
Scripts contain no paths and no `source()`: the invariant
`grep -rn 'Sys.getenv\|source(' build/ analysis/` returns nothing.

## Checks (tools/)

```
python3 tools/make_synthetic_raw.py /tmp/fake/raw raw/choice_sets_with_substitutes.json 60
Rscript tools/compare_values.R outputs/values_BookingAnalysis.tex <overleaf>/values/values_BookingAnalysis.tex
Rscript tools/check_paper_coverage.R <overleaf> outputs/values_BookingAnalysis.tex <overleaf>/values/values_BookingAnalysis.tex
```

The first fabricates a raw tree in the export formats (random values, no
participant data) for smoke tests. The second compares two values files
name by name. The third lists every value command the paper cites that the
package does not define.
