# Booking Experiment Analysis Script
#
# Authors: Calvin Pan Created: June 8th 2026 Last modified: June 30th 2026
#
# NOTE: Before using this script, please run your data through 00a-00c.py.
#
# - 00a cleans all of the json files created by the extension, and outputs a
#   CSV with all the events by all experimental subjects concatenated together.
# - 00b joins this data from the extension with the choice set (option of the
#   /config folder version or a file in /inputs). It also gets rid of events
#   that should not exist.
#     - Deletes hotels that are not in the choice set that pop up and get
#       recorded when the extension refreshes Booking.com repeatedly (users
#       never see those hotels).
#     - Flags if users somehow manage to access features of Booking.com that
#       they shouldn't have access to, like making their own searches.
# - 00c joins the extension data with data from oTree. It creates a version of
#   the data that is ready to analyze, with each observation being
#   subject x city x weekend x listing. Each subject should have
#   3 cities x 4 weekends x 9 listings = 108 observations.
# - The data is structured in this format so that you can use groupby to
#   analyze at the level that you would like.
#
# --- Converted from 01_analysis.rmd to a plain R script (this run uses the
# synthetic dataset, see the "Adapted from Rmd" note below for what changed). ---

# Import cleaned data outputted by 00a-00c.py (data cleaning pipeline in Python)

# First, find filepaths
library(fs)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Adapted from Rmd: the original chunk detected script_path via
# knitr::current_input() (when knitting) or rstudioapi (interactive RStudio).
# Neither applies to a plain Rscript run. Also adapted to be portable across
# machines/OSes (this now also runs on Windows, on the SSD-equipped machine)
# instead of a hardcoded Mac path: parsed from the --file= argument Rscript
# passes itself, falling back to rstudioapi for interactive sessions.
get_script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[1])))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(rstudioapi::getActiveDocumentContext()$path)
  }
  stop("Could not determine this script's own path (not run via Rscript or RStudio).")
}
script_path <- get_script_path()

script_dir <- path_dir(path_dir(script_path)) # Define script directory

# Adapted from Rmd: raw_dir/target_dir mirror the Python cleaning scripts'
# _ssd_paths.py convention (one shared SSD_DATA_ROOT env var across
# projects). On the SSD-equipped machine this reads/writes REAL data under
# <SSD_DATA_ROOT>/Booking_in_the_dark/{raw,inputs}/ instead of the
# synced project. Unset SSD_DATA_ROOT (the default) keeps the old
# local analyse/inputs, analyse/outputs behaviour for both.
#
# LOCAL_OUTPUT_DIR is separate and ALWAYS the local analyse/outputs/
# folder (never the SSD target dir), regardless of SSD_DATA_ROOT: the
# synthetic dataset and values.tex generated at the end of this script are
# privacy-safe artifacts meant to be picked up from the synced
# project, so they are written there directly rather than needing a manual
# copy step off the SSD machine.
ssd_root <- Sys.getenv("SSD_DATA_ROOT", unset = "")
if (nzchar(ssd_root)) {
  project_root <- path(ssd_root, "Booking_in_the_dark")
  INPUT_DIR  <- path(project_root, "raw")
  OUTPUT_DIR <- path(project_root, "inputs")
} else {
  INPUT_DIR  <- path(script_dir, "inputs")
  OUTPUT_DIR <- path(script_dir, "outputs")
}
LOCAL_OUTPUT_DIR <- path(script_dir, "outputs")
dir_create(LOCAL_OUTPUT_DIR)

# Locate the Overleaf-synced paper project (a sibling of script_dir named
# "overleaf_<id>") so values.tex and paper figures can be written straight
# into it instead of needing a manual copy step after every run. Found by
# pattern rather than hardcoding the id, since the id is specific to this
# Overleaf project and could change if the project is ever recreated.
# Resolved relative to script_dir (itself derived from the running script's
# own path, never a hardcoded absolute path) so this works unmodified
# regardless of where the sync folder sits on a given machine.
overleaf_candidates <- dir_ls(path_dir(script_dir), type = "directory", regexp = "/overleaf_")
if (length(overleaf_candidates) == 0) {
  stop("Could not find an overleaf_* project directory next to ", script_dir,
       " -- values.tex and figures would silently not reach the paper. ",
       "Check the Overleaf project is synced locally, or update this path logic ",
       "if it has been moved/renamed.")
}
OVERLEAF_DIR <- overleaf_candidates[[1]]
OVERLEAF_VALUES_DIR <- path(OVERLEAF_DIR, "values")
OVERLEAF_FIGURES_DIR <- path(OVERLEAF_DIR, "illustrations", "h3_cluster_corr")
dir_create(OVERLEAF_VALUES_DIR)
dir_create(OVERLEAF_FIGURES_DIR)

CLEANED_CSV <- path(OUTPUT_DIR, "analysis_dataset.csv")

# Load packages used throughout the script
library(readr)
library(dplyr)
library(fixest)   # fixed-effects OLS / logit / probit, clustered SE
library(ggplot2)  # descriptives plots

# Adapted from Rmd: save descriptive/audit plots to PDF instead of relying on
# knitr to embed them inline, since a plain Rscript run has no such capture.
PLOTS_PDF <- path(OUTPUT_DIR, "01_analysis_plots.pdf")
pdf(PLOTS_PDF)

# Adapted from Rmd: source the shared \newcommand-writer helpers and start a
# fresh values.tex file for this run, so every computed value below (coefs,
# SEs, p-values, descriptive stats) gets written out for \input{} into the
# LaTeX article, instead of only living in this script's console/log output.
source(path(script_dir, "R", "tex_values.R"))
VALUES_TEX <- path(LOCAL_OUTPUT_DIR, "values_BookingAnalysis.tex")
file.create(VALUES_TEX)

# Load the cleaned analysis dataset (one row per subject x city x weekend x listing)
df <- read_csv(CLEANED_CSV, show_col_types = FALSE)

# Make sure ID variables used as fixed effects are factors
df <- df %>%
  mutate(
    participant_code = factor(participant_code),
    city = factor(city)
  )

glimpse(df)

### Sample exclusions
# Per the pre-registration's data inclusion and exclusion criteria, two rules
# apply to this dataset: (1) "Technical issues/link failures" — subjects who
# hit a technical issue that prevented them from completing all 12 choices (3
# cities x 4 weekends) are fully excluded from the analysis (e.g. a `reserve`
# event that never got tracked for some weekend, or a session abandoned
# partway through)
#
# (2) "Reduced choice sets" — if a city x weekend choice set ends up with
# fewer than 9 listings, only the four weekends for that subject x city are
# dropped, while the subject's other cities and post-experiment questionnaire
# variables remain in the analysis. The code below currently implements
# exclusion (1) only.

n_choices_per_subject <- df %>%
  group_by(participant_code) %>%
  summarise(
    n_choices = n_distinct(weekend_number_global[listing_chosen]),
    .groups = "drop"
  )

excluded_subjects <- n_choices_per_subject %>%
  filter(n_choices < 12) %>%
  pull(participant_code)

if (length(excluded_subjects) > 0) {
  cat("Excluding", length(excluded_subjects), "subject(s) with fewer than 12 choices made:\n")
  print(n_choices_per_subject %>% filter(participant_code %in% excluded_subjects))
} else {
  cat("No subjects excluded — every subject made all 12 choices.\n")
}

# Keep a pre-exclusion copy for the descriptives below — loading time is a
# technical/logistics measure independent of whether the subject completed
# all 12 choices, so it's reported on the full collected sample, not just the
# subjects that survive the exclusion.
df_all <- df

df <- df %>%
  filter(!participant_code %in% excluded_subjects) %>%
  droplevels()

# Reduced choice sets: if a city x weekend choice set ends up with fewer than 9
# listings, drop all 4 weekends for that subject x city pair (the subject's
# other cities and questionnaire variables are kept). Reuses 00c's own
# n_hotels_in_choice_set (nunique of property_slug per weekend) rather than
# recomputing a row count here, so this stays correct even if a future upstream
# change ever produces duplicate listing rows within a weekend.
listings_per_weekend <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(n_listings = first(n_hotels_in_choice_set), .groups = "drop")

reduced_choice_sets <- listings_per_weekend %>%
  filter(n_listings < 9) %>%
  distinct(participant_code, city)

if (nrow(reduced_choice_sets) > 0) {
  cat("Dropping", nrow(reduced_choice_sets),
      "subject x city pair(s) with a reduced (<9 listings) choice set in at least one weekend:\n")
  print(reduced_choice_sets)
} else {
  cat("No reduced choice sets (<9 listings) found — no subject x city pairs dropped.\n")
}

df <- df %>%
  anti_join(reduced_choice_sets, by = c("participant_code", "city")) %>%
  droplevels()

### Descriptives — choice completeness & loading time
# Two data-quality checks on the full collected sample (before the exclusion
# above): the share of subjects who did not complete all 12 choices, and how
# long Booking took to load the 9-listing choice set each weekend (the
# loading_time_seconds measure built in 00c from the extension's preload
# event).

# --- Share of subjects with fewer than 12 choices made ---
n_subjects_total    <- nrow(n_choices_per_subject)
n_subjects_excluded <- sum(n_choices_per_subject$n_choices < 12)
pct_excluded        <- 100 * n_subjects_excluded / n_subjects_total

cat(n_subjects_excluded, "of", n_subjects_total, "subjects (",
    round(pct_excluded, 1), "%) made fewer than 12 choices.\n\n")

write_tex_value("NSubjectsTotal", n_subjects_total, fmt = "%d", file = VALUES_TEX)
write_tex_value("NSubjectsExcluded", n_subjects_excluded, fmt = "%d", file = VALUES_TEX)
write_tex_value("NSubjectsRetained", n_distinct(df$participant_code), fmt = "%d", file = VALUES_TEX)
write_tex_value("NChoiceSetsRetained", nrow(distinct(df, participant_code, weekend_number_global)), fmt = "%d", file = VALUES_TEX)
write_tex_value("PctSubjectsExcluded", format_pct(pct_excluded / 100), file = VALUES_TEX)

# --- Pretask selection: of the 4 candidate cities and 9 candidate weekends,
# each subject keeps 3 cities and 4 weekends. Since analysis_dataset.csv only
# carries the RETAINED cities/checkin dates (not the full menu each subject
# was offered), infer what a subject dropped as (full universe) minus (cities
# / checkin dates actually present in their rows) — computed on df_all (the
# full collected sample, before the technical/reduced-choice-set exclusions
# below), since this is about voluntary pretask selection, not those rules. ---
all_cities <- sort(unique(df_all$city))

city_by_subject <- df_all %>%
  group_by(participant_code) %>%
  summarise(cities = list(unique(city)), .groups = "drop")

n_pretask_subjects <- nrow(city_by_subject)

city_exclusion_counts <- setNames(sapply(all_cities, function(ct) {
  sum(sapply(city_by_subject$cities, function(cs) !(ct %in% cs)))
}), all_cities)

# Adapted from Rmd: no such labels existed before this addition — CamelCase,
# letters-only per the r-tex-values naming convention (LaTeX command names
# can't contain digits/hyphens/underscores).
city_labels <- c(
  "arcachon" = "Arcachon", "la-ciotat" = "LaCiotat",
  "le-treport" = "LeTreport", "sete" = "Sete"
)

cat("\nPretask city exclusion (of", n_pretask_subjects, "subjects, each drops 1 of 4 cities):\n")
for (ct in all_cities) {
  pct <- 100 * city_exclusion_counts[[ct]] / n_pretask_subjects
  cat(sprintf("  %-12s excluded by %d subjects (%.1f%%)\n", ct, city_exclusion_counts[[ct]], pct))
  label <- city_labels[[ct]]
  write_tex_value(paste0("NCityExcluded", label), city_exclusion_counts[[ct]], fmt = "%d", file = VALUES_TEX)
  write_tex_value(paste0("PctCityExcluded", label), format_pct(city_exclusion_counts[[ct]] / n_pretask_subjects), file = VALUES_TEX)
}
write_tex_value("NPretaskSubjects", n_pretask_subjects, fmt = "%d", file = VALUES_TEX)

# Same logic for weekend inclusion: of the 9 candidate checkin dates, each
# subject keeps 4. Reported "just in case" — not currently cited numerically
# in the paper, only qualitatively (which weekends were chosen less often).
weekend_by_subject <- df_all %>%
  group_by(participant_code) %>%
  summarise(weekends = list(unique(checkin)), .groups = "drop")

all_weekends <- sort(unique(df_all$checkin))
weekend_inclusion_counts <- setNames(sapply(all_weekends, function(wk) {
  sum(sapply(weekend_by_subject$weekends, function(ws) wk %in% ws))
}), as.character(all_weekends))

weekend_labels <- c(
  "2026-10-02" = "OctTwo", "2026-10-09" = "OctNine", "2026-10-16" = "OctSixteen",
  "2026-10-23" = "OctTwentyThree", "2026-10-30" = "OctThirty", "2026-11-06" = "NovSix",
  "2026-11-13" = "NovThirteen", "2026-11-20" = "NovTwenty", "2026-11-27" = "NovTwentySeven"
)

cat("\nPretask weekend inclusion (of", n_pretask_subjects, "subjects, each keeps 4 of 9 candidate weekends):\n")
for (i in seq_along(all_weekends)) {
  wk <- all_weekends[i]
  wk_str <- format(wk)
  n_incl <- weekend_inclusion_counts[[i]]
  pct <- 100 * n_incl / n_pretask_subjects
  cat(sprintf("  %-12s included by %d subjects (%.1f%%)\n", wk_str, n_incl, pct))
  label <- weekend_labels[[wk_str]]
  write_tex_value(paste0("NWeekendIncluded", label), n_incl, fmt = "%d", file = VALUES_TEX)
  write_tex_value(paste0("PctWeekendIncluded", label), format_pct(n_incl / n_pretask_subjects), file = VALUES_TEX)
}

# --- Loading time: weekend-level measure, so collapse the listing-level
# df_all (9 rows/weekend) down to one row per (participant_code,
# weekend_number_global) before summarising/plotting — otherwise every
# weekend's loading time would be counted 9 times over. ---
loading_by_weekend <- df_all %>%
  group_by(participant_code, weekend_number_global) %>%
  summarise(loading_time_seconds = first(loading_time_seconds), .groups = "drop")

n_missing_loading <- sum(is.na(loading_by_weekend$loading_time_seconds))

cat("Loading time (seconds), across", nrow(loading_by_weekend), "subject x weekend observations",
    if (n_missing_loading > 0) paste0(" (", n_missing_loading, " missing — no preload event tracked):\n") else ":\n")
cat("  Mean:  ", round(mean(loading_by_weekend$loading_time_seconds, na.rm = TRUE), 2), "s\n")
cat("  Median:", round(median(loading_by_weekend$loading_time_seconds, na.rm = TRUE), 2), "s\n")
cat("  SD:    ", round(sd(loading_by_weekend$loading_time_seconds, na.rm = TRUE), 2), "s\n")

write_tex_value("MeanLoadingTime", mean(loading_by_weekend$loading_time_seconds, na.rm = TRUE), fmt = "%.2f", file = VALUES_TEX)
write_tex_value("MedianLoadingTime", median(loading_by_weekend$loading_time_seconds, na.rm = TRUE), fmt = "%.2f", file = VALUES_TEX)
write_tex_value("SDLoadingTime", sd(loading_by_weekend$loading_time_seconds, na.rm = TRUE), fmt = "%.2f", file = VALUES_TEX)
write_tex_value("NObsLoadingTime", nrow(loading_by_weekend), fmt = "%d", file = VALUES_TEX)

print(ggplot(loading_by_weekend, aes(x = loading_time_seconds)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", na.rm = TRUE) +
  labs(
    title = "Distribution of loading time per weekend",
    x = "Loading time (seconds)",
    y = "Number of subject x weekend observations"
  ) +
  theme_minimal())

# --- Substitute usage per choice set: how often the extension had to replace a
# listing missing from Booking's live inventory with one from the same cluster
# and cue status (the mechanism described in the paper's "Controlling the
# choice sets" paragraph). One row per (participant_code,
# weekend_number_global) — i.e. per choice set — counting how many of its nine
# listings were substitutes. Uses df_all, not df: this describes what the
# extension actually delivered during the sessions, before any preregistered
# analysis exclusion. ---
if (!"is_substitute_listing" %in% names(df_all)) {
  cat("! 'is_substitute_listing' is not a column in analysis_dataset.csv --",
      "00c_build_analysis_dataset.py must be re-run; substitute values skipped.\n")
} else {
  substitutes_by_set <- df_all %>%
    group_by(participant_code, weekend_number_global) %>%
    summarise(n_substitutes = sum(as.logical(is_substitute_listing), na.rm = TRUE),
              .groups = "drop")

  n_sets_total    <- nrow(substitutes_by_set)
  n_sets_sub_zero <- sum(substitutes_by_set$n_substitutes == 0)
  n_sets_sub_one  <- sum(substitutes_by_set$n_substitutes == 1)
  n_sets_sub_two  <- sum(substitutes_by_set$n_substitutes == 2)
  n_sets_sub_more <- sum(substitutes_by_set$n_substitutes >= 3)
  n_sets_sub_any  <- sum(substitutes_by_set$n_substitutes > 0)
  n_sub_listings  <- sum(substitutes_by_set$n_substitutes)
  max_sub         <- if (n_sets_total > 0) max(substitutes_by_set$n_substitutes) else 0L

  cat("\nSubstitute usage across", n_sets_total, "delivered choice sets:\n")
  print(table(substitutes_by_set$n_substitutes, dnn = "substitutes per set"))
  cat("  At least one substitute:", n_sets_sub_any,
      sprintf("(%.1f%%)\n", 100 * n_sets_sub_any / n_sets_total))
  cat("  Maximum in a single set:", max_sub, "\n")
  cat("  Substitute listings displayed in total:", n_sub_listings, "\n")

  write_tex_value("NChoiceSetsSubstitutesTotal", n_sets_total,    fmt = "%d", file = VALUES_TEX)
  write_tex_value("NChoiceSetsSubstitutesZero",  n_sets_sub_zero, fmt = "%d", file = VALUES_TEX)
  write_tex_value("NChoiceSetsSubstitutesOne",   n_sets_sub_one,  fmt = "%d", file = VALUES_TEX)
  write_tex_value("NChoiceSetsSubstitutesTwo",   n_sets_sub_two,  fmt = "%d", file = VALUES_TEX)
  write_tex_value("NChoiceSetsSubstitutesThreeOrMore", n_sets_sub_more, fmt = "%d", file = VALUES_TEX)
  write_tex_value("NChoiceSetsSubstitutesAny",   n_sets_sub_any,  fmt = "%d", file = VALUES_TEX)
  write_tex_value("PctChoiceSetsSubstitutesAny", format_pct(n_sets_sub_any / n_sets_total), file = VALUES_TEX)
  write_tex_value("MaxSubstitutesPerChoiceSet",  max_sub,         fmt = "%d", file = VALUES_TEX)
  write_tex_value("NSubstituteListings",         n_sub_listings,  fmt = "%d", file = VALUES_TEX)
}

# --- Session duration: whole-experiment and choice-task time, from oTree's
# PageTimes export (see 00c PART F0). Subject-level (one value per subject),
# so distinct() rather than a weekend-level collapse. Reported on the
# post-exclusion analysis sample (df, not df_all): unlike loading time, an
# excluded (abandoned) subject's session duration is not a comparable
# "how long does the task normally take" observation. Plain descriptives, not
# an effect or a correlation, so they live here with the other sample
# descriptives rather than in the exploratory-analysis section below. ---
resp_time_df <- df %>% distinct(participant_code, whole_experiment_time_seconds, choice_task_time_seconds)

write_tex_value("MeanWholeExperimentTimeMinutes", mean(resp_time_df$whole_experiment_time_seconds, na.rm = TRUE) / 60, fmt = "%.1f", file = VALUES_TEX)
write_tex_value("MedianWholeExperimentTimeMinutes", median(resp_time_df$whole_experiment_time_seconds, na.rm = TRUE) / 60, fmt = "%.1f", file = VALUES_TEX)
write_tex_value("SDWholeExperimentTimeMinutes", sd(resp_time_df$whole_experiment_time_seconds, na.rm = TRUE) / 60, fmt = "%.1f", file = VALUES_TEX)
write_tex_value("MeanChoiceTaskTimeMinutes", mean(resp_time_df$choice_task_time_seconds, na.rm = TRUE) / 60, fmt = "%.1f", file = VALUES_TEX)
write_tex_value("MedianChoiceTaskTimeMinutes", median(resp_time_df$choice_task_time_seconds, na.rm = TRUE) / 60, fmt = "%.1f", file = VALUES_TEX)
write_tex_value("SDChoiceTaskTimeMinutes", sd(resp_time_df$choice_task_time_seconds, na.rm = TRUE) / 60, fmt = "%.1f", file = VALUES_TEX)
write_tex_value("NObsExperimentTime", sum(!is.na(resp_time_df$whole_experiment_time_seconds)), fmt = "%d", file = VALUES_TEX)

### Descriptives — sample characteristics (demographics, comprehension checks)
# Demographics (gender, age, student status, household structure, Paris
# residency, Booking.com familiarity) and comprehension-check failure rates
# are subject-level, collected in the post-experiment questionnaire /
# instructions block respectively. Guarded by column existence so this
# section is a no-op against an older analysis_dataset.csv that predates
# these fields (00c_build_analysis_dataset.py's COLUMNS list).
demographic_cols <- c(
  "gender", "age", "student_status", "household_structure",
  "paris_resident", "booking_familiarity"
)
comprehension_cols <- c(
  "failed_comprehension_prize", "failed_comprehension_choice_city_weekend",
  "failed_comprehension_no_cancellation"
)

if (any(c(demographic_cols, comprehension_cols) %in% names(df_all))) {
  sample_chars_df <- df_all %>%
    group_by(participant_code) %>%
    summarise(across(any_of(c(demographic_cols, comprehension_cols)), first), .groups = "drop")

  n_sample_chars <- nrow(sample_chars_df)
  cat("\nSample characteristics (N =", n_sample_chars, "subjects):\n")

  # Age: mean/median/SD.
  if ("age" %in% names(sample_chars_df)) {
    cat("  Age: mean", round(mean(sample_chars_df$age, na.rm = TRUE), 1),
        "median", round(median(sample_chars_df$age, na.rm = TRUE), 1),
        "SD", round(sd(sample_chars_df$age, na.rm = TRUE), 1), "\n")
    write_tex_value("MeanAge", mean(sample_chars_df$age, na.rm = TRUE), fmt = "%.1f", file = VALUES_TEX)
    write_tex_value("MedianAge", median(sample_chars_df$age, na.rm = TRUE), fmt = "%.1f", file = VALUES_TEX)
    write_tex_value("SDAge", sd(sample_chars_df$age, na.rm = TRUE), fmt = "%.1f", file = VALUES_TEX)
  }

  # Booking.com familiarity (1-4 integer scale): mean/SD.
  if ("booking_familiarity" %in% names(sample_chars_df)) {
    write_tex_value("MeanBookingFamiliarity", mean(sample_chars_df$booking_familiarity, na.rm = TRUE), fmt = "%.2f", file = VALUES_TEX)
    write_tex_value("SDBookingFamiliarity", sd(sample_chars_df$booking_familiarity, na.rm = TRUE), fmt = "%.2f", file = VALUES_TEX)
  }

  # Paris residency: share TRUE.
  if ("paris_resident" %in% names(sample_chars_df)) {
    write_tex_value("PctParisResident", format_pct(mean(sample_chars_df$paris_resident, na.rm = TRUE)), file = VALUES_TEX)
  }

  # Categorical fields (gender, student_status, household_structure): one
  # \Pct<Field><Level> command per observed level, letters-only labels built
  # from a fixed lookup (raw French questionnaire text -> CamelCase label) so
  # command names stay stable across runs regardless of level ordering.
  categorical_field_labels <- list(
    gender = c("Homme" = "Male", "Femme" = "Female", "Autre" = "Other"),
    student_status = c("Étudiant" = "Student", "Non étudiant" = "NonStudent"),
    household_structure = c(
      "J'habite seul" = "Alone", "J'habite en couple" = "Couple",
      "J'habite avec ma famille" = "Family", "Autre" = "OtherHousehold"
    )
  )
  for (field in names(categorical_field_labels)) {
    if (!field %in% names(sample_chars_df)) next
    field_label <- tools::toTitleCase(gsub("_", " ", field))
    field_label <- gsub(" ", "", field_label)
    lookup <- categorical_field_labels[[field]]
    tab <- table(sample_chars_df[[field]], useNA = "no")
    for (level in names(tab)) {
      level_label <- if (level %in% names(lookup)) lookup[[level]] else gsub("[^A-Za-z]", "", level)
      pct <- tab[[level]] / sum(tab)
      cat("  ", field, "=", level, ":", round(100 * pct, 1), "%\n")
      write_tex_value(paste0("Pct", field_label, level_label), format_pct(pct), file = VALUES_TEX)
    }
  }

  # Comprehension-check failure rates: share of subjects who failed each
  # check, plus the share who failed at least one.
  present_comprehension_cols <- intersect(comprehension_cols, names(sample_chars_df))
  if (length(present_comprehension_cols) > 0) {
    comprehension_labels <- c(
      failed_comprehension_prize = "Prize",
      failed_comprehension_choice_city_weekend = "ChoiceCityWeekend",
      failed_comprehension_no_cancellation = "NoCancellation"
    )
    for (col in present_comprehension_cols) {
      pct_failed <- mean(sample_chars_df[[col]], na.rm = TRUE)
      cat("  Failed comprehension check (", col, "):", round(100 * pct_failed, 1), "%\n")
      write_tex_value(paste0("PctFailedComprehension", comprehension_labels[[col]]), format_pct(pct_failed), file = VALUES_TEX)
    }
    any_failed <- rowSums(sample_chars_df[present_comprehension_cols] == TRUE, na.rm = TRUE) > 0
    pct_any_failed <- mean(any_failed)
    cat("  Failed at least one comprehension check:", round(100 * pct_any_failed, 1), "%\n")
    write_tex_value("PctFailedComprehensionAny", format_pct(pct_any_failed), file = VALUES_TEX)
  }

  write_tex_value("NSampleCharacteristics", n_sample_chars, fmt = "%d", file = VALUES_TEX)
}

### Testing H1.1 — Effect of the cue on choice probability
# We test H1.1 using a fixed-effects logistic regression. The dependent
# variable is "Cue-eligible listing chosen", and the key predictor is
# "Endorsement Cue Visibility" (Treatment 1). The model includes subject and
# city fixed effects, which control for stable differences across people and
# across cities that are unrelated to the treatment itself. Our unit of
# analysis is the choice set: each subject contributes one observation per
# city per weekend, for a total of 12 observations per subject. H1.1 predicts
# that the cue increases the probability of choosing cue-eligible listings, so
# we use a one-sided test on the coefficient of interest (H0 : beta1 <=0, H1 :
# beta1 >0), with a Type I error rate of alpha = 0.10 on the positive tail.

# Collapse listing-level rows (9 per weekend) to one row per subject x city x weekend,
# since H1.1's DV is defined at the weekend level, not the listing level.
h11_df <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(
    # First, construction of the DV. In every single choice set, there are 3 listings out of the 9
    # (1 per cluster) that are ones that in the real-world Booking.com environment have a thumb:
    # i.e. they would have a cue if the subject is assigned to see cues that weekend. We are trying
    # to compare if the probability that a subject chooses a "would_be_cued" listing is higher
    # when the cue is visible vs. not. As such, chosen_cued_listing, the DV, is TRUE if the subject
    # chose a would_be_cued listing.
    chose_cued_listing = any(listing_chosen & would_be_cued, na.rm = TRUE),
    # treatment 1: TRUE if the cue graphic was actually shown this weekend (cell_thumb == "P")
    cue_visible = first(cued_weekend),
    .groups = "drop"
  ) %>%
  mutate(participant_code = factor(participant_code), city = factor(city))

# Fixed-effects logit: feglm() absorbs participant_code and city fixed effects
# (instead of estimating a dummy per subject/city), so cue_visible is identified
# off within-subject, within-city variation only.
m_h11 <- feglm(
  chose_cued_listing ~ cue_visible | participant_code + city,
  data = h11_df,
  family = "logit"
)

summary(m_h11)

# summary() above gives a two-sided p-value (H0: beta = 0); H1.1 predicts a
# specific direction (H0: beta <= 0, H1: beta > 0), so compute the one-sided
# p-value manually from the z-statistic.
coef_h11 <- coef(m_h11)["cue_visibleTRUE"]
se_h11   <- se(m_h11)["cue_visibleTRUE"]
z_h11    <- coef_h11 / se_h11
p_onesided_h11 <- pnorm(z_h11, lower.tail = FALSE)

cat("z =", round(z_h11, 3), " | one-sided p-value (H1: beta > 0) =", round(p_onesided_h11, 4), "\n")

# Adapted from Rmd: hypothesis labels used in \newcommand names must be
# letters-only (LaTeX rejects digits/dots in command names), so "H1.1" becomes
# "HOneOne" etc. throughout this script — same digit->word mapping every time.
write_tex_value("RegCoefHOneOne", coef_h11, file = VALUES_TEX)
write_tex_value("RegSEHOneOne", se_h11, file = VALUES_TEX)
write_tex_value("RegZHOneOne", z_h11, file = VALUES_TEX)
write_pvalue_pair("RegPvalHOneOne", p_onesided_h11, file = VALUES_TEX)
write_tex_value("RegStarsHOneOne", stars_from_pvalue(p_onesided_h11), file = VALUES_TEX)
write_tex_value("RegNHOneOne", nobs(m_h11), fmt = "%d", file = VALUES_TEX)


### Testing H1.2 — Effect of the cue on attention
#
# We test H1.2.1 using a fixed-effects logistic regression on "Cue-eligible
# listing clicked" — whether at least one cue-eligible listing's detail page
# is clicked on by the subject within a choice set (city x weekend) — and
# H1.2.2 using an OLS regression on "Time on the detail page of cue-eligible
# listings" — the log(seconds + 1) of the summed time spent across
# cue-eligible listings' pages within a choice set, coded as zero if no time
# was spent on any cue-eligible detail page. The key predictor in both models
# is "Endorsement Cue Visibility" (Treatment 1). Both models include subject
# and city fixed effects, which control for stable differences across people
# and across cities that are unrelated to the treatment itself. Our unit of
# analysis is the choice set: each subject contributes one observation per
# city per weekend, for a total of 12 observations per subject. H1.2 predicts
# that the cue increases attention to cue-eligible listings, so we use
# one-sided tests on the coefficient of interest (H0 : beta1 <=0, H1 : beta1
# >0), with a Type I error rate of alpha = 0.10 on the positive tail.

#### H1.2.1 first: Binary outcome "whether cued listing is clicked on" with logit & fixed effects
# Collapse listing-level rows (9 per weekend) to one row per subject x city x weekend:
# "Cue-eligible listing clicked" is defined as whether AT LEAST ONE cue-eligible
# listing is clicked within a choice set (city x weekend), built the same way
# H1.1's DV was.
h121_df <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(
    cue_listing_clicked = any(listing_clicked & would_be_cued, na.rm = TRUE),
    cue_visible = first(cued_weekend),
    .groups = "drop"
  ) %>%
  mutate(participant_code = factor(participant_code), city = factor(city))

# Fixed-effects logit: feglm() absorbs participant_code and city fixed effects
# (instead of estimating a dummy per subject/city), so cue_visible is identified
# off within-subject, within-city variation only.
m_h121 <- feglm(
  cue_listing_clicked ~ cue_visible | participant_code + city,
  data = h121_df,
  family = "logit"
)

summary(m_h121)

# summary() above gives a two-sided p-value (H0: beta = 0); H1.2 predicts a
# specific direction (H0: beta <= 0, H1: beta > 0), so compute the one-sided
# p-value manually from the z-statistic.
coef_h121 <- coef(m_h121)["cue_visibleTRUE"]
se_h121   <- se(m_h121)["cue_visibleTRUE"]
z_h121    <- coef_h121 / se_h121
p_onesided_h121 <- pnorm(z_h121, lower.tail = FALSE)

cat("z =", round(z_h121, 3), " | one-sided p-value (H1: beta > 0) =", round(p_onesided_h121, 4), "\n")

write_tex_value("RegCoefHOneTwoOne", coef_h121, file = VALUES_TEX)
write_tex_value("RegSEHOneTwoOne", se_h121, file = VALUES_TEX)
write_tex_value("RegZHOneTwoOne", z_h121, file = VALUES_TEX)
write_pvalue_pair("RegPvalHOneTwoOne", p_onesided_h121, file = VALUES_TEX)
write_tex_value("RegStarsHOneTwoOne", stars_from_pvalue(p_onesided_h121), file = VALUES_TEX)
write_tex_value("RegNHOneTwoOne", nobs(m_h121), fmt = "%d", file = VALUES_TEX)

#### H1.2.2 first: OLS "time on detail page of listing" with fixed effects
# Choice-set level: "Time on the detail page of cue-eligible listings" is the
# SUM of seconds across the (up to 3) cue-eligible listings within a weekend
# (0 if none were clicked/no time recorded), then log(seconds + 1)-transformed
# per the pre-registration's Transformations section.
h122_df <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(
    cue_time_seconds = sum(time_on_listing_page_seconds[would_be_cued], na.rm = TRUE),
    cue_visible = first(cued_weekend),
    .groups = "drop"
  ) %>%
  mutate(
    participant_code = factor(participant_code),
    city = factor(city),
    log_cue_time_seconds = log(cue_time_seconds + 1)
  )

# Fixed-effects OLS: feols() absorbs participant_code and city fixed effects
# (instead of estimating a dummy per subject/city), so cue_visible is identified
# off within-subject, within-city variation only.
m_h122 <- feols(
  log_cue_time_seconds ~ cue_visible | participant_code + city,
  data = h122_df
)

summary(m_h122)

# summary() above gives a two-sided p-value (H0: beta = 0); H1.2 predicts a
# specific direction (H0: beta <= 0, H1: beta > 0), so compute the one-sided
# p-value manually from the z-statistic.
coef_h122 <- coef(m_h122)["cue_visibleTRUE"]
se_h122   <- se(m_h122)["cue_visibleTRUE"]
z_h122    <- coef_h122 / se_h122
p_onesided_h122 <- pnorm(z_h122, lower.tail = FALSE)

cat("z =", round(z_h122, 3), " | one-sided p-value (H1: beta > 0) =", round(p_onesided_h122, 4), "\n")

write_tex_value("RegCoefHOneTwoTwo", coef_h122, file = VALUES_TEX)
write_tex_value("RegSEHOneTwoTwo", se_h122, file = VALUES_TEX)
write_tex_value("RegZHOneTwoTwo", z_h122, file = VALUES_TEX)
write_pvalue_pair("RegPvalHOneTwoTwo", p_onesided_h122, file = VALUES_TEX)
write_tex_value("RegStarsHOneTwoTwo", stars_from_pvalue(p_onesided_h122), file = VALUES_TEX)
write_tex_value("RegNHOneTwoTwo", nobs(m_h122), fmt = "%d", file = VALUES_TEX)


### Figure — Probability of choice by cue-eligibility x cue visibility
# Descriptive companion to Table tab:h1's H1.1 column: decomposes P(chosen)
# for CUE-ELIGIBLE listings by whether the badge was actually visible that
# weekend (`cued_weekend`), pooled and separately by city. Uses the full
# listing-level `df` (one row per subject x weekend x listing) rather than
# the h11_df collapse above. Non-cue-eligible listings are computed too (see
# `compute_choice_prob`) but dropped from the figure below: their choice
# probability is complementary to the cue-eligible one, so plotting both is
# redundant.

wilson_ci <- function(x, n, conf = 0.95) {
  # Wilson score interval for a binomial proportion -- better-behaved than the
  # normal (Wald) approximation at the sample sizes/probabilities here (some
  # city x condition cells are thin, and cue-eligible listings are only 1/3
  # of a choice set).
  z <- qnorm(1 - (1 - conf) / 2)
  p <- x / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half_width <- (z / denom) * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2)))
  data.frame(lower = pmax(0, centre - half_width), upper = pmin(1, centre + half_width))
}

compute_choice_prob <- function(data) {
  data %>%
    group_by(would_be_cued, cued_weekend) %>%
    summarise(n = n(), n_chosen = sum(listing_chosen), .groups = "drop") %>%
    mutate(prob = n_chosen / n) %>%
    bind_cols(wilson_ci(.$n_chosen, .$n))
}

# Human-readable facet labels (accent-free, matching the fig:h3clustercorr
# subcaptions' "Le Treport"/"Sete" convention for text embedded in a figure --
# body prose elsewhere keeps the accents). Kept separate from `city_labels`
# (slug -> CamelCase, used for \newcommand names), since the two mappings
# serve different purposes.
city_display_labels <- c(
  "arcachon" = "Arcachon", "la-ciotat" = "La Ciotat",
  "le-treport" = "Le Treport", "sete" = "Sete"
)

# CHOICE-SET level, aligned with the preregistered H1.1 outcome ("whether the
# participant chose a cue-eligible listing in a choice set"): one row per
# subject x city x weekend, outcome = any cue-eligible listing chosen. The
# earlier listing-level version (P(listing chosen) over 3 cue-eligible rows
# per set) had non-independent rows, since exactly one listing is chosen
# per set. Reuses h11_df, the very data frame the H1.1 regression is fit on,
# so the figure and Table tab:h1 describe the same observations.
set_choice_df <- h11_df %>%
  transmute(city, cued_weekend = cue_visible, would_be_cued = TRUE,
            listing_chosen = chose_cued_listing)

prob_pooled <- compute_choice_prob(set_choice_df) %>% mutate(city_facet = "Pooled")
prob_by_city <- set_choice_df %>%
  group_by(city) %>%
  group_modify(~ compute_choice_prob(.x)) %>%
  ungroup() %>%
  mutate(city_facet = unname(city_display_labels[as.character(city)])) %>%
  select(-city)

prob_fig_df <- bind_rows(prob_pooled, prob_by_city) %>%
  filter(would_be_cued) %>%
  mutate(
    city_facet = factor(city_facet, levels = c("Pooled", unname(city_display_labels))),
    cue_visible_label = factor(ifelse(cued_weekend, "Cue visible", "Cue hidden"),
                                levels = c("Cue hidden", "Cue visible"))
  )

# Categorical colors: slots 1-2 of the project's validated CVD-safe
# categorical palette (adjacent pair, passes the CVD/normal-vision gates in
# both light and dark mode -- see the dataviz skill's reference palette).
# Deliberately NOT color-matched to the actual (yellow) badge: hue here
# encodes the FACTOR (cue visible/hidden), not the stimulus.
prob_colors <- c("Cue hidden" = "#2a78d6", "Cue visible" = "#eb6834")

# NOTE: no ggtitle()/subtitle() baked into the plot itself -- see the
# latex-figures skill and the h3_cluster_corr figure below for why. The
# audit-PDF copy gets a title purely for flipping through PLOTS_PDF by eye;
# the saved/paper-facing PNG stays title-free.
# Only cue-eligible listings are plotted: their choice probability plus that
# of non-cue-eligible listings is redundant (the two are complementary), so
# showing the non-cue-eligible bars added no information.
p_choice_prob <- ggplot(prob_fig_df, aes(x = cue_visible_label, y = prob, fill = cue_visible_label)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, color = "#3a3a3a") +
  facet_wrap(~ city_facet, nrow = 1) +
  scale_fill_manual(values = prob_colors, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(x = NULL, y = "P(listing chosen)") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p_choice_prob_audit <- p_choice_prob + labs(
  title = "Choice probability by cue-eligibility and cue visibility",
  subtitle = "Pooled and by city, with 95% Wilson score CIs"
)
print(p_choice_prob_audit) # page in the audit PDF (PLOTS_PDF)

CHOICE_PROB_FIGURES_DIR <- path(LOCAL_OUTPUT_DIR, "figures")
dir_create(CHOICE_PROB_FIGURES_DIR)
choice_prob_fig_path <- path(CHOICE_PROB_FIGURES_DIR, "cue_choice_probability.png")
ggsave(choice_prob_fig_path, p_choice_prob, width = 9, height = 3.2, dpi = 300)
cat("Wrote cue choice-probability figure to", choice_prob_fig_path, "\n")

overleaf_choice_prob_dir <- path(OVERLEAF_DIR, "illustrations", "cue_probability")
dir_create(overleaf_choice_prob_dir)
overleaf_choice_prob_path <- path(overleaf_choice_prob_dir, "cue_choice_probability.png")
file_copy(choice_prob_fig_path, overleaf_choice_prob_path, overwrite = TRUE)
cat("Copied it into the Overleaf project at", overleaf_choice_prob_path, "\n")

# Sample sizes cited in the figure's Notes block (Pooled + one per city),
# named via city_labels (CamelCase) keyed off city_display_labels so the two
# mappings stay in lockstep without hardcoding the pairing twice.
facet_camel_lookup <- setNames(c("Pooled", unname(city_labels[names(city_display_labels)])),
                                c("Pooled", unname(city_display_labels)))
n_by_facet <- prob_fig_df %>%
  distinct(city_facet, would_be_cued, cued_weekend, n) %>%
  group_by(city_facet) %>%
  summarise(n_total = sum(n), .groups = "drop")
for (i in seq_len(nrow(n_by_facet))) {
  camel <- facet_camel_lookup[[as.character(n_by_facet$city_facet[i])]]
  write_tex_value(paste0("NObsChoiceProb", camel), n_by_facet$n_total[i], fmt = "%d", file = VALUES_TEX)
}


### Figure — Probability of choice by cue-eligibility x cue visibility x clutter
# Pooled-only variant of the figure above (no per-city facets): instead splits
# each cue-hidden/cue-visible bar into its no-clutter and clutter versions, so
# the four bars per x-axis category read as two adjacent pairs (hidden vs
# visible), each pair's clutter bar sitting right next to its no-clutter twin.

darken_hex <- function(hex, amount = 0.35) {
  # Simple multiplicative darken (no colorspace dependency): scale each RGB
  # channel toward 0 by `amount`, keeping the same hue/saturation family so
  # the clutter bar reads as "the same color, darker" rather than a new hue.
  rgb_mat <- grDevices::col2rgb(hex) * (1 - amount)
  grDevices::rgb(rgb_mat[1, ], rgb_mat[2, ], rgb_mat[3, ], maxColorValue = 255)
}

compute_choice_prob_clutter <- function(data) {
  data %>%
    group_by(would_be_cued, cued_weekend, clutter_high) %>%
    summarise(n = n(), n_chosen = sum(listing_chosen), .groups = "drop") %>%
    mutate(prob = n_chosen / n) %>%
    bind_cols(wilson_ci(.$n_chosen, .$n))
}

prob_clutter_df <- compute_choice_prob_clutter(df) %>%
  mutate(
    cue_eligible_label = factor(ifelse(would_be_cued, "Cue-eligible", "Not cue-eligible"),
                                 levels = c("Cue-eligible", "Not cue-eligible")),
    clutter_label = factor(ifelse(clutter_high, "Clutter", "No clutter"),
                            levels = c("No clutter", "Clutter")),
    # One fill level per (cue visibility x clutter) combo, ordered so that
    # each clutter bar is adjacent to its no-clutter analog within a
    # cue-visibility group: hidden/no-clutter, hidden/clutter,
    # visible/no-clutter, visible/clutter.
    bar_group = factor(
      paste(ifelse(cued_weekend, "Cue visible", "Cue hidden"), clutter_label, sep = " - "),
      levels = c("Cue hidden - No clutter", "Cue hidden - Clutter",
                 "Cue visible - No clutter", "Cue visible - Clutter")
    )
  )

# Same base hues as prob_colors (blue = cue hidden, orange = cue visible);
# the clutter bar of each pair is a darker shade of the same color, not a
# new hue, so the pairing reads visually without needing a second legend.
prob_clutter_colors <- c(
  "Cue hidden - No clutter"  = unname(prob_colors["Cue hidden"]),
  "Cue hidden - Clutter"     = darken_hex(prob_colors["Cue hidden"]),
  "Cue visible - No clutter" = unname(prob_colors["Cue visible"]),
  "Cue visible - Clutter"    = darken_hex(prob_colors["Cue visible"])
)

p_choice_prob_clutter <- ggplot(prob_clutter_df, aes(x = cue_eligible_label, y = prob, fill = bar_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.8),
                width = 0.15, color = "#3a3a3a") +
  scale_fill_manual(values = prob_clutter_colors, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(x = NULL, y = "P(listing chosen)") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p_choice_prob_clutter_audit <- p_choice_prob_clutter + labs(
  title = "Choice probability by cue-eligibility, cue visibility, and clutter",
  subtitle = "Pooled across cities, with 95% Wilson score CIs"
)
print(p_choice_prob_clutter_audit) # page in the audit PDF (PLOTS_PDF)

choice_prob_clutter_fig_path <- path(CHOICE_PROB_FIGURES_DIR, "cue_choice_probability_clutter.png")
ggsave(choice_prob_clutter_fig_path, p_choice_prob_clutter, width = 6, height = 4, dpi = 300)
cat("Wrote cue choice-probability-by-clutter figure to", choice_prob_clutter_fig_path, "\n")

overleaf_choice_prob_clutter_path <- path(overleaf_choice_prob_dir, "cue_choice_probability_clutter.png")
file_copy(choice_prob_clutter_fig_path, overleaf_choice_prob_clutter_path, overwrite = TRUE)
cat("Copied it into the Overleaf project at", overleaf_choice_prob_clutter_path, "\n")

# Sample sizes cited in the figure's Notes block, one per (cue-eligible x
# cue-visible x clutter) cell, named analogously to NObsChoiceProb* above.
clutter_camel_lookup <- c("No clutter" = "NoClutter", "Clutter" = "Clutter")
# One command per (eligibility x bar_group) cell: prob_clutter_df has both
# eligibility levels, so keying on bar_group alone wrote every name twice
# (with different n), which LaTeX rejects as a redefined \newcommand.
n_by_bar_group <- prob_clutter_df %>%
  distinct(would_be_cued, bar_group, n)
for (i in seq_len(nrow(n_by_bar_group))) {
  camel <- paste0(ifelse(n_by_bar_group$would_be_cued[i], "Eligible", "NotEligible"),
                  gsub("[^A-Za-z]", "", as.character(n_by_bar_group$bar_group[i])))
  write_tex_value(paste0("NObsChoiceProbClutter", camel), n_by_bar_group$n[i], fmt = "%d", file = VALUES_TEX)
}


### Figure — Median decision time by cue visibility, pooled and by city
# Companion to the choice-probability figure directly above (same Pooled +
# per-city panel layout, same fill colors), one weekend-level row per subject
# x city x weekend. Median, not mean, with interquartile-range whiskers:
# decision time is heavily right-skewed (a handful of very long sessions),
# so the median is the more representative bar height here, unlike the
# choice-probability figure's proportions (already well-behaved).

decision_time_fig_df <- df %>%
  distinct(participant_code, city, weekend_number_global, decision_time_seconds, cued_weekend) %>%
  filter(!is.na(decision_time_seconds), decision_time_seconds > 0)

compute_decision_time_summary <- function(data) {
  data %>%
    group_by(cued_weekend) %>%
    summarise(
      n = n(),
      median_time = median(decision_time_seconds),
      q25 = quantile(decision_time_seconds, 0.25),
      q75 = quantile(decision_time_seconds, 0.75),
      .groups = "drop"
    )
}

dtime_pooled <- compute_decision_time_summary(decision_time_fig_df) %>% mutate(city_facet = "Pooled")
dtime_by_city <- decision_time_fig_df %>%
  group_by(city) %>%
  group_modify(~ compute_decision_time_summary(.x)) %>%
  ungroup() %>%
  mutate(city_facet = unname(city_display_labels[as.character(city)])) %>%
  select(-city)

dtime_fig_df <- bind_rows(dtime_pooled, dtime_by_city) %>%
  mutate(
    city_facet = factor(city_facet, levels = c("Pooled", unname(city_display_labels))),
    cue_visible_label = factor(ifelse(cued_weekend, "Cue visible", "Cue hidden"),
                                levels = c("Cue hidden", "Cue visible"))
  )

# Same prob_colors mapping as the choice-probability figure above, so the two
# stacked figures read as one visual pair (blue = cue hidden, orange = cue
# visible, both times).
p_decision_time <- ggplot(dtime_fig_df, aes(x = cue_visible_label, y = median_time, fill = cue_visible_label)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.15, color = "#3a3a3a") +
  facet_wrap(~ city_facet, nrow = 1) +
  scale_fill_manual(values = prob_colors, name = NULL) +
  labs(x = NULL, y = "Median decision time (s)") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

p_decision_time_audit <- p_decision_time + labs(
  title = "Median decision time by cue visibility",
  subtitle = "Pooled and by city, whiskers show the interquartile range (25th-75th pctile)"
)
print(p_decision_time_audit) # page in the audit PDF (PLOTS_PDF)

decision_time_fig_path <- path(CHOICE_PROB_FIGURES_DIR, "cue_decision_time.png")
ggsave(decision_time_fig_path, p_decision_time, width = 9, height = 3.2, dpi = 300)
cat("Wrote decision-time figure to", decision_time_fig_path, "\n")

overleaf_decision_time_path <- path(overleaf_choice_prob_dir, "cue_decision_time.png")
file_copy(decision_time_fig_path, overleaf_decision_time_path, overwrite = TRUE)
cat("Copied it into the Overleaf project at", overleaf_decision_time_path, "\n")

n_by_facet_dtime <- dtime_fig_df %>% group_by(city_facet) %>% summarise(n_total = sum(n), .groups = "drop")
for (i in seq_len(nrow(n_by_facet_dtime))) {
  camel <- facet_camel_lookup[[as.character(n_by_facet_dtime$city_facet[i])]]
  write_tex_value(paste0("NObsDecisionTimeFig", camel), n_by_facet_dtime$n_total[i], fmt = "%d", file = VALUES_TEX)
}


### Figures — Click probability and time on the detail page, by cue-eligibility
### x cue visibility, pooled and by city (listing level)
# Descriptive companions to Table tab:h1's H1.2.1/H1.2.2 columns, in the same
# Pooled + per-city layout as the two figures above. Unlike the choice-
# probability figure (where the non-cue-eligible bars are redundant, since
# exactly one listing is chosen per set), a click or a long dwell on one
# listing does not preclude it on another, so here the cue-eligible vs
# not-cue-eligible split is informative: it shows whether the badge REDIRECTS
# attention (eligible up, non-eligible down) or merely adds to it.
#
# Time on page is the per-listing MEAN of seconds on the detail page, with
# never-clicked listings counted as zero (so it is P(click) x dwell given
# click, the same attention quantity H1.2.2 sums per set); a clicked listing
# whose dwell 00c could not measure (unknown, not zero) is dropped rather than
# zeroed. Mean +/- 1.96 SE rather than median/IQR: the median is zero in every
# cell (most listings are never clicked), which would leave nothing to plot.

# CHOICE-SET level, aligned with the preregistered H1.2 outcomes: per set,
# "at least one cue-eligible listing's detail page clicked" (H1.2.1) and the
# SUM of seconds on cue-eligible detail pages (H1.2.2), plus the same two
# quantities over the set's non-cue-eligible listings. One row per set x
# eligibility group (2 x 716 rows), so the CIs are over sets, not over the
# nine non-independent listings of a set.
listing_attention_df <- df %>%
  filter(!(listing_clicked & is.na(time_on_listing_page_seconds))) %>%
  mutate(time_on_page = ifelse(listing_clicked, time_on_listing_page_seconds, 0)) %>%
  group_by(participant_code, city, weekend_number_global, cued_weekend, would_be_cued) %>%
  summarise(listing_clicked = any(listing_clicked), time_on_page = sum(time_on_page), .groups = "drop")

compute_click_prob <- function(data) {
  data %>%
    group_by(would_be_cued, cued_weekend) %>%
    summarise(n = n(), n_clicked = sum(listing_clicked), .groups = "drop") %>%
    mutate(prob = n_clicked / n) %>%
    bind_cols(wilson_ci(.$n_clicked, .$n))
}

# Mean over all sets (never-clicked = 0 s), +/- 1.96 SE.
compute_time_on_page <- function(data) {
  data %>%
    group_by(would_be_cued, cued_weekend) %>%
    summarise(
      n = n(),
      mean_time = mean(time_on_page),
      se_time = sd(time_on_page) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(lower = pmax(0, mean_time - 1.96 * se_time), upper = mean_time + 1.96 * se_time)
}

# Same pooled + by-city assembly as prob_fig_df above, factored out since it
# now serves two figures.
build_eligibility_fig_df <- function(data, summarise_fn) {
  pooled <- summarise_fn(data) %>% mutate(city_facet = "Pooled")
  by_city <- data %>%
    group_by(city) %>%
    group_modify(~ summarise_fn(.x)) %>%
    ungroup() %>%
    mutate(city_facet = unname(city_display_labels[as.character(city)])) %>%
    select(-city)
  bind_rows(pooled, by_city) %>%
    mutate(
      city_facet = factor(city_facet, levels = c("Pooled", unname(city_display_labels))),
      cue_eligible_label = factor(ifelse(would_be_cued, "Cue-eligible", "Not cue-eligible"),
                                   levels = c("Cue-eligible", "Not cue-eligible")),
      cue_visible_label = factor(ifelse(cued_weekend, "Cue visible", "Cue hidden"),
                                  levels = c("Cue hidden", "Cue visible"))
    )
}

# Dodged-bar layout (x = eligibility, fill = visibility), the layout the
# choice-probability figure used before its non-eligible bars were dropped.
plot_eligibility_bars <- function(fig_df, y, ylab, y_labels = waiver()) {
  ggplot(fig_df, aes(x = cue_eligible_label, y = .data[[y]], fill = cue_visible_label)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.7),
                  width = 0.15, color = "#3a3a3a") +
    facet_wrap(~ city_facet, nrow = 1) +
    scale_fill_manual(values = prob_colors, name = NULL) +
    scale_y_continuous(labels = y_labels, limits = c(0, NA)) +
    labs(x = NULL, y = ylab) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 20, hjust = 1)
    )
}

click_fig_df <- build_eligibility_fig_df(listing_attention_df, compute_click_prob)
time_fig_df  <- build_eligibility_fig_df(listing_attention_df, compute_time_on_page)

p_click_prob <- plot_eligibility_bars(click_fig_df, "prob", "P(at least one detail page clicked)",
                                      y_labels = scales::percent_format(accuracy = 1))
p_time_on_page <- plot_eligibility_bars(time_fig_df, "mean_time", "Mean time on detail pages per set (s)")

print(p_click_prob + labs(title = "Click probability by cue-eligibility and cue visibility",
                          subtitle = "Pooled and by city, with 95% Wilson score CIs"))
print(p_time_on_page + labs(title = "Mean time on detail page by cue-eligibility and cue visibility",
                            subtitle = "Pooled and by city, never-clicked listings = 0 s, mean +/- 1.96 SE"))

for (spec in list(list(p = p_click_prob,   file = "cue_click_probability.png", cmd = "NObsClickProbFig",  d = click_fig_df),
                  list(p = p_time_on_page, file = "cue_time_on_page.png",      cmd = "NObsTimeOnPageFig", d = time_fig_df))) {
  local_path <- path(CHOICE_PROB_FIGURES_DIR, spec$file)
  ggsave(local_path, spec$p, width = 9, height = 3.2, dpi = 300)
  file_copy(local_path, path(overleaf_choice_prob_dir, spec$file), overwrite = TRUE)
  cat("Wrote", spec$file, "to", local_path, "and copied it into the Overleaf project\n")
  # n per facet = number of SETS (each set contributes one row per
  # eligibility group, so sum n over the two cue-visibility cells of one group).
  n_by_facet_spec <- spec$d %>% filter(would_be_cued) %>% group_by(city_facet) %>% summarise(n_total = sum(n), .groups = "drop")
  for (i in seq_len(nrow(n_by_facet_spec))) {
    camel <- facet_camel_lookup[[as.character(n_by_facet_spec$city_facet[i])]]
    write_tex_value(paste0(spec$cmd, camel), n_by_facet_spec$n_total[i], fmt = "%d", file = VALUES_TEX)
  }
}


### Testing H2.1 — Clutter as moderator of the cue effect
# We test H2.1 using the same models described above. For binary outcomes —
# Whether the cue-eligible listing is chosen and Whether the cue-eligible
# listing is clicked on — we use fixed-effects logistic regression. For the
# continuous outcome Time spent on the detail page of the cue-eligible
# listing, we use OLS with fixed effects. To test whether clutter moderates
# the effect of the cue, we augment each model with an interaction term
# between:
# - Treatment 1 (Endorsement Cue Visibility): whether the cue is displayed,
# - Treatment 2 (Visual Clutter): whether the interface is in the Cluttered
#   condition.
# Because the clutter condition is between-subject and constant within
# subject, the subject fixed effect absorbs its main effect; we are therefore
# only able to identify the interaction, not the main effect of clutter. Since
# H2.1 does not specify the direction of the interaction effect, we use a
# two-sided test on the interaction coefficient (H0 : beta12 = 0, H1 : beta12
# != 0), with a Type I error rate of alpha = 0.10 split across both tails.

#### H2.1.1 DV: Whether cued listing is chosen, binary logit with fixed effects and interaction
# Collapse listing-level rows (9 per weekend) to one row per subject x city x weekend, since we're working at the weekend level
h211_df <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(
    # Like before, DV, chosen_cued_listing, is TRUE if the subject chose a would_be_cued listing.
    chose_cued_listing = any(listing_chosen & would_be_cued, na.rm = TRUE),
    # treatment 1: TRUE if the cue graphic was actually shown this weekend (cell_thumb == "P")
    cue_visible = first(cued_weekend),
    # clutter_high is subject-level constant; first() avoids the same row-duplication
    # bug we hit in h22_df/h231_df (bare column references aren't aggregated by summarise()).
    clutter_high = first(clutter_high),
    .groups = "drop"
  ) %>%
  mutate(participant_code = factor(participant_code), city = factor(city))

# Fixed-effects logit: feglm() absorbs participant_code and city fixed effects
# (instead of estimating a dummy per subject/city), so cue_visible is identified
# off within-subject, within-city variation only. This time, interaction between cue_visible and clutter_high
m_h211 <- feglm(
  chose_cued_listing ~ cue_visible * clutter_high | participant_code + city,
  data = h211_df,
  family = "logit"
)

summary(m_h211)

# Adapted from Rmd: the interaction coefficient/SE/p-value (two-sided, per
# H2.1's spec) weren't previously pulled out as named variables — extracted
# here purely so they can be written to values.tex.
coef_h211 <- coef(m_h211)["cue_visibleTRUE:clutter_highTRUE"]
se_h211   <- se(m_h211)["cue_visibleTRUE:clutter_highTRUE"]
z_h211    <- coef_h211 / se_h211
p_twosided_h211 <- 2 * pnorm(abs(z_h211), lower.tail = FALSE)

write_tex_value("RegCoefHTwoOneOne", coef_h211, file = VALUES_TEX)
write_tex_value("RegSEHTwoOneOne", se_h211, file = VALUES_TEX)
write_tex_value("RegZHTwoOneOne", z_h211, file = VALUES_TEX)
write_pvalue_pair("RegPvalHTwoOneOne", p_twosided_h211, file = VALUES_TEX)
write_tex_value("RegStarsHTwoOneOne", stars_from_pvalue(p_twosided_h211), file = VALUES_TEX)
write_tex_value("RegNHTwoOneOne", nobs(m_h211), fmt = "%d", file = VALUES_TEX)

# Adapted from Rmd: the model also includes the cue_visible main effect
# (first-order term) alongside the interaction, per the `cue_visible *
# clutter_high` formula above; extracted here so the table can report it
# explicitly rather than showing the interaction alone.
coef_h211_main <- coef(m_h211)["cue_visibleTRUE"]
se_h211_main   <- se(m_h211)["cue_visibleTRUE"]
z_h211_main    <- coef_h211_main / se_h211_main
p_twosided_h211_main <- 2 * pnorm(abs(z_h211_main), lower.tail = FALSE)

write_tex_value("RegCoefHTwoOneOneMain", coef_h211_main, file = VALUES_TEX)
write_tex_value("RegSEHTwoOneOneMain", se_h211_main, file = VALUES_TEX)
write_tex_value("RegStarsHTwoOneOneMain", stars_from_pvalue(p_twosided_h211_main), file = VALUES_TEX)

#### H2.1.2 DV: Whether cued listing is clicked on, binary logit with fixed effects and interaction
# Choice-set level, same DV construction as H1.2.1: "at least one cue-eligible
# listing clicked" per choice set (city x weekend), with the clutter interaction added.
h212_df <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(
    cue_listing_clicked = any(listing_clicked & would_be_cued, na.rm = TRUE),
    cue_visible = first(cued_weekend),
    clutter_high = first(clutter_high),
    .groups = "drop"
  ) %>%
  mutate(participant_code = factor(participant_code), city = factor(city))

# Fixed-effects logit: feglm() absorbs participant_code and city fixed effects
# (instead of estimating a dummy per subject/city), so cue_visible is identified
# off within-subject, within-city variation only. Interaction added too.
m_h212 <- feglm(
  cue_listing_clicked ~ cue_visible * clutter_high | participant_code + city,
  data = h212_df,
  family = "logit"
)

summary(m_h212)

coef_h212 <- coef(m_h212)["cue_visibleTRUE:clutter_highTRUE"]
se_h212   <- se(m_h212)["cue_visibleTRUE:clutter_highTRUE"]
z_h212    <- coef_h212 / se_h212
p_twosided_h212 <- 2 * pnorm(abs(z_h212), lower.tail = FALSE)

write_tex_value("RegCoefHTwoOneTwo", coef_h212, file = VALUES_TEX)
write_tex_value("RegSEHTwoOneTwo", se_h212, file = VALUES_TEX)
write_tex_value("RegZHTwoOneTwo", z_h212, file = VALUES_TEX)
write_pvalue_pair("RegPvalHTwoOneTwo", p_twosided_h212, file = VALUES_TEX)
write_tex_value("RegStarsHTwoOneTwo", stars_from_pvalue(p_twosided_h212), file = VALUES_TEX)
write_tex_value("RegNHTwoOneTwo", nobs(m_h212), fmt = "%d", file = VALUES_TEX)

coef_h212_main <- coef(m_h212)["cue_visibleTRUE"]
se_h212_main   <- se(m_h212)["cue_visibleTRUE"]
z_h212_main    <- coef_h212_main / se_h212_main
p_twosided_h212_main <- 2 * pnorm(abs(z_h212_main), lower.tail = FALSE)

write_tex_value("RegCoefHTwoOneTwoMain", coef_h212_main, file = VALUES_TEX)
write_tex_value("RegSEHTwoOneTwoMain", se_h212_main, file = VALUES_TEX)
write_tex_value("RegStarsHTwoOneTwoMain", stars_from_pvalue(p_twosided_h212_main), file = VALUES_TEX)

#### H2.1.3 DV: Time spent on detail page, OLS with fixed effects and interaction
# Choice-set level, same DV construction as H1.2.2: log(seconds + 1) of the
# summed cue-eligible listing time within a choice set, with the clutter
# interaction added.
h213_df <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(
    cue_time_seconds = sum(time_on_listing_page_seconds[would_be_cued], na.rm = TRUE),
    cue_visible = first(cued_weekend),
    clutter_high = first(clutter_high),
    .groups = "drop"
  ) %>%
  mutate(
    participant_code = factor(participant_code),
    city = factor(city),
    log_cue_time_seconds = log(cue_time_seconds + 1)
  )

# Fixed-effects OLS: feols() absorbs participant_code and city fixed effects
# (instead of estimating a dummy per subject/city), so cue_visible is identified
# off within-subject, within-city variation only.
m_h213 <- feols(
  log_cue_time_seconds ~ cue_visible * clutter_high | participant_code + city,
  data = h213_df
)

summary(m_h213)

coef_h213 <- coef(m_h213)["cue_visibleTRUE:clutter_highTRUE"]
se_h213   <- se(m_h213)["cue_visibleTRUE:clutter_highTRUE"]
t_h213    <- coef_h213 / se_h213
p_twosided_h213 <- 2 * pnorm(abs(t_h213), lower.tail = FALSE)

write_tex_value("RegCoefHTwoOneThree", coef_h213, file = VALUES_TEX)
write_tex_value("RegSEHTwoOneThree", se_h213, file = VALUES_TEX)
write_tex_value("RegTHTwoOneThree", t_h213, file = VALUES_TEX)
write_pvalue_pair("RegPvalHTwoOneThree", p_twosided_h213, file = VALUES_TEX)
write_tex_value("RegStarsHTwoOneThree", stars_from_pvalue(p_twosided_h213), file = VALUES_TEX)
write_tex_value("RegNHTwoOneThree", nobs(m_h213), fmt = "%d", file = VALUES_TEX)

coef_h213_main <- coef(m_h213)["cue_visibleTRUE"]
se_h213_main   <- se(m_h213)["cue_visibleTRUE"]
t_h213_main    <- coef_h213_main / se_h213_main
p_twosided_h213_main <- 2 * pnorm(abs(t_h213_main), lower.tail = FALSE)

write_tex_value("RegCoefHTwoOneThreeMain", coef_h213_main, file = VALUES_TEX)
write_tex_value("RegSEHTwoOneThreeMain", se_h213_main, file = VALUES_TEX)
write_tex_value("RegStarsHTwoOneThreeMain", stars_from_pvalue(p_twosided_h213_main), file = VALUES_TEX)

### Testing H2.2 — Dilution mechanism
# We test H2.2 using a logistic regression. The dependent variable is the
# binary indicator "Cue recognition". The key predictor is "Visual Clutter"
# (Treatment 2). We have one observation per subject. H2.2 predicts that
# clutter reduces cue recognition, so we use a one-sided test (H0 : beta1 >=0,
# H1 : beta1 <0), with a Type I error rate of alpha = 0.10 on the negative
# tail.

# Collapse listing-level to one row per subject only, since we're working at the subject level.
# cue_recognition and clutter_high are subject-level constants (same value on every one of the
# subject's 108 rows), so first() picks that single value instead of keeping all repeats.
h22_df <- df %>%
  group_by(participant_code) %>%
  summarise(
    cue_recognition = first(cue_recognition),
    clutter_high = first(clutter_high),
    .groups = "drop"
  ) %>%
  mutate(participant_code = factor(participant_code))

# Logit: glm(), no fixed effects (1 observation per subject — no panel structure to absorb).
m_h22 <- glm(
  cue_recognition ~ clutter_high,
  data = h22_df,
  family = binomial(link = "logit")
)

summary(m_h22)

# H2.2 predicts clutter REDUCES cue recognition (H0: beta1 >= 0, H1: beta1 < 0),
# so the one-sided p-value is the area to the LEFT of the z-statistic.
coef_h22 <- coef(m_h22)["clutter_highTRUE"]
se_h22   <- summary(m_h22)$coefficients["clutter_highTRUE", "Std. Error"]
z_h22    <- coef_h22 / se_h22
p_onesided_h22 <- pnorm(z_h22, lower.tail = TRUE)

cat("z =", round(z_h22, 3), " | one-sided p-value (H1: beta < 0) =", round(p_onesided_h22, 4), "\n")

write_tex_value("RegCoefHTwoTwo", coef_h22, file = VALUES_TEX)
write_tex_value("RegSEHTwoTwo", se_h22, file = VALUES_TEX)
write_tex_value("RegZHTwoTwo", z_h22, file = VALUES_TEX)
write_pvalue_pair("RegPvalHTwoTwo", p_onesided_h22, file = VALUES_TEX)
write_tex_value("RegStarsHTwoTwo", stars_from_pvalue(p_onesided_h22), file = VALUES_TEX)
write_tex_value("RegNHTwoTwo", nobs(m_h22), fmt = "%d", file = VALUES_TEX)
write_tex_value("RegInterceptHTwoTwo", coef(m_h22)["(Intercept)"], file = VALUES_TEX)
write_tex_value("RegInterceptSEHTwoTwo", summary(m_h22)$coefficients["(Intercept)", "Std. Error"], file = VALUES_TEX)
# Raw recognition rates by clutter condition, quoted in the H2.2 prose.
write_tex_value("PctCueRecognitionHighClutter", format_pct(mean(h22_df$cue_recognition[h22_df$clutter_high], na.rm = TRUE)), file = VALUES_TEX)
write_tex_value("PctCueRecognitionLowClutter", format_pct(mean(h22_df$cue_recognition[!h22_df$clutter_high], na.rm = TRUE)), file = VALUES_TEX)

### Testing H2.3 — Decision mode mechanism
# We test H2.3 using a set of OLS regressions. The dependent variables are (i)
# decision time and (ii) number of listing page visits. The key predictor is
# "Visual Clutter" (Treatment 2). All specifications include city-level fixed
# effects to control for location-specific factors. We additionally test the
# effect of clutter on the NASA-TLX items on effort, performance, and mental
# demand; since the NASA-TLX is collected once per participant at the end of
# the experiment, this yields one observation per subject, so we estimate
# this model using OLS without fixed effects and without clustering. H2.3
# predicts that clutter induces greater reliance on heuristics. We interpret
# lower decision time and fewer listing page visits as indicative of
# heuristic use. We interpret lower effort with no lower mental demand in the
# Cluttered condition as indicative of heuristic use. We interpret higher
# expected performance for no lower effort in the Cluttered condition as
# rejecting the use of heuristics. Other configurations of the NASA-TLX items
# are interpreted as ambiguous. We use one-sided tests throughout: we expect
# lower decision time and fewer page visits under high clutter (H0 : beta1
# >=0, H1 : beta1 <0), and higher perceived cognitive load (H0 : beta1 <=0, H1
# : beta1 > 0), each with a Type I error rate of alpha = 0.10 on the
# respective tail.

#### H2.3.1: DV: Decision time, IV: Clutter, OLS with fixed effects
# Weekend level: decision_time_seconds and clutter_high are both constant across
# the 9 listing-rows of a weekend, so first() picks that single value per group.
# log(seconds + 1)-transformed per the pre-registration's Transformations section.
h231_df <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(
    decision_time_seconds = first(decision_time_seconds),
    clutter_high = first(clutter_high),
    .groups = "drop"
  ) %>%
  mutate(
    participant_code = factor(participant_code),
    city = factor(city),
    log_decision_time_seconds = log(decision_time_seconds + 1)
  )

# NOTE: clutter_high is a BETWEEN-subject treatment — it never varies across a
# given subject's weekends/cities, so it is perfectly collinear with the
# participant_code fixed effect (each subject dummy already fully encodes their
# own clutter_high). Including participant_code here leaves zero residual
# variation to estimate beta, so we drop it and keep only the city fixed effect
# (this matches the pre-registration, which only specifies city-level fixed
# effects for H2.3).

# City entered as dummies (not absorbed) so the model has an explicit
# intercept -- the low-clutter mean in the reference city (Arcachon, first
# level) -- reported in the paper's table; the clutter coefficient is
# identical either way.
m_h231 <- feols(
  log_decision_time_seconds ~ clutter_high + city,
  data = h231_df
)
write_tex_value("RegInterceptHTwoThreeOne", coef(m_h231)["(Intercept)"], file = VALUES_TEX)
write_tex_value("RegInterceptSEHTwoThreeOne", se(m_h231)["(Intercept)"], file = VALUES_TEX)

summary(m_h231)

# H2.3 predicts LOWER decision time under high clutter (H0: beta1 >= 0, H1: beta1 < 0),
# so the one-sided p-value is the area to the LEFT of the t-statistic.
coef_h231 <- coef(m_h231)["clutter_highTRUE"]
se_h231   <- se(m_h231)["clutter_highTRUE"]
t_h231    <- coef_h231 / se_h231
p_onesided_h231 <- pnorm(t_h231, lower.tail = TRUE)

cat("t =", round(t_h231, 3), " | one-sided p-value (H1: beta < 0) =", round(p_onesided_h231, 4), "\n")

write_tex_value("RegCoefHTwoThreeOne", coef_h231, file = VALUES_TEX)
write_tex_value("RegSEHTwoThreeOne", se_h231, file = VALUES_TEX)
write_tex_value("RegTHTwoThreeOne", t_h231, file = VALUES_TEX)
write_pvalue_pair("RegPvalHTwoThreeOne", p_onesided_h231, file = VALUES_TEX)
# Two-sided p as well: the estimate runs AGAINST the preregistered direction
# (longer decisions under clutter), which a one-sided p near 1 hides.
write_tex_value("RegPvalTwoSidedHTwoThreeOne", format_pvalue(2 * pnorm(-abs(t_h231))), file = VALUES_TEX)
write_tex_value("RegStarsHTwoThreeOne", stars_from_pvalue(p_onesided_h231), file = VALUES_TEX)
write_tex_value("RegNHTwoThreeOne", nobs(m_h231), fmt = "%d", file = VALUES_TEX)

#### H2.3.2: DV: Number of property page visits, IV: Clutter, OLS with fixed effect
# Weekend level: property_page_visits_number and clutter_high are both constant across
# the 9 listing-rows of a weekend, so first() picks that single value per group.
h232_df <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(
    property_page_visits_number = first(property_page_visits_number),
    clutter_high = first(clutter_high),
    .groups = "drop"
  ) %>%
  mutate(participant_code = factor(participant_code), city = factor(city))

# NOTE: clutter_high is a BETWEEN-subject treatment — it never varies across a
# given subject's weekends/cities, so it is perfectly collinear with the
# participant_code fixed effect (each subject dummy already fully encodes their
# own clutter_high). Including participant_code here leaves zero residual
# variation to estimate beta, so we drop it and keep only the city fixed effect
# (this matches the pre-registration, which only specifies city-level fixed
# effects for H2.3).
m_h232 <- feols(
  property_page_visits_number ~ clutter_high + city,  # dummies, see H2.3.1
  data = h232_df
)
write_tex_value("RegInterceptHTwoThreeTwo", coef(m_h232)["(Intercept)"], file = VALUES_TEX)
write_tex_value("RegInterceptSEHTwoThreeTwo", se(m_h232)["(Intercept)"], file = VALUES_TEX)

summary(m_h232)

# H2.3 predicts LOWER decision time under high clutter (H0: beta1 >= 0, H1: beta1 < 0),
# so the one-sided p-value is the area to the LEFT of the t-statistic.
coef_h232 <- coef(m_h232)["clutter_highTRUE"]
se_h232   <- se(m_h232)["clutter_highTRUE"]
t_h232    <- coef_h232 / se_h232
p_onesided_h232 <- pnorm(t_h232, lower.tail = TRUE)

cat("t =", round(t_h232, 3), " | one-sided p-value (H1: beta < 0) =", round(p_onesided_h232, 4), "\n")

write_tex_value("RegCoefHTwoThreeTwo", coef_h232, file = VALUES_TEX)
write_tex_value("RegSEHTwoThreeTwo", se_h232, file = VALUES_TEX)
write_tex_value("RegTHTwoThreeTwo", t_h232, file = VALUES_TEX)
write_pvalue_pair("RegPvalHTwoThreeTwo", p_onesided_h232, file = VALUES_TEX)
write_tex_value("RegPvalTwoSidedHTwoThreeTwo", format_pvalue(2 * pnorm(-abs(t_h232))), file = VALUES_TEX)  # see H2.3.1
write_tex_value("RegStarsHTwoThreeTwo", stars_from_pvalue(p_onesided_h232), file = VALUES_TEX)
write_tex_value("RegNHTwoThreeTwo", nobs(m_h232), fmt = "%d", file = VALUES_TEX)

#### H2.3.3: NASA-TLX dimensions, OLS without fixed effects
# NASA-TLX is collected once per participant -> collapse to 1 row/subject.
nasa_vars <- c("nasa_tlx_mental", "nasa_tlx_physical", "nasa_tlx_temporal",
               "nasa_tlx_performance", "nasa_tlx_effort", "nasa_tlx_frustration")

# Adapted from Rmd: CamelCase tex-safe label for each NASA-TLX dimension,
# reused for every value/command name derived from that dimension below.
nasa_var_labels <- c(
  nasa_tlx_mental = "NasaMental",
  nasa_tlx_physical = "NasaPhysical",
  nasa_tlx_temporal = "NasaTemporal",
  nasa_tlx_performance = "NasaPerformance",
  nasa_tlx_effort = "NasaEffort",
  nasa_tlx_frustration = "NasaFrustration"
)

h233_df <- df %>%
  group_by(participant_code) %>%
  summarise(
    clutter_high = first(clutter_high),
    across(all_of(nasa_vars), first),
    .groups = "drop"
  )

# Loop over each NASA-TLX dimension: OLS, no FE (1 row/subject = no panel
# structure to absorb) and no clustering (same reason — see H2.2).
m_h233_list <- setNames(vector("list", length(nasa_vars)), nasa_vars)
h233_results <- data.frame()

for (v in nasa_vars) {
  fml <- as.formula(paste(v, "~ clutter_high"))
  m <- feols(fml, data = h233_df)
  m_h233_list[[v]] <- m

  # H2.3 predicts HIGHER cognitive load under high clutter (H0: beta1 <= 0,
  # H1: beta1 > 0), so the one-sided p-value is the area to the RIGHT of t.
  b          <- coef(m)["clutter_highTRUE"]
  std_err    <- se(m)["clutter_highTRUE"]
  t_stat     <- b / std_err
  p_onesided <- pnorm(t_stat, lower.tail = FALSE)

  h233_results <- rbind(h233_results, data.frame(
    dv = v, estimate = b, std_error = std_err, t_stat = t_stat, p_onesided = p_onesided
  ))

  label <- nasa_var_labels[[v]]
  # "%.3g" = three SIGNIFICANT digits (4.18, 12.7, -1.58), not three decimals:
  # the subscales are 0-100 scores, so a 3-decimal coefficient (12.706) shows
  # spurious precision next to a 7-point SE.
  write_tex_value(paste0("RegCoefHTwoThreeThree", label), b, fmt = "%.3g", file = VALUES_TEX)
  write_tex_value(paste0("RegSEHTwoThreeThree", label), std_err, fmt = "%.3g", file = VALUES_TEX)
  write_tex_value(paste0("RegTHTwoThreeThree", label), t_stat, fmt = "%.3g", file = VALUES_TEX)
  write_tex_value(paste0("RegInterceptHTwoThreeThree", label), coef(m)["(Intercept)"], fmt = "%.3g", file = VALUES_TEX)
  write_tex_value(paste0("RegInterceptSEHTwoThreeThree", label), se(m)["(Intercept)"], fmt = "%.3g", file = VALUES_TEX)
  write_tex_value(paste0("RegPvalHTwoThreeThree", label), format_pvalue(p_onesided), file = VALUES_TEX)
  write_tex_value(paste0("RegStarsHTwoThreeThree", label), stars_from_pvalue(p_onesided), file = VALUES_TEX)
  write_tex_value(paste0("RegNHTwoThreeThree", label), nobs(m), fmt = "%d", file = VALUES_TEX)
}

# esttab()-style side-by-side regression table — fixest's equivalent.
etable(m_h233_list)

# Compact one-sided-test summary across all 6 dimensions.
print(h233_results, row.names = FALSE)

#### Audit: decision_time_seconds (sum) vs NASA-TLX dimensions
# Exploratory/descriptive only — not a pre-registered analysis. Subject-level
# total decision time: raw (unlogged) decision_time_seconds summed across all
# 12 weekends per subject (weekends with no tracked preload event, and thus no
# decision_time_seconds, contribute 0 via na.rm = TRUE rather than dropping the
# subject entirely).
decision_time_by_subject <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(decision_time_seconds = first(decision_time_seconds), .groups = "drop") %>%
  group_by(participant_code) %>%
  summarise(total_decision_time_seconds = sum(decision_time_seconds, na.rm = TRUE), .groups = "drop")

audit_df <- h233_df %>%
  select(participant_code, all_of(nasa_vars)) %>%
  left_join(decision_time_by_subject, by = "participant_code")

for (v in nasa_vars) {
  p <- ggplot(audit_df, aes(x = total_decision_time_seconds, y = .data[[v]])) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
    labs(
      title = paste("Total decision time vs", v),
      x = "Total decision time across all weekends (seconds)",
      y = v
    ) +
    theme_minimal()
  print(p)
}


### Testing H3.1 — Reduced-form analysis of the welfare effect of visual clutter in absence of cue
# We test H3.1 using a fixed-effects logistic regression. The dependent
# variable is the binary indicator "Preference consistency". The key
# regressor is "Visual Clutter" (Treatment 2). The unit of observation is
# subject x city, so each subject is observed 3 times. The model includes
# city-level fixed effects, which control for stable differences across
# cities that are unrelated to the treatment itself. We test whether
# preference consistency differs between clutter conditions using a
# two-sided test (H0 : beta1 = 0, H1 : beta1 != 0), with a Type I error rate
# of alpha = 0.10 split across both tails.

# Subject x city level: preference consistency is calculated at that level. Clutter condition is assigned on a per-subject level, so we can just take the first value for each observation.
h31_df <- df %>%
  group_by(participant_code, city) %>%
  summarise(
    preference_consistency = first(preference_consistency),
    clutter_high = first(clutter_high),
    .groups = "drop"
  ) %>%
  mutate(participant_code = factor(participant_code), city = factor(city))

# NOTE: clutter_high is a BETWEEN-subject treatment — it never varies across a
# given subject's weekends/cities, so it is perfectly collinear with the
# participant_code fixed effect (each subject dummy already fully encodes their
# own clutter_high). Including participant_code here leaves zero residual
# variation to estimate beta, so we drop it and keep only the city fixed effect.

# NOTE: feglm(family = "logit") to match the spec ("fixed-effects logistic
# regression") — preference_consistency is binary, so OLS (feols) would be the
# wrong estimator here.
m_h31 <- feglm(
  preference_consistency ~ clutter_high | city,
  data = h31_df,
  family = "logit"
)

summary(m_h31)

# H3.1 is a two-sided test (H0: beta1 = 0, H1: beta1 != 0).
coef_h31 <- coef(m_h31)["clutter_highTRUE"]
se_h31   <- se(m_h31)["clutter_highTRUE"]
z_h31    <- coef_h31 / se_h31
p_twosided_h31 <- 2 * pnorm(abs(z_h31), lower.tail = FALSE)

cat("z =", round(z_h31, 3), " | two-sided p-value (H1: beta != 0) =", round(p_twosided_h31, 4), "\n")

write_tex_value("RegCoefHThreeOne", coef_h31, file = VALUES_TEX)
write_tex_value("RegSEHThreeOne", se_h31, file = VALUES_TEX)
write_tex_value("RegZHThreeOne", z_h31, file = VALUES_TEX)
write_tex_value("RegPvalHThreeOne", format_pvalue(p_twosided_h31), file = VALUES_TEX)
write_tex_value("RegStarsHThreeOne", stars_from_pvalue(p_twosided_h31), file = VALUES_TEX)
write_tex_value("RegNHThreeOne", nobs(m_h31), fmt = "%d", file = VALUES_TEX)

# Print mean preference consistency to audit
mean(h31_df$preference_consistency)


### Testing H3.2 — Reduced-form analysis of the welfare effect of cue visibility
# We will test H3.2 using two one-sample binomial tests. We will test whether
# the frequency at which the binary indicator "Cued choice-preferences
# consistency" equals 1 to two fixed values. This frequency will be computed
# on the subject x city pair for with the "Preference consistency" indicator
# equals 1, no matter the Clutter condition. The fixed values are
# theoretically motivated. A value above the square root of the frequency of
# "Preference consistency" across all subject x city pair is interpreted as
# indicative of a positive welfare effect. A value below the square root of
# the frequency of "Preference consistency" divided by the square root of 3
# is interpreted as indicative of a negative welfare effect. Intermediary
# values are interpreted as ambiguous.

# preference_consistency and cued_choice_preference_consistency are both
# constant within a (participant_code, city) pair, so collapse to 1 row per pair.
h32_df <- df %>%
  group_by(participant_code, city) %>%
  summarise(
    preference_consistency = first(preference_consistency),
    cued_choice_preference_consistency = first(cued_choice_preference_consistency),
    .groups = "drop"
  )

# Reference frequency: share of ALL subject x city pairs with a consistent
# revealed preference across the two no-cue weekends.
p_consistency <- mean(h32_df$preference_consistency, na.rm = TRUE)

# Restrict to pairs where preference_consistency == TRUE — cued_choice_preference_consistency
# is only defined (non-NA) there, since it's only meaningful when the subject has a
# well-defined preferred cluster to compare the cued choices against.
h32_eligible <- h32_df %>% filter(preference_consistency == TRUE)

n_eligible        <- nrow(h32_eligible)
n_cued_consistent <- sum(h32_eligible$cued_choice_preference_consistency, na.rm = TRUE)
freq_cued         <- n_cued_consistent / n_eligible

# Theoretically motivated thresholds from the pre-registration.
upper_threshold <- sqrt(p_consistency)
lower_threshold <- sqrt(p_consistency) / sqrt(3)

cat("p_consistency (all pairs):", round(p_consistency, 4), "\n")
cat("Observed frequency (cued, among consistent pairs):", round(freq_cued, 4),
    "out of", n_eligible, "pairs\n")
cat("Upper threshold (positive welfare):", round(upper_threshold, 4), "\n")
cat("Lower threshold (negative welfare):", round(lower_threshold, 4), "\n\n")

# Two one-sample binomial tests: is the observed frequency significantly
# different from each fixed, theoretically motivated value?
test_upper <- binom.test(n_cued_consistent, n_eligible, p = upper_threshold)
test_lower <- binom.test(n_cued_consistent, n_eligible, p = lower_threshold)

print(test_upper)
print(test_lower)

# Interpretation per the pre-registration rule.
welfare_conclusion <- dplyr::case_when(
  freq_cued > upper_threshold ~ "Positive welfare effect",
  freq_cued < lower_threshold ~ "Negative welfare effect",
  TRUE ~ "Ambiguous"
)

cat("\nConclusion:", welfare_conclusion, "\n")

write_tex_value("PropConsistency", format_pct(p_consistency), file = VALUES_TEX)
write_tex_value("PropCuedConsistent", format_pct(freq_cued), file = VALUES_TEX)
write_tex_value("NEligibleHThreeTwo", n_eligible, fmt = "%d", file = VALUES_TEX)
write_tex_value("ThresholdUpperHThreeTwo", upper_threshold, file = VALUES_TEX)
write_tex_value("ThresholdLowerHThreeTwo", lower_threshold, file = VALUES_TEX)
write_tex_value("PvalBinomUpperHThreeTwo", format_pvalue(test_upper$p.value), file = VALUES_TEX)
write_tex_value("PvalBinomLowerHThreeTwo", format_pvalue(test_lower$p.value), file = VALUES_TEX)
write_tex_value("ConclusionHThreeTwo", welfare_conclusion, file = VALUES_TEX)


### Figure — H3.1 cluster x cluster co-occurrence matrix of no-cue choices, by city
# Visualizes revealed-preference consistency directly (companion to the H3.1
# regression above): for each subject x city with two no-cue weekends, map
# both chosen listings to their cluster and tally how often each unordered
# cluster pair occurs (the diagonal is "both choices in the same cluster" --
# i.e. a consistent pair). Cell values are the SHARE of that city's subject
# pairs landing on each cluster combo, not raw counts, since the four cities
# have different numbers of eligible subject pairs and only shares are
# comparable across city panels. One matrix per city; darker = higher share.
no_cue_choices <- df %>%
  filter(!cued_weekend, listing_chosen) %>%
  distinct(participant_code, city, weekend_number_global, cluster)

all_clusters <- sort(unique(df$cluster))

# Keep only subject x city pairs with exactly two no-cue choices (one per
# no-cue weekend) -- pairs with 0/1 don't have a "two choices" comparison.
cluster_pairs_by_city <- no_cue_choices %>%
  group_by(participant_code, city) %>%
  filter(n() == 2) %>%
  summarise(
    cluster_lo = min(cluster),
    cluster_hi = max(cluster),
    .groups = "drop"
  )

FIGURES_DIR <- path(LOCAL_OUTPUT_DIR, "figures")
dir_create(FIGURES_DIR)

for (ct in sort(unique(cluster_pairs_by_city$city))) {
  city_pairs <- cluster_pairs_by_city %>% filter(city == ct)
  n_pairs_city <- nrow(city_pairs)

  counts <- city_pairs %>% count(cluster_lo, cluster_hi, name = "n")

  # Full symmetric grid over all_clusters x all_clusters: an unordered pair
  # (lo, hi) with lo != hi is split evenly across its two mirrored cells
  # (lo, hi) and (hi, lo), each getting half its share, so the matrix reads
  # as symmetric AND the cells sum to 100% (a proper contingency table).
  # Diagonal cells (lo == hi) keep their full share, since they have no
  # mirror to split with.
  grid <- expand.grid(cluster_x = all_clusters, cluster_y = all_clusters) %>%
    mutate(
      cluster_lo = pmin(cluster_x, cluster_y),
      cluster_hi = pmax(cluster_x, cluster_y)
    ) %>%
    left_join(counts, by = c("cluster_lo", "cluster_hi")) %>%
    mutate(
      n = ifelse(is.na(n), 0, n),
      share = ifelse(cluster_lo == cluster_hi, n, n / 2) / n_pairs_city
    )

  # NOTE: no ggtitle()/subtitle() baked into the plot itself -- this PNG is
  # meant to be dropped straight into the LaTeX paper as one panel of a grid
  # figure, where the city label, N, and any explanation belong in the
  # surrounding \caption{}/Notes block (see the latex-figures skill), not
  # burned into the image. The audit-PDF copy below gets a title added
  # on top (p_audit) purely for flipping through PLOTS_PDF by eye -- the
  # saved/paper-facing PNG (p) stays title-free.
  p <- ggplot(grid, aes(x = factor(cluster_x), y = factor(cluster_y), fill = share)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.0f%%", 100 * share)), size = 3) +
    scale_fill_gradient(low = "white", high = "#08306b", limits = c(0, NA),
                         name = "Share of\nsubject pairs") +
    coord_fixed() +
    labs(x = "Cluster (choice 1 or 2)", y = "Cluster (choice 1 or 2)") +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank())

  p_audit <- p + labs(
    title = paste0("No-cue choice cluster co-occurrence — ", city_labels[[ct]]),
    subtitle = paste0("N = ", n_pairs_city, " subject x city pairs")
  )
  print(p_audit) # page in the audit PDF (PLOTS_PDF)

  fig_path <- path(FIGURES_DIR, paste0("h3_cluster_matrix_", city_labels[[ct]], ".png"))
  ggsave(fig_path, p, width = 5, height = 5, dpi = 300)
  cat("Wrote cluster co-occurrence matrix for", ct, "to", fig_path, "\n")

  # Phi coefficient (the binary-variable analogue of a Pearson correlation):
  # treat "one of the pair's two choices is cluster i" and "...is cluster j"
  # as 0/1 indicators X_i, Y_j with means marginal_i, marginal_j (the row/col
  # marginals of the symmetric proportion table above). The lift ratio only
  # divided by the marginals' product; a real correlation coefficient also
  # CENTERS the joint probability on its independence baseline before
  # normalizing:
  #   phi_ij = Cov(X_i, Y_j) / sqrt(Var(X_i) * Var(Y_j))
  #          = (share_ij - marginal_i * marginal_j) /
  #            sqrt(marginal_i*(1-marginal_i) * marginal_j*(1-marginal_j))
  # phi = 0 means no association (independence); phi > 0 means that pair
  # co-occurs more than chance, phi < 0 means less. Bounded in [-1, 1] like
  # any Pearson correlation -- the diagonal (self-association, i.e.
  # "consistent" pairs) is expected to skew positive given H3's finding that
  # preference consistency is well above the 1/3 chance rate.
  marginal <- grid %>%
    group_by(cluster_x) %>%
    summarise(marginal = sum(share), .groups = "drop") %>%
    rename(cluster = cluster_x)

  grid_corr <- grid %>%
    left_join(marginal, by = c("cluster_x" = "cluster")) %>%
    rename(marginal_x = marginal) %>%
    left_join(marginal, by = c("cluster_y" = "cluster")) %>%
    rename(marginal_y = marginal) %>%
    mutate(
      cov_xy = share - marginal_x * marginal_y,
      sd_x = sqrt(marginal_x * (1 - marginal_x)),
      sd_y = sqrt(marginal_y * (1 - marginal_y)),
      phi = cov_xy / (sd_x * sd_y)
    )

  # Same rule as p above: no title/subtitle on the paper-facing plot -- the
  # city label and N belong in the LaTeX \caption{}/Notes block around the
  # 2x2 grid, not burned into the PNG (see the latex-figures skill).
  p_corr <- ggplot(grid_corr, aes(x = factor(cluster_x), y = factor(cluster_y), fill = phi)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", phi)), size = 3) +
    scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#08306b", midpoint = 0,
                          limits = c(-1, 1), name = "Correlation\n(phi)") +
    coord_fixed() +
    labs(x = "Cluster (choice 1 or 2)", y = "Cluster (choice 1 or 2)") +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank())

  p_corr_audit <- p_corr + labs(
    title = paste0("No-cue choice cluster correlation — ", city_labels[[ct]]),
    subtitle = paste0("N = ", n_pairs_city, " subject x city pairs; phi coefficient vs. independence")
  )
  print(p_corr_audit) # page in the audit PDF (PLOTS_PDF)

  # N per city, written to values.tex so the LaTeX Notes block can cite it
  # by macro instead of a hardcoded number (same "values, never hardcoded
  # numbers" rule the latex-tables/r-tex-values skills apply to tables).
  write_tex_value(paste0("NClusterCorr", city_labels[[ct]]), n_pairs_city, fmt = "%d", file = VALUES_TEX)

  # Filename matches exactly what the CHI paper's \includegraphics calls
  # reference (see the fig:h3clustercorr grid in
  # pilot_experiment_chi2027_article.tex), so writing straight to
  # OVERLEAF_FIGURES_DIR needs no rename step -- it just overwrites the
  # placeholder/previous run's PNG in place.
  fig_corr_filename <- paste0("h3_cluster_corr_", city_labels[[ct]], ".png")
  fig_corr_path <- path(FIGURES_DIR, fig_corr_filename)
  ggsave(fig_corr_path, p_corr, width = 5, height = 5, dpi = 300)
  cat("Wrote cluster correlation (phi) matrix for", ct, "to", fig_corr_path, "\n")

  overleaf_fig_corr_path <- path(OVERLEAF_FIGURES_DIR, fig_corr_filename)
  file_copy(fig_corr_path, overleaf_fig_corr_path, overwrite = TRUE)
  cat("Copied it into the Overleaf project at", overleaf_fig_corr_path, "\n")
}


### Figure — Triangular version of the phi correlation matrix, plus a
# cued-choice-match column, by city (the version actually used in the paper)
# Two changes from the 2x2-subfigure phi matrix built just above: (1) the phi
# matrix is symmetric by construction (cell (i,j) == cell (j,i)), so only the
# upper triangle including the diagonal is drawn -- the mirrored lower half
# is redundant; (2) a fourth column reports, for the subject x city pairs
# with a consistent (same-cluster) revealed preference across the two no-cue
# weekends, the share of their CUE-VISIBLE choices that matched that same
# preferred cluster -- H3.2's headline number (\PropCuedConsistent), but
# broken out by which cluster the preference actually was, rather than
# pooled across clusters. All four cities in one combined facet PNG rather
# than four separate subfigure files.
cued_choices <- df %>%
  filter(cued_weekend, listing_chosen) %>%
  distinct(participant_code, city, weekend_number_global, cluster)

# A pair's "preferred cluster" is only defined when its two no-cue choices
# agree (cluster_lo == cluster_hi in cluster_pairs_by_city, built above).
preferred_cluster_by_pair <- cluster_pairs_by_city %>%
  filter(cluster_lo == cluster_hi) %>%
  transmute(participant_code, city, preferred_cluster = cluster_lo)

# One row per (city, preferred cluster): p_match is the share of that group's
# CUE-VISIBLE choices (up to 2 per subject) landing back in the same cluster;
# n is the number of such cue-visible choices (the denominator), not the
# number of subjects, since a subject can contribute up to 2 observations.
cued_match_by_cluster <- cued_choices %>%
  inner_join(preferred_cluster_by_pair, by = c("participant_code", "city")) %>%
  mutate(matches = cluster == preferred_cluster) %>%
  group_by(city, preferred_cluster) %>%
  summarise(p_match = mean(matches), n = n(), .groups = "drop")

triangular_fig_rows <- list()
for (ct in sort(unique(cluster_pairs_by_city$city))) {
  city_pairs <- cluster_pairs_by_city %>% filter(city == ct)
  n_pairs_city <- nrow(city_pairs)
  counts <- city_pairs %>% count(cluster_lo, cluster_hi, name = "n")
  grid <- expand.grid(cluster_x = all_clusters, cluster_y = all_clusters) %>%
    mutate(cluster_lo = pmin(cluster_x, cluster_y), cluster_hi = pmax(cluster_x, cluster_y)) %>%
    left_join(counts, by = c("cluster_lo", "cluster_hi")) %>%
    mutate(n = ifelse(is.na(n), 0, n), share = ifelse(cluster_lo == cluster_hi, n, n / 2) / n_pairs_city)
  marginal <- grid %>% group_by(cluster_x) %>% summarise(marginal = sum(share), .groups = "drop") %>%
    rename(cluster = cluster_x)
  grid_corr <- grid %>%
    left_join(marginal, by = c("cluster_x" = "cluster")) %>% rename(marginal_x = marginal) %>%
    left_join(marginal, by = c("cluster_y" = "cluster")) %>% rename(marginal_y = marginal) %>%
    mutate(
      cov_xy = share - marginal_x * marginal_y,
      sd_x = sqrt(marginal_x * (1 - marginal_x)),
      sd_y = sqrt(marginal_y * (1 - marginal_y)),
      phi = cov_xy / (sd_x * sd_y)
    ) %>%
    # Keep the upper triangle (row index <= column index) including the
    # diagonal; drop the mirrored lower half.
    filter(match(cluster_y, all_clusters) <= match(cluster_x, all_clusters)) %>%
    transmute(city = ct, row = cluster_y, col = as.character(cluster_x), value = phi,
              label = sprintf("%.2f", phi))

  match_col <- tibble(cluster = all_clusters) %>%
    left_join(cued_match_by_cluster %>% filter(city == ct), by = c("cluster" = "preferred_cluster")) %>%
    transmute(
      city = ct, row = cluster, col = "match", value = p_match,
      label = ifelse(is.na(p_match), "n/a", sprintf("%.2f\n(N=%d)", p_match, n))
    )

  triangular_fig_rows[[ct]] <- bind_rows(grid_corr, match_col)
}

triangular_fig_df <- bind_rows(triangular_fig_rows) %>%
  mutate(
    city_facet = factor(unname(city_display_labels[city]), levels = unname(city_display_labels)),
    col = factor(col, levels = c(as.character(all_clusters), "match"),
                 labels = c(as.character(all_clusters), "P(cued\nmatch)")),
    row = factor(row, levels = rev(all_clusters))
  )

# Same diverging red-white-blue scale as the phi matrix above, shared across
# both quantities: a match probability is already bounded in [0, 1], a subset
# of phi's [-1, 1] range, so 0 reads as "no better than chance"/"no
# association" on both, and no second fill scale (e.g. ggnewscale) is needed.
p_triangular <- ggplot(triangular_fig_df, aes(x = col, y = row, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), size = 2.6, lineheight = 0.85) +
  geom_vline(xintercept = length(all_clusters) + 0.5, color = "grey40", linewidth = 0.4) +
  facet_wrap(~ city_facet, ncol = 2) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#08306b", midpoint = 0,
                        limits = c(-1, 1), na.value = "grey90", name = NULL) +
  coord_fixed() +
  labs(x = NULL, y = "Preferred cluster (no-cue choices)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

p_triangular_audit <- p_triangular + labs(
  title = "Triangular cluster correlation + cued-choice match probability",
  subtitle = "Upper triangle of the phi matrix (Fig. h3clustercorr), plus P(cued choice matches | consistent)"
)
print(p_triangular_audit) # page in the audit PDF (PLOTS_PDF)

TRIANGULAR_FIGURES_DIR <- path(LOCAL_OUTPUT_DIR, "figures")
triangular_fig_path <- path(TRIANGULAR_FIGURES_DIR, "h3_cluster_corr_triangular.png")
ggsave(triangular_fig_path, p_triangular, width = 7, height = 6.3, dpi = 300)
cat("Wrote triangular cluster-correlation figure to", triangular_fig_path, "\n")

overleaf_triangular_dir <- path(OVERLEAF_DIR, "illustrations", "h3_cluster_corr_triangular")
dir_create(overleaf_triangular_dir)
overleaf_triangular_path <- path(overleaf_triangular_dir, "h3_cluster_corr_triangular.png")
file_copy(triangular_fig_path, overleaf_triangular_path, overwrite = TRUE)
cat("Copied it into the Overleaf project at", overleaf_triangular_path, "\n")


### Manipulation/ Internal validity checks
# Clutter perception: To verify that Treatment 2 successfully manipulates
# perceived visual complexity, we compare subjects in the Cluttered and Low
# Clutter groups on their answers to the post-experiment questionnaire item
# "How visually complex did you find the website interface?" (7-point Likert
# scale). We test whether the mean difference between groups is non-zero
# using a two-sided two-sample t-test, with a Type I error rate of alpha =
# 0.10.

# visual_complexity is collected once per participant (post-experiment
# questionnaire) -> collapse to 1 row per subject.
h_manip_df <- df %>%
  group_by(participant_code) %>%
  summarise(
    visual_complexity = first(visual_complexity),
    clutter_high = first(clutter_high),
    .groups = "drop"
  )

# Two-sided, two-sample t-test (Welch by default) comparing perceived visual
# complexity between High Clutter and Low Clutter subjects.
t_manip <- t.test(visual_complexity ~ clutter_high, data = h_manip_df)
print(t_manip)

cat("Mean visual complexity — High Clutter:",
    round(mean(h_manip_df$visual_complexity[h_manip_df$clutter_high], na.rm = TRUE), 2),
    "| Low Clutter:",
    round(mean(h_manip_df$visual_complexity[!h_manip_df$clutter_high], na.rm = TRUE), 2), "\n")

write_tex_value("MeanVisualComplexityHighClutter", mean(h_manip_df$visual_complexity[h_manip_df$clutter_high], na.rm = TRUE), fmt = "%.2f", file = VALUES_TEX)
write_tex_value("MeanVisualComplexityLowClutter", mean(h_manip_df$visual_complexity[!h_manip_df$clutter_high], na.rm = TRUE), fmt = "%.2f", file = VALUES_TEX)
write_tex_value("TestStatManip", unname(t_manip$statistic), file = VALUES_TEX)
write_tex_value("PvalManip", format_pvalue(t_manip$p.value), file = VALUES_TEX)
write_tex_value("StarsManip", stars_from_pvalue(t_manip$p.value), file = VALUES_TEX)

# --- Not preregistered: false recognition. The recognition item shows the
# badge next to two decoy icons that never appeared on the site; counting
# subjects who "noticed" a decoy qualifies the cue_recognition measure.
# Columns arrive from 00c's oTree mapping; n/a placeholders if absent. ---
decoy_cols <- c("noticed_decoy_checkmark", "noticed_decoy_badge")
if (all(c("cue_recognition", decoy_cols) %in% names(df))) {
  recog_df <- df %>%
    distinct(participant_code, cue_recognition, noticed_decoy_checkmark, noticed_decoy_badge) %>%
    mutate(across(-participant_code, ~ as.logical(.x)))
  write_tex_value("NSubjectsRecognition", sum(!is.na(recog_df$cue_recognition)), fmt = "%d", file = VALUES_TEX)
  # False recognition (any decoy) by clutter condition, Fisher exact test.
  decoy_clutter <- recog_df %>%
    mutate(any_decoy = noticed_decoy_checkmark | noticed_decoy_badge) %>%
    left_join(df %>% distinct(participant_code, clutter_high), by = "participant_code") %>%
    filter(!is.na(any_decoy), !is.na(clutter_high))
  write_tex_value("PctDecoyAnyHighClutter", format_pct(mean(decoy_clutter$any_decoy[decoy_clutter$clutter_high])), file = VALUES_TEX)
  write_tex_value("PctDecoyAnyLowClutter", format_pct(mean(decoy_clutter$any_decoy[!decoy_clutter$clutter_high])), file = VALUES_TEX)
  write_pvalue_pair("PvalDecoyAnyClutter", fisher.test(table(decoy_clutter$clutter_high, decoy_clutter$any_decoy))$p.value, file = VALUES_TEX)
  write_tex_value("NSubjectsNoticedThumb", sum(recog_df$cue_recognition, na.rm = TRUE), fmt = "%d", file = VALUES_TEX)
  write_tex_value("NSubjectsNoticedAnyDecoy",
                  sum(recog_df$noticed_decoy_checkmark | recog_df$noticed_decoy_badge, na.rm = TRUE), fmt = "%d", file = VALUES_TEX)
  write_tex_value("NSubjectsNoticedBothDecoys",
                  sum(recog_df$noticed_decoy_checkmark & recog_df$noticed_decoy_badge, na.rm = TRUE), fmt = "%d", file = VALUES_TEX)
  write_tex_value("PctRecognitionCorrect",
                  format_pct(mean(recog_df$cue_recognition & !(recog_df$noticed_decoy_checkmark | recog_df$noticed_decoy_badge), na.rm = TRUE)),
                  file = VALUES_TEX)
  write_tex_value("NSubjectsNoticedThumbNoDecoy",
                  sum(recog_df$cue_recognition & !(recog_df$noticed_decoy_checkmark | recog_df$noticed_decoy_badge), na.rm = TRUE),
                  fmt = "%d", file = VALUES_TEX)
} else {
  for (nm in c("NSubjectsRecognition", "NSubjectsNoticedThumb", "NSubjectsNoticedAnyDecoy",
               "NSubjectsNoticedBothDecoys", "NSubjectsNoticedThumbNoDecoy")) {
    write_tex_value(nm, "n/a", file = VALUES_TEX)
  }
}

# --- Not preregistered: heterogeneity of the clutter manipulation check, by
# age (median split) and by quartile of the subject's median decision time.
# One row per subgroup: N, mean perceived complexity under High vs Low
# clutter, the difference and its Welch two-sided p-value. Written as a
# house-style LaTeX table straight into the Overleaf tables/ folder. ---
subject_dtime <- df %>%
  filter(!is.na(decision_time_seconds), decision_time_seconds > 0) %>%
  distinct(participant_code, weekend_number_global, decision_time_seconds) %>%
  group_by(participant_code) %>%
  summarise(median_dtime = median(decision_time_seconds), .groups = "drop")
manip_het_df <- h_manip_df %>%
  left_join(df %>% distinct(participant_code, age), by = "participant_code") %>%
  left_join(subject_dtime, by = "participant_code")
age_cut <- median(manip_het_df$age, na.rm = TRUE)
manip_het_groups <- bind_rows(
  manip_het_df %>% filter(!is.na(age)) %>%
    mutate(group = ifelse(age <= age_cut, sprintf("Age $\\le$ %d", age_cut), sprintf("Age $>$ %d", age_cut)),
           block = "Age"),
  manip_het_df %>% filter(!is.na(median_dtime)) %>%
    mutate(q = dplyr::ntile(median_dtime, 4),
           group = paste0("Decision time Q", q, c(" (fastest)", "", "", " (slowest)")[q]),
           block = "Decision time")
)
manip_het_rows <- manip_het_groups %>%
  group_by(block, group) %>%
  summarise(
    n = n(),
    mean_high = mean(visual_complexity[clutter_high], na.rm = TRUE),
    mean_low = mean(visual_complexity[!clutter_high], na.rm = TRUE),
    p = tryCatch(t.test(visual_complexity ~ clutter_high)$p.value, error = function(e) NA_real_),
    .groups = "drop"
  ) %>%
  mutate(diff = mean_high - mean_low) %>%
  # Age block first, "<=" row before ">" row; decision-time quartiles Q1..Q4.
  arrange(block, grepl("^Age \\$>", group), group)
# Same numbers as \newcommand values, so the paper can quote them in the text
# instead of the table: \ManipHet<Group><High|Low|N>, groups AgeYoung / AgeOld
# (median split, cut point in \ManipHetAgeCut) and DtimeQOne..DtimeQFour.
for (i in seq_len(nrow(manip_het_rows))) {
  g <- manip_het_rows$group[i]
  key <- if (grepl("^Age \\$\\\\le", g)) "AgeYoung" else if (grepl("^Age \\$>", g)) "AgeOld" else
    paste0("DtimeQ", c("One", "Two", "Three", "Four")[as.integer(sub("^Decision time Q(\\d).*$", "\\1", g))])
  write_tex_value(paste0("ManipHet", key, "High"), manip_het_rows$mean_high[i], fmt = "%.2f", file = VALUES_TEX)
  write_tex_value(paste0("ManipHet", key, "Low"),  manip_het_rows$mean_low[i],  fmt = "%.2f", file = VALUES_TEX)
  write_tex_value(paste0("ManipHet", key, "N"),    manip_het_rows$n[i],         fmt = "%d",   file = VALUES_TEX)
  write_pvalue_pair(paste0("ManipHet", key, "Pval"), manip_het_rows$p[i], file = VALUES_TEX)
}
write_tex_value("ManipHetAgeCut", age_cut, fmt = "%d", file = VALUES_TEX)

manip_het_lines <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule\\toprule",
  "Subgroup & $N$ & Cluttered & Low clutter & Difference \\\\[-1.8ex]",
  "\\midrule"
)
for (b in unique(manip_het_rows$block)) {
  rows_b <- manip_het_rows %>% filter(block == b)
  for (i in seq_len(nrow(rows_b))) {
    manip_het_lines <- c(manip_het_lines, sprintf(
      "%s & %d & %.2f & %.2f & %.2f%s \\\\", rows_b$group[i], rows_b$n[i], rows_b$mean_high[i],
      rows_b$mean_low[i], rows_b$diff[i], stars_from_pvalue(rows_b$p[i])))
  }
  if (b != tail(unique(manip_het_rows$block), 1)) manip_het_lines <- c(manip_het_lines, "\\midrule")
}
manip_het_lines <- c(manip_het_lines, "\\bottomrule\\bottomrule", "\\end{tabular}")
manip_het_path <- path(OVERLEAF_DIR, "tables", "table_manip_heterogeneity.tex")
dir_create(path_dir(manip_het_path))
writeLines(manip_het_lines, file(manip_het_path, open = "wb")); # binary: LF on Windows too

# Preference consistency: As validation that the clusters capture meaningful
# and stable consumer preferences, we report the average preference
# consistency rate across all subject x city pairs — that is, the share of
# subject x city pairs for which the two choices made in no-cue weekends
# belong to the same cluster. A high consistency rate provides evidence that
# clusters reflect genuine preference types and that subjects make coherent
# choices across weekends in the absence of the cue. We test that preference
# consistency is higher than chance (corresponding to a random allocation of
# listings to clusters) using a one-sample, one-sided t-test comparing the
# mean preference consistency with 1/3, with a Type I error rate of alpha =
# 0.10.

# preference_consistency is constant within a (participant_code, city) pair,
# so collapse to 1 row per pair before averaging (otherwise the 108 listing-rows
# per pair would just repeat the same value and not bias the mean, but it's
# cleaner/cheaper to collapse first).
h_consistency_df <- df %>%
  group_by(participant_code, city) %>%
  summarise(preference_consistency = first(preference_consistency), .groups = "drop")

consistency_rate <- mean(h_consistency_df$preference_consistency, na.rm = TRUE)
n_pairs <- sum(!is.na(h_consistency_df$preference_consistency))

cat("Preference consistency rate:", round(consistency_rate * 100, 1), "%",
    "(", n_pairs, "subject x city pairs )\n")

# One-sample, one-sided t-test: is preference consistency higher than chance
# (1/3, the rate expected under random allocation of listings to clusters)?
t_consistency <- t.test(
  h_consistency_df$preference_consistency,
  mu = 1/3,
  alternative = "greater"
)
print(t_consistency)

write_tex_value("ConsistencyRate", format_pct(consistency_rate), file = VALUES_TEX)
write_tex_value("NPairsConsistency", n_pairs, fmt = "%d", file = VALUES_TEX)

# --- Not preregistered: an empirical benchmark for preference consistency
# in place of the theoretical 1/3. One third is the consistency rate of a
# subject who picks each cluster with probability 1/3; but consistency is
# the squared norm of the subject's cluster-choice probabilities, whose
# expectation exceeds 1/3 as soon as the clusters are not equally attractive
# -- and even randomly assembled clusters would not be, since a few listings
# draw most choices. So we re-label the city's property pool into clusters
# at random (preserving cluster sizes, fixed across a subject's weekends),
# recompute the consistency rate over the same no-cue choices, and repeat:
# the mean of that distribution is the consistency rate that "clusters
# carrying no information" would already produce, and the share of draws at
# or above the observed rate is the empirical one-sided p-value. Ported from
# 01c_cluster_randomization.R (same procedure) so the values reach the paper.
set.seed(20260905)
N_CLUSTER_RANDOMIZATIONS <- 2000
cluster_pool <- df %>% distinct(city, property_slug, cluster) %>% filter(!is.na(cluster))
nocue_choices <- df %>%
  filter(listing_chosen, !cued_weekend) %>%
  distinct(participant_code, city, weekend_number_global, property_slug)
consistency_rate_from_pool <- function(pool) {
  nocue_choices %>%
    inner_join(pool, by = c("city", "property_slug")) %>%
    group_by(participant_code, city) %>%
    summarise(n_choices = n(), same = n_distinct(cluster) == 1, .groups = "drop") %>%
    filter(n_choices == 2) %>%
    pull(same) %>% mean()
}
random_cluster_rates <- vapply(seq_len(N_CLUSTER_RANDOMIZATIONS), function(i) {
  consistency_rate_from_pool(cluster_pool %>% group_by(city) %>% mutate(cluster = sample(cluster)) %>% ungroup())
}, numeric(1))
observed_rate_recomputed <- consistency_rate_from_pool(cluster_pool)
write_tex_value("RandomClusterConsistencyMean", 100 * mean(random_cluster_rates), fmt = "%.1f", file = VALUES_TEX)
write_tex_value("RandomClusterConsistencySD", 100 * sd(random_cluster_rates), fmt = "%.1f", file = VALUES_TEX)
write_tex_value("RandomClusterConsistencyQNinetyFive", 100 * quantile(random_cluster_rates, 0.95), fmt = "%.1f", file = VALUES_TEX)
write_tex_value("PvalRandomClusterConsistency",
                format_pvalue(mean(random_cluster_rates >= observed_rate_recomputed)), file = VALUES_TEX)
write_tex_value("NRandomClusterDraws", N_CLUSTER_RANDOMIZATIONS, fmt = "%d", file = VALUES_TEX)
write_tex_value("TestStatConsistency", unname(t_consistency$statistic), file = VALUES_TEX)
write_tex_value("PvalConsistency", format_pvalue(t_consistency$p.value), file = VALUES_TEX)
write_tex_value("StarsConsistency", stars_from_pvalue(t_consistency$p.value), file = VALUES_TEX)


### ===========================================================================
### Exploratory analysis (NOT preregistered)
# Everything below was not specified in the preregistration. It's kept in its
# own clearly-marked block, after every confirmatory (H1-H3 + manipulation
# check) analysis above, so it's obvious at a glance which values.tex numbers
# feed a preregistered test and which don't. Each sub-block is wrapped in its
# own tryCatch: a failure here (e.g. a column this analysis relies on being
# absent from an older extension_joined.csv) should not cost the confirmatory
# results above, which have already been written to VALUES_TEX by this point,
# nor prevent dev.off()/the synthetic-dataset generation below from running.
### ===========================================================================

subject_clutter <- df %>%
  distinct(participant_code, clutter_high) %>%
  mutate(participant_code = as.character(participant_code))

### --- Belief about the badge x individual-level cue-effect magnitude ---
# Individual cue effect: within-subject difference (across a subject's 12
# weekends) in the share of cue-eligible listings chosen when the badge was
# visible vs hidden. Reuses h11_df (built for H1.1 above: one row per subject
# x city x weekend, chose_cued_listing + cue_visible). belief_thumb_quality is
# the post-experiment item "properties with the thumbs-up badge correspond
# more to my preferences than others" (1-4 scale) -- pulled into
# analysis_dataset.csv by 00c_build_analysis_dataset.py's subject_cols; if
# analysis_dataset.csv predates that addition (00c wasn't re-run), the column
# won't exist and this whole block writes "n/a" placeholders below rather
# than silently omitting the six \newcommand{}s the paper's prose cites --
# re-run 00c, then this script, to get real numbers here.
write_belief_na <- function() {
  # "=n/a", not bare "n/a", on both Pval* commands: prose cites these as
  # "$p\PvalX$"/"$p\PvalSpearmanX$", relying on the command's own value to
  # supply the "="/"<" prefix (same convention as format_pvalue()'s real
  # output) -- a bare "n/a" would render as the ungrammatical "$pn/a$".
  write_tex_value("CorrBeliefCueEffect", "n/a", file = VALUES_TEX)
  write_tex_value("TestStatBeliefCueEffect", "n/a", file = VALUES_TEX)
  write_tex_value("PvalBeliefCueEffect", "=n/a", file = VALUES_TEX)
  write_tex_value("StarsBeliefCueEffect", "", file = VALUES_TEX)
  write_tex_value("NObsBeliefCueEffect", 0, fmt = "%d", file = VALUES_TEX)
  write_tex_value("CorrSpearmanBeliefCueEffect", "n/a", file = VALUES_TEX)
  write_tex_value("PvalSpearmanBeliefCueEffect", "=n/a", file = VALUES_TEX)
}

if (!"belief_thumb_quality" %in% names(df)) {
  cat("! 'belief_thumb_quality' is not a column in analysis_dataset.csv -- 00c must be",
      "re-run (it now pulls postexperiment_block.1.player.belief_thumb_quality) before this",
      "correlation can be computed. Writing n/a placeholders so the paper still compiles.\n")
  write_belief_na()
} else {
  tryCatch({
    individual_cue_effect <- h11_df %>%
      group_by(participant_code) %>%
      summarise(
        p_chosen_visible = mean(chose_cued_listing[cue_visible], na.rm = TRUE),
        p_chosen_hidden  = mean(chose_cued_listing[!cue_visible], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(cue_effect = p_chosen_visible - p_chosen_hidden)

    belief_df <- df %>% distinct(participant_code, belief_thumb_quality)

    explore_belief_df <- individual_cue_effect %>%
      inner_join(belief_df, by = "participant_code") %>%
      filter(!is.na(belief_thumb_quality), !is.na(cue_effect))

    if (nrow(explore_belief_df) < 3 || length(unique(explore_belief_df$belief_thumb_quality)) < 2) {
      cat("  ! Not enough non-missing belief/cue-effect pairs (", nrow(explore_belief_df),
          ") to correlate -- writing n/a placeholders\n")
      write_belief_na()
    } else {
      corr_belief <- cor.test(explore_belief_df$belief_thumb_quality, explore_belief_df$cue_effect,
                               method = "pearson")
      corr_belief_sp <- suppressWarnings(cor.test(explore_belief_df$belief_thumb_quality,
                                                   explore_belief_df$cue_effect, method = "spearman"))

      write_tex_value("CorrBeliefCueEffect", unname(corr_belief$estimate), file = VALUES_TEX)
      write_tex_value("TestStatBeliefCueEffect", unname(corr_belief$statistic), file = VALUES_TEX)
      write_tex_value("PvalBeliefCueEffect", format_pvalue(corr_belief$p.value), file = VALUES_TEX)
      write_tex_value("StarsBeliefCueEffect", stars_from_pvalue(corr_belief$p.value), file = VALUES_TEX)
      write_tex_value("NObsBeliefCueEffect", nrow(explore_belief_df), fmt = "%d", file = VALUES_TEX)
      write_tex_value("CorrSpearmanBeliefCueEffect", unname(corr_belief_sp$estimate), file = VALUES_TEX)
      write_tex_value("PvalSpearmanBeliefCueEffect", format_pvalue(corr_belief_sp$p.value), file = VALUES_TEX)

      # Same correlation restricted to subjects who report the highest
      # Booking.com familiarity (4 = "use it regularly"): does the belief-
      # effect link survive among those who know what the badge is?
      familiar_df <- if ("booking_familiarity" %in% names(df)) {
        explore_belief_df %>%
          inner_join(df %>% distinct(participant_code, booking_familiarity), by = "participant_code") %>%
          filter(booking_familiarity == 4)
      } else explore_belief_df[0, ]
      if (nrow(familiar_df) >= 3 && length(unique(familiar_df$belief_thumb_quality)) >= 2) {
        corr_fam <- suppressWarnings(cor.test(familiar_df$belief_thumb_quality, familiar_df$cue_effect,
                                              method = "spearman"))
        write_tex_value("CorrSpearmanBeliefCueEffectFamiliar", unname(corr_fam$estimate), file = VALUES_TEX)
        write_tex_value("PvalSpearmanBeliefCueEffectFamiliar", format_pvalue(corr_fam$p.value), file = VALUES_TEX)
        write_tex_value("StarsBeliefCueEffectFamiliar", stars_from_pvalue(corr_fam$p.value), file = VALUES_TEX)
      } else {
        write_tex_value("CorrSpearmanBeliefCueEffectFamiliar", "n/a", file = VALUES_TEX)
        write_tex_value("PvalSpearmanBeliefCueEffectFamiliar", "=n/a", file = VALUES_TEX)
        write_tex_value("StarsBeliefCueEffectFamiliar", "", file = VALUES_TEX)
      }
      write_tex_value("NObsBeliefCueEffectFamiliar", nrow(familiar_df), fmt = "%d", file = VALUES_TEX)

      # Mean cue effect by belief level (1-4), full sample and regular users.
      for (sub in list(list(d = explore_belief_df, sfx = ""), list(d = familiar_df, sfx = "Familiar"))) {
        for (b in 1:4) {
          v <- sub$d$cue_effect[sub$d$belief_thumb_quality == b]
          lab <- c("One", "Two", "Three", "Four")[b]
          write_tex_value(paste0("MeanCueEffectBelief", lab, sub$sfx), if (length(v)) mean(v) else NA, fmt = "%.3f", file = VALUES_TEX)
          write_tex_value(paste0("NCueEffectBelief", lab, sub$sfx), length(v), fmt = "%d", file = VALUES_TEX)
        }
      }

      p_belief_audit <- ggplot(explore_belief_df, aes(x = factor(belief_thumb_quality), y = cue_effect)) +
        geom_boxplot(outlier.shape = NA, fill = "#cde2fb") +
        geom_jitter(width = 0.1, height = 0, alpha = 0.6) +
        labs(
          title = "Individual cue effect by self-reported belief in the badge",
          x = "Belief thumb quality (1-4)", y = "Individual cue effect (visible - hidden)"
        ) +
        theme_minimal(base_size = 11)
      print(p_belief_audit)

      cat("Exploratory: belief x cue-effect correlation r =", round(unname(corr_belief$estimate), 3),
          " (N =", nrow(explore_belief_df), ")\n")
    }
  }, error = function(e) {
    cat("! Exploratory belief x cue-effect block failed:", conditionMessage(e),
        "-- writing n/a placeholders\n")
    write_belief_na()
  })
}

### --- Response time patterns ---
# Plain descriptives (session/task duration) moved out of this block and up
# into the sample-descriptives paragraph near MeanLoadingTime -- everything
# left here is a correlation or an estimated effect, per the "exploratory
# means a correlation or an effect, not a summary statistic" rule for this
# section.
tryCatch({
  decision_time_df <- df %>%
    distinct(participant_code, weekend_number_global, decision_time_seconds) %>%
    filter(!is.na(decision_time_seconds), decision_time_seconds > 0)

  # weekend_number_global is already the subject's 1-12 trial-presentation
  # order (city order and, within city, choice-set order are both randomised
  # at session creation -- see the "oTree application and randomization"
  # paragraph in Methods -- so this is a genuine learning/fatigue-curve
  # check, not just a re-statement of city or weekend identity).
  m_trial <- feols(log(decision_time_seconds) ~ weekend_number_global | participant_code,
                    data = decision_time_df)
  print(summary(m_trial))
  coef_trial <- coef(m_trial)["weekend_number_global"]
  se_trial   <- se(m_trial)["weekend_number_global"]
  p_trial    <- pvalue(m_trial)["weekend_number_global"]

  write_tex_value("RegCoefTrialOrder", coef_trial, file = VALUES_TEX)
  write_tex_value("RegSETrialOrder", se_trial, file = VALUES_TEX)
  write_tex_value("RegPvalTrialOrder", format_pvalue(p_trial), file = VALUES_TEX)
  write_tex_value("RegStarsTrialOrder", stars_from_pvalue(p_trial), file = VALUES_TEX)
  write_tex_value("RegNTrialOrder", nobs(m_trial), fmt = "%d", file = VALUES_TEX)

  # Median decision time by block of four choices (choices 1-4, 5-8, 9-12 = the three cities in presentation order).
  block_medians <- decision_time_df %>%
    mutate(block = ceiling(weekend_number_global / 4)) %>%
    group_by(block) %>%
    summarise(med = median(decision_time_seconds), n = n(), .groups = "drop")
  for (i in seq_len(nrow(block_medians))) {
    lab <- c("One", "Two", "Three")[block_medians$block[i]]
    write_tex_value(paste0("MedianDecisionTimeBlock", lab), block_medians$med[i], fmt = "%.0f", file = VALUES_TEX)
    write_tex_value(paste0("NObsDecisionTimeBlock", lab), block_medians$n[i], fmt = "%d", file = VALUES_TEX)
  }

  timing_corr_df <- df %>%
    distinct(participant_code, weekend_number_global, decision_time_seconds, loading_time_seconds) %>%
    filter(!is.na(decision_time_seconds), !is.na(loading_time_seconds))
  corr_timing <- cor.test(timing_corr_df$decision_time_seconds, timing_corr_df$loading_time_seconds)
  write_tex_value("CorrDecisionLoadingTime", unname(corr_timing$estimate), file = VALUES_TEX)
  write_tex_value("PvalDecisionLoadingTime", format_pvalue(corr_timing$p.value), file = VALUES_TEX)
  write_tex_value("NObsDecisionLoadingTime", nrow(timing_corr_df), fmt = "%d", file = VALUES_TEX)

  p_trial_audit <- ggplot(decision_time_df, aes(x = weekend_number_global, y = decision_time_seconds)) +
    geom_jitter(width = 0.15, alpha = 0.3) +
    geom_smooth(method = "loess", se = TRUE, color = "#2a78d6") +
    labs(title = "Decision time across trial order (1-12)",
         x = "Trial order (weekend_number_global)", y = "Decision time (s)") +
    theme_minimal(base_size = 11)
  print(p_trial_audit)

  # --- Appendix figure: decision time by trial order (1-12), as three
  # quantile regressions (tau = 0.1, 0.5, 0.9) of raw seconds on trial order,
  # WITHOUT subject fixed effects (a pooled learning curve across subjects,
  # not the within-subject slope m_trial above estimates). The three lines
  # show whether a session compresses the whole distribution (all three
  # slopes negative) or mainly its slow tail (the 0.9 line alone). Points are
  # the empirical quantiles at each order position, so the reader can see how
  # well the linear fit summarises them. Raw seconds rather than log: the
  # figure's y-axis is in seconds and starts at 0, and a quantile regression
  # slope on the raw scale is directly "seconds per additional trial". ---
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    stop("Package 'quantreg' is not installed -- run install.packages(\"quantreg\") ",
         "(needed for the trial-order quantile regression figure).")
  }
  order_taus <- c(0.1, 0.5, 0.9)
  order_tau_labels <- c("0.1" = "QTen", "0.5" = "QFifty", "0.9" = "QNinety")
  m_order_rq <- quantreg::rq(decision_time_seconds ~ weekend_number_global,
                             tau = order_taus, data = decision_time_df)
  # Bootstrap SEs (xy-pair resampling): the sandwich/"nid" SE assumes a
  # locally smooth density, which is shaky at tau = 0.9 in a right-skewed
  # duration variable. Seeded for determinism (replication-package rule).
  set.seed(20260904)
  m_order_rq_summary <- summary(m_order_rq, se = "boot", R = 500)
  print(m_order_rq_summary)

  order_rq_coefs <- lapply(seq_along(order_taus), function(i) {
    cf <- m_order_rq_summary[[i]]$coefficients
    data.frame(tau = order_taus[i],
               intercept = cf["(Intercept)", "Value"],
               slope = cf["weekend_number_global", "Value"],
               se = cf["weekend_number_global", "Std. Error"],
               p = cf["weekend_number_global", "Pr(>|t|)"])
  }) %>% bind_rows()
  print(order_rq_coefs, row.names = FALSE)

  for (i in seq_len(nrow(order_rq_coefs))) {
    lab <- order_tau_labels[[as.character(order_rq_coefs$tau[i])]]
    write_tex_value(paste0("RegCoefTrialOrder", lab), order_rq_coefs$slope[i], fmt = "%.2f", file = VALUES_TEX)
    write_tex_value(paste0("RegSETrialOrder", lab), order_rq_coefs$se[i], fmt = "%.2f", file = VALUES_TEX)
    write_tex_value(paste0("RegPvalTrialOrder", lab), format_pvalue(order_rq_coefs$p[i]), file = VALUES_TEX)
    write_tex_value(paste0("RegStarsTrialOrder", lab), stars_from_pvalue(order_rq_coefs$p[i]), file = VALUES_TEX)
  }

  # Empirical quantiles at each order position (the points), one row per
  # position x tau, plus the fitted lines from the coefficients above.
  order_quantile_df <- decision_time_df %>%
    group_by(weekend_number_global) %>%
    summarise(
      n = n(),
      value = quantile(decision_time_seconds, order_taus),
      tau = order_taus,
      .groups = "drop"
    )
  order_fit_df <- tidyr::expand_grid(order_rq_coefs, weekend_number_global = 1:12) %>%
    mutate(fitted = intercept + slope * weekend_number_global)

  # Sequential blues (light -> dark = 0.1 -> 0.9): one hue, since the three
  # lines are ordered levels of one variable, not three categories.
  order_tau_colors <- c("0.1" = "#9ec3ea", "0.5" = "#2a78d6", "0.9" = "#0f3f7a")
  p_order_quantile <- ggplot() +
    geom_point(data = order_quantile_df,
               aes(x = weekend_number_global, y = value, color = factor(tau)), size = 2) +
    geom_line(data = order_fit_df,
              aes(x = weekend_number_global, y = fitted, color = factor(tau)), linewidth = 0.8) +
    scale_color_manual(values = order_tau_colors, name = "Quantile",
                       labels = c("0.1" = "10th", "0.5" = "50th", "0.9" = "90th")) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Trial order (1-12)", y = "Decision time (s)") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), legend.position = "top")

  print(p_order_quantile + labs(
    title = "Decision time by trial order: quantile regressions",
    subtitle = "Points: empirical 10th/50th/90th percentiles per position; lines: linear quantile fits (no subject FE)"
  )) # page in the audit PDF (PLOTS_PDF)

  ORDER_MEDIAN_FIGURES_DIR <- path(OVERLEAF_DIR, "illustrations", "response_time_by_order")
  dir_create(ORDER_MEDIAN_FIGURES_DIR)
  order_median_fig_path <- path(LOCAL_OUTPUT_DIR, "figures", "response_time_by_order.png")
  ggsave(order_median_fig_path, p_order_quantile, width = 5.5, height = 3.5, dpi = 300)
  overleaf_order_median_path <- path(ORDER_MEDIAN_FIGURES_DIR, "response_time_by_order.png")
  file_copy(order_median_fig_path, overleaf_order_median_path, overwrite = TRUE)
  cat("Wrote decision-time-by-order quantile-regression appendix figure to", overleaf_order_median_path, "\n")

  write_tex_value("NObsOrderMedian", nrow(decision_time_df), fmt = "%d", file = VALUES_TEX)

  cat("Exploratory: response time patterns written to values.tex\n")
}, error = function(e) {
  cat("! Exploratory response-time block failed:", conditionMessage(e), "\n")
})

### --- Tracked interface use beyond the preregistered measures ---
# Reads the same 00b/00a event-level outputs 00c consumes
# (extension_joined.csv, the choice-set-joined events; extension_converted.csv,
# every raw event unfiltered) directly, since none of these signals (hover
# dwell, scroll depth, viewport dwell, badge-tooltip exposure) were carried
# into analysis_dataset.csv -- they were never part of the preregistered
# pipeline. Every contrast below ALWAYS writes its full set of \newcommand{}s
# (an "n/a" placeholder when a contrast can't be computed) rather than
# skipping silently, so the paper never fails to compile for want of an
# undefined command regardless of what the real data looks like.
tryCatch({
  EXTENSION_JOINED_CSV    <- path(OUTPUT_DIR, "extension_joined.csv")
  EXTENSION_CONVERTED_CSV <- path(OUTPUT_DIR, "extension_converted.csv")

  if (!file_exists(EXTENSION_JOINED_CSV) || !file_exists(EXTENSION_CONVERTED_CSV)) {
    stop("extension_joined.csv / extension_converted.csv not found in ", OUTPUT_DIR,
         " -- 00a/00b must have run for this exploratory block to work.")
  }

  extension_joined <- read_csv(EXTENSION_JOINED_CSV, show_col_types = FALSE,
                                col_types = cols(.default = col_character()))
  extension_converted <- read_csv(EXTENSION_CONVERTED_CSV, show_col_types = FALSE,
                                   col_types = cols(.default = col_character()))

  # One mean-per-subject value, then a two-sided Welch t-test between High and
  # Low Clutter subjects -- every contrast below (hover dwell, scroll depth,
  # viewport dwell, tooltip duration) follows this same pattern.
  report_clutter_contrast <- function(label, per_subject_df, value_col, file = VALUES_TEX) {
    write_na <- function() {
      write_tex_value(paste0("Mean", label, "HighClutter"), "n/a", file = file)
      write_tex_value(paste0("Mean", label, "LowClutter"), "n/a", file = file)
      write_tex_value(paste0("TestStat", label), "n/a", file = file)
      # "=n/a", not bare "n/a": prose cites this as "$p\PvalX$", relying on
      # the command's own value to supply the "="/"<" prefix (same convention
      # as format_pvalue()'s real output) -- a bare "n/a" here would render
      # as the ungrammatical "$pn/a$".
      write_tex_value(paste0("Pval", label), "=n/a", file = file)
      write_tex_value(paste0("Stars", label), "", file = file)
      write_tex_value(paste0("NObs", label), 0, fmt = "%d", file = file)
    }
    d <- per_subject_df %>%
      left_join(subject_clutter, by = "participant_code") %>%
      filter(!is.na(.data[[value_col]]), !is.na(clutter_high))
    if (n_distinct(d$clutter_high) < 2 || min(table(d$clutter_high)) < 2) {
      cat("  ! Skipping", label, "-- not enough subjects in both clutter groups (",
          nrow(d), "total obs) -- writing n/a placeholders\n")
      write_na(); return(invisible(NULL))
    }
    tt <- tryCatch(t.test(d[[value_col]][d$clutter_high], d[[value_col]][!d$clutter_high]),
                   error = function(e) NULL)
    if (is.null(tt)) {
      cat("  ! t.test failed for", label, "-- writing n/a placeholders\n")
      write_na(); return(invisible(NULL))
    }
    # "%.3g" = three significant digits (2.56 s, 78.4 %, 164 s), matching the
    # NASA-TLX table: these are means of raw seconds/percentages, where a
    # third decimal is noise.
    write_tex_value(paste0("Mean", label, "HighClutter"), mean(d[[value_col]][d$clutter_high], na.rm = TRUE), fmt = "%.3g", file = file)
    write_tex_value(paste0("Mean", label, "LowClutter"), mean(d[[value_col]][!d$clutter_high], na.rm = TRUE), fmt = "%.3g", file = file)
    write_tex_value(paste0("TestStat", label), unname(tt$statistic), fmt = "%.3g", file = file)
    write_tex_value(paste0("Pval", label), format_pvalue(tt$p.value), file = file)
    write_tex_value(paste0("Stars", label), stars_from_pvalue(tt$p.value), file = file)
    write_tex_value(paste0("NObs", label), nrow(d), fmt = "%d", file = file)
    cat("  OK:", label, "N =", nrow(d), "\n")
    invisible(tt)
  }

  # --- Hover dwell on property cards ---
  hover_dwell_by_subject <- extension_joined %>%
    filter(type == "hover", kind == "product_card", !is.na(durationMs)) %>%
    mutate(dwell_s = as.numeric(durationMs) / 1000) %>%
    group_by(participant_code) %>%
    summarise(mean_hover_dwell_s = mean(dwell_s, na.rm = TRUE), .groups = "drop")
  report_clutter_contrast("HoverDwell", hover_dwell_by_subject, "mean_hover_dwell_s")

  # --- Scroll depth ---
  scroll_depth_by_subject <- extension_joined %>%
    filter(type == "scroll") %>%
    mutate(scrollDepthPercent = as.numeric(scrollDepthPercent)) %>%
    group_by(participant_code, cell_index) %>%
    summarise(max_depth = max(scrollDepthPercent, na.rm = TRUE), .groups = "drop") %>%
    group_by(participant_code) %>%
    summarise(mean_max_scroll_depth = mean(max_depth, na.rm = TRUE), .groups = "drop")
  report_clutter_contrast("MaxScrollDepth", scroll_depth_by_subject, "mean_max_scroll_depth")

  # --- Viewport dwell (total card-visible time per weekend) ---
  # An IntersectionObserver fires once per THRESHOLD crossing (e.g. 0.5 and
  # 1), so one continuous "card in view" episode can produce several
  # consecutive isIntersecting=TRUE rows (ratio climbing 0.5 -> 1, or easing
  # back down without leaving) before the row that actually marks the exit
  # (isIntersecting=FALSE). Naively pairing every TRUE row with the very next
  # row would fragment one real viewing episode into several short pieces --
  # harmless for a simple SUM (the pieces still add up to the true total) but
  # biased for a MEAN (it overcounts short episodes). Group consecutive TRUE
  # rows into one episode first (episode_id increments only at a genuine
  # not-intersecting -> intersecting transition), then take the episode's
  # start as its first row's timestamp and its end as its last row's
  # `next_ts` (the row immediately after the run, i.e. the real exit) --
  # both rows are sorted within the group, so min(timestamp)/max(next_ts)
  # land on them without needing to index by position. Episodes with no
  # observed exit (still open when the group's events end, e.g. page
  # navigated away) have next_ts = NA and are dropped, same convention as
  # 00c's own time-on-listing-page dwell (unknown, not zero).
  viewport_dwell_episodes <- extension_joined %>%
    filter(type == "viewport", !is.na(targetPropertyId), targetPropertyId != "") %>%
    mutate(timestamp = as.numeric(timestamp),
           is_intersecting = tolower(as.character(isIntersecting)) %in% c("true", "1")) %>%
    filter(!is.na(timestamp)) %>%
    arrange(participant_code, cell_index, targetPropertyId, timestamp) %>%
    group_by(participant_code, cell_index, targetPropertyId) %>%
    mutate(
      next_ts = lead(timestamp),
      is_episode_start = is_intersecting & !coalesce(lag(is_intersecting), FALSE),
      episode_id = cumsum(is_episode_start)
    ) %>%
    ungroup() %>%
    filter(is_intersecting) %>%
    group_by(participant_code, cell_index, targetPropertyId, episode_id) %>%
    summarise(dwell_s = (max(next_ts) - min(timestamp)) / 1000, .groups = "drop") %>%
    filter(!is.na(dwell_s), dwell_s >= 0)

  viewport_dwell_by_subject <- viewport_dwell_episodes %>%
    group_by(participant_code, cell_index) %>%
    summarise(total_viewport_s = sum(dwell_s), .groups = "drop") %>%
    group_by(participant_code) %>%
    summarise(mean_total_viewport_s = mean(total_viewport_s, na.rm = TRUE), .groups = "drop")
  report_clutter_contrast("ViewportDwell", viewport_dwell_by_subject, "mean_total_viewport_s")

  # Share of listings with >=10s cumulative viewport-visible time (across
  # episodes); listing-level share plus per-subject contrast by clutter.
  viewport_10s_by_listing <- viewport_dwell_episodes %>%
    group_by(participant_code, cell_index, targetPropertyId) %>%
    summarise(total_dwell_s = sum(dwell_s), .groups = "drop") %>%
    mutate(reached_10s = total_dwell_s >= 10)

  write_tex_value("PctViewportLongDwell", format_pct(mean(viewport_10s_by_listing$reached_10s)), file = VALUES_TEX)
  write_tex_value("NObsViewportLongDwellListings", nrow(viewport_10s_by_listing), fmt = "%d", file = VALUES_TEX)

  # 0-100 scale, as MaxScrollDepth.
  viewport_10s_by_subject <- viewport_10s_by_listing %>%
    group_by(participant_code) %>%
    summarise(share_reached_10s = 100 * mean(reached_10s), .groups = "drop")
  report_clutter_contrast("ViewportLongDwell", viewport_10s_by_subject, "share_reached_10s")

  # --- Badge tooltip (pouce_explanation) exposure ---
  tooltip_by_cell <- extension_converted %>%
    filter(type == "pouce_explanation") %>%
    mutate(durationMs = as.numeric(durationMs), cell_index = as.integer(cell_index)) %>%
    group_by(participant_code, cell_index) %>%
    summarise(n_opens = n(), mean_duration_s = mean(durationMs, na.rm = TRUE) / 1000, .groups = "drop")

  tooltip_by_subject <- tooltip_by_cell %>%
    group_by(participant_code) %>%
    summarise(mean_tooltip_duration_s = mean(mean_duration_s, na.rm = TRUE), .groups = "drop")
  report_clutter_contrast("TooltipDuration", tooltip_by_subject, "mean_tooltip_duration_s")
  if (nrow(tooltip_by_cell) == 0) {
    cat("  (No pouce_explanation/badge-tooltip events found in the tracking data at all --",
        "TooltipDuration above is an n/a placeholder, not a null result.)\n")
  }

  # Share of cue-visible weekends where the tooltip was opened at least once
  # -- a separate two-proportion test, so it gets its own always-write helper
  # rather than reusing report_clutter_contrast (a t-test over a continuous
  # per-subject mean isn't the right test for a share/count outcome).
  report_tooltip_share <- function(tooltip_by_cell) {
    cue_visible_cells <- df %>%
      filter(cued_weekend, !is.na(clutter_high)) %>%
      distinct(participant_code, weekend_number_global, clutter_high) %>%
      mutate(participant_code = as.character(participant_code))
    tooltip_opened_cells <- tooltip_by_cell %>%
      distinct(participant_code, cell_index) %>%
      mutate(tooltip_opened = TRUE) %>%
      rename(weekend_number_global = cell_index)
    cue_visible_tooltip <- cue_visible_cells %>%
      left_join(tooltip_opened_cells, by = c("participant_code", "weekend_number_global")) %>%
      mutate(tooltip_opened = coalesce(tooltip_opened, FALSE))

    write_na <- function() {
      write_tex_value("PctTooltipOpenedHighClutter", "n/a", file = VALUES_TEX)
      write_tex_value("PctTooltipOpenedLowClutter", "n/a", file = VALUES_TEX)
      write_tex_value("PvalTooltipOpened", "=n/a", file = VALUES_TEX) # see report_clutter_contrast's write_na() for why "=n/a" not bare "n/a"
      write_tex_value("StarsTooltipOpened", "", file = VALUES_TEX)
      write_tex_value("NObsTooltipOpened", nrow(cue_visible_tooltip), fmt = "%d", file = VALUES_TEX)
    }
    if (n_distinct(cue_visible_tooltip$clutter_high) < 2) {
      cat("  ! Skipping TooltipOpened share test -- not enough clutter variation -- n/a\n")
      write_na(); return(invisible(NULL))
    }
    prop_test_tooltip <- tryCatch(
      suppressWarnings(prop.test(
        x = c(sum(cue_visible_tooltip$tooltip_opened[cue_visible_tooltip$clutter_high]),
              sum(cue_visible_tooltip$tooltip_opened[!cue_visible_tooltip$clutter_high])),
        n = c(sum(cue_visible_tooltip$clutter_high), sum(!cue_visible_tooltip$clutter_high))
      )),
      error = function(e) NULL
    )
    if (is.null(prop_test_tooltip)) {
      cat("  ! prop.test failed for TooltipOpened -- n/a\n")
      write_na(); return(invisible(NULL))
    }
    write_tex_value("PctTooltipOpenedHighClutter",
                     format_pct(mean(cue_visible_tooltip$tooltip_opened[cue_visible_tooltip$clutter_high])),
                     file = VALUES_TEX)
    write_tex_value("PctTooltipOpenedLowClutter",
                     format_pct(mean(cue_visible_tooltip$tooltip_opened[!cue_visible_tooltip$clutter_high])),
                     file = VALUES_TEX)
    write_tex_value("PvalTooltipOpened", format_pvalue(prop_test_tooltip$p.value), file = VALUES_TEX)
    write_tex_value("StarsTooltipOpened", stars_from_pvalue(prop_test_tooltip$p.value), file = VALUES_TEX)
    write_tex_value("NObsTooltipOpened", nrow(cue_visible_tooltip), fmt = "%d", file = VALUES_TEX)
    cat("  OK: TooltipOpened share test, N =", nrow(cue_visible_tooltip), "\n")
    invisible(prop_test_tooltip)
  }
  report_tooltip_share(tooltip_by_cell)

  # --- Tooltip exposure at the SUBJECT level (the weekend-level share above
  # is too sparse to split by clutter): how many subjects opened the badge
  # tooltip at least once, by clutter condition, and whether having opened it
  # goes with the belief item and self-reported Booking.com familiarity
  # (Spearman, since both are ordinal). Always writes its commands. ---
  subject_traits <- df %>%
    distinct(participant_code, clutter_high,
             belief_thumb_quality = if ("belief_thumb_quality" %in% names(df)) belief_thumb_quality else NA,
             booking_familiarity = if ("booking_familiarity" %in% names(df)) booking_familiarity else NA) %>%
    mutate(participant_code = as.character(participant_code))
  tooltip_subjects <- subject_traits %>%
    mutate(tooltip_opened = participant_code %in% unique(as.character(tooltip_by_cell$participant_code)))
  write_tex_value("NSubjectsTooltipOpened", sum(tooltip_subjects$tooltip_opened), fmt = "%d", file = VALUES_TEX)
  write_tex_value("NSubjectsTooltip", nrow(tooltip_subjects), fmt = "%d", file = VALUES_TEX)
  write_tex_value("NSubjectsTooltipOpenedHighClutter",
                  sum(tooltip_subjects$tooltip_opened & tooltip_subjects$clutter_high, na.rm = TRUE), fmt = "%d", file = VALUES_TEX)
  write_tex_value("NSubjectsTooltipOpenedLowClutter",
                  sum(tooltip_subjects$tooltip_opened & !tooltip_subjects$clutter_high, na.rm = TRUE), fmt = "%d", file = VALUES_TEX)
  write_tex_value("NSubjectsHighClutter", sum(tooltip_subjects$clutter_high, na.rm = TRUE), fmt = "%d", file = VALUES_TEX)
  write_tex_value("NSubjectsLowClutter", sum(!tooltip_subjects$clutter_high, na.rm = TRUE), fmt = "%d", file = VALUES_TEX)
  for (trait in c(Belief = "belief_thumb_quality", Familiarity = "booking_familiarity")) {
    label <- names(which(c(Belief = "belief_thumb_quality", Familiarity = "booking_familiarity") == trait))
    d <- tooltip_subjects %>% filter(!is.na(.data[[trait]]))
    if (sum(d$tooltip_opened) >= 2 && sum(!d$tooltip_opened) >= 2 && n_distinct(d[[trait]]) >= 2) {
      ct <- suppressWarnings(cor.test(as.numeric(d$tooltip_opened), d[[trait]], method = "spearman"))
      write_tex_value(paste0("CorrTooltip", label), unname(ct$estimate), file = VALUES_TEX)
      write_tex_value(paste0("PvalTooltip", label), format_pvalue(ct$p.value), file = VALUES_TEX)
      write_tex_value(paste0("StarsTooltip", label), stars_from_pvalue(ct$p.value), file = VALUES_TEX)
    } else {
      write_tex_value(paste0("CorrTooltip", label), "n/a", file = VALUES_TEX)
      write_tex_value(paste0("PvalTooltip", label), "=n/a", file = VALUES_TEX)
      write_tex_value(paste0("StarsTooltip", label), "", file = VALUES_TEX)
    }
    write_tex_value(paste0("NObsTooltip", label), nrow(d), fmt = "%d", file = VALUES_TEX)
  }

  cat("Exploratory: tracked-interface-use contrasts written to values.tex\n")
}, error = function(e) {
  cat("! Exploratory tracking-data block failed:", conditionMessage(e), "\n")
})

### --- Subject clusters on decision-mode indicators (not preregistered) ---
# k-means on the subject-level variables the preregistration names as
# indicative of decision mode: NASA-TLX mental demand, performance and
# effort, median decision time and mean number of listings clicked per set.
# Variables are standardised; k = 3 (chosen a priori, small sample), seeded,
# 50 restarts. Clusters are labelled 1..k by increasing median decision time
# so the labels are stable across runs. Output: a table of cluster profiles
# (centroids in original units, share of subjects per clutter condition) and
# a Fisher test of cluster membership x clutter.
tryCatch({
  clicks_per_set <- df %>%
    group_by(participant_code, weekend_number_global) %>%
    summarise(n_clicked = sum(listing_clicked), .groups = "drop") %>%
    group_by(participant_code) %>%
    summarise(mean_clicked_per_set = mean(n_clicked), .groups = "drop")
  cluster_df <- df %>%
    distinct(participant_code, clutter_high, nasa_tlx_mental, nasa_tlx_performance, nasa_tlx_effort) %>%
    inner_join(subject_dtime, by = "participant_code") %>%
    inner_join(clicks_per_set, by = "participant_code") %>%
    filter(if_all(c(nasa_tlx_mental, nasa_tlx_performance, nasa_tlx_effort, median_dtime, mean_clicked_per_set), ~ !is.na(.x)))
  cluster_vars <- c("nasa_tlx_mental", "nasa_tlx_performance", "nasa_tlx_effort", "median_dtime", "mean_clicked_per_set")
  K_SUBJECT_CLUSTERS <- 3
  set.seed(20260905)
  km <- kmeans(scale(cluster_df[cluster_vars]), centers = K_SUBJECT_CLUSTERS, nstart = 50)
  cluster_df$cluster_raw <- km$cluster
  relabel <- cluster_df %>% group_by(cluster_raw) %>% summarise(m = median(median_dtime), .groups = "drop") %>%
    arrange(m) %>% mutate(cluster = row_number())
  cluster_df <- cluster_df %>% left_join(relabel %>% select(cluster_raw, cluster), by = "cluster_raw")
  profile <- cluster_df %>%
    group_by(cluster) %>%
    summarise(n = n(), n_high = sum(clutter_high), n_low = sum(!clutter_high),
              across(all_of(cluster_vars), mean), .groups = "drop") %>%
    mutate(pct_high = 100 * n_high / sum(n_high), pct_low = 100 * n_low / sum(n_low))
  p_cluster <- tryCatch(fisher.test(table(cluster_df$cluster, cluster_df$clutter_high))$p.value, error = function(e) NA_real_)
  write_tex_value("NSubjectClusters", K_SUBJECT_CLUSTERS, fmt = "%d", file = VALUES_TEX)
  write_tex_value("NObsSubjectClusters", nrow(cluster_df), fmt = "%d", file = VALUES_TEX)
  write_tex_value("PvalSubjectClustersClutter", format_pvalue(p_cluster), file = VALUES_TEX)
  write_tex_value("StarsSubjectClustersClutter", stars_from_pvalue(p_cluster), file = VALUES_TEX)
  for (i in seq_len(nrow(profile))) {
    lab <- c("One", "Two", "Three", "Four", "Five")[profile$cluster[i]]
    write_tex_value(paste0("NSubjectCluster", lab), profile$n[i], fmt = "%d", file = VALUES_TEX)
    write_tex_value(paste0("PctSubjectCluster", lab, "HighClutter"), profile$pct_high[i], fmt = "%.0f", file = VALUES_TEX)
    write_tex_value(paste0("PctSubjectCluster", lab, "LowClutter"), profile$pct_low[i], fmt = "%.0f", file = VALUES_TEX)
  }
  cl_lines <- c("\\begin{tabular}{lccc}", "\\toprule\\toprule",
                paste0(" & ", paste(sprintf("Cluster %d", profile$cluster), collapse = " & "), " \\\\[-1.8ex]"),
                "\\midrule",
                paste0("$N$ & ", paste(profile$n, collapse = " & "), " \\\\"),
                paste0("Share of Cluttered subjects (\\%) & ", paste(sprintf("%.0f", profile$pct_high), collapse = " & "), " \\\\"),
                paste0("Share of Low Clutter subjects (\\%) & ", paste(sprintf("%.0f", profile$pct_low), collapse = " & "), " \\\\"),
                "\\midrule")
  var_labels <- c(nasa_tlx_mental = "NASA-TLX mental demand (0--100)", nasa_tlx_performance = "NASA-TLX performance (0--100)",
                  nasa_tlx_effort = "NASA-TLX effort (0--100)", median_dtime = "Median decision time (s)",
                  mean_clicked_per_set = "Listings clicked per set")
  for (v in cluster_vars) {
    cl_lines <- c(cl_lines, paste0(var_labels[[v]], " & ", paste(sprintf("%.1f", profile[[v]]), collapse = " & "), " \\\\"))
  }
  cl_lines <- c(cl_lines, "\\bottomrule\\bottomrule", "\\end{tabular}")
  writeLines(cl_lines, file(path(OVERLEAF_DIR, "tables", "table_subject_clusters.tex"), open = "wb"))
  cat("Exploratory: subject clusters written (Fisher p =", round(p_cluster, 3), ")\n")
}, error = function(e) {
  cat("! Subject-cluster block failed:", conditionMessage(e), "\n")
  write_tex_value("PvalSubjectClustersClutter", "=n/a", file = VALUES_TEX)
})

### --- Open-text answers: keyword coding (not preregistered) ---
# Rules live in R/open_text_coding.R (calibrated on the anonymised export and
# validated with DM, 2026-09-05). Writes, per category, the count and share
# of subjects (NOpen*/PctOpen*), and for a few key categories the contrast
# of subject traits between flagged and non-flagged subjects.
tryCatch({
  source(path(script_dir, "R", "open_text_coding.R"))
  open_cols <- c("choice_process_open", "belief_thumb_meaning_open", "thumb_use_open", "feedback_open")
  if (!all(open_cols %in% names(df))) stop("open-text columns missing from analysis_dataset.csv -- rerun 00c")
  open_df <- df %>%
    distinct(participant_code, across(all_of(open_cols)), belief_thumb_quality, booking_familiarity,
             cue_recognition, clutter_high, across(any_of(c("noticed_decoy_checkmark", "noticed_decoy_badge")))) %>%
    mutate(participant_code = as.character(participant_code),
           cue_recognition = as.logical(cue_recognition))
  if (all(c("noticed_decoy_checkmark", "noticed_decoy_badge") %in% names(open_df))) {
    open_df <- open_df %>%
      mutate(recognition_correct = cue_recognition & !(as.logical(noticed_decoy_checkmark) | as.logical(noticed_decoy_badge)))
  }
  if (exists("individual_cue_effect")) {
    open_df <- open_df %>% left_join(individual_cue_effect %>% mutate(participant_code = as.character(participant_code)) %>%
                                       select(participant_code, cue_effect), by = "participant_code")
  }
  if (exists("tooltip_subjects")) {
    open_df <- open_df %>% left_join(tooltip_subjects %>% select(participant_code, tooltip_opened), by = "participant_code")
  }
  coded <- code_open_text(open_df)
  write_tex_value("NOpenText", nrow(coded), fmt = "%d", file = VALUES_TEX)

  flag_labels <- c(
    MeaningReviews = "meaning_reviews", MeaningPlatform = "meaning_platform", MeaningPaid = "meaning_paid",
    MeaningGeneric = "meaning_generic", MeaningValue = "meaning_value", MeaningDontKnow = "meaning_dontknow",
    MeaningOther = "meaning_other",
    UseNotNoticed = "use_not_noticed", UseNoticedNotUsed = "use_noticed_not_used", UseBareNo = "use_bare_no",
    UseAsSignal = "use_as_signal", UseToClick = "use_to_click", UseUsed = "use_used",
    UseNotUnderstood = "use_not_understood", UseDistrust = "use_distrust", UseNotUsedAny = "use_not_used_any",
    ChoiceLocation = "choice_location", ChoicePrice = "choice_price", ChoiceRating = "choice_rating",
    ChoicePhotos = "choice_photos", ChoiceAmenities = "choice_amenities", ChoiceBadge = "choice_badge",
    FeedbackSlowOrLong = "feedback_slow_or_long", FeedbackPositive = "feedback_positive"
  )
  for (lab in names(flag_labels)) {
    v <- coded[[flag_labels[[lab]]]]
    write_tex_value(paste0("NOpen", lab), sum(v, na.rm = TRUE), fmt = "%d", file = VALUES_TEX)
    write_tex_value(paste0("PctOpen", lab), format_pct(mean(v, na.rm = TRUE), 0), file = VALUES_TEX)
  }
  # "Reviews" among those who ALSO say they used it, and used among "reviews".
  write_tex_value("NOpenUseUsedMeaningReviews", sum(coded$use_used & coded$meaning_reviews), fmt = "%d", file = VALUES_TEX)
  write_tex_value("PctOpenMeaningReviewsAmongUsed",
                  format_pct(mean(coded$meaning_reviews[coded$use_used]), 0), file = VALUES_TEX)

  # Did the (few) subjects who opened the badge tooltip -- which states the
  # Preferred Partner meaning -- give a correct reading of the badge?
  if ("tooltip_opened" %in% names(coded)) {
    openers <- coded %>% filter(coalesce(tooltip_opened, FALSE))
    write_tex_value("NTooltipOpenersCoded", nrow(openers), fmt = "%d", file = VALUES_TEX)
    for (lab in c("MeaningPaid", "MeaningPlatform", "MeaningReviews", "MeaningGeneric", "UseUsed")) {
      write_tex_value(paste0("NTooltipOpeners", lab), sum(openers[[flag_labels[[lab]]]]), fmt = "%d", file = VALUES_TEX)
    }
  }

  # Trait contrasts: mean (or share) of a trait among flagged vs non-flagged
  # subjects, with a Wilcoxon (ordinal/continuous) or Fisher (binary) p-value.
  traits <- list(
    Belief      = list(col = "belief_thumb_quality", binary = FALSE, fmt = "%.2f"),
    Familiarity = list(col = "booking_familiarity",  binary = FALSE, fmt = "%.2f"),
    CueEffect   = list(col = "cue_effect",           binary = FALSE, fmt = "%.3f"),
    Recognition = list(col = "cue_recognition",      binary = TRUE),
    RecognitionCorrect = list(col = "recognition_correct", binary = TRUE),
    Clutter     = list(col = "clutter_high",         binary = TRUE),
    Tooltip     = list(col = "tooltip_opened",       binary = TRUE)
  )
  key_flags <- c("MeaningReviews", "MeaningGeneric", "MeaningPlatform", "UseUsed", "UseNotNoticed", "UseNotUsedAny")
  for (lab in key_flags) for (tr in names(traits)) {
    tc <- traits[[tr]]; nm <- paste0("Open", lab, tr)
    if (!tc$col %in% names(coded)) next
    v <- coded[[flag_labels[[lab]]]]; y <- coded[[tc$col]]
    ok <- !is.na(v) & !is.na(y)
    if (tc$binary) {
      y <- as.logical(y)
      write_tex_value(paste0("Pct", nm, "Yes"), format_pct(mean(y[ok & v]), 0), file = VALUES_TEX)
      write_tex_value(paste0("Pct", nm, "No"),  format_pct(mean(y[ok & !v]), 0), file = VALUES_TEX)
      p <- tryCatch(fisher.test(table(v[ok], y[ok]))$p.value, error = function(e) NA_real_)
    } else {
      write_tex_value(paste0("Mean", nm, "Yes"), mean(y[ok & v]), fmt = tc$fmt, file = VALUES_TEX)
      write_tex_value(paste0("Mean", nm, "No"),  mean(y[ok & !v]), fmt = tc$fmt, file = VALUES_TEX)
      p <- tryCatch(suppressWarnings(wilcox.test(y[ok & v], y[ok & !v])$p.value), error = function(e) NA_real_)
    }
    write_tex_value(paste0("Pval", nm), if (is.na(p)) "=n/a" else format_pvalue(p), file = VALUES_TEX)
    write_tex_value(paste0("Stars", nm), stars_from_pvalue(p), file = VALUES_TEX)
  }
  cat("Exploratory: open-text coding written to values.tex (", nrow(coded), "subjects )\n")
}, error = function(e) {
  cat("! Open-text coding block failed:", conditionMessage(e), "\n")
})

# Adapted from Rmd: close the PDF device opened above for the descriptive/audit plots.
# Guarded: an unbalanced dev.off() would error here and skip the copy of
# values.tex into the Overleaf clone just below.
if (dev.cur() > 1) dev.off()

cat("\nWrote LaTeX values to:", VALUES_TEX, "\n")

# Copy straight into the Overleaf project's values/ folder too, so \input{}
# in the paper always picks up this run's numbers without a manual copy step.
overleaf_values_path <- path(OVERLEAF_VALUES_DIR, path_file(VALUES_TEX))
file_copy(VALUES_TEX, overleaf_values_path, overwrite = TRUE)
cat("Copied it into the Overleaf project at", overleaf_values_path, "\n")

# ---------------------------------------------------------------------------
# Adapted from Rmd: generate the synthetic dataset from the same real data
# this run just analyzed, writing it (like values.tex above) directly to
# LOCAL_OUTPUT_DIR — never the SSD target dir — so both privacy-safe
# artifacts land straight in the synced project with no manual
# copy step off the SSD machine. This script does NOT delete anything from
# the SSD-side target directory: real-data intermediates
# (extension_converted.csv, extension_joined.csv, analysis_dataset.csv,
# merged/, the plots PDF) are left in place there for you to keep, inspect,
# or clean up yourself — the privacy boundary this pipeline enforces is
# "never write real data to the synced tree," not "delete real
# data off the SSD you already own."
# ---------------------------------------------------------------------------
source(path(script_dir, "R", "generate_synthetic.R"))
SYNTHETIC_CSV <- path(LOCAL_OUTPUT_DIR, "analysis_dataset_synthetic.csv")
generate_synthetic_dataset(CLEANED_CSV, SYNTHETIC_CSV)

cat("Synthetic dataset and values.tex written to", LOCAL_OUTPUT_DIR, "\n")
