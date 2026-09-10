# =============================================================================
# Booking in the dark -- replication package
# =============================================================================
#   Rscript main.R            everything: build the analysis dataset, then analyse
#   Rscript main.R build      stage 1 only (raw/ -> inputs/)
#   Rscript main.R analyse    stage 2 only (inputs/ -> outputs/)
#   Rscript main.R <script>   one script, e.g. analysis/02_hypotheses.R
# Sourcing this file sets the paths and helpers without running anything.
# This is the only file that knows where anything lives.
# =============================================================================

# --- Settings ----------------------------------------------------------------
# Defaults are relative to this file, so the package runs with no configuration
# once raw/ holds the participant folders, the oTree exports and the choice-set file.
RAW_DIR    <- Sys.getenv("BOOKING_RAW",     unset = "raw")
INPUTS_DIR <- Sys.getenv("BOOKING_INPUTS",  unset = "inputs")   # intermediate, participant-level files
OUTPUT_DIR <- Sys.getenv("BOOKING_OUTPUTS", unset = "outputs")  # values and figures for the paper
PAPER_DIR  <- Sys.getenv("BOOKING_PAPER",   unset = "")         # Overleaf clone; "" = do not copy
PYTHON     <- Sys.getenv("BOOKING_PYTHON",  unset = if (.Platform$OS.type == "windows") "python" else "python3")

# --- INTERIM: delete this block when the package moves to the SSD -----------
# While the package sits in the synced project folder, raw and intermediate
# participant-level files stay on the SSD; only outputs/ is written here.
.ssd <- Sys.getenv("SSD_DATA_ROOT", unset = "")
if (nzchar(.ssd)) {
  if (!nzchar(Sys.getenv("BOOKING_RAW")))    RAW_DIR    <- file.path(.ssd, "Booking_in_the_dark", "raw")
  if (!nzchar(Sys.getenv("BOOKING_INPUTS"))) INPUTS_DIR <- file.path(.ssd, "Booking_in_the_dark", "inputs")
}
# --- end interim -------------------------------------------------------------

# --- Locate this file; everything below is machine-independent --------------
.args_file <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
.invoked_directly <- length(.args_file) == 1 && basename(.args_file) == "main.R"
.this_file <- if (.invoked_directly) .args_file else NULL
if (is.null(.this_file)) for (.i in seq_len(sys.nframe())) {
  .of <- sys.frame(.i)$ofile
  if (!is.null(.of)) { .this_file <- .of; break }
}
if (!is.null(.this_file)) setwd(dirname(normalizePath(.this_file)))
CODE_DIR <- getwd()

for (.d in c("INPUTS_DIR", "OUTPUT_DIR")) dir.create(get(.d), showWarnings = FALSE, recursive = TRUE)
RAW_DIR    <- normalizePath(RAW_DIR, mustWork = FALSE)
INPUTS_DIR <- normalizePath(INPUTS_DIR)
OUTPUT_DIR <- normalizePath(OUTPUT_DIR)
VALUES_DIR  <- file.path(OUTPUT_DIR, "values")
FIGURES_DIR <- file.path(OUTPUT_DIR, "figures")
dir.create(VALUES_DIR, showWarnings = FALSE); dir.create(FIGURES_DIR, showWarnings = FALSE)

# The choice-set file is not participant data: it ships with the package.
CHOICE_SETS_JSON <- file.path(RAW_DIR, "choice_sets_with_substitutes.json")
if (!file.exists(CHOICE_SETS_JSON)) CHOICE_SETS_JSON <- file.path(CODE_DIR, "raw", "choice_sets_with_substitutes.json")
ANALYSIS_DATASET <- file.path(INPUTS_DIR, "analysis_dataset.csv")   # built by stage 1
ANALYSIS_SAMPLE  <- file.path(INPUTS_DIR, "analysis_sample.csv")    # after the preregistered exclusions
VALUES_TEX       <- file.path(OUTPUT_DIR, "values_BookingAnalysis.tex")

source(file.path(CODE_DIR, "analysis", "tex_values.R"), encoding = "UTF-8")
source(file.path(CODE_DIR, "analysis", "open_text_coding.R"), encoding = "UTF-8")

# Each analysis script writes its own values file; they are concatenated after every run.
new_values_file <- function(name) {
  f <- file.path(VALUES_DIR, paste0("values_", name, ".tex"))
  file.create(f)
  f
}
assemble_values <- function() {
  parts <- sort(list.files(VALUES_DIR, pattern = "^values_.*\\.tex$", full.names = TRUE))
  con <- file(VALUES_TEX, open = "wb"); on.exit(close(con))
  for (p in parts) writeLines(readLines(p, encoding = "UTF-8"), con, sep = "\n")
}

# --- Copying outputs into the paper -------------------------------------------
.paper_subdir <- function(filename) {
  if (filename == basename(VALUES_TEX)) "values"
  else if (grepl("^cue_.*\\.png$", filename)) file.path("illustrations", "cue_probability")
  else if (grepl("^h3_cluster_corr_triangular\\.png$", filename)) file.path("illustrations", "h3_cluster_corr_triangular")
  else NA_character_
}
copy_to_paper <- function() {
  if (!nzchar(PAPER_DIR)) return(invisible(NULL))
  for (f in c(VALUES_TEX, list.files(FIGURES_DIR, full.names = TRUE))) {
    sub <- .paper_subdir(basename(f))
    if (is.na(sub)) next
    dest <- file.path(PAPER_DIR, sub)
    dir.create(dest, showWarnings = FALSE, recursive = TRUE)
    file.copy(f, file.path(dest, basename(f)), overwrite = TRUE)
  }
}

# --- Running one script ---------------------------------------------------------
# R scripts run in their own environment and communicate through files only.
# A failure is reported and the run continues; failures are listed at the end.
.failures <- character(0)
run <- function(script, args = character(0)) {
  label <- basename(script)
  message("=== ", label)
  t0 <- Sys.time()
  err <- tryCatch({
    if (grepl("\\.py$", script)) {
      status <- system2(PYTHON, c(shQuote(file.path(CODE_DIR, script)), shQuote(args)))
      if (status != 0) stop("exit status ", status)
    } else {
      source(file.path(CODE_DIR, script), local = new.env(), encoding = "UTF-8")
    }
    NULL
  }, error = function(e) conditionMessage(e))
  while (sink.number() > 0) sink()
  if (grepl("^analysis/", script)) { assemble_values(); copy_to_paper() }
  if (is.null(err)) {
    message(sprintf("    done (%.0f s)", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  } else {
    message("!!! FAILED: ", label, "\n    ", gsub("\n", "\n    ", err))
    .failures <<- c(.failures, label)
  }
}

# --- The pipeline: stage 1 builds inputs/ from raw/, stage 2 builds outputs/ from inputs/ ----
PIPELINE <- list(
  build = list(
    "build/01_convert_tracking.py"       = c(RAW_DIR, INPUTS_DIR),
    "build/02_join_choice_sets.py"       = c(INPUTS_DIR, CHOICE_SETS_JSON),
    "build/03_build_analysis_dataset.py" = c(RAW_DIR, INPUTS_DIR, CHOICE_SETS_JSON),
    "build/04_tracking_measures.R"       = character(0)),
  analyse = list(
    "analysis/01_sample.R"      = character(0),
    "analysis/02_hypotheses.R"  = character(0),
    "analysis/03_figures.R"     = character(0),
    "analysis/04_exploratory.R" = character(0)))
run_stage <- function(stage) for (s in names(PIPELINE[[stage]])) run(s, PIPELINE[[stage]][[s]])
build   <- function() run_stage("build")
analyse <- function() run_stage("analyse")

if (.invoked_directly) {
  stage <- commandArgs(trailingOnly = TRUE)
  stage <- if (length(stage) == 0) "all" else stage[1]
  scripts <- c(PIPELINE$build, PIPELINE$analyse)
  if (stage %in% names(scripts)) {
    run(stage, scripts[[stage]])
  } else {
    switch(stage,
      all = { build(); analyse() },
      build = build(),
      analyse = analyse(),
      stop("Unknown argument '", stage, "'. Use: all, build, analyse, or a script path such as analysis/02_hypotheses.R"))
  }
  if (length(.failures) == 0) {
    message("=== finished, no failures")
  } else {
    message("=== finished with ", length(.failures), " failed script(s): ", paste(.failures, collapse = ", "))
    quit(status = 1)
  }
}
