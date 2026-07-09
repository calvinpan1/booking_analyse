# AUTHOR: Calvin Pan
# DATE CREATED: 9 July 2026
# PURPOSE: Property-pool-level cluster randomization test — a follow-up to the
#   pre-registered preference-consistency manipulation check (see
#   01_analysis.rmd's "Preference consistency" validity check), NOT part of
#   the pre-registered analysis plan itself.
#
#   Question: clusters are built to group similar properties (K-means on
#   normalized price/review/lat/lng, refined by an MILP — cf.
#   bookiing_scraping/cluster_builder/app.R's create_clusters()), and Booking's
#   search results are sorted by price (&order=price). So a listing's cluster
#   is structurally correlated with its typical on-page position — meaning
#   observed "preference consistency" above the theoretical 1/3 chance level
#   could reflect ordinary position/price bias riding along on the real
#   clusters, not necessarily genuine attribute-based taste. This script asks:
#   if cluster assignment carried NO information (a uniformly random relabeling
#   of the SAME property pool, unrelated to price/location/rating), would
#   consistency still exceed 1/3?
#
#   Randomization is done at the property-POOL level within each city (i.e.
#   which property gets which cluster label is reshuffled, preserving each
#   city's real cluster-size distribution) and held FIXED across a subject's
#   weekends for a given draw — NOT re-rolled independently per weekend (that
#   would force exactly 1/3 by construction and test nothing) and NOT a
#   trivial renaming of the 3 existing groups (that reproduces the real rate
#   exactly, since group membership is unchanged).
# OUTPUTS: cluster_randomization_histogram.png (in outputs/) — distribution of
#   the 500 randomized-cluster consistency rates, vs. the real observed rate
#   and the theoretical 1/3 benchmark.

library(readr)
library(dplyr)
library(purrr)
library(ggplot2)

# ---------------------------------------------------------------------------
# Locate analysis_dataset.csv. Unlike 01_analysis.rmd (processed by knitr,
# where sys.frames()[[1]]$ofile resolves to the .Rmd being knit), this is a
# plain script run via `Rscript` — sys.frames()[[1]]$ofile is NULL there, so
# that fallback silently resolves to the working directory instead of this
# file's own path. Use Rscript's --file= command-line argument instead, with
# rstudioapi for interactive/sourced use.
# ---------------------------------------------------------------------------
script_path <- if (requireNamespace("rstudioapi", quietly = TRUE) &&
                   rstudioapi::isAvailable()) {
  rstudioapi::getActiveDocumentContext()$path
} else {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 0) {
    stop("Could not determine this script's own path (not run via Rscript --file= ",
         "and no active RStudio document). Run with: Rscript 01c_cluster_randomization.R")
  }
  normalizePath(sub("^--file=", "", file_arg[1]))
}

script_dir  <- dirname(dirname(script_path))  # 01_analysis/ -> booking_analyse/
OUTPUT_DIR  <- file.path(script_dir, "outputs")
CLEANED_CSV <- file.path(OUTPUT_DIR, "analysis_dataset.csv")

set.seed(42)  # reproducible draws
N_RANDOMIZATIONS <- 5000

df <- read_csv(CLEANED_CSV, show_col_types = FALSE)

# ---------------------------------------------------------------------------
# Property pool: one real cluster label per (city, property_slug). Verifies
# cluster is indeed a stable per-property attribute (never varies for the same
# property within a city) before using it as the basis for randomization.
# ---------------------------------------------------------------------------
property_pool <- df %>%
  distinct(city, property_slug, cluster) %>%
  filter(!is.na(cluster))

n_inconsistent <- property_pool %>%
  count(city, property_slug) %>%
  filter(n > 1) %>%
  nrow()
if (n_inconsistent > 0) {
  stop(n_inconsistent, " (city, property_slug) pair(s) have more than one cluster ",
       "value — cluster is not the stable per-property attribute this script assumes.")
}

cat("Property pool:", nrow(property_pool), "distinct (city, property) pairs across",
    n_distinct(property_pool$city), "cities.\n\n")

# ---------------------------------------------------------------------------
# Non-cued chosen listings per (participant_code, city, weekend) — same
# restriction as 00c's preference_consistency (built on cell_thumb == "A"
# weekends only, i.e. cued_weekend == FALSE).
# ---------------------------------------------------------------------------
non_cued_choices <- df %>%
  filter(listing_chosen, !cued_weekend) %>%
  distinct(participant_code, city, weekend_number_global, property_slug)

# ---------------------------------------------------------------------------
# Real observed consistency rate — recomputed directly here (property_slug +
# real cluster) as a same-data comparison baseline for the randomized rates.
# ---------------------------------------------------------------------------
consistency_from_pool <- function(pool, cluster_col) {
  cluster_col <- rlang::sym(cluster_col)
  non_cued_choices %>%
    left_join(pool, by = c("city", "property_slug")) %>%
    filter(!is.na(!!cluster_col)) %>%
    group_by(participant_code, city) %>%
    summarise(n_choices = n(), same_cluster = n_distinct(!!cluster_col) == 1, .groups = "drop") %>%
    filter(n_choices == 2)
}

real_consistency <- consistency_from_pool(property_pool, "cluster")
real_rate <- mean(real_consistency$same_cluster)

cat("Real observed preference-consistency rate (this script's own recomputation):\n  ",
    round(real_rate, 4), "across", nrow(real_consistency), "subject x city pairs.\n\n")

# ---------------------------------------------------------------------------
# One randomization draw: shuffle cluster labels across properties WITHIN each
# city (preserves each city's real cluster-size distribution), held fixed for
# every subject/weekend in that city for this draw, then recompute the same
# consistency statistic using the shuffled labels.
# ---------------------------------------------------------------------------
one_randomization <- function() {
  fake_pool <- property_pool %>%
    group_by(city) %>%
    mutate(fake_cluster = sample(cluster)) %>%
    ungroup()

  fake_consistency <- consistency_from_pool(fake_pool, "fake_cluster")
  mean(fake_consistency$same_cluster)
}

cat("Running", N_RANDOMIZATIONS, "property-pool-level randomizations...\n")
randomized_rates <- map_dbl(seq_len(N_RANDOMIZATIONS), function(i) one_randomization())

cat("\nRandomized-cluster consistency rate across", N_RANDOMIZATIONS, "draws:\n")
cat("  Mean:  ", round(mean(randomized_rates), 4), "\n")
cat("  SD:    ", round(sd(randomized_rates), 4), "\n")
cat("  Range: [", round(min(randomized_rates), 4), ",", round(max(randomized_rates), 4), "]\n\n")

# Empirical permutation-test p-value: share of randomized draws at least as
# extreme as the real rate (one-sided — the manipulation check predicts real
# consistency ABOVE chance).
p_empirical <- mean(randomized_rates >= real_rate)

cat("Comparison:\n")
cat("  Theoretical chance level (1/3):                 ", round(1/3, 4), "\n")
cat("  Mean randomized-cluster rate (n =", N_RANDOMIZATIONS, "):        ", round(mean(randomized_rates), 4), "\n")
cat("  Real observed rate:                              ", round(real_rate, 4), "\n")
cat("  Empirical one-sided p-value (P(randomized >= real)):", round(p_empirical, 4), "\n\n")

# ---------------------------------------------------------------------------
# Histogram: distribution of randomized-cluster rates vs. the real rate and
# the theoretical 1/3 benchmark.
# ---------------------------------------------------------------------------
plot_df <- data.frame(rate = randomized_rates)

p <- ggplot(plot_df, aes(x = rate)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "white") +
  geom_vline(aes(xintercept = real_rate, color = "Real observed rate"), linewidth = 1) +
  geom_vline(aes(xintercept = 1/3, color = "Theoretical chance (1/3)"),
             linewidth = 1, linetype = "dashed") +
  scale_color_manual(name = NULL, values = c(
    "Real observed rate" = "firebrick",
    "Theoretical chance (1/3)" = "black"
  )) +
  labs(
    title = paste0("Preference consistency under ", N_RANDOMIZATIONS,
                    " property-pool-level cluster randomizations"),
    x = "Consistency rate (both non-cued choices in the same [fake] cluster)",
    y = "Number of randomization draws"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

OUTPUT_DIR_created <- dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(OUTPUT_DIR, "cluster_randomization_histogram.png")
ggsave(out_path, p, width = 8, height = 5.5, dpi = 150)
cat("Saved histogram ->", out_path, "\n")
