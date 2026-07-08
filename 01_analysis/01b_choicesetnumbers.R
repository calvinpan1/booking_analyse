# Choice set size check
# Author: Calvin Pan
#
# For each participant x weekend, how many distinct hotels actually showed up
# in the choice set (n_hotels_in_choice_set, built in 00c from the extension's
# tracked `displayed` events)? Every weekend should show all 9 listings from
# the choice set — this script checks how often that held, with a histogram
# of the distribution and the % of weekends that hit exactly 9.

library(fs)
library(readr)
library(dplyr)
library(ggplot2)

`%||%` <- function(x, y) if (is.null(x)) y else x

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

script_path <- if (requireNamespace("rstudioapi", quietly = TRUE) &&
                    rstudioapi::isAvailable()) {
  rstudioapi::getActiveDocumentContext()$path
} else if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg))
} else {
  normalizePath(".")
}

script_dir <- path_dir(path_dir(script_path)) # project root (parent of 01_analysis)

OUTPUT_DIR <- path(script_dir, "outputs")
CLEANED_CSV <- path(OUTPUT_DIR, "analysis_dataset.csv")

df <- read_csv(CLEANED_CSV, show_col_types = FALSE)

# n_hotels_in_choice_set is a weekend-level constant repeated on every one of
# the weekend's listing-rows — collapse to one row per participant x weekend
# before summarising/plotting, otherwise each weekend gets counted 9x over.
choice_set_sizes <- df %>%
  group_by(participant_code, weekend_number_global) %>%
  summarise(n_hotels = first(n_hotels_in_choice_set), .groups = "drop")

cat("Participant x weekend observations:", nrow(choice_set_sizes), "\n\n")

cat("Distribution of number of hotels shown per weekend:\n")
print(table(choice_set_sizes$n_hotels))

n_total   <- nrow(choice_set_sizes)
n_nine    <- sum(choice_set_sizes$n_hotels == 9, na.rm = TRUE)
pct_nine  <- 100 * n_nine / n_total

cat("\n", n_nine, "of", n_total, "weekends (", round(pct_nine, 1),
    "%) showed exactly 9 hotels.\n")

ggplot(choice_set_sizes, aes(x = n_hotels)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "Number of hotels shown per participant x weekend",
    subtitle = paste0(round(pct_nine, 1), "% of weekends showed all 9 hotels"),
    x = "Number of distinct hotels in choice set",
    y = "Number of participant x weekend observations"
  ) +
  theme_minimal()

ggsave(path(OUTPUT_DIR, "choice_set_size_histogram.png"), width = 7, height = 5)
