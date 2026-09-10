# Preregistered exclusions and sample descriptives.
# Reads inputs/analysis_dataset.csv; writes inputs/analysis_sample.csv (the
# sample every later script uses) and outputs/values/values_01_sample.tex.
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))

VALUES <- new_values_file("01_sample")
df_all <- read_csv(ANALYSIS_DATASET, show_col_types = FALSE)

# (1) Subjects who did not complete the twelve choices are dropped entirely.
n_choices <- df_all %>%
  group_by(participant_code) %>%
  summarise(n = n_distinct(weekend_number_global[listing_chosen]), .groups = "drop")
excluded <- n_choices$participant_code[n_choices$n < 12]
df <- df_all %>% filter(!participant_code %in% excluded)

# (2) A subject x city whose choice set had fewer than nine listings in any weekend is dropped.
reduced <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(n_listings = first(n_hotels_in_choice_set), .groups = "drop") %>%
  filter(n_listings < 9) %>%
  distinct(participant_code, city)
df <- df %>% anti_join(reduced, by = c("participant_code", "city"))
write_csv(df, ANALYSIS_SAMPLE)

write_tex_value("NSubjectsTotal", nrow(n_choices), fmt = "%d", file = VALUES)
write_tex_value("NSubjectsExcluded", length(excluded), fmt = "%d", file = VALUES)
write_tex_value("NSubjectsRetained", n_distinct(df$participant_code), fmt = "%d", file = VALUES)
write_tex_value("NChoiceSetsRetained", nrow(distinct(df, participant_code, weekend_number_global)), fmt = "%d", file = VALUES)

# Pretask: each subject dropped one of the four candidate cities.
city_labels <- c("arcachon" = "Arcachon", "la-ciotat" = "LaCiotat", "le-treport" = "LeTreport", "sete" = "Sete")
cities_by_subject <- df_all %>% distinct(participant_code, city)
n_subjects <- n_distinct(cities_by_subject$participant_code)
for (ct in names(city_labels)) {
  n_excl <- n_subjects - sum(cities_by_subject$city == ct)
  write_tex_value(paste0("PctCityExcluded", city_labels[[ct]]), format_pct(n_excl / n_subjects), file = VALUES)
}

# Loading time and substitute use describe what the extension delivered, before exclusions.
by_weekend <- df_all %>%
  group_by(participant_code, weekend_number_global) %>%
  summarise(loading = first(loading_time_seconds),
            n_substitutes = sum(as.logical(is_substitute_listing), na.rm = TRUE), .groups = "drop")
write_tex_value("MeanLoadingTime", mean(by_weekend$loading, na.rm = TRUE), fmt = "%.2f", file = VALUES)
write_tex_value("MedianLoadingTime", median(by_weekend$loading, na.rm = TRUE), fmt = "%.2f", file = VALUES)
write_tex_value("SDLoadingTime", sd(by_weekend$loading, na.rm = TRUE), fmt = "%.2f", file = VALUES)
write_tex_value("NObsLoadingTime", nrow(by_weekend), fmt = "%d", file = VALUES)
write_tex_value("NChoiceSetsSubstitutesTotal", nrow(by_weekend), fmt = "%d", file = VALUES)
write_tex_value("NChoiceSetsSubstitutesOne", sum(by_weekend$n_substitutes == 1), fmt = "%d", file = VALUES)
write_tex_value("NChoiceSetsSubstitutesTwo", sum(by_weekend$n_substitutes == 2), fmt = "%d", file = VALUES)
write_tex_value("NChoiceSetsSubstitutesThreeOrMore", sum(by_weekend$n_substitutes >= 3), fmt = "%d", file = VALUES)
write_tex_value("MaxSubstitutesPerChoiceSet", max(by_weekend$n_substitutes), fmt = "%d", file = VALUES)

# Session duration, on the analysis sample.
sessions <- df %>% distinct(participant_code, whole_experiment_time_seconds, choice_task_time_seconds)
for (v in c(WholeExperiment = "whole_experiment_time_seconds", ChoiceTask = "choice_task_time_seconds")) {
  lab <- names(which(c(WholeExperiment = "whole_experiment_time_seconds", ChoiceTask = "choice_task_time_seconds") == v))
  x <- sessions[[v]] / 60
  write_tex_value(paste0("Mean", lab, "TimeMinutes"), mean(x, na.rm = TRUE), fmt = "%.1f", file = VALUES)
  write_tex_value(paste0("Median", lab, "TimeMinutes"), median(x, na.rm = TRUE), fmt = "%.1f", file = VALUES)
  write_tex_value(paste0("SD", lab, "TimeMinutes"), sd(x, na.rm = TRUE), fmt = "%.1f", file = VALUES)
}

# Demographics and comprehension checks, on all subjects.
subjects <- df_all %>%
  group_by(participant_code) %>%
  summarise(across(c(gender, age, student_status, household_structure, paris_resident, booking_familiarity,
                     failed_comprehension_prize, failed_comprehension_choice_city_weekend,
                     failed_comprehension_no_cancellation), first), .groups = "drop")
write_tex_value("MeanAge", mean(subjects$age, na.rm = TRUE), fmt = "%.1f", file = VALUES)
write_tex_value("MedianAge", median(subjects$age, na.rm = TRUE), fmt = "%.1f", file = VALUES)
write_tex_value("SDAge", sd(subjects$age, na.rm = TRUE), fmt = "%.1f", file = VALUES)
write_tex_value("MeanBookingFamiliarity", mean(subjects$booking_familiarity, na.rm = TRUE), fmt = "%.2f", file = VALUES)
write_tex_value("SDBookingFamiliarity", sd(subjects$booking_familiarity, na.rm = TRUE), fmt = "%.2f", file = VALUES)
write_tex_value("PctParisResident", format_pct(mean(as.logical(subjects$paris_resident), na.rm = TRUE)), file = VALUES)
level_labels <- list(
  Gender = c(col = "gender", "Homme" = "Male", "Femme" = "Female", "Autre" = "Other"),
  StudentStatus = c(col = "student_status", "Étudiant" = "Student", "Non étudiant" = "NonStudent"))
for (field in names(level_labels)) {
  lookup <- level_labels[[field]]
  tab <- table(subjects[[lookup[["col"]]]])
  for (level in names(tab)) {
    lab <- if (level %in% names(lookup)) lookup[[level]] else gsub("[^A-Za-z]", "", level)
    write_tex_value(paste0("Pct", field, lab), format_pct(tab[[level]] / sum(tab)), file = VALUES)
  }
}
checks <- c(failed_comprehension_prize = "Prize", failed_comprehension_choice_city_weekend = "ChoiceCityWeekend",
            failed_comprehension_no_cancellation = "NoCancellation")
for (col in names(checks)) {
  write_tex_value(paste0("PctFailedComprehension", checks[[col]]),
                  format_pct(mean(as.logical(subjects[[col]]), na.rm = TRUE)), file = VALUES)
}
any_failed <- rowSums(sapply(names(checks), function(c) as.logical(subjects[[c]])), na.rm = TRUE) > 0
write_tex_value("PctFailedComprehensionAny", format_pct(mean(any_failed)), file = VALUES)
