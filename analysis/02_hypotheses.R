# Preregistered tests (H1, H2, H3), the manipulation check and the validity of
# the preference measure. Reads inputs/analysis_sample.csv; writes values only.
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(fixest))
setFixest_notes(FALSE)   # the clutter main effect is absorbed by the subject fixed effect, by design

VALUES <- new_values_file("02_hypotheses")
df <- read_csv(ANALYSIS_SAMPLE, show_col_types = FALSE) %>%
  mutate(participant_code = factor(participant_code), city = factor(city))

# Writes coefficient, SE, p (with the bare-number twin), stars and N for one term.
write_term <- function(label, m, term, p, n = nobs(m)) {
  write_tex_value(paste0("RegCoef", label), coef(m)[[term]], file = VALUES)
  write_tex_value(paste0("RegSE", label), se(m)[[term]], file = VALUES)
  write_pvalue_pair(paste0("RegPval", label), p, file = VALUES)
  write_tex_value(paste0("RegStars", label), stars_from_pvalue(p), file = VALUES)
  write_tex_value(paste0("RegN", label), n, fmt = "%d", file = VALUES)
}
p_one_sided <- function(m, term, direction) {   # direction: +1 tests beta > 0, -1 tests beta < 0
  pnorm(direction * coef(m)[[term]] / se(m)[[term]], lower.tail = FALSE)
}
p_two_sided <- function(m, term) 2 * pnorm(-abs(coef(m)[[term]] / se(m)[[term]]))
se <- function(m) if (inherits(m, "fixest")) fixest::se(m) else summary(m)$coefficients[, "Std. Error"]

# One row per choice set: whether a cue-eligible listing was chosen / clicked, seconds on their pages.
sets <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(chose_cued = any(listing_chosen & would_be_cued, na.rm = TRUE),
            clicked_cued = any(listing_clicked & would_be_cued, na.rm = TRUE),
            log_time_cued = log(1 + sum(time_on_listing_page_seconds[would_be_cued], na.rm = TRUE)),
            cue_visible = first(cued_weekend), clutter_high = first(clutter_high),
            log_decision_time = log(1 + first(decision_time_seconds)),
            page_visits = first(property_page_visits_number), .groups = "drop")

# --- H1: cue effect, subject and city fixed effects, one-sided (beta > 0) ---------
h1 <- list(HOneOne = c("chose_cued", "logit"), HOneTwoOne = c("clicked_cued", "logit"), HOneTwoTwo = c("log_time_cued", "ols"))
fit_fe <- function(fml, family) {
  if (family == "logit") feglm(fml, data = sets, family = "logit") else feols(fml, data = sets)
}
for (lab in names(h1)) {
  m <- fit_fe(as.formula(paste(h1[[lab]][1], "~ cue_visible | participant_code + city")), h1[[lab]][2])
  write_term(lab, m, "cue_visibleTRUE", p_one_sided(m, "cue_visibleTRUE", +1))
}

# --- H2.1: cue x clutter interaction, two-sided; the clutter main effect is absorbed ----
h21 <- list(HTwoOneOne = h1$HOneOne, HTwoOneTwo = h1$HOneTwoOne, HTwoOneThree = h1$HOneTwoTwo)
for (lab in names(h21)) {
  m <- fit_fe(as.formula(paste(h21[[lab]][1], "~ cue_visible * clutter_high | participant_code + city")), h21[[lab]][2])
  write_term(lab, m, "cue_visibleTRUE:clutter_highTRUE", p_two_sided(m, "cue_visibleTRUE:clutter_highTRUE"))
  write_tex_value(paste0("RegCoef", lab, "Main"), coef(m)[["cue_visibleTRUE"]], file = VALUES)
  write_tex_value(paste0("RegSE", lab, "Main"), se(m)[["cue_visibleTRUE"]], file = VALUES)
  write_tex_value(paste0("RegStars", lab, "Main"), stars_from_pvalue(p_two_sided(m, "cue_visibleTRUE")), file = VALUES)
}

# --- H2.2: clutter reduces cue recognition, subject level, one-sided (beta < 0) --------
subjects <- df %>%
  group_by(participant_code) %>%
  summarise(across(c(cue_recognition, clutter_high, visual_complexity, age, noticed_decoy_checkmark, noticed_decoy_badge,
                     nasa_tlx_mental, nasa_tlx_physical, nasa_tlx_temporal, nasa_tlx_performance,
                     nasa_tlx_effort, nasa_tlx_frustration), first), .groups = "drop") %>%
  mutate(across(c(cue_recognition, noticed_decoy_checkmark, noticed_decoy_badge), as.logical))
m <- glm(cue_recognition ~ clutter_high, data = subjects, family = binomial)
write_term("HTwoTwo", m, "clutter_highTRUE", p_one_sided(m, "clutter_highTRUE", -1))
write_tex_value("RegInterceptHTwoTwo", coef(m)[["(Intercept)"]], file = VALUES)
write_tex_value("RegInterceptSEHTwoTwo", se(m)[["(Intercept)"]], file = VALUES)
write_tex_value("PctCueRecognitionHighClutter", format_pct(mean(subjects$cue_recognition[subjects$clutter_high], na.rm = TRUE)), file = VALUES)
write_tex_value("PctCueRecognitionLowClutter", format_pct(mean(subjects$cue_recognition[!subjects$clutter_high], na.rm = TRUE)), file = VALUES)

# --- H2.3: decision mode. City dummies keep an explicit intercept (low clutter, reference city). ----
for (spec in list(c("HTwoThreeOne", "log_decision_time"), c("HTwoThreeTwo", "page_visits"))) {
  m <- feols(as.formula(paste(spec[2], "~ clutter_high + city")), data = sets)
  write_term(spec[1], m, "clutter_highTRUE", p_one_sided(m, "clutter_highTRUE", -1))
  write_tex_value(paste0("RegPvalTwoSided", spec[1]), format_pvalue(p_two_sided(m, "clutter_highTRUE")), file = VALUES)
  write_tex_value(paste0("RegIntercept", spec[1]), coef(m)[["(Intercept)"]], file = VALUES)
  write_tex_value(paste0("RegInterceptSE", spec[1]), se(m)[["(Intercept)"]], file = VALUES)
}
nasa <- c(nasa_tlx_mental = "NasaMental", nasa_tlx_physical = "NasaPhysical", nasa_tlx_temporal = "NasaTemporal",
          nasa_tlx_performance = "NasaPerformance", nasa_tlx_effort = "NasaEffort", nasa_tlx_frustration = "NasaFrustration")
for (v in names(nasa)) {   # one-sided: higher load under clutter (beta > 0)
  m <- feols(as.formula(paste(v, "~ clutter_high")), data = subjects)
  lab <- paste0("HTwoThreeThree", nasa[[v]])
  p <- p_one_sided(m, "clutter_highTRUE", +1)
  write_tex_value(paste0("RegCoef", lab), coef(m)[["clutter_highTRUE"]], fmt = "%.3g", file = VALUES)
  write_tex_value(paste0("RegSE", lab), se(m)[["clutter_highTRUE"]], fmt = "%.3g", file = VALUES)
  write_tex_value(paste0("RegIntercept", lab), coef(m)[["(Intercept)"]], fmt = "%.3g", file = VALUES)
  write_tex_value(paste0("RegInterceptSE", lab), se(m)[["(Intercept)"]], fmt = "%.3g", file = VALUES)
  write_tex_value(paste0("RegPval", lab), format_pvalue(p), file = VALUES)
  write_tex_value(paste0("RegStars", lab), stars_from_pvalue(p), file = VALUES)
  write_tex_value(paste0("RegN", lab), nobs(m), fmt = "%d", file = VALUES)
}

# --- H3.1: preference consistency by clutter, city fixed effects, two-sided ----------
pairs <- df %>%
  group_by(participant_code, city) %>%
  summarise(preference_consistency = as.logical(first(preference_consistency)),
            cued_consistent = as.logical(first(cued_choice_preference_consistency)),
            clutter_high = first(clutter_high), .groups = "drop")
m <- feglm(preference_consistency ~ clutter_high | city, data = pairs, family = "logit")
write_tex_value("RegCoefHThreeOne", coef(m)[["clutter_highTRUE"]], file = VALUES)
write_tex_value("RegPvalHThreeOne", format_pvalue(p_two_sided(m, "clutter_highTRUE")), file = VALUES)
write_tex_value("RegNHThreeOne", nobs(m), fmt = "%d", file = VALUES)

# --- H3.2: cued choices among consistent pairs, against the preregistered thresholds ----
p_cons <- mean(pairs$preference_consistency, na.rm = TRUE)
eligible <- pairs %>% filter(preference_consistency)
n_cued_ok <- sum(eligible$cued_consistent, na.rm = TRUE)
upper <- sqrt(p_cons); lower <- sqrt(p_cons) / sqrt(3)
freq <- n_cued_ok / nrow(eligible)
write_tex_value("PropConsistency", format_pct(p_cons), file = VALUES)
write_tex_value("PropCuedConsistent", format_pct(freq), file = VALUES)
write_tex_value("NEligibleHThreeTwo", nrow(eligible), fmt = "%d", file = VALUES)
write_tex_value("ThresholdUpperHThreeTwo", upper, file = VALUES)
write_tex_value("ThresholdLowerHThreeTwo", lower, file = VALUES)
write_tex_value("PvalBinomUpperHThreeTwo", format_pvalue(binom.test(n_cued_ok, nrow(eligible), p = upper)$p.value), file = VALUES)
write_tex_value("PvalBinomLowerHThreeTwo", format_pvalue(binom.test(n_cued_ok, nrow(eligible), p = lower)$p.value), file = VALUES)
write_tex_value("ConclusionHThreeTwo", if (freq > upper) "Positive welfare effect" else if (freq < lower) "Negative welfare effect" else "Ambiguous", file = VALUES)

# --- Preference consistency above chance: 1/3, then an empirical benchmark from random clusters ----
tt <- t.test(pairs$preference_consistency, mu = 1 / 3, alternative = "greater")
write_tex_value("ConsistencyRate", format_pct(p_cons), file = VALUES)
write_tex_value("NPairsConsistency", sum(!is.na(pairs$preference_consistency)), fmt = "%d", file = VALUES)
write_tex_value("TestStatConsistency", unname(tt$statistic), file = VALUES)
write_tex_value("PvalConsistency", format_pvalue(tt$p.value), file = VALUES)
write_tex_value("StarsConsistency", stars_from_pvalue(tt$p.value), file = VALUES)

set.seed(20260905)
N_DRAWS <- 2000
pool <- df %>% distinct(city, property_slug, cluster) %>% filter(!is.na(cluster))
nocue <- df %>% filter(listing_chosen, !cued_weekend) %>% distinct(participant_code, city, weekend_number_global, property_slug)
rate_with <- function(pool) {
  nocue %>%
    inner_join(pool, by = c("city", "property_slug")) %>%
    group_by(participant_code, city) %>%
    summarise(n = n(), same = n_distinct(cluster) == 1, .groups = "drop") %>%
    filter(n == 2) %>% pull(same) %>% mean()
}
draws <- vapply(seq_len(N_DRAWS), function(i) rate_with(pool %>% group_by(city) %>% mutate(cluster = sample(cluster)) %>% ungroup()), numeric(1))
write_tex_value("RandomClusterConsistencyMean", 100 * mean(draws), fmt = "%.1f", file = VALUES)
write_tex_value("RandomClusterConsistencySD", 100 * sd(draws), fmt = "%.1f", file = VALUES)
write_tex_value("RandomClusterConsistencyQNinetyFive", 100 * quantile(draws, 0.95), fmt = "%.1f", file = VALUES)
write_tex_value("PvalRandomClusterConsistency", format_pvalue(mean(draws >= rate_with(pool))), file = VALUES)
write_tex_value("NRandomClusterDraws", N_DRAWS, fmt = "%d", file = VALUES)

# --- Manipulation check: perceived visual complexity by clutter, and by age / decision-time subgroup ----
tt <- t.test(visual_complexity ~ clutter_high, data = subjects)
write_tex_value("MeanVisualComplexityHighClutter", mean(subjects$visual_complexity[subjects$clutter_high], na.rm = TRUE), fmt = "%.2f", file = VALUES)
write_tex_value("MeanVisualComplexityLowClutter", mean(subjects$visual_complexity[!subjects$clutter_high], na.rm = TRUE), fmt = "%.2f", file = VALUES)
write_tex_value("TestStatManip", unname(tt$statistic), file = VALUES)
write_tex_value("PvalManip", format_pvalue(tt$p.value), file = VALUES)

median_dtime <- df %>%
  filter(!is.na(decision_time_seconds), decision_time_seconds > 0) %>%
  distinct(participant_code, weekend_number_global, decision_time_seconds) %>%
  group_by(participant_code) %>% summarise(median_dtime = median(decision_time_seconds), .groups = "drop")
het <- subjects %>% left_join(median_dtime, by = "participant_code")
age_cut <- median(het$age, na.rm = TRUE)
groups <- bind_rows(
  het %>% filter(!is.na(age)) %>% mutate(key = ifelse(age <= age_cut, "AgeYoung", "AgeOld")),
  het %>% filter(!is.na(median_dtime)) %>% mutate(key = paste0("DtimeQ", c("One", "Two", "Three", "Four")[ntile(median_dtime, 4)])))
for (k in unique(groups$key)) {
  g <- groups %>% filter(key == k)
  write_tex_value(paste0("ManipHet", k, "High"), mean(g$visual_complexity[g$clutter_high], na.rm = TRUE), fmt = "%.2f", file = VALUES)
  write_tex_value(paste0("ManipHet", k, "Low"), mean(g$visual_complexity[!g$clutter_high], na.rm = TRUE), fmt = "%.2f", file = VALUES)
  write_tex_value(paste0("ManipHet", k, "N"), nrow(g), fmt = "%d", file = VALUES)
  write_pvalue_pair(paste0("ManipHet", k, "Pval"), tryCatch(t.test(visual_complexity ~ clutter_high, data = g)$p.value, error = function(e) NA_real_), file = VALUES)
}
write_tex_value("ManipHetAgeCut", age_cut, fmt = "%d", file = VALUES)

# --- Badge recognition and the two decoy icons ----------------------------------------
any_decoy <- subjects$noticed_decoy_checkmark | subjects$noticed_decoy_badge
correct <- subjects$cue_recognition & !any_decoy
write_tex_value("NSubjectsNoticedThumb", sum(subjects$cue_recognition, na.rm = TRUE), fmt = "%d", file = VALUES)
write_tex_value("NSubjectsNoticedAnyDecoy", sum(any_decoy, na.rm = TRUE), fmt = "%d", file = VALUES)
write_tex_value("NSubjectsNoticedBothDecoys", sum(subjects$noticed_decoy_checkmark & subjects$noticed_decoy_badge, na.rm = TRUE), fmt = "%d", file = VALUES)
write_tex_value("NSubjectsNoticedThumbNoDecoy", sum(correct, na.rm = TRUE), fmt = "%d", file = VALUES)
write_tex_value("PctRecognitionCorrect", format_pct(mean(correct, na.rm = TRUE)), file = VALUES)
write_tex_value("PctDecoyAnyHighClutter", format_pct(mean(any_decoy[subjects$clutter_high], na.rm = TRUE)), file = VALUES)
write_tex_value("PctDecoyAnyLowClutter", format_pct(mean(any_decoy[!subjects$clutter_high], na.rm = TRUE)), file = VALUES)
write_pvalue_pair("PvalDecoyAnyClutter", fisher.test(table(subjects$clutter_high, any_decoy))$p.value, file = VALUES)
