# Analyses not in the preregistration: belief about the badge, decision time
# over the session, tracked interface use by clutter, coded open answers.
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))

VALUES <- new_values_file("04_exploratory")
df <- read_csv(ANALYSIS_SAMPLE, show_col_types = FALSE)
tracking <- read_csv(file.path(INPUTS_DIR, "subject_tracking.csv"), show_col_types = FALSE)
subjects <- df %>%
  group_by(participant_code) %>%
  summarise(across(c(clutter_high, cue_recognition, belief_thumb_quality, booking_familiarity,
                     noticed_decoy_checkmark, noticed_decoy_badge, choice_process_open,
                     belief_thumb_meaning_open, thumb_use_open, feedback_open), first), .groups = "drop") %>%
  mutate(across(c(cue_recognition, noticed_decoy_checkmark, noticed_decoy_badge), as.logical),
         recognition_correct = cue_recognition & !(noticed_decoy_checkmark | noticed_decoy_badge)) %>%
  left_join(tracking, by = "participant_code")

# --- Individual cue effect (share of cue-eligible choices, visible minus hidden) and the belief item ----
cue_effect <- df %>%
  group_by(participant_code, weekend_number_global) %>%
  summarise(chose_cued = any(listing_chosen & would_be_cued, na.rm = TRUE), cue_visible = first(cued_weekend), .groups = "drop") %>%
  group_by(participant_code) %>%
  summarise(cue_effect = mean(chose_cued[cue_visible]) - mean(chose_cued[!cue_visible]), .groups = "drop")
subjects <- subjects %>% left_join(cue_effect, by = "participant_code")
belief <- subjects %>% filter(!is.na(belief_thumb_quality), !is.na(cue_effect))
familiar <- belief %>% filter(booking_familiarity == 4)
for (s in list(list(d = belief, sfx = ""), list(d = familiar, sfx = "Familiar"))) {
  ct <- suppressWarnings(cor.test(s$d$belief_thumb_quality, s$d$cue_effect, method = "spearman"))
  write_tex_value(paste0("CorrSpearmanBeliefCueEffect", s$sfx), unname(ct$estimate), file = VALUES)
  write_tex_value(paste0("PvalSpearmanBeliefCueEffect", s$sfx), format_pvalue(ct$p.value), file = VALUES)
  write_tex_value(paste0("NObsBeliefCueEffect", s$sfx), nrow(s$d), fmt = "%d", file = VALUES)
  for (b in 1:4) {
    v <- s$d$cue_effect[s$d$belief_thumb_quality == b]
    lab <- c("One", "Two", "Three", "Four")[b]
    write_tex_value(paste0("MeanCueEffectBelief", lab, s$sfx), if (length(v)) mean(v) else NA, fmt = "%.3f", file = VALUES)
    write_tex_value(paste0("NCueEffectBelief", lab, s$sfx), length(v), fmt = "%d", file = VALUES)
  }
}

# --- Median decision time by block of four choices (the three cities in presentation order) ----
blocks <- df %>%
  distinct(participant_code, weekend_number_global, decision_time_seconds) %>%
  filter(!is.na(decision_time_seconds), decision_time_seconds > 0) %>%
  group_by(block = ceiling(weekend_number_global / 4)) %>%
  summarise(med = median(decision_time_seconds), n = n(), .groups = "drop")
for (i in seq_len(nrow(blocks))) {
  lab <- c("One", "Two", "Three")[blocks$block[i]]
  write_tex_value(paste0("MedianDecisionTimeBlock", lab), blocks$med[i], fmt = "%.0f", file = VALUES)
  write_tex_value(paste0("NObsDecisionTimeBlock", lab), blocks$n[i], fmt = "%d", file = VALUES)
}

# --- Tracked interface use by clutter: Welch t-tests on per-subject means ------------------
clutter_contrast <- function(label, col) {
  d <- subjects %>% filter(!is.na(.data[[col]]), !is.na(clutter_high))
  tt <- t.test(d[[col]][d$clutter_high], d[[col]][!d$clutter_high])
  write_tex_value(paste0("Mean", label, "HighClutter"), mean(d[[col]][d$clutter_high]), fmt = "%.3g", file = VALUES)
  write_tex_value(paste0("Mean", label, "LowClutter"), mean(d[[col]][!d$clutter_high]), fmt = "%.3g", file = VALUES)
  write_tex_value(paste0("Pval", label), format_pvalue(tt$p.value), file = VALUES)
  write_tex_value(paste0("Stars", label), stars_from_pvalue(tt$p.value), file = VALUES)
}
clutter_contrast("HoverDwell", "mean_hover_dwell_s")
clutter_contrast("MaxScrollDepth", "mean_max_scroll_depth")
clutter_contrast("ViewportDwell", "mean_total_viewport_s")
clutter_contrast("ViewportLongDwell", "share_listings_10s")
write_tex_value("PctViewportLongDwell",
                format_pct(sum(subjects$share_listings_10s / 100 * subjects$n_listings_viewport, na.rm = TRUE) /
                             sum(subjects$n_listings_viewport, na.rm = TRUE)), file = VALUES)
write_tex_value("NObsViewportLongDwellListings", sum(subjects$n_listings_viewport, na.rm = TRUE), fmt = "%d", file = VALUES)
write_tex_value("NSubjectsTooltipOpened", sum(subjects$tooltip_opened), fmt = "%d", file = VALUES)
write_tex_value("NSubjectsTooltipOpenedHighClutter", sum(subjects$tooltip_opened & subjects$clutter_high), fmt = "%d", file = VALUES)
write_tex_value("NSubjectsTooltipOpenedLowClutter", sum(subjects$tooltip_opened & !subjects$clutter_high), fmt = "%d", file = VALUES)

# --- Open answers: keyword coding, shares, and trait contrasts by category -----------------
coded <- code_open_text(subjects)
write_tex_value("NOpenText", nrow(coded), fmt = "%d", file = VALUES)
flags <- c(MeaningReviews = "meaning_reviews", MeaningPlatform = "meaning_platform", MeaningPaid = "meaning_paid",
           MeaningGeneric = "meaning_generic", MeaningValue = "meaning_value", MeaningDontKnow = "meaning_dontknow",
           MeaningOther = "meaning_other", UseNotNoticed = "use_not_noticed", UseNoticedNotUsed = "use_noticed_not_used",
           UseBareNo = "use_bare_no", UseAsSignal = "use_as_signal", UseToClick = "use_to_click", UseUsed = "use_used",
           UseNotUnderstood = "use_not_understood", UseDistrust = "use_distrust", UseNotUsedAny = "use_not_used_any",
           ChoiceLocation = "choice_location", ChoicePrice = "choice_price", ChoiceRating = "choice_rating",
           ChoicePhotos = "choice_photos", ChoiceAmenities = "choice_amenities", ChoiceBadge = "choice_badge",
           FeedbackSlowOrLong = "feedback_slow_or_long", FeedbackPositive = "feedback_positive")
for (lab in names(flags)) {
  write_tex_value(paste0("NOpen", lab), sum(coded[[flags[[lab]]]]), fmt = "%d", file = VALUES)
  write_tex_value(paste0("PctOpen", lab), format_pct(mean(coded[[flags[[lab]]]]), 0), file = VALUES)
}
write_tex_value("PctOpenMeaningReviewsAmongUsed", format_pct(mean(coded$meaning_reviews[coded$use_used]), 0), file = VALUES)
openers <- coded %>% filter(tooltip_opened)
write_tex_value("NTooltipOpenersCoded", nrow(openers), fmt = "%d", file = VALUES)
for (lab in c("MeaningPaid", "MeaningPlatform", "MeaningReviews", "MeaningGeneric")) {
  write_tex_value(paste0("NTooltipOpeners", lab), sum(openers[[flags[[lab]]]]), fmt = "%d", file = VALUES)
}

# Mean or share of a trait among flagged versus other subjects; Wilcoxon or Fisher p-value.
traits <- list(Belief = list(col = "belief_thumb_quality", binary = FALSE, fmt = "%.2f"),
               Familiarity = list(col = "booking_familiarity", binary = FALSE, fmt = "%.2f"),
               CueEffect = list(col = "cue_effect", binary = FALSE, fmt = "%.3f"),
               Recognition = list(col = "cue_recognition", binary = TRUE),
               RecognitionCorrect = list(col = "recognition_correct", binary = TRUE),
               Clutter = list(col = "clutter_high", binary = TRUE))
for (lab in c("MeaningReviews", "MeaningGeneric", "MeaningPlatform", "UseUsed", "UseNotNoticed")) for (tr in names(traits)) {
  tc <- traits[[tr]]; nm <- paste0("Open", lab, tr)
  v <- coded[[flags[[lab]]]]; y <- coded[[tc$col]]
  ok <- !is.na(v) & !is.na(y)
  if (tc$binary) {
    write_tex_value(paste0("Pct", nm, "Yes"), format_pct(mean(y[ok & v]), 0), file = VALUES)
    write_tex_value(paste0("Pct", nm, "No"), format_pct(mean(y[ok & !v]), 0), file = VALUES)
    p <- tryCatch(fisher.test(table(v[ok], y[ok]))$p.value, error = function(e) NA_real_)
  } else {
    write_tex_value(paste0("Mean", nm, "Yes"), mean(y[ok & v]), fmt = tc$fmt, file = VALUES)
    write_tex_value(paste0("Mean", nm, "No"), mean(y[ok & !v]), fmt = tc$fmt, file = VALUES)
    p <- tryCatch(suppressWarnings(wilcox.test(y[ok & v], y[ok & !v])$p.value), error = function(e) NA_real_)
  }
  write_tex_value(paste0("Pval", nm), if (is.na(p)) "=n/a" else format_pvalue(p), file = VALUES)
  write_tex_value(paste0("Stars", nm), stars_from_pvalue(p), file = VALUES)
}
