# The three figures of the paper and the sample sizes quoted in their notes.
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(ggplot2))

VALUES <- new_values_file("03_figures")
df <- read_csv(ANALYSIS_SAMPLE, show_col_types = FALSE)

city_labels <- c("arcachon" = "Arcachon", "la-ciotat" = "LaCiotat", "le-treport" = "LeTreport", "sete" = "Sete")
city_display <- c("arcachon" = "Arcachon", "la-ciotat" = "La Ciotat", "le-treport" = "Le Treport", "sete" = "Sete")
facet_levels <- c("Pooled", unname(city_display))
facet_camel <- setNames(c("Pooled", unname(city_labels)), facet_levels)
fill_colors <- c("Cue hidden" = "#2a78d6", "Cue visible" = "#eb6834")

wilson_ci <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2); p <- x / n; denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- (z / denom) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  data.frame(lower = pmax(0, centre - half), upper = pmin(1, centre + half))
}
pooled_and_by_city <- function(data, summarise_fn) {
  bind_rows(summarise_fn(data) %>% mutate(city_facet = "Pooled"),
            data %>% group_by(city) %>% group_modify(~ summarise_fn(.x)) %>% ungroup() %>%
              mutate(city_facet = unname(city_display[as.character(city)])) %>% select(-city)) %>%
    mutate(city_facet = factor(city_facet, levels = facet_levels),
           cue_visible_label = factor(ifelse(cued_weekend, "Cue visible", "Cue hidden"), levels = names(fill_colors)))
}
base_theme <- function(p) {
  p + facet_wrap(~ city_facet, nrow = 1) +
    scale_fill_manual(values = fill_colors, name = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
}
write_n_by_facet <- function(fig_df, cmd) {
  n <- fig_df %>% filter(would_be_cued) %>% group_by(city_facet) %>% summarise(n = sum(n), .groups = "drop")
  for (i in seq_len(nrow(n))) write_tex_value(paste0(cmd, facet_camel[[as.character(n$city_facet[i])]]), n$n[i], fmt = "%d", file = VALUES)
}
save_fig <- function(p, name, width, height) ggsave(file.path(FIGURES_DIR, name), p, width = width, height = height, dpi = 300)

# --- Choice probability of a cue-eligible listing, per choice set ---------------------
set_choice <- df %>%
  group_by(participant_code, city, weekend_number_global) %>%
  summarise(cued_weekend = first(cued_weekend), listing_chosen = any(listing_chosen & would_be_cued, na.rm = TRUE), .groups = "drop") %>%
  mutate(would_be_cued = TRUE)
choice_fig <- pooled_and_by_city(set_choice, function(d) {
  d %>% group_by(would_be_cued, cued_weekend) %>% summarise(n = n(), n_chosen = sum(listing_chosen), .groups = "drop") %>%
    mutate(prob = n_chosen / n) %>% bind_cols(wilson_ci(.$n_chosen, .$n))
})
p <- base_theme(ggplot(choice_fig, aes(x = cue_visible_label, y = prob, fill = cue_visible_label)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, color = "#3a3a3a") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(x = NULL, y = "P(listing chosen)"))
save_fig(p, "cue_choice_probability.png", 9, 3.2)
write_n_by_facet(choice_fig, "NObsChoiceProb")

# --- Clicks and time on the detail page, per choice set x eligibility group -------------
attention <- df %>%
  filter(!(listing_clicked & is.na(time_on_listing_page_seconds))) %>%
  mutate(time_on_page = ifelse(listing_clicked, time_on_listing_page_seconds, 0)) %>%
  group_by(participant_code, city, weekend_number_global, cued_weekend, would_be_cued) %>%
  summarise(listing_clicked = any(listing_clicked), time_on_page = sum(time_on_page), .groups = "drop")
click_fig <- pooled_and_by_city(attention, function(d) {
  d %>% group_by(would_be_cued, cued_weekend) %>% summarise(n = n(), n_clicked = sum(listing_clicked), .groups = "drop") %>%
    mutate(prob = n_clicked / n) %>% bind_cols(wilson_ci(.$n_clicked, .$n))
})
time_fig <- pooled_and_by_city(attention, function(d) {
  d %>% group_by(would_be_cued, cued_weekend) %>%
    summarise(n = n(), mean_time = mean(time_on_page), se = sd(time_on_page) / sqrt(n()), .groups = "drop") %>%
    mutate(lower = pmax(0, mean_time - 1.96 * se), upper = mean_time + 1.96 * se)
})
eligibility_bars <- function(fig_df, y, ylab, y_labels = waiver()) {
  fig_df <- fig_df %>% mutate(cue_eligible_label = factor(ifelse(would_be_cued, "Cue-eligible", "Not cue-eligible"),
                                                          levels = c("Cue-eligible", "Not cue-eligible")))
  base_theme(ggplot(fig_df, aes(x = cue_eligible_label, y = .data[[y]], fill = cue_visible_label)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.7), width = 0.15, color = "#3a3a3a") +
    scale_y_continuous(labels = y_labels, limits = c(0, NA)) +
    labs(x = NULL, y = ylab)) + theme(axis.text.x = element_text(angle = 20, hjust = 1))
}
save_fig(eligibility_bars(click_fig, "prob", "P(at least one detail page clicked)", scales::percent_format(accuracy = 1)),
         "cue_click_probability.png", 9, 3.2)
save_fig(eligibility_bars(time_fig, "mean_time", "Mean time on detail pages per set (s)"), "cue_time_on_page.png", 9, 3.2)
write_n_by_facet(click_fig, "NObsClickProbFig")
write_n_by_facet(time_fig, "NObsTimeOnPageFig")

# --- H3: cluster of the second no-cue choice given the first, and cued-choice match, by city ----
clusters <- sort(unique(df$cluster))
chosen <- df %>% filter(listing_chosen) %>% distinct(participant_code, city, weekend_number_global, cued_weekend, cluster)
nocue_pairs <- chosen %>%
  filter(!cued_weekend) %>% group_by(participant_code, city) %>% filter(n() == 2) %>%
  arrange(weekend_number_global, .by_group = TRUE) %>%
  summarise(first = cluster[1], second = cluster[2], .groups = "drop")
cued_match <- chosen %>%
  filter(cued_weekend) %>%
  inner_join(nocue_pairs %>% filter(first == second) %>% transmute(participant_code, city, preferred = first), by = c("participant_code", "city")) %>%
  group_by(city, preferred) %>% summarise(n_match = sum(cluster == preferred), n = n(), .groups = "drop")

cells <- list()
for (ct in sort(unique(nocue_pairs$city))) {
  pairs_ct <- nocue_pairs %>% filter(city == ct)
  write_tex_value(paste0("NClusterCorr", city_labels[[ct]]), nrow(pairs_ct), fmt = "%d", file = VALUES)
  transitions <- expand.grid(first = clusters, second = clusters) %>%
    left_join(pairs_ct %>% count(first, second, name = "n"), by = c("first", "second")) %>%
    left_join(pairs_ct %>% count(first, name = "n_first"), by = "first") %>%
    mutate(n = coalesce(n, 0L), n_first = coalesce(n_first, 0L), value = ifelse(n_first > 0, n / n_first, NA)) %>%
    transmute(city = ct, row = first, x = as.numeric(second), value, k = n, n = n_first)
  match_col <- tibble(cluster = clusters) %>%
    left_join(cued_match %>% filter(city == ct), by = c("cluster" = "preferred")) %>%
    transmute(city = ct, row = cluster, x = length(clusters) + 1.4, value = n_match / n, k = n_match, n = n)
  cells[[ct]] <- bind_rows(transitions, match_col)
}
tri <- bind_rows(cells) %>%
  mutate(city_facet = factor(unname(city_display[city]), levels = unname(city_display)),
         row = factor(row, levels = rev(clusters)),
         label = ifelse(is.na(value), "n/a", sprintf("%.0f%%", 100 * value)),
         n_label = ifelse(is.na(value), "", sprintf("%d/%d", k, n)),
         dark = coalesce(value > 0.55, FALSE))
p <- ggplot(tri, aes(x = x, y = row, fill = value)) +
  geom_tile(color = "white", width = 1, height = 1) +
  geom_text(aes(label = label, color = dark), size = 3, vjust = -0.1) +
  geom_text(aes(label = n_label, color = dark), size = 2, vjust = 1.8) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "white"), guide = "none") +
  geom_vline(xintercept = length(clusters) + 0.65, color = "grey40", linewidth = 0.4) +
  facet_wrap(~ city_facet, ncol = 2) +
  scale_x_continuous(breaks = c(seq_along(clusters), length(clusters) + 1.4),
                     labels = c(as.character(clusters), "P(cued match)")) +
  scale_fill_gradient(low = "white", high = "#08306b", limits = c(0, 1), na.value = "grey90", guide = "none") +
  coord_fixed() +
  labs(x = "Cluster of the second no-cue choice", y = "Cluster of the first no-cue choice") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), panel.spacing = unit(2, "lines"),
        axis.title.x = element_text(margin = margin(t = 12)), axis.title.y = element_text(margin = margin(r = 12)))
save_fig(p, "h3_cluster_corr_triangular.png", 7, 6.5)
