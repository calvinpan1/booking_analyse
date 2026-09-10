# Subject-level measures of interface use taken from the event files, which the
# analysis dataset does not carry: hover dwell, scroll depth, viewport dwell,
# badge-tooltip exposure. Writes INPUTS_DIR/subject_tracking.csv (one row per
# subject) and INPUTS_DIR/tooltip_cells.csv (subject x weekend with the tooltip opened).
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))

joined <- read_csv(file.path(INPUTS_DIR, "extension_joined.csv"), col_types = cols(.default = col_character()))
converted <- read_csv(file.path(INPUTS_DIR, "extension_converted.csv"), col_types = cols(.default = col_character()))

hover <- joined %>%
  filter(type == "hover", kind == "product_card", !is.na(durationMs)) %>%
  group_by(participant_code) %>%
  summarise(mean_hover_dwell_s = mean(as.numeric(durationMs) / 1000, na.rm = TRUE), .groups = "drop")

scroll <- joined %>%
  filter(type == "scroll") %>%
  group_by(participant_code, cell_index) %>%
  summarise(max_depth = max(as.numeric(scrollDepthPercent), na.rm = TRUE), .groups = "drop") %>%
  group_by(participant_code) %>%
  summarise(mean_max_scroll_depth = mean(max_depth, na.rm = TRUE), .groups = "drop")

# One viewport episode = a run of consecutive isIntersecting rows; its end is the next row after the run.
episodes <- joined %>%
  filter(type == "viewport", !is.na(targetPropertyId), targetPropertyId != "") %>%
  mutate(timestamp = as.numeric(timestamp),
         is_intersecting = tolower(as.character(isIntersecting)) %in% c("true", "1")) %>%
  filter(!is.na(timestamp)) %>%
  arrange(participant_code, cell_index, targetPropertyId, timestamp) %>%
  group_by(participant_code, cell_index, targetPropertyId) %>%
  mutate(next_ts = lead(timestamp),
         episode_id = cumsum(is_intersecting & !coalesce(lag(is_intersecting), FALSE))) %>%
  ungroup() %>%
  filter(is_intersecting) %>%
  group_by(participant_code, cell_index, targetPropertyId, episode_id) %>%
  summarise(dwell_s = (max(next_ts) - min(timestamp)) / 1000, .groups = "drop") %>%
  filter(!is.na(dwell_s), dwell_s >= 0)

viewport <- episodes %>%
  group_by(participant_code, cell_index) %>%
  summarise(total_s = sum(dwell_s), .groups = "drop") %>%
  group_by(participant_code) %>%
  summarise(mean_total_viewport_s = mean(total_s), .groups = "drop")

by_listing <- episodes %>%
  group_by(participant_code, cell_index, targetPropertyId) %>%
  summarise(reached_10s = sum(dwell_s) >= 10, .groups = "drop")
long_dwell <- by_listing %>%
  group_by(participant_code) %>%
  summarise(n_listings_viewport = n(), share_listings_10s = 100 * mean(reached_10s), .groups = "drop")

tooltip_cells <- converted %>%
  filter(type == "pouce_explanation") %>%
  distinct(participant_code, weekend_number_global = as.integer(cell_index))

subjects <- converted %>% distinct(participant_code)
tracking <- subjects %>%
  left_join(hover, by = "participant_code") %>%
  left_join(scroll, by = "participant_code") %>%
  left_join(viewport, by = "participant_code") %>%
  left_join(long_dwell, by = "participant_code") %>%
  mutate(tooltip_opened = participant_code %in% tooltip_cells$participant_code)

write_csv(tracking, file.path(INPUTS_DIR, "subject_tracking.csv"))
write_csv(tooltip_cells, file.path(INPUTS_DIR, "tooltip_cells.csv"))
