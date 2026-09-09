# Generate a small, edge-case-rich synthetic version of analysis_dataset.csv,
# structurally faithful to the real experimental design (subject x city x
# weekend x listing; 3 cities x 4 weekends x 9 listings = 108 rows/subject;
# 3 clusters of 3 listings per choice set, one cue-eligible listing/cluster)
# but with every value fabricated.
#
# Generic by column, not by name: any column NOT in STRUCTURAL_COLS below
# that is constant within participant_code for almost every real subject is
# treated as a "subject-level" column and synthesized by sampling
# independently from its own real per-subject marginal distribution (never
# a real subject's whole row/profile — only the marginal, matching the
# project's aggregate-only synthetic-data convention). This is what lets new
# subject-level columns (e.g. demographics, comprehension-check fields) flow
# through automatically without editing this script.

library(dplyr)

STRUCTURAL_COLS <- c(
  "participant_code", "city", "weekend_number_global", "property_slug",
  "n_hotels_in_choice_set", "property_page_visits_number",
  "decision_time_seconds", "loading_time_seconds", "cued_weekend",
  "listing_chosen", "listing_clicked", "listing_n_clicks",
  "time_on_listing_page_seconds", "cluster", "cued_listing",
  "checkin", "choice_set", "would_be_cued", "is_substitute_listing",
  "replaced_property_slug", "preference_consistency",
  "cued_choice_preference_consistency"
)

#' Detect which non-structural columns are (almost always) constant within a
#' subject, i.e. safe to synthesize as one draw per synthetic subject.
detect_subject_level_cols <- function(df, threshold = 0.95) {
  candidates <- setdiff(names(df), STRUCTURAL_COLS)
  is_subject_level <- sapply(candidates, function(col) {
    per_subject_nunique <- df %>%
      group_by(participant_code) %>%
      summarise(n = n_distinct(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
      pull(n)
    mean(per_subject_nunique <= 1) >= threshold
  })
  candidates[is_subject_level]
}

#' One independent draw per synthetic subject from a real column's marginal
#' distribution (per-subject value, sampled with replacement) — never a real
#' subject's full row, only this one column's aggregate distribution.
sample_subject_level_col <- function(real_col, n) {
  values <- real_col[!is.na(real_col)]
  if (length(values) == 0) return(rep(NA, n))
  sample(values, n, replace = TRUE)
}

rand_slug <- function(n) {
  letters_pool <- c(letters, 0:9, "-")
  paste(sample(letters_pool, n, replace = TRUE), collapse = "")
}

#' Build one (participant, city, weekend) choice-set block of n_hotels rows.
make_weekend_block <- function(participant_code, city, weekend, global_weekend,
                                choice_set, n_hotels = 9) {
  cued_weekend <- sample(c(TRUE, FALSE), 1)
  decision_time <- runif(1, 1, 500)
  loading_time <- if (runif(1) > 0.05) runif(1, 1, 200) else NA
  property_page_visits <- sample(0:11, 1)
  checkin <- sample(c("2026-10-02","2026-10-09","2026-10-16","2026-10-23",
                       "2026-10-30","2026-11-06","2026-11-13","2026-11-20",
                       "2026-11-27"), 1)

  clusters <- rep(1:3, length.out = n_hotels)
  clusters <- sample(clusters)
  would_be_cued <- rep(FALSE, n_hotels)
  for (cl in unique(clusters)) {
    idxs <- which(clusters == cl)
    would_be_cued[sample(idxs, 1)] <- TRUE
  }

  chosen_idx <- sample.int(n_hotels, 1)
  n_clicked <- min(sample(1:3, 1), n_hotels)
  clicked_idxs <- unique(c(sample.int(n_hotels, n_clicked), chosen_idx))
  cued_idx <- if (cued_weekend) sample.int(n_hotels, 1) else NA

  rows <- vector("list", n_hotels)
  for (i in seq_len(n_hotels)) {
    listing_chosen <- i == chosen_idx
    listing_clicked <- i %in% clicked_idxs
    is_substitute <- runif(1) < 0.02
    rows[[i]] <- list(
      participant_code = participant_code, city = city,
      weekend_number_global = global_weekend,
      property_slug = rand_slug(sample(6:60, 1)),
      n_hotels_in_choice_set = n_hotels,
      property_page_visits_number = property_page_visits,
      decision_time_seconds = decision_time, loading_time_seconds = loading_time,
      cued_weekend = cued_weekend, listing_chosen = listing_chosen,
      listing_clicked = listing_clicked,
      listing_n_clicks = if (listing_clicked) sample(1:14, 1) else 0,
      time_on_listing_page_seconds = if (listing_clicked && runif(1) > 0.4) runif(1, 5, 300) else NA,
      cluster = clusters[i], cued_listing = !is.na(cued_idx) && i == cued_idx,
      checkin = checkin, choice_set = choice_set,
      would_be_cued = would_be_cued[i], is_substitute_listing = is_substitute,
      replaced_property_slug = if (is_substitute) rand_slug(sample(10:40, 1)) else NA
    )
  }
  rows
}

#' Build one synthetic subject: subject-level columns sampled from real
#' marginals, structural rows built via make_weekend_block(), preference
#' consistency derived exactly like 00c would.
make_subject <- function(participant_code, cities, weekends_per_city, n_hotels_default,
                          subject_level_values, reduce_choice_set_city_weekend = NULL) {
  all_rows <- list()
  global_weekend <- 1
  for (city in cities) {
    for (w in seq_len(weekends_per_city)) {
      n_hotels <- n_hotels_default
      if (!is.null(reduce_choice_set_city_weekend) &&
          identical(reduce_choice_set_city_weekend$city, city) &&
          reduce_choice_set_city_weekend$weekend == w) {
        n_hotels <- 8
      }
      block <- make_weekend_block(participant_code, city, w, global_weekend,
                                   paste0("choice_set_", w), n_hotels)
      all_rows <- c(all_rows, block)
      global_weekend <- global_weekend + 1
    }
  }
  df <- bind_rows(lapply(all_rows, as.data.frame, stringsAsFactors = FALSE))
  for (col in names(subject_level_values)) {
    df[[col]] <- subject_level_values[[col]]
  }
  df
}

#' Derive preference_consistency / cued_choice_preference_consistency exactly
#' as 00c does: per (participant, city), do the two no-cue weekends' chosen
#' listings share a cluster; if so, do the cue weekend(s) match that cluster.
derive_preference_consistency <- function(df) {
  df$preference_consistency <- NA
  df$cued_choice_preference_consistency <- NA
  keys <- unique(df[, c("participant_code", "city")])
  for (i in seq_len(nrow(keys))) {
    pc <- keys$participant_code[i]; ct <- keys$city[i]
    g <- df[df$participant_code == pc & df$city == ct, ]
    no_cue_w <- unique(g$weekend_number_global[!g$cued_weekend])
    cue_w    <- unique(g$weekend_number_global[g$cued_weekend])
    chosen_clusters <- sapply(no_cue_w, function(w) {
      chosen <- g[g$weekend_number_global == w & g$listing_chosen, ]
      if (nrow(chosen)) chosen$cluster[1] else NA
    })
    chosen_clusters <- chosen_clusters[!is.na(chosen_clusters)]
    is_consistent <- if (length(chosen_clusters) >= 2) length(unique(chosen_clusters)) == 1 else NA
    df$preference_consistency[df$participant_code == pc & df$city == ct] <- is_consistent
    if (isTRUE(is_consistent) && length(cue_w) > 0) {
      preferred <- chosen_clusters[1]
      cue_flags <- sapply(cue_w, function(w) {
        chosen <- g[g$weekend_number_global == w & g$listing_chosen, ]
        if (nrow(chosen)) chosen$cluster[1] == preferred else NA
      })
      cue_flags <- cue_flags[!is.na(cue_flags)]
      if (length(cue_flags)) {
        df$cued_choice_preference_consistency[df$participant_code == pc & df$city == ct] <- all(cue_flags)
      }
    }
  }
  df
}

#' Generate and write the synthetic dataset. `real_csv_path` is the real
#' analysis_dataset.csv (read only to build a privacy-safe profile — never
#' copied verbatim); `out_path` is where analysis_dataset_synthetic.csv is
#' written.
generate_synthetic_dataset <- function(real_csv_path, out_path, seed = 42) {
  set.seed(seed)
  df_real <- readr::read_csv(real_csv_path, show_col_types = FALSE)

  cities <- sort(unique(df_real$city))
  subject_level_cols <- detect_subject_level_cols(df_real)
  cat("Synthesizing", length(subject_level_cols), "subject-level column(s) generically:",
      paste(subject_level_cols, collapse = ", "), "\n")

  n_synth_subjects <- 6

  # Guard rail, not just a small hardcoded number: the synthetic subject
  # count must stay well below the real participant count, since sample
  # size is one of the few cheap signals (alongside "is this file named
  # _synthetic") that lets anyone eyeballing an output later tell a
  # synthetic run from a real one apart. Fails loudly rather than silently
  # producing a synthetic dataset that could pass for real-sized.
  n_real_subjects <- dplyr::n_distinct(df_real$participant_code)
  if (n_synth_subjects >= n_real_subjects / 2) {
    stop("Synthetic subject count (", n_synth_subjects, ") is not comfortably below ",
         "the real participant count (", n_real_subjects, ") -- this defeats the point ",
         "of a small synthetic dataset. Lower n_synth_subjects (or investigate why the ",
         "real dataset now has so few participants) before proceeding.")
  }
  synth_subjects <- list()

  draw_subject_level_values <- function() {
    setNames(
      lapply(subject_level_cols, function(col) sample_subject_level_col(df_real[[col]], 1)),
      subject_level_cols
    )
  }

  # Per the real design, each subject keeps 3 of the (up to 4) candidate
  # cities (one voluntarily dropped in the pretask) — see
  # detect_subject_level_cols()'s STRUCTURAL_COLS note above; sampling a
  # fresh 3-of-cities draw per synthetic subject mirrors that, rather than
  # giving every synthetic subject all of `cities`.
  n_cities_per_subject <- min(3, length(cities))
  subject_cities <- function() sample(cities, n_cities_per_subject)

  # Subjects 1-4: complete, well-behaved, exercise ordinary subject-level draws.
  for (k in 1:4) {
    pc <- sprintf("synth%04d", k)
    synth_subjects[[pc]] <- make_subject(pc, subject_cities(), 4, 9, draw_subject_level_values())
  }

  # Subject 5: reduced (8-listing) choice set in one city/weekend -> exercises
  # the analysis pipeline's "reduced choice set" exclusion rule.
  pc <- "synth0005"
  cities5 <- subject_cities()
  synth_subjects[[pc]] <- make_subject(
    pc, cities5, 4, 9, draw_subject_level_values(),
    reduce_choice_set_city_weekend = list(city = cities5[1], weekend = 1)
  )

  # Subject 6: forces the recorded min/max of the first numeric subject-level
  # column (if any) as an edge case, and NA on the first subject-level column
  # that has any NA in the real data.
  pc <- "synth0006"
  vals6 <- draw_subject_level_values()
  numeric_cols <- subject_level_cols[sapply(subject_level_cols, function(c) is.numeric(df_real[[c]]))]
  if (length(numeric_cols)) {
    c1 <- numeric_cols[1]
    vals6[[c1]] <- max(df_real[[c1]], na.rm = TRUE)
  }
  na_prone_cols <- subject_level_cols[sapply(subject_level_cols, function(c) any(is.na(df_real[[c]])))]
  if (length(na_prone_cols)) vals6[[na_prone_cols[1]]] <- NA
  synth_subjects[[pc]] <- make_subject(pc, subject_cities(), 4, 9, vals6)

  df <- bind_rows(synth_subjects)
  df <- derive_preference_consistency(df)

  # A couple of extra listing-level NA/duplicate edge cases.
  df$time_on_listing_page_seconds[1] <- NA
  df <- bind_rows(df, df[which(df$listing_clicked)[1], ])

  col_order <- names(df_real)[names(df_real) %in% names(df)]
  df <- df[, c(col_order, setdiff(names(df), col_order))]

  readr::write_csv(df, out_path)
  cat("Wrote synthetic dataset (", nrow(df), "rows) to", out_path, "\n")
  invisible(df)
}
