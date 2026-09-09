# Shared R -> LaTeX value-writing helpers for the booking-experiment analysis.
#
# Concept vocabulary used across this project (keep this list authoritative —
# don't invent a synonym elsewhere): RegCoef, RegSE, RegZ, RegT, RegPval,
# RegPvalOneSided, RegStars, RegN, Mean, Median, SD, NObs, Pct, PvalBinom,
# TestStat, Max (a maximum over a grouping, e.g. MaxSubstitutesPerChoiceSet),
# Corr (a Pearson/Spearman correlation estimate — pairs with the
# existing TestStat/Pval/Stars concepts for its test statistic/p-value/stars,
# e.g. CorrBeliefCueEffect + TestStatBeliefCueEffect + PvalBeliefCueEffect).

#' Write (or append) one \newcommand{...}{...} line to a values.tex file.
write_tex_value <- function(name, value, fmt = "%.3f", file, append = TRUE) {
  if (!grepl("^[A-Za-z]+$", name)) {
    stop(sprintf(
      "Invalid LaTeX command name '%s': letters only, no digits/underscores/dots. Run it through sanitize_tex_name() first.",
      name
    ))
  }
  formatted <- if (is.numeric(value)) sprintf(fmt, value) else as.character(value)
  line <- sprintf("\\newcommand{\\%s}{%s}\n", name, formatted)
  # Binary-mode connection: on Windows a text-mode cat() turns "\n" into
  # "\r\n", so a run on the SSD machine rewrote the whole values file with
  # CRLF and produced spurious whole-file git diffs in the Overleaf clone.
  con <- file(file, open = if (append) "ab" else "wb")
  on.exit(close(con), add = TRUE)
  cat(line, file = con)
  invisible(line)
}

#' Format a p-value the way this project's papers report it
#' ("<0.001" below threshold, otherwise "=0.023").
format_pvalue <- function(p, threshold = 0.001, digits = 3) {
  if (is.na(p)) return("NA")
  if (p < threshold) sprintf("<%s", format(threshold, scientific = FALSE))
  else sprintf("=%s", formatC(round(p, digits), format = "f", digits = digits))
}

#' Standard significance stars from a p-value.
stars_from_pvalue <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "$^{***}$" else if (p < 0.01) "$^{**}$" else
  if (p < 0.05) "$^{*}$"   else if (p < 0.1)  "$^{\\dagger}$" else ""
}

#' Percent formatting convenience (e.g. a share of 0.418 -> "41.8").
format_pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f"), 100 * x)

#' Write a p-value as TWO commands: \name (with the "=" / "<" prefix baked in,
#' for prose like "$p\name$") and \nameNum (the bare number, no prefix, for
#' table cells that already have their own "$p$ (one-sided)" row label and
#' don't need a repeated "=" in every column).
write_pvalue_pair <- function(name, p, threshold = 0.001, digits = 3, file, append = TRUE) {
  write_tex_value(name, format_pvalue(p, threshold, digits), file = file, append = append)
  bare <- if (is.na(p)) "NA" else if (p < threshold) sprintf("<%s", format(threshold, scientific = FALSE))
          else formatC(round(p, digits), format = "f", digits = digits)
  write_tex_value(paste0(name, "Num"), bare, file = file, append = TRUE)
}
