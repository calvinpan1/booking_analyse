# Helpers writing \newcommand values for the paper. Names: letters only, CamelCase.
write_tex_value <- function(name, value, fmt = "%.3f", file, append = TRUE) {
  if (!grepl("^[A-Za-z]+$", name)) stop("Invalid LaTeX command name: ", name)
  formatted <- if (is.numeric(value)) sprintf(fmt, value) else as.character(value)
  con <- file(file, open = if (append) "ab" else "wb")   # binary: LF on Windows too
  on.exit(close(con), add = TRUE)
  cat(sprintf("\\newcommand{\\%s}{%s}\n", name, formatted), file = con)
}

format_pvalue <- function(p, threshold = 0.001, digits = 3) {
  if (is.na(p)) return("NA")
  if (p < threshold) sprintf("<%s", format(threshold, scientific = FALSE))
  else sprintf("=%s", formatC(round(p, digits), format = "f", digits = digits))
}

stars_from_pvalue <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "$^{***}$" else if (p < 0.01) "$^{**}$" else
  if (p < 0.05) "$^{*}$" else if (p < 0.1) "$^{\\dagger}$" else ""
}

format_pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f"), 100 * x)

# \name carries the "=" or "<" prefix (for prose), \nameNum is the bare number (for table cells).
write_pvalue_pair <- function(name, p, file) {
  write_tex_value(name, format_pvalue(p), file = file)
  bare <- if (is.na(p)) "NA" else if (p < 0.001) "<0.001" else formatC(round(p, 3), format = "f", digits = 3)
  write_tex_value(paste0(name, "Num"), bare, file = file)
}
