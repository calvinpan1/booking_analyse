# Compare two values files name by name (a line diff is useless across OSes).
# Usage: Rscript tools/compare_values.R NEW.tex REFERENCE.tex
args <- commandArgs(trailingOnly = TRUE)
parse_values <- function(f) {
  lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
  m <- regmatches(lines, regexec("^\\\\newcommand\\{\\\\([A-Za-z]+)\\}\\{(.*)\\}$", lines))
  m <- m[lengths(m) == 3]
  setNames(vapply(m, `[`, "", 3), vapply(m, `[`, "", 2))
}
new <- parse_values(args[1]); ref <- parse_values(args[2])
common <- intersect(names(new), names(ref))
diff <- common[new[common] != ref[common]]
cat(sprintf("%d values in new, %d in reference, %d common, %d differ\n", length(new), length(ref), length(common), length(diff)))
for (n in diff) cat(sprintf("  %-45s new=%-20s ref=%s\n", n, new[[n]], ref[[n]]))
missing <- setdiff(names(ref), names(new))
if (length(missing)) cat("Only in reference:", paste(missing, collapse = " "), "\n")
extra <- setdiff(names(new), names(ref))
if (length(extra)) cat("Only in new:", paste(extra, collapse = " "), "\n")
