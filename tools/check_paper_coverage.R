# Every \Command the paper uses that looks like a value must be defined in the package's values file.
# Usage: Rscript tools/check_paper_coverage.R PAPER_DIR VALUES.tex
args <- commandArgs(trailingOnly = TRUE)
paper_dir <- args[1]; values_file <- args[2]
main <- file.path(paper_dir, "papers", "pilot_experiment_chi2027_article.tex")
tex <- paste(readLines(main, encoding = "UTF-8", warn = FALSE), collapse = "\n")
tex <- gsub("(?s)\\\\begin\\{comment\\}.*?\\\\end\\{comment\\}", "", tex, perl = TRUE)
tex <- strsplit(tex, "\\\\end\\{document\\}")[[1]][1]
lines <- strsplit(tex, "\n")[[1]]
lines <- ifelse(grepl("^\\s*%", lines), "", sub("(^|[^\\\\])%.*$", "\\1", lines))
body <- paste(lines, collapse = "\n")
inputs <- regmatches(body, gregexpr("\\\\input\\{([^}]*)\\}", body))[[1]]
inputs <- sub("\\\\input\\{([^}]*)\\}", "\\1", inputs)
for (i in inputs[!grepl("values", inputs)]) {
  p <- file.path(paper_dir, ifelse(grepl("\\.tex$", i), i, paste0(i, ".tex")))
  if (file.exists(p)) body <- paste(body, paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
}
used <- unique(regmatches(body, gregexpr("\\\\[A-Za-z]+", body))[[1]])
used <- sub("^\\\\", "", used)
defined <- sub("^\\\\newcommand\\{\\\\([A-Za-z]+)\\}.*$", "\\1",
               grep("^\\\\newcommand", readLines(values_file, encoding = "UTF-8", warn = FALSE), value = TRUE))
ref <- args[3]
if (!is.na(ref)) {   # optional: restrict to names the reference values file defines (i.e. real value commands)
  ref_defined <- sub("^\\\\newcommand\\{\\\\([A-Za-z]+)\\}.*$", "\\1",
                     grep("^\\\\newcommand", readLines(ref, encoding = "UTF-8", warn = FALSE), value = TRUE))
  used <- intersect(used, ref_defined)
}
missing <- setdiff(used, defined)
cat(sprintf("%d value commands used by the paper, %d missing from %s\n", length(used), length(missing), basename(values_file)))
if (length(missing)) cat(paste(sort(missing), collapse = " "), "\n")
