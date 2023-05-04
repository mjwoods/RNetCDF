# Create R/config.R on Windows.
# This mimics the substitutions performed by configure.
# Command line arguments have the form "variable=value",
# and each occurrence of "@variable@" in input file R/config.R.in
# is replaced by value in output file R/config.R .

text <- readLines("../R/config.R.in")

args <- commandArgs(trailingOnly = TRUE)

for (arg in args) {
  lhs <- sub("=.*", "", arg)
  rhs <- sub(".*=", "", arg)
  text <- gsub(paste0("@", lhs, "@"), rhs, text)
}

writeLines(text, "../R/config.R")

