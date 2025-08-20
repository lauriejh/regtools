# Helper function to redact unstable paths for snapshots

snap_redact_paths <- function(lines) {
  normalize <- function(x) tryCatch(normalizePath(x, winslash = "/", mustWork = FALSE), error = function(e) x)
  to_posix <- function(x) chartr("\\", "/", x)

  wd   <- normalize(getwd())
  tmp  <- normalize(tempdir())
  home <- normalize("~")

  lines <- to_posix(lines)

  lines <- gsub(wd,   "<WD>",   lines, fixed = TRUE)
  lines <- gsub(tmp,  "<TMP>",  lines, fixed = TRUE)
  lines <- gsub(home, "<HOME>", lines, fixed = TRUE)

  lines <- gsub("(?:(?:[A-Za-z]:/)|/|//)[^\\s\"'`<>]+", "<PATH>", lines, perl = TRUE)

  return(lines)
}
