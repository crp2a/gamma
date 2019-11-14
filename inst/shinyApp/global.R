# Check dependencies
pkg_needed <- c("gamma", "ggplot2", "knitr", "kableExtra", "shinyWidgets")
pkg_missing <- !vapply(X = pkg_needed, FUN = requireNamespace,
                       FUN.VALUE = logical(1), quietly = TRUE)

if (any(pkg_missing)) {
  n <- sum(pkg_missing)
  msg1 <- ngettext(n, "this", "these")
  msg2 <- ngettext(n, "package", "packages")

  message(sprintf("The following %s must be installed to run the app:\n", msg2),
          paste0("* ", pkg_needed[pkg_missing], collapse = "\n"))

  answer <- readline(sprintf("Do you want to install %s %s? (Y/N) ", msg1, msg2))
  answer <- ifelse(answer %in% c("y", "Y", "1"), TRUE, FALSE)

  if (answer) {
    utils::install.packages(pkg_needed[pkg_missing])
  } else {
    stop(sprintf("Please install the needed %s.", msg2), call. = FALSE)
  }
  rm(n, msg1, msg2, answer)
}
rm(pkg_needed, pkg_missing)
