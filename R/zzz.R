.onLoad <- function(libname, pkgname) { # nocov start
  op <- options()
  op.gamma <- list(
    gamma.debug = TRUE,
    gamma.verbose = TRUE
  )
  toset <- !(names(op.gamma) %in% names(op))
  if(any(toset)) options(op.gamma[toset])

  invisible()
} # nocov end
