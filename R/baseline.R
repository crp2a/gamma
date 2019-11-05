# ESTIMATE AND REMOVE BASELINE
#' @include AllGenerics.R operators.R
NULL

#' @export
#' @rdname baseline
#' @aliases estimate_baseline,GammaSpectra-method
setMethod(
  f = "estimate_baseline",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, method = c("SNIP"),
                        LLS = FALSE, decreasing = FALSE, k = 100) {

    baseline <- lapply(
      X = object,
      FUN = estimate_baseline,
      method, LLS, decreasing, k
    )
    names(baseline) <- names(object)

    .GammaSpectra(baseline)
  }
)

#' @export
#' @rdname baseline
#' @aliases estimate_baseline,GammaSpectrum-method
setMethod(
  f = "estimate_baseline",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("SNIP"),
                        LLS = FALSE, decreasing = FALSE, k = 100) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)

    # Get count data
    x <- methods::as(object, "data.frame")
    x_counts <- x$count

    baseline <- switch (
      method,
      SNIP = SNIP(x_counts, LLS, decreasing, iterations = k),
      stop("There is no such method: ", method, call. = FALSE)
    )

    spc <- methods::initialize(object, count = baseline)
    methods::as(spc, "BaseLine")
  }
)

#' @export
#' @rdname baseline
#' @aliases remove_baseline,GammaSpectra-method
setMethod(
  f = "remove_baseline",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, method = c("SNIP"), ...) {

    baseline <- lapply(
      X = object,
      FUN = function(x, method, ...) remove_baseline(x, method, ...),
      method = method, ...
    )
    names(baseline) <- names(object)

    .GammaSpectra(baseline)
  }
)

#' @export
#' @rdname baseline
#' @aliases remove_baseline,GammaSpectrum-method
setMethod(
  f = "remove_baseline",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("SNIP"), ...) {

    baseline <- estimate_baseline(object, method = method, ...)
    object - baseline
  }
)

#' SNIP Algorithm
#'
#' @param x A \code{\link{numeric}} vector.
#' @param LLS A \code{\link{logical}} scalar: should the LLS operator be applied
#'  on \code{x} before employing SNIP algorithm?
#' @param decreasing A \code{\link{logical}} scalar: should a decreasing
#'  clipping window be used?
#' @param iterations An \code{\link{integer}} value giving the numerber of
#'  iterations.
#' @return A \code{numeric} vector.
#' @author N. Frerebeau
#' @references
#'  Morháč, M., Kliman, J., Matoušek, V., Veselský, M. and Turzo, I. (1997).
#'  Background elimination methods for multidimensional gamma-ray spectra.
#'  \emph{Nuclear Instruments and Methods in Physics Research Section A:
#'  Accelerators, Spectrometers, Detectors and Associated Equipment}, 401(1), p. 113-132.
#'  DOI: \href{https://doi.org/10.1016/S0168-9002(97)01023-1}{10.1016/S0168-9002(97)01023-1}
#'
#'  Morháč, M. and Matoušek, V. (2008). Peak Clipping Algorithms for Background
#'  Estimation in Spectroscopic Data. \emph{Applied Spectroscopy}, 62(1), p. 91-106.
#'  DOI: \href{https://doi.org/10.1366/000370208783412762}{10.1366/000370208783412762}
#'
#'  Ryan, C. G., Clayton, E., Griffin, W. L., Sie, S. H. and Cousens, D. R.
#'  (1988). SNIP, a statistics-sensitive background treatment for the
#'  quantitative analysis of PIXE spectra in geoscience applications.
#'  \emph{Nuclear Instruments and Methods in Physics Research Section B:
#'  Beam Interactions with Materials and Atoms}, 34(3), p. 396-402.
#'  DOI: \href{https://doi.org/10.1016/0168-583X(88)90063-8}{10.1016/0168-583X(88)90063-8}
#' @keywords internal
SNIP <- function(x, LLS = FALSE, decreasing = FALSE, iterations = 100) {
  # Validation
  if (!is.atomic(x) | !is.numeric(x))
    stop("A numeric vector is expected.")
  m <- as.integer(iterations)[[1L]]

  # LLS operator
  x <- if (LLS) LLS(x) else x

  n <- length(x)
  iter <- if (decreasing) rev(seq_len(m)) else seq_len(m)

  y <- x
  for (p in iter) {
    for (i in p:(n-p)) {
      a <- x[i]
      b <- (x[i - p] + x[i + p]) / 2
      y[i] <- min(a, b)
    }
    x <- y
  }

  # Inverse LLS operator
  x <- if (LLS) inverseLLS(x) else x

  return(x)
}

#' @rdname SNIP
#' @keywords internal
#' @noRd
LLS <- function(x) {
  log(log(sqrt(x + 1) + 1) + 1)
}
#' @rdname SNIP
#' @keywords internal
#' @noRd
inverseLLS <- function(x) {
  (exp(exp(x) - 1) - 1)^2 - 1
}
