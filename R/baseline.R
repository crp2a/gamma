# ESTIMATE AND REMOVE BASELINE
#' @include AllGenerics.R operators.R
NULL

#' @export
#' @rdname processBaseline
#' @aliases estimateBaseline,GammaSpectra-method
setMethod(
  f = "estimateBaseline",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, method = c("SNIP"), ...) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    baseline <- lapply(
      X = spectra,
      FUN = function(x, method, ...) estimateBaseline(x, method, ...),
      method = method, ...
    )
    names(baseline) <- names(object)

    .GammaSpectra(baseline)
  }
)

#' @export
#' @rdname processBaseline
#' @aliases estimateBaseline,GammaSpectrum-method
setMethod(
  f = "estimateBaseline",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("SNIP"), ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)

    # Get count data
    x <- methods::as(object, "data.frame")
    x_counts <- x$counts

    baseline <- switch (
      method,
      SNIP = SNIP(x_counts, ...)
    )

    .BaseLine(
      hash = object@hash,
      reference = object@reference,
      date = as.POSIXct(object@date),
      instrument = object@instrument,
      file_format = object@file_format,
      chanel = x$chanel,
      energy = x$energy,
      counts = baseline,
      live_time = object@live_time,
      real_time = object@real_time
    )
  }
)

#' @export
#' @rdname processBaseline
#' @aliases removeBaseline,GammaSpectra-method
setMethod(
  f = "removeBaseline",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, method = c("SNIP"), ...) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    baseline <- lapply(
      X = spectra,
      FUN = function(x, method, ...) removeBaseline(x, method, ...),
      method = method, ...
    )
    names(baseline) <- names(object)

    methods::new("GammaSpectra", baseline)
  }
)

#' @export
#' @rdname processBaseline
#' @aliases removeBaseline,GammaSpectrum-method
setMethod(
  f = "removeBaseline",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("SNIP"), ...) {

    baseline <- estimateBaseline(object, method = method, ...)
    object - baseline
  }
)

#' SNIP algorithm
#'
#' @param x A \code{\link{numeric}} vector.
#' @param LLS A \code{\link{logical}} scalar: should the LLS operator be applied
#'  on \code{x} before employing SNIP algorithm?
#' @param decreasing A \code{\link{logical}} scalar: should a decreasing
#'  clipping window be used?
#' @param k An \code{\link{integer}} value giving the numerber of iterations.
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
SNIP <- function(x, LLS = FALSE, decreasing = FALSE, k = 100) {
  # Validation
  if (!is.vector(x) | !is.numeric(x))
    stop("A numeric vector is expected.")
  k <- as.integer(k)

  # LLS operator
  x <- if (LLS) LLS(x) else x

  N <- length(x)
  y <- vector(mode = "numeric", length = N)
  clip <- if (decreasing) k:1 else 1:k

  for (p in clip) {
    i <- p
    while (i < (N - p)) {
      a1 <- x[i]
      a2 <- (x[i - p] + x[i + p]) / 2
      y[i] <- min(a1, a2)
      i <- i + 1
    }
    x <- y
  }

  # Inverse LLS operator
  x <- if (LLS) inverseLLS(x) else x

  return(x)
}

#' LLS operator
#'
#' @param x A \code{numeric} vector.
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
#' @keywords internal
#' @name LLS
#' @rdname LLS
NULL

#' @rdname LLS
LLS <- function(x) {
  log(log(sqrt(x + 1) + 1) + 1)
}
#' @rdname LLS
inverseLLS <- function(x) {
  (exp(exp(x) - 1) - 1)^2 - 1
}
