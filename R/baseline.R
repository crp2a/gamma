# ESTIMATE AND REMOVE BASELINE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname baseline
#' @aliases estimateBaseline,GammaSpectra-method
setMethod(
  f = "estimateBaseline",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, method = c("SNIP"), LLS = FALSE, ...) {

    baseline <- lapply(X = object, FUN = estimateBaseline,
                       method = method, LLS = LLS, ...)
    names(baseline) <- names(object)
    return(baseline)
  }
)

#' @export
#' @rdname baseline
#' @aliases estimateBaseline,GammaSpectrum-method
setMethod(
  f = "estimateBaseline",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("SNIP"), LLS = FALSE, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    # Get count data
    x <- methods::as(object, "data.frame")
    # Cut data, starting at maximum count value
    x_cut <- subset(x, x$chanel > which.max(x$counts))
    x_counts <- x_cut$counts
    # LLS operator
    counts <- if (LLS) LLS(x_counts) else x_counts

    y <- switch (
      method,
      SNIP = SNIP(counts, ...)
    )

    # Inverse LLS operator
    z <- if (LLS) inverseLLS(y) else y

    baseline <- data.frame(
      chanel = x_cut$chanel,
      energy = x_cut$energy,
      counts = x_cut$counts,
      baseline = z
    )
    return(baseline)
  }
)

#' @export
#' @rdname baseline
#' @aliases removeBaseline,GammaSpectra-method
setMethod(
  f = "removeBaseline",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, ...) {

    baseline <- lapply(X = object, FUN = removeBaseline, ...)
    names(baseline) <- names(object)

    methods::new("GammaSpectra", baseline)
  }
)

#' @export
#' @rdname baseline
#' @aliases removeBaseline,GammaSpectrum-method
setMethod(
  f = "removeBaseline",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, ...) {

    baseline <- estimateBaseline(object, ...)

    methods::new(
      "GammaSpectrum",
      reference = object@reference,
      date = object@date,
      instrument = object@instrument,
      file_format = object@file_format,
      chanel = baseline$chanel,
      energy = baseline$energy,
      counts = baseline$counts - baseline$baseline,
      live_time = object@live_time,
      real_time = object@real_time
    )
  }
)

#' SNIP algorithm
#'
#' @param x A \code{\link{numeric}} vector.
#' @param decreasing A \code{\link{logical}} scalar: should a decreasing
#'  clipping window be used?
#' @param m An \code{\link{integer}} value giving the numerber of iterations.
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
#' @noRd
SNIP <- function(x, decreasing = FALSE, m = 100) {
  # Validation
  if (!is.vector(x) | !is.numeric(x))
    stop("A numeric vector is expected.")
  m <- as.integer(m)

  N <- length(x)
  y <- vector(mode = "numeric", length = N)
  clip <- if (decreasing) m:1 else 1:m

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
#' @noRd
LLS <- function(x) {
  log(log(sqrt(x + 1) + 1) + 1)
}
inverseLLS <- function(x) {
  (exp(exp(x) - 1) - 1)^2 - 1
}
