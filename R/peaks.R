# FIND PEAKS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname peaks
#' @aliases findPeaks,GammaSpectrum-method
setMethod(
  f = "findPeaks",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("MAD"), SNR = 2, span = NULL, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    SNR <- as.integer(SNR)

    # Get count data
    data <- methods::as(object, "data.frame")
    counts <- data$counts
    span <- if (is.null(span)) round(length(counts) * 0.05) else span

    shape <- diff(sign(diff(counts, na.pad = FALSE)))
    index_shape <- sapply(
      X = which(shape < 0),
      FUN = function(i, data, span) {
        n <- length(data)
        z <- i - span + 1
        z <- ifelse(z > 0, z, 1)
        w <- i + span + 1
        w <- ifelse(w < n, w, n)
        if (all(data[c(z:i, (i + 2):w)] <= data[i + 1])) {
          return(i + 1)
        } else {
          return(numeric(0))
        }
      },
      data = counts,
      span = span
    )

    noise <- switch (
      method,
      MAD = MAD(counts, ...)
    )
    threshold <- noise * SNR
    index_noise <- index_shape %>%
      unlist() %>%
      subset(., counts[.] >= threshold)

    pks <- data[index_noise, ]
    rownames(pks) <- NULL

    methods::new(
      "PeakPosition",
      method = method,
      noise = threshold,
      window = span,
      peaks = pks,
      spectrum = object
    )
  }
)

#' @export
#' @rdname peaks
#' @aliases fitPeaks,GammaSpectrum-method
setMethod(
  f = "fitPeaks",
  signature = signature(object = "GammaSpectrum", peaks = "numeric"),
  definition = function(object, peaks, ...) {
    # Get spectrum data
    spc <- methods::as(object, "data.frame")

    # Find peaks in spectrum data
    pks_index <- findClosest(spc$energy, peaks)
    pks <- spc[pks_index, ]
    rownames(pks) <- NULL

    fitNLS(object, pks)
  }
)

#' @export
#' @rdname peaks
#' @aliases fitPeaks,PeakPosition,missing-method
setMethod(
  f = "fitPeaks",
  signature = signature(object = "PeakPosition", peaks = "missing"),
  definition = function(object, ...) {
    # Get data
    spc <- object@spectrum
    pks <- object@peaks

    fitNLS(spc, pks)
  }
)

#' NLS
#'
#' Determine the nonlinear least-squares estimates of the peaks parameters.
#' @param x A \linkS4class{GammaSpectrum} object.
#' @param peaks A \code{\link[=data.frame]{data frame}}.
#' @param ... Currently not used.
#' @return A \linkS4class{PeakModel} object.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
fitNLS <- function(object, peaks, ...) {
    # Get data
    spc <- methods::as(object, "data.frame")

    # Get starting values for each peak
    ## Mean
    mu <- peaks$energy
    names(mu) <- paste("mu", 1:length(mu), sep = "")
    ## Standart deviation
    fwhm <- sapply(X = peaks$energy,
                   FUN = function(i, x, y) FWHM(x = x, y = y, center = i),
                   x = spc$energy, y = spc$counts)
    sigma <- fwhm / (2 * sqrt(2 * log(2)))
    names(sigma) <- paste("sigma", 1:length(sigma), sep = "")
    ## Height
    height <- peaks$counts
    names(height) <- paste("C", 1:length(height), sep = "")

    parameters <- as.list(c(mu, sigma, height))

    # Build formula
    n <- 1:nrow(peaks)
    term_labels <- sprintf("C%d * exp(-0.5 * ((energy - mu%d) / sigma%d)^2)",
                           n, n, n)
    fit_formula <- stats::reformulate(termlabels = term_labels,
                                      response = "counts")

    # Fit model
    fit <- stats::nls(formula = fit_formula,
                      data = spc[, c("energy", "counts")],
                      start = parameters,
                      algorithm = "port")

    # Find peaks in spectrum data
    fit_mu <- stats::coef(fit)[n]
    pks_index <- findClosest(spc$energy, fit_mu)
    pks <- spc[pks_index, ]
    rownames(pks) <- NULL

    methods::new(
      "PeakModel",
      model = fit,
      peaks = pks,
      spectrum = object
    )
  }

#' MAD
#'
#' Calculates the median absolute deviation.
#' @param x A \code{\link{numeric}} vector.
#' @param k A \code{\link{numeric}} value.
#' @param na.rm A \code{\link{logical}} scalar.
#' @return A \code{numeric} value.
#' @author N. Frerebeau
#' @keywords internal
MAD <- function(x, k = 1.4826, na.rm = FALSE) {
  k * stats::median(abs(x - stats::median(x, na.rm = na.rm)), na.rm = na.rm)
}

#' FWHM
#'
#' Estimates the half-width at half-maximum for a given peak.
#' @param x,y A \code{\link{numeric}} vector giving the \eqn{x} and \eqn{y}
#'  coordinates of a set of points. Alternatively, a single argument \eqn{x}
#'  can be provided.
#' @param center A \code{\link{numeric}} value giving the peak position in
#'  \code{x} units.
#' @return A \code{numeric} value.
#' @details
#'  It tries to get the smallest possible estimate.
#' @author N. Frerebeau
#' @keywords internal
FWHM <- function(x, y, center) {
  if (missing(y)) {
    z <- x
    if (is.list(z) & c("x", "y") %in% names(z)) {
      x <- z[["x"]]
      y <- z[["y"]]
    }
    if (is.matrix(z) | is.data.frame(z)) {
      x <- z[, 1]
      y <- z[, 2]
    }
  } else {
    if (length(x) != length(y))
      stop("'x' and 'y' lengths differ.")
  }

  i <- which(x == center)
  peak_height <- y[i]
  scale_for_roots <- y - peak_height / 2
  root_indices <- which(diff(sign(scale_for_roots)) != 0)

  tmp <- c(root_indices, i) %>% sort()
  k <- which(tmp == i)

  root_left <- root_indices[k - 1]
  root_right <- root_indices[k]

  HWHM_left <- x[i] - x[root_left]
  HWHM_right <- x[root_right] - x[i]

  FWHM <- 2 * min(c(HWHM_left, HWHM_right))
  return(FWHM)
}
