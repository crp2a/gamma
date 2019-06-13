# FIND PEAKS
#' @include AllGenerics.R
NULL

# AUTOMATIC PEAK DETECTION =====================================================
#' @export
#' @rdname peaks
#' @aliases findPeaks,GammaSpectrum-method
setMethod(
  f = "findPeaks",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("MAD"), SNR = 2, span = NULL, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    SNR <- as.integer(SNR)[[1L]]

    # Get count data
    spc <- methods::as(object, "data.frame")
    counts <- spc[["counts"]]
    if (is.null(span)) span <- round(length(counts) * 0.05)
    span <- as.integer(span)[[1L]]

    shape <- diff(sign(diff(counts, na.pad = FALSE)))
    index_shape <- lapply(
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

    pks <- spc[index_noise, ]
    rownames(pks) <- paste0("peak #", seq_len(nrow(pks)))

    .PeakPosition(
      hash = object@hash,
      noise_method = method,
      noise_threshold = threshold,
      window = span,
      chanel = pks[["chanel"]],
      energy = pks[["energy"]]
    )
  }
)

# PEAK FITTING =================================================================
#' @export
#' @rdname peaks
#' @aliases fitPeaks,GammaSpectrum,numeric-method
setMethod(
  f = "fitPeaks",
  signature = signature(object = "GammaSpectrum", peaks = "numeric"),
  definition = function(object, peaks, bounds = NULL, ...) {
    # Remove baseline
    baseline <- estimateBaseline(object, ...)
    spc_clean <- object - baseline
    # Get spectrum data
    spc <- methods::as(spc_clean, "data.frame")

    # Find peaks in spectrum data
    pks <- spc[which(spc$chanel %in% peaks), , drop = FALSE]
    rownames(pks) <- paste0("peak #", seq_len(nrow(pks)))
    # Fit peaks at given postion
    fit <- apply(
      X = pks,
      MARGIN = 1,
      FUN = function(peaks, spectrum, bounds) {
        tryCatch(
          error = conditionMessage,
          fitNLS(x = spectrum, peaks = peaks, scale = "chanel", bounds = bounds)
        )
      },
      spectrum = spc, bounds = bounds
    )
    # Find errors, if any
    isNotNLS <- function(x) !inherits(x, "nls")
    errors <- detect(isNotNLS, fit)
    if (any(errors)) {
      # Remove errors, if any
      fit_clean <- compact(isNotNLS, fit)
      if (isEmpty(fit_clean)) {
        stop("What a failure! No peaks could be fitted correctly.",
             call. = FALSE)
      } else {
        warning(
          "Something goes wrong, the following peaks were skipped:\n",
          paste0("* ", names(fit)[errors], ": ", fit[errors], collapse = "\n"),
          call. = FALSE
        )
      }
    } else {
      fit_clean <- fit
    }
    # Find peaks in spectrum data
    param <- do.call(rbind, lapply(X = fit_clean, FUN = stats::coef))

    .PeakModel(
      model = fit_clean,
      coefficients = param,
      spectrum = object,
      baseline = baseline
    )
  }
)

#' NLS
#'
#' Determine the nonlinear least-squares estimates of the peaks parameters.
#' @param x A \code{\link[=data.frame]{data frame}}.
#' @param peaks A \code{\link{numeric}} vector.
#' @param scale A \code{\link{character}} string specifying the scale of
#'  \code{peaks}. It must be one of "\code{chanel}" (the default) or
#'  "\code{energy}". Any unambiguous substring can be given.
#' @param bounds A \code{\link{numeric}} vector.
#' @param ... Currently not used.
#' @return A \linkS4class{PeakModel} object.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
fitNLS <- function(x, peaks, scale = c("chanel", "energy"),
                   bounds = NULL, ...) {
  # Validation
  scale <- match.arg(scale, several.ok = FALSE)
  if (!is.numeric(peaks))
    stop("`peaks` must be a numeric vector.", call. = FALSE)
  if (!all(c(scale, "counts") %in% names(peaks)))
    stop("`peaks` is a numeric vector, but does not have components '", scale,
         "' and 'counts'.", call. = FALSE)

  # Get starting values for each peak
  ## Mean
  mean_start <- peaks[scale]
  ## Standart deviation
  fwhm <- vapply(
    X = mean_start,
    FUN = function(i, x, y) FWHM(x = x, y = y, center = i),
    FUN.VALUE = numeric(1),
    x = x[, scale], y = x[, "counts"]
  )
  sd_start <- fwhm / (2 * sqrt(2 * log(2)))
  ## Height
  height <- peaks["counts"]
  ## Check starting values
  if (sd_start == 0)
    stopCustom("nls_wrong_start_value",
               "This peak has a zero standard deviation.")
  if (height == 0)
    stopCustom("nls_wrong_start_value",
               "This peak has a zero height.")

  parameters <- c(mean_start, sd_start, height)
  names(parameters) <- c("mean", "sd", "height")

  # Lower and upper paramters bounds
  lower_bounds <- upper_bounds <- NULL
  if (is.numeric(bounds)) {
    n_bounds <- length(bounds)
    n_param <- length(parameters)
    if (n_bounds != 1 && n_bounds != n_param)
      stop(sprintf("`bounds` must be of length 1 or %d, not %d.",
                   n_param, n_bounds), call. = FALSE)
    # if (any(bounds > 1))
    #   stop(sprintf("%s between 0 and 1", sQuote("bounds")))
    lower_bounds <- parameters * (1 - bounds)
    upper_bounds <- parameters * (1 + bounds)
  }

  # Build formula
  term_labels <- sprintf("height * exp(-0.5 * ((%s - mean) / sd)^2)", scale)
  fit_formula <- stats::reformulate(termlabels = term_labels,
                                    response = "counts")

  # Fit model
  fit_control <- stats::nls.control(maxiter = 50,
                                    tol = 1e-05,
                                    minFactor = 1/1024,
                                    printEval = FALSE,
                                    warnOnly = FALSE)
  fit <- stats::nls(formula = fit_formula,
                    data = x[, c(scale, "counts")],
                    start = as.list(parameters),
                    control = fit_control,
                    algorithm = "port",
                    lower = lower_bounds,
                    upper = upper_bounds)

  # TODO: Check parameters
  # mean_end <- stats::coef(fit)[["mean"]]
  # if (abs(mean_end - mean_start))
  #   stopCustom("nls_wrong_end_value",
  #              "The difference between the starting and adjusted position is greater than 10 channels.")

  return(fit)
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
    if (is.list(z)) {
      x <- z[[1]]
      y <- z[[2]]
    }
    if (is.matrix(z) | is.data.frame(z)) {
      x <- z[, 1]
      y <- z[, 2]
    }
  } else {
    if (length(x) != length(y))
      stop("`x` and `y` lengths differ.", call. = FALSE)
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
