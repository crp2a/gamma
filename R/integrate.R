# INTEGRATE SIGNAL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectra,numeric,numeric-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectra", range = "numeric",
                        noise = "numeric"),
  definition = function(object, range, noise, NiEi = TRUE,
                        simplify = FALSE, ...) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    signals <- lapply(X = spectra, FUN = integrate_signal,
                      range = range, noise = noise, NiEi = NiEi)
    if (simplify) {
      do.call(rbind, signals)
    } else {
      signals
    }
  }
)

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectra,numeric,missing-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectra", range = "numeric",
                        noise = "missing"),
  definition = function(object, range, NiEi = TRUE, simplify = FALSE, ...) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    signals <- lapply(X = spectra, FUN = integrate_signal,
                      range = range, NiEi = NiEi)
    if (simplify) {
      do.call(rbind, signals)
    } else {
      signals
    }
  }
)

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectra,numeric,GammaSpectrum-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectra", range = "numeric",
                        noise = "GammaSpectrum"),
  definition = function(object, range, noise,
                        NiEi = TRUE, simplify = FALSE, ...) {
    # Compute noise value
    noise_value <- integrate_signal(noise, range = range, NiEi = NiEi)
    # Integrate spectra
    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    signals <- lapply(X = spectra, FUN = integrate_signal,
                      range = range, noise = noise_value, NiEi = NiEi)

    if (simplify) {
      do.call(rbind, signals)
    } else {
      signals
    }
  }
)

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectrum,numeric,missing-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectrum", range = "numeric",
                        noise = "missing"),
  definition = function(object, range, NiEi = TRUE, ...) {
    # Validation
    if (!is.numeric(range) || length(range) != 2)
      stop(sprintf("`range` must be a numeric vector of length two, not %d",
                   length(range)), call. = FALSE)
    if (length(object@energy) == 0)
      stop("You must calibrate the energy scale of your spectrum first.",
           call. = FALSE)

    # Get data
    spc_data <- methods::as(object, "data.frame")

    # Integrate signal between boundaries
    int_index <- which(spc_data$energy >= range[1] &
                         spc_data$energy <= range[2])
    if (NiEi) {
      int_signal <- as.numeric(crossprod(spc_data$energy[int_index],
                                         spc_data$counts[int_index]))
    } else {
      int_signal <- sum(spc_data$counts[int_index])
    }

    # Normalize integrated signal to time
    active_time <- object@live_time
    norm_signal <- int_signal / active_time
    norm_error <- sqrt(2 * int_signal) / active_time

    # list(nls = nls_fit, poly = poly_fit, peaks = energy)
    c(value = norm_signal, error = norm_error)
  }
)

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectrum,numeric,numeric-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectrum", range = "numeric",
                        noise = "numeric"),
  definition = function(object, range, noise, NiEi = TRUE, ...) {
    # Validation
    if (!is.numeric(noise) || length(noise) != 2)
      stop(sprintf("`noise` must be a numeric vector of length two, not %d",
                   length(noise)), call. = FALSE)

    # Compute normalized signal
    signal <- integrate_signal(object = object, range = range, NiEi = NiEi, ...)

    names(signal) <- names(noise) <- NULL
    # Compute net signal (substracted background noise)
    net_signal <- signal[1] - noise[1]
    net_error <- sqrt(signal[2]^2 + noise[2]^2)

    c(value = net_signal, error = net_error)
  }
)

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectrum,numeric,GammaSpectrum-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectrum", range = "numeric",
                        noise = "GammaSpectrum"),
  definition = function(object, range, noise, NiEi = TRUE, ...) {
    # Compute noise value
    noise_value <- integrate_signal(noise, range = range, NiEi = NiEi)
    # Integrate spectrum
    signal_value <- integrate_signal(object, range = range, noise = noise_value,
                                    NiEi = NiEi)
    signal_value
  }
)
