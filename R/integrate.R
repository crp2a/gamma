# INTEGRATE SIGNAL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname integrateSignal
#' @aliases integrateSignal,GammaSpectra-method
setMethod(
  f = "integrateSignal",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, range = c(200, 2800),
                        peaks = c(238, 1461, 2614.5),
                        noise = NULL, ...) {
    spectra <- as.list(object)
    signals <- lapply(X = spectra, FUN = integrateSignal,
                      range = range, peaks = peaks, noise = noise, ...)
    return(signals)
  }
)

#' @export
#' @rdname integrateSignal
#' @aliases integrateSignal,GammaSpectrum-method
setMethod(
  f = "integrateSignal",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, range = c(200, 2800),
                        peaks = c(238, 1461, 2614.5),
                        noise = NULL, ...) {
    # Validation
    if (!is.numeric(range) | length(range) != 2)
      stop("'range' must be a length two numeric value (energy range in keV).")
    if (!is.numeric(peaks) | length(peaks) != 3)
      stop("'peaks' must be a length three numeric value (expected peaks energy in keV).")

    # Get spectrum data
    spc_data <- methods::as(object, "data.frame")
    # Remove baseline
    spc_clean <- removeBaseline(object)

    # Adjust spectrum for energy shift
    ## Fit peaks corresponding to 238 keV, 1461 keV and 2614.5 keV
    peaks_index <- fitPeaks(spc_clean, peaks = peaks)
    ## Get corresponding chanels
    fit_data <- data.frame(energy = peaks_index@peaks$energy,
                           chanel = peaks_index@peaks$chanel)
    ## Fit second order polynomial
    fit_poly <- stats::lm(chanel ~ stats::poly(energy, degree = 2, raw = TRUE),
                          data = fit_data)
    ## Get chanels corresponding to 200 keV, 2800 keV
    bound_chanel <- round(stats::predict(fit_poly, data.frame(energy = range)))
    seq_chanel <- which(spc_data$chanel >= bound_chanel[1] &
                          spc_data$chanel <= bound_chanel[2])

    # Integrate signal between boundaries
    int_signal <- crossprod(spc_data$energy[seq_chanel],
                            spc_data$counts[seq_chanel])
    int_signal %<>% as.numeric()

    # Normalize integrated signal to time
    active_time <- object@live_time
    norm_signal <- int_signal / active_time
    norm_error <- 1 # FIXME

    # If noise value is known, substract it form normalized signal
    if (!is.null(noise)) {
      # Validation
      if (length(noise) != 2)
        stop("'noise' must be a list of two elements ('value' and 'error')")
      if (!all.equal(lengths(noise), c(value = 1, error = 1)))
        stop("'noise' must be a list of two elements ('value' and 'error')")

      # Net signal (substracted background noise)
      net_signal <- norm_signal - noise$value
      net_error <- sqrt((sqrt(2 * int_signal) / active_time)^2 + noise$error^2)

      return(list(value = net_signal, error = net_error))
    } else {
      # return(list(nls = nls_fit, poly = poly_fit, peaks = energy))
      return(list(value = norm_signal, error = norm_error))
    }
  }
)
