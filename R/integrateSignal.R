# INTEGRATE SIGNAL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname integrateSignal
#' @aliases integrateSignal,GammaSpectra-method
setMethod(
  f = "integrateSignal",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, range = c(200, 2800), noise = NULL, ...) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    signals <- lapply(X = spectra, FUN = integrateSignal,
                      range = range, noise = noise, ...)
    return(signals)
  }
)

#' @export
#' @rdname integrateSignal
#' @aliases integrateSignal,GammaSpectrum-method
setMethod(
  f = "integrateSignal",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, range = c(200, 2800), noise = NULL, ...) {
    # Validation
    if (!is.numeric(range) | length(range) != 2)
      stop("'range' must be a length two numeric vector (integration range in keV).")

    # Get data
    spc_data <- methods::as(object, "data.frame")

    # Integrate signal between boundaries
    int_index <- which(spc_data$energy >= range[1] &
                         spc_data$energy <= range[2])
    int_signal <- crossprod(spc_data$energy[int_index],
                            spc_data$counts[int_index])
    int_signal %<>% as.numeric()

    # Normalize integrated signal to time
    active_time <- object@live_time
    norm_signal <- int_signal / active_time
    norm_error <- sqrt(2 * int_signal) / active_time

    # If noise value is known, substract it form normalized signal
    if (!is.null(noise)) {
      # Validation
      if (length(noise) != 2)
        stop("'noise' must be a length-two numeric vector.")

      names(noise) <- NULL
      # Net signal (substracted background noise)
      net_signal <- norm_signal - noise[1]
      net_error <- sqrt(norm_error^2 + noise[2]^2)

      return(c(value = net_signal, error = net_error))
    } else {
      # return(list(nls = nls_fit, poly = poly_fit, peaks = energy))
      return(c(value = norm_signal, error = norm_error))
    }
  }
)
