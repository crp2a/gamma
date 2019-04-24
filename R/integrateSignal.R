# INTEGRATE SIGNAL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname integrateSignal
#' @aliases integrateSignal,GammaSpectra-method
setMethod(
  f = "integrateSignal",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, range = c(200, 2800), noise = NULL, NiEi = TRUE,
                        ..., simplify = FALSE) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    signals <- sapply(X = spectra, FUN = integrateSignal,
                      range = range, noise = noise, NiEi = NiEi,
                      simplify = simplify)
    return(signals)
  }
)

#' @export
#' @rdname integrateSignal
#' @aliases integrateSignal,GammaSpectrum-method
setMethod(
  f = "integrateSignal",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, range = c(200, 2800), noise = NULL, NiEi = TRUE, ...) {
    # Validation
    if (!is.numeric(range) | length(range) != 2)
      stop(sprintf("%s must be a numeric vector of length two, not %d",
                   sQuote("range"), length(range)))

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

    # If noise value is known, substract it form normalized signal
    if (!is.null(noise)) {
      # Validation
      if (!is.numeric(noise) | length(noise) != 2)
        stop(sprintf("%s must be a numeric vector of length two, not %d",
                     sQuote("noise"), length(noise)))

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
