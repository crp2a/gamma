# INTEGRATE SIGNAL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectrum-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, range, noise = NULL, NiEi = TRUE, ...) {
    # Validation
    if (!is.numeric(range) || length(range) != 2)
      stop(sprintf("`range` must be a numeric vector of length 2, not %d.",
                   length(range)), call. = FALSE)
    if (!is.null(noise)) {
      if (!is.numeric(noise) || length(noise) != 2)
        stop(sprintf("`noise` must be a numeric vector of length 2, not %d.",
                     length(noise)), call. = FALSE)
    } else {
      noise <- c(0, 0)
    }
    if (!is_calibrated(object))
      stop("You must calibrate the energy scale of your spectrum first.",
           call. = FALSE)

    # Get data
    spc_data <- methods::as(object, "data.frame")

    # Integrate signal between boundaries
    int_index <- which(spc_data$energy >= range[[1L]] &
                         spc_data$energy <= range[[2L]])

    if (NiEi) {
      int_signal <- crossprod(spc_data$energy[int_index],
                              spc_data$count[int_index])
    } else {
      int_signal <- sum(spc_data$count[int_index])
    }

    # Normalize integrated signal to time
    live_time <- object@live_time
    norm_signal <- int_signal / live_time
    norm_error <- sqrt(2 * int_signal) / live_time

    # Compute net signal (substracted background noise)
    net_signal <- norm_signal - noise[[1L]]
    net_error <- sqrt(norm_error^2 + noise[[2L]]^2)

    c(value = net_signal, error = net_error)
  }
)

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectra-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, range, noise = c(0, 0), NiEi = TRUE,
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
