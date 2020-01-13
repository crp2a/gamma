# INTEGRATE SIGNAL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectrum-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, range, background = NULL,
                        threshold = c("Ni", "NiEi"), ...) {
    # Validation
    threshold <- match.arg(threshold, several.ok = FALSE)
    if (!is.numeric(range) || length(range) != 2)
      stop(sprintf("`range` must be a numeric vector of length 2, not %d.",
                   length(range)), call. = FALSE)
    if (!is.null(background)) {
      if (!is.numeric(background) || length(background) != 2)
        stop(sprintf("`background` must be a numeric vector of length 2, not %d.",
                     length(background)), call. = FALSE)
    } else {
      background <- c(0, 0)
    }
    if (!is_calibrated(object))
      stop("You must calibrate the energy scale of your spectrum first.",
           call. = FALSE)

    # Get data
    spc_data <- methods::as(object, "data.frame")

    # Integrate signal between boundaries
    int_index <- which(spc_data$energy >= range[[1L]] &
                         spc_data$energy <= range[[2L]])
    int_signal <- c(
      Ni = sum(spc_data$count[int_index]),
      NiEi = crossprod(spc_data$energy[int_index], spc_data$count[int_index])
    )
    signal2net <- function(signal, live_time, background_value, background_error) {
      # Normalize integrated signal to time
      norm_signal <- signal / live_time
      norm_error <- sqrt(2 * signal) / live_time
      # Compute net signal (substracted background background)
      net_signal <- norm_signal - background_value
      net_error <- sqrt(norm_error^2 + background_error^2)

      c(signal = net_signal, error = net_error)
    }

    # Normalize integrated signal to time
    net <- vapply(
      X = int_signal,
      FUN = signal2net,
      FUN.VALUE = numeric(2),
      live_time = object@live_time,
      background_value = background[[1L]],
      background_error = background[[2L]]
    )
    net <- as.data.frame(net)[[threshold]]
    names(net) <- c(paste0(threshold, "_signal"), paste0(threshold, "_error"))
    return(net)
  }
)

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectra-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, range, background = NULL,
                        threshold = c("Ni", "NiEi"), simplify = FALSE, ...) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    signals <- lapply(X = spectra, FUN = integrate_signal,
                      range = range, background = background,
                      threshold = threshold)
    if (simplify) {
      do.call(rbind, signals)
    } else {
      signals
    }
  }
)
