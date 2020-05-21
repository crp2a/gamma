# INTEGRATE SIGNAL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectrum,missing-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectrum", background = "missing"),
  definition = function(object, range, energy = FALSE) {
    # Validation
    if (!is.numeric(range) || length(range) != 2)
      stop(sprintf("%s must be a numeric vector of length 2, not %d.",
                   sQuote("range"), length(range)), call. = FALSE)
    if (!is_calibrated(object))
      stop("You must calibrate the energy scale of your spectrum first.",
           call. = FALSE)

    # Get data
    spc_data <- methods::as(object, "data.frame")

    # Integrate signal between boundaries
    spc_index <- which(spc_data$energy >= range[[1L]] &
                         spc_data$energy <= range[[2L]])
    if (energy) {
      int_spc <- integrate(spc_data$count, spc_data$energy, spc_index)
    } else {
      int_spc <- integrate(spc_data$count, index = spc_index)
    }

    # Normalize integrated signal to time
    live_time <- get_livetime(object)
    norm_signal <- int_spc / live_time
    norm_error <- sqrt(2 * int_spc) / live_time

    c(value = norm_signal, error = norm_error)
  }
)
#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectrum,GammaSpectrum-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectrum", background = "GammaSpectrum"),
  definition = function(object, background, range, energy = FALSE) {
    # Normalized signal
    int_bkg <- integrate_signal(background, range = range, energy = energy)

    integrate_signal(object, background = int_bkg, range = range,
                     energy = energy)
  }
)
#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectrum,numeric-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectrum", background = "numeric"),
  definition = function(object, background, range, energy = FALSE) {
    # Validation
    if (length(background) != 2)
      stop(sprintf("%s must be of length 2; not %d", sQuote("background"),
                   length(background)), call. = FALSE)

    # Normalized signal
    int_spc <- integrate_signal(object, range = range, energy = energy)

    # Compute net signal (substracted background background)
    net_signal <- int_spc[[1L]] - background[[1L]]
    net_error <- sqrt(int_spc[[2L]]^2 + background[[2L]]^2)

    c(value = net_signal, error = net_error)
  }
)

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectra,missing-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectra", background = "missing"),
  definition = function(object, range, energy = FALSE, simplify = TRUE) {

    signals <- lapply(X = object, FUN = integrate_signal,
                      range = range, energy = energy)
    if (simplify) do.call(rbind, signals) else signals
  }
)

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectra,GammaSpectrum-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectra", background = "GammaSpectrum"),
  definition = function(object, background, range, energy = FALSE,
                        simplify = TRUE) {

    signals <- lapply(X = object, FUN = integrate_signal,
                      background = background, range = range, energy = energy)
    if (!simplify) return(signals)
    do.call(rbind, signals)
  }
)

#' @export
#' @rdname integrate
#' @aliases integrate_signal,GammaSpectra,numeric-method
setMethod(
  f = "integrate_signal",
  signature = signature(object = "GammaSpectra", background = "numeric"),
  definition = function(object, background, range, energy = FALSE,
                        simplify = TRUE) {

    signals <- lapply(X = object, FUN = integrate_signal,
                      background = background, range = range, energy = energy)
    if (!simplify) return(signals)
    do.call(rbind, signals)
  }
)

integrate <- function(count, energy, index) {
  if (missing(energy)) sum(count[index])
  else crossprod(energy[index], count[index])
}
