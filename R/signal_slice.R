# SIGNAL SLICING
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname signal_slice
#' @aliases signal_slice,GammaSpectrum-method
setMethod(
  f = "signal_slice",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, ...) {
    index <- as.integer(c(...))
    if (length(index) == 0) {
      index <- -seq_len(which.max(object[["count"]]))
    }

    if (all(index > 0) || all(index < 0)) {
      channel <- object[["channel"]][index]
      energy <- object[["energy"]][index]
      count <- object[["count"]][index]
      rate <- object[["rate"]][index]
    } else {
      stop("A vector of strictly positive of negative integers is expected.",
           call. = FALSE)
    }

    methods::initialize(object, channel = channel, energy = energy,
                        count = count, rate = rate)
  }
)

#' @export
#' @rdname signal_slice
#' @aliases signal_slice,GammaSpectra-method
setMethod(
  f = "signal_slice",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, ...) {
    spc <- lapply(X = object, FUN = signal_slice, ...)
    .GammaSpectra(spc)
  }
)
