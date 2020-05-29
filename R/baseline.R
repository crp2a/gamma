# ESTIMATE AND REMOVE BASELINE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname baseline
#' @aliases baseline,GammaSpectrum-method
setMethod(
  f = "baseline",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("SNIP", "rubberband"), ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)

    fun <- switch (
      method,
      SNIP = baseline_snip,
      rubberband = baseline_rubberband,
      stop("There is no such method: ", method, call. = FALSE)
    )
    fun(object, ...)
  }
)

#' @export
#' @rdname baseline
#' @aliases baseline,GammaSpectra-method
setMethod(
  f = "baseline",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, method = c("SNIP", "rubberband"), ...) {
    bsl <- lapply(X = object, FUN = baseline, method = method, ...)
    .GammaSpectra(bsl)
  }
)

#' @export
#' @rdname baseline
#' @aliases signal_correct,GammaSpectrum-method
setMethod(
  f = "signal_correct",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("SNIP", "rubberband"), ...) {
    bsl <- baseline(object, method = method, ...)
    object - bsl
  }
)

#' @export
#' @rdname baseline
#' @aliases signal_correct,GammaSpectra-method
setMethod(
  f = "signal_correct",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, method = c("SNIP", "rubberband"), ...) {
    bsl <- lapply(X = object, FUN = signal_correct, method = method, ...)
    .GammaSpectra(bsl)
  }
)
