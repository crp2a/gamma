# PREDICATES
#' @include AllClasses.R
NULL

# ================================================================ GammaSpectrum
#' @export
#' @rdname predicates
#' @aliases is_calibrated,GammaSpectrum-method
setMethod(
  f = "is_calibrated",
  signature = "GammaSpectrum",
  definition = function(object) length(object@energy != 0)
)

# ================================================================= GammaSpectra
#' @export
#' @rdname predicates
#' @aliases is_calibrated,GammaSpectra-method
setMethod(
  f = "is_calibrated",
  signature = "GammaSpectra",
  definition = function(object) {
    vapply(object, FUN = is_calibrated, FUN.VALUE = logical(1))
  }
)
