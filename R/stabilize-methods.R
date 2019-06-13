# TRANSFORM INTENSITY
#' @include AllGenerics.R
NULL

#' @export
#' @rdname stabilize
#' @aliases stabilize,GammaSpectrum-method
setMethod(
  f = "stabilize",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, transformation, ...) {
    tmp <- object
    tmp@counts <- transformation(object@counts, ...)
    tmp
  }
)
