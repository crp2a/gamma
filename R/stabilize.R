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
    count <- transformation(object@counts, ...)
    methods::initialize(object, counts = count)
  }
)
