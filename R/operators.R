# OPPERATORS
#' @include AllGenerics.R
NULL

#' Operators
#'
#' Performs operation on spectra.
#' @param e1,e2 A \linkS4class{GammaSpectrum} object.
#' @return A \linkS4class{GammaSpectrum} object.
#' @author N. Frerebeau
#' @docType methods
#' @family IO
#' @name operators
#' @rdname operators
#' @aliases operators-method
NULL

#' @export
#' @rdname operators
#' @aliases -,GammaSpectrum,GammaSpectrum-method
setMethod(
  f = "-",
  signature(e1 = "GammaSpectrum", e2 = "GammaSpectrum"),
  definition = function (e1, e2) {
    # Validation
    # TODO

    methods::initialize(e1, count = e1@count - e2@count)
  }
)

#' @export
#' @rdname operators
#' @aliases +,GammaSpectrum,GammaSpectrum-method
setMethod(
  f = "+",
  signature(e1 = "GammaSpectrum", e2 = "GammaSpectrum"),
  definition = function (e1, e2) {
    # Validation
    # TODO

    methods::initialize(e1, count = e1@count + e2@count)
  }
)
