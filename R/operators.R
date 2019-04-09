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
    if (e1@reference != e2@reference |
        e1@date != e2@date |
        e1@instrument != e2@instrument |
        e1@file_format != e2@file_format |
        e1@live_time != e2@live_time |
        e1@real_time != e2@real_time)
      stop("Spectrum and baseline do not match.")

    methods::new(
      "GammaSpectrum",
      reference = e1@reference,
      date = e1@date,
      instrument = e1@instrument,
      file_format = e1@file_format,
      chanel = e2@chanel,
      energy = e2@energy,
      counts = e1@counts[e2@chanel] - e2@counts,
      live_time = e1@live_time,
      real_time = e1@real_time
    )
  }
)
