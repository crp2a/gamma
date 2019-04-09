# CALIBRATE SPECTRUM (ENERGY)
#' @include AllGenerics.R
NULL

#' @export
#' @rdname calibrateEnergy
#' @aliases calibrateEnergy,GammaSpectra-method
setMethod(
  f = "calibrateEnergy",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, lines = c(238, 1461, 2614.5), ...) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    shifted <- lapply(X = spectra, FUN = calibrateEnergy, lines = lines, ...)
    names(shifted) <- sapply(X = shifted, FUN = "[[", i = "reference")
    methods::new("GammaSpectra", shifted)
  }
)

#' @export
#' @rdname calibrateEnergy
#' @aliases calibrateEnergy,GammaSpectrum-method
setMethod(
  f = "calibrateEnergy",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, lines = c(238, 1461, 2614.5), ...) {
    # Validation
    if (!is.numeric(lines))
      stop("'lines' must be a numeric vector (known peak positions in keV).")

    # Get spectrum data
    spc_data <- methods::as(object, "data.frame")
    # Remove baseline
    spc_clean <- removeBaseline(object)

    # Adjust spectrum for energy shift
    ## Fit peaks corresponding to 238 keV, 1461 keV and 2614.5 keV
    peaks_index <- fitPeaks(spc_clean, peaks = lines)
    ## Get corresponding chanels
    fit_data <- data.frame(energy = lines,
                           chanel = peaks_index@peaks$chanel)
    ## Fit second order polynomial
    fit_poly <- stats::lm(energy ~ stats::poly(chanel, degree = 2, raw = TRUE),
                          data = fit_data)
    ## Predict shifted energy values
    fit_spc <- stats::predict(fit_poly, spc_data[, "chanel", drop = FALSE])

    # Return a new gamma spectrum with adjusted energy
    methods::new(
      "GammaSpectrum",
      hash = object@hash,
      reference = object@reference,
      instrument = object@instrument,
      file_format = object@file_format,
      chanel = spc_data$chanel,
      energy = fit_spc,
      counts = spc_data$counts,
      live_time = object@live_time,
      real_time = object@real_time,
      calibration = fit_poly
    )
  }
)
