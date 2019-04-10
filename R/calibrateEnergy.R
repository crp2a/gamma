# CALIBRATE SPECTRUM (ENERGY)
#' @include AllGenerics.R
NULL

#' @export
#' @rdname calibrateEnergy
#' @aliases calibrateEnergy,GammaSpectra-method
setMethod(
  f = "calibrateEnergy",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, lines, force = FALSE, ...) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    shifted <- lapply(X = spectra, FUN = calibrateEnergy,
                      lines = lines, force = force, ...)
    names(shifted) <- sapply(X = shifted, FUN = "[[", i = "reference")
    methods::new("GammaSpectra", shifted)
  }
)

#' @export
#' @rdname calibrateEnergy
#' @aliases calibrateEnergy,GammaSpectrum,list-method
setMethod(
  f = "calibrateEnergy",
  signature = signature(object = "GammaSpectrum", lines = "list"),
  definition = function(object, lines, force = FALSE, ...) {
    # Get calibration lines
    lines_chanel <- sapply(X = lines, FUN = "[", i = 1)
    lines_energy <- sapply(X = lines, FUN = "[", i = 2)

    # Get spectrum data
    spc_data <- methods::as(object, "data.frame")
    # Remove baseline
    spc_clean <- removeBaseline(object, ...)

    # Adjust spectrum for energy shift
    ## Fit peaks
    if (!force) {
      peaks <- fitPeaks(spc_clean, peaks = lines_chanel, scale = "chanel")
      lines_chanel <- peaks@peaks$chanel
    }
    ## Get corresponding chanels
    fit_data <- data.frame(energy = lines_energy,
                           chanel = lines_chanel)
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
