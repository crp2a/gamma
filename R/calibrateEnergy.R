# CALIBRATE SPECTRUM (ENERGY)
#' @include AllGenerics.R
NULL

#' @export
#' @rdname calibrateEnergy
#' @aliases calibrateEnergy,GammaSpectra,data.frame-method
setMethod(
  f = "calibrateEnergy",
  signature = signature(object = "GammaSpectra", lines = "data.frame"),
  definition = function(object, lines, ...) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    energy <- lapply(X = spectra, FUN = calibrateEnergy, lines = lines, ...)
    names(energy) <- names(object)

    methods::new("GammaSpectra", energy)
  }
)

#' @export
#' @rdname calibrateEnergy
#' @aliases calibrateEnergy,GammaSpectrum,data.frame-method
setMethod(
  f = "calibrateEnergy",
  signature = signature(object = "GammaSpectrum", lines = "data.frame"),
  definition = function(object, lines, ...) {
    # Get calibration lines
    lines_chanel <- lines$chanel
    lines_energy <- lines$energy
    # Get spectrum data
    spc_data <- methods::as(object, "data.frame")

    # Adjust spectrum for energy shift
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

#' @export
#' @rdname calibrateEnergy
#' @aliases calibrateEnergy,PeakModel,numeric-method
setMethod(
  f = "calibrateEnergy",
  signature = signature(object = "PeakModel", lines = "numeric"),
  definition = function(object, lines, ...) {
    # Get data
    peaks <- object@peaks
    spectrum <- object@spectrum

    fit_data <- data.frame(energy = lines, chanel = peaks$chanel)
    calibrateEnergy(spectrum, fit_data)
  }
)
