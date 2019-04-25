# ESTIMATE DOSE RATE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname predict
#' @aliases predict,CalibrationCurve-method
setMethod(
  f = "predict",
  signature = signature(object = "CalibrationCurve"),
  definition = function(object, spectra, epsilon = 0.03, ...) {

    # Get noise value and integration range
    noise <- object@noise
    int_range <- object@integration

    if (missing(spectra)) {
      new_data <- methods::as(object@data, "data.frame")
    } else {
      # Validation
      if (!methods::is(spectra, "GammaSpectra")) {
        if (methods::is(spectra, "GammaSpectrum")) {
          spectra <- methods::as(spectra, "GammaSpectra")
        } else {
          stop(sprintf("%s must be a %s or %s object.",
                       sQuote("spectra"), sQuote("GammaSpectrum"),
                       sQuote("GammaSpectra")))
        }
      }
      # Integrate spectra
      new_data <- integrateSignal(spectra, range = int_range, noise = noise) %>%
        do.call(rbind, .) %>%
        as.data.frame() %>%
        dplyr::mutate(reference = rownames(.)) %>%
        dplyr::rename(signal_value = "value", signal_error = "error") %>%
        dplyr::mutate(signal_value = as.numeric(.data$signal_value),
                      signal_error = as.numeric(.data$signal_error))
    }

    # Get linear regression results
    fit <- object@model
    fit_coef <- summary(object@model)$coef
    slope <- fit_coef["signal_value", "Estimate"]
    slope_error <- fit_coef["signal_value", "Std. Error"]

    dose_value <- stats::predict.lm(fit, new_data[, "signal_value", drop = FALSE])

    dose_error <- dose_value * sqrt((slope_error / slope)^2 +
                                (new_data$signal_error / new_data$signal_value)^2 +
                                epsilon^2)

    methods::new("DoseRate",
                 reference = as.character(new_data$reference),
                 dose_value = dose_value,
                 dose_error = dose_error,
                 signal_value = new_data$signal_value,
                 signal_error = new_data$signal_error)
  }
)
