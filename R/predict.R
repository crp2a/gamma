# ESTIMATE DOSE RATE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname predict
#' @aliases predict_dose,CalibrationCurve-method
setMethod(
  f = "predict_dose",
  signature = signature(object = "CalibrationCurve"),
  definition = function(object, spectra, epsilon = 0, simplify = FALSE, ...) {

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
          stop(sprintf("`spectra` must be a %s or a %s object.",
                       sQuote("GammaSpectrum"), sQuote("GammaSpectra")))
        }
      }
      # Integrate spectra
      new_data <- integrate_signal(spectra, range = int_range, noise = noise,
                                   simplify = TRUE)
      new_data <- cbind.data.frame(new_data, rownames(new_data),
                                   stringsAsFactors = FALSE)
      colnames(new_data) <- c("signal_value", "signal_error", "reference")
    }

    # Get linear regression results
    fit <- object@model
    fit_coef <- summary(object@model)$coef
    slope <- fit_coef["signal_value", "Estimate"]
    slope_error <- fit_coef["signal_value", "Std. Error"]

    dose_value <- stats::predict.lm(fit, new_data[, "signal_value", drop = FALSE])

    dose_error <- dose_value *
      sqrt((slope_error / slope)^2 +
             (new_data$signal_error / new_data$signal_value)^2 +
             epsilon^2)

    results <- cbind.data.frame(
      reference = new_data$reference,
      signal_value = new_data$signal_value,
      signal_error = new_data$signal_error,
      dose_value = dose_value,
      dose_error = dose_error,
      stringsAsFactors = FALSE
    )
    rownames(results) <- new_data$reference
    if (simplify) {
      results
    } else {
      split(results, f = new_data$reference)
    }
  }
)
