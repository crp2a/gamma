# ESTIMATE DOSE RATE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname predict
#' @aliases predict,CalibrationCurve-method
setMethod(
  f = "predict",
  signature = signature(object = "CalibrationCurve"),
  definition = function(object, spectra, epsilon = 0.03, simplify = FALSE, ...) {

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
      new_data <- integrateSignal(spectra, range = int_range, noise = noise) %>%
        do.call(rbind, .) %>%
        as.data.frame() %>%
        dplyr::transmute(reference = rownames(.),
                         signal_value = .data$value,
                         signal_error = .data$error)
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

    results <- cbind(
      value = dose_value,
      error = dose_error
    )
    rownames(results) <- new_data$reference
    if (simplify) {
      results
    } else {
      split(results, f = new_data$reference)
    }
  }
)
