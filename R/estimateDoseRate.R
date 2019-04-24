# ESTIMATE DOSE RATE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname estimateDoseRate
#' @aliases estimateDoseRate,GammaSpectrum-method
setMethod(
  f = "estimateDoseRate",
  signature = signature(object = "GammaSpectrum", curve = "CalibrationCurve"),
  definition = function(object, curve, epsilon = 0.03, ...) {

    # Coerce to GammaSpectra
    spc <- methods::as(object, "GammaSpectra")
    estimateDoseRate(spc, curve = curve, epsilon = epsilon, ...)
  }
)

#' @export
#' @rdname estimateDoseRate
#' @aliases estimateDoseRate,GammaSpectra-method
setMethod(
  f = "estimateDoseRate",
  signature = signature(object = "GammaSpectra", curve = "CalibrationCurve"),
  definition = function(object, curve, epsilon = 0.03, ...) {

    # Get noise value and integration range
    noise <- curve@noise
    int_range <- curve@integration
    # Integrate signal
    signals <- integrateSignal(object, range = int_range, noise = noise) %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      dplyr::mutate(reference = rownames(.)) %>%
      dplyr::rename(signal = "value", signal_error = "error") %>%
      dplyr::mutate(signal = as.numeric(.data$signal),
                    signal_error = as.numeric(.data$signal_error))

    # Get linear regression results
    fit <- curve@model
    fit_coef <- summary(curve@model)$coef
    slope <- fit_coef["signal", "Estimate"]
    slope_error <- fit_coef["signal", "Std. Error"]

    dose <- stats::predict.lm(fit, signals[, "signal", drop = FALSE])

    dose_error <- dose * sqrt((slope_error / slope)^2 +
                                (signals$signal_error / signals$signal)^2 +
                                epsilon^2)

    methods::new("DoseRate",
                 reference = signals$reference,
                 dose_value = dose,
                 dose_error = dose_error,
                 signal_value = signals$signal,
                 signal_error = signals$signal_error)
  }
)
