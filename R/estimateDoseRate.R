# ESTIMATE DOSE RATE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname estimateDoseRate
#' @aliases estimateDoseRate,GammaSpectrum-method
setMethod(
  f = "estimateDoseRate",
  signature = signature(object = "GammaSpectrum", curve = "CalibrationCurve"),
  definition = function(object, curve, ...) {

    # Coerce to GammaSpectra
    # TODO: pas propre, reprendre tout ça
    ls <- list(object)
    names(ls) <- object@reference
    spc <- methods::new("GammaSpectra", ls)
    estimateDoseRate(spc, curve = curve, ...)
  }
)

#' @export
#' @rdname estimateDoseRate
#' @aliases estimateDoseRate,GammaSpectra-method
setMethod(
  f = "estimateDoseRate",
  signature = signature(object = "GammaSpectra", curve = "CalibrationCurve"),
  definition = function(object, curve, ...) {

    # Get noise value
    noise <- as.list(curve@noise)

    signals <- integrateSignal(object, noise = noise, ...) %>%
      dplyr::bind_rows(.id = "reference") %>%
      dplyr::rename(signal = "value", signal_error = "error") %>%
      dplyr::mutate(signal = as.numeric(.data$signal),
                    signal_error = as.numeric(.data$signal_error))

    # Get linear regression results
    fit <- curve@model
    slope <- stats::coef(fit)
    slope_error <- summary(curve@model)$coef[, "Std. Error"]

    dose <- stats::predict.lm(fit, signals[, "signal", drop = FALSE])
    dose_error <- dose * sqrt((slope_error / slope)^2 +
                                (signals$signal_error / signals$signal)^2 + 0.03^2)

    methods::new("DoseRate",
                 reference = signals$reference,
                 dose_value = dose,
                 dose_error = dose_error,
                 signal_value = signals$signal,
                 signal_error = signals$signal_error)
  }
)
