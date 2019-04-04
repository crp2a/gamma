# ESTIMATE DOSE RATE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname estimateDoseRate
#' @aliases estimateDoseRate,GammaSpectrum-method
setMethod(
  f = "estimateDoseRate",
  signature = signature(object = "GammaSpectrum", curve = "CalibrationCurve"),
  definition = function(object, curve, noise, ...) {

    # TODO: pas propre, reprendre tout ça
    ls <- list(object)
    names(ls) <- object@reference
    spc <- methods::new("GammaSpectra", ls)
    estimateDoseRate(spc, curve = curve, noise = noise, ...)
  }
)

#' @export
#' @rdname estimateDoseRate
#' @aliases estimateDoseRate,GammaSpectra-method
setMethod(
  f = "estimateDoseRate",
  signature = signature(object = "GammaSpectra", curve = "CalibrationCurve"),
  definition = function(object, curve, noise, ...) {

    signals <- integrateSignal(object, noise = noise, ...) %>%
      dplyr::bind_rows(.id = "reference") %>%
      dplyr::rename(signal = "value", signal_error = "error")

    # Get linear regression results
    fit <- curve@model
    slope_value <- stats::coef(fit)
    slope_error <- summary(curve@model)$coef[, "Std. Error"]

    # @param x a data.frame
    # @param slope
    # @param error
    # @return A list
    calcDoseRate <- function(x, slope, error, calibration = 3) {
      signal_value <- as.numeric(x["signal"])
      signal_error <- as.numeric(x["signal_error"])
      dose_rate <- signal_value / slope
      dose_error <- sqrt((100 * signal_error / signal_value)^2 +
                           error^2 + calibration^2) * dose_rate / 100
      return(list(dose = dose_rate, error = dose_error))
    }

    final <- apply(X = signals, MARGIN = 1, FUN = calcDoseRate,
                   slope = slope_value, error = slope_error) %>%
      magrittr::set_names(names(object)) %>%
      dplyr::bind_rows(.id = "reference")

    methods::new("DoseRate",
                 reference = signals$reference,
                 dose_value = final$dose,
                 dose_error = final$error,
                 signal_value = signals$signal,
                 signal_error = signals$signal_error)
  }
)
