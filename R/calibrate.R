# BUILD CALIBRATION CURVE AND ADJUST EXPERIMENTAL DATA
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

#' @export
#' @rdname calibrate
#' @aliases calibrate,GammaSpectra-method
setMethod(
  f = "calibrate",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, dose, noise, ...) {

    signals <- integrateSignal(object, noise = noise, ...) %>%
      dplyr::bind_rows(.id = "reference") %>%
      dplyr::rename(signal = "value", signal_error = "error")

    # Check names
    if (sum(signals$reference %in% names(dose)) != nrow(signals))
      stop("XXX")

    # Fit linear regression
    data <- dose %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      dplyr::mutate(reference = rownames(.)) %>%
      dplyr::rename(dose = "V1", dose_error = "V2") %>%
      dplyr::inner_join(signals, by = "reference") %>%
      dplyr::select(.data$reference, .data$dose, .data$dose_error,
                    .data$signal, .data$signal_error)

    fit <- stats::lm(signal ~ 0 + dose, data = data)

    methods::new("CalibrationCurve", model = fit, data = data)
  }
)
