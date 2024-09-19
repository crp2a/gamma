# PREDICT DOSE RATE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname doserate
#' @aliases dose_predict,CalibrationCurve,missing-method
setMethod(
  f = "dose_predict",
  signature = signature(object = "CalibrationCurve", spectrum = "missing"),
  definition = function(object, sigma = 1, epsilon = 0.015, use_MC = FALSE) {

    ## calculate for count threshold
    Ni <- predict_york(
      model = object[["Ni"]],
      energy = FALSE,
      sigma = sigma,
      epsilon = epsilon,
      use_MC = use_MC)

    ## calculate for energy threshold
    NiEi <- predict_york(
      model = object[["NiEi"]],
      energy = TRUE,
      sigma = sigma,
      epsilon = epsilon,
      use_MC = use_MC)

    ## calculate the mean and error of both values
    dose_final <- rowMeans(matrix(c(Ni$dose, NiEi$dose), ncol = 2))
    dose_err_final <- dose_final *
      sqrt((Ni$dose_err/Ni$dose)^2 + (NiEi$dose_err/NiEi$dose)^2)
    FINAL <- data.frame(dose_final, dose_err_final)

    Ni_NiEi <- merge(Ni, NiEi, by = "names", sort = FALSE, suffixes = c("_Ni","_NiEi"))
    cbind(Ni_NiEi, FINAL)
  }
)

#' @export
#' @rdname doserate
#' @aliases dose_predict,CalibrationCurve,GammaSpectrum-method
setMethod(
  f = "dose_predict",
  signature = signature(object = "CalibrationCurve", spectrum = "GammaSpectrum"),
  definition = function(object, spectrum, sigma = 1, epsilon = 0.015, use_MC = FALSE) {
    spectrum <- methods::as(spectrum, "GammaSpectra")
    dose_predict(object, spectrum, sigma = sigma, epsilon = epsilon, use_MC = use_MC)
  }
)

#' @export
#' @rdname doserate
#' @aliases dose_predict,CalibrationCurve,GammaSpectra-method
setMethod(
  f = "dose_predict",
  signature = signature(object = "CalibrationCurve", spectrum = "GammaSpectra"),
  definition = function(object, spectrum, sigma = 1, epsilon = 0.015, use_MC = FALSE) {
    ## we ensure that the calibration curve and the GammaSpectra have all
    ## energy calibrations. We have the following possible scenarios
    ## (1) energy calibrations are entirely missing or all available -> proceed
    ## (2) energy calibrations are missing and not missing -> hard stop
    ## (3) calibration has but spectrum not -> apply the calibration with message

    ## determine the energy calibration status
    has_energy_cal <- !any(is.na(object@details$energy_calibration))
    has_energy_spc <- all(vapply(spectrum, has_calibration, logical(1)))

    if (!has_energy_cal & has_energy_spc)
      stop("Your dose-rate calibration does not have an energy calibration, while your spectra have!",
           call. = FALSE)

    if (has_energy_cal & !has_energy_spc) {
      if (length(object@details$energy_calibration) > 1)
        stop("No energy calibration found 'spectrum' and 'object' has more than one calibration!",
             call. = FALSE)

      spectrum <- energy_calibrate(spectrum, object@details$energy_calibration[[1]])
      message("No energy calibration found for 'spectrum', apply calibration from dose-rate calibration model!")

    }

    ## calculate for count threshold
    Ni <- predict_york(
      model = object[["Ni"]],
      spectrum = spectrum,
      energy = FALSE,
      sigma = sigma,
      epsilon = epsilon,
      use_MC = use_MC)

    ## calculate for energy threshold
    NiEi <- predict_york(
      model = object[["NiEi"]],
      spectrum = spectrum,
      energy = TRUE,
      sigma = sigma,
      epsilon = epsilon,
      use_MC = use_MC)

    ## calculate the mean and error of both values
    dose_final <- rowMeans(matrix(c(Ni$dose, NiEi$dose), ncol = 2))
    dose_err_final <- dose_final *
      sqrt((Ni$dose_err/Ni$dose)^2 + (NiEi$dose_err/NiEi$dose)^2)
    FINAL <- data.frame(dose_final, dose_err_final)

    Ni_NiEi <- merge(Ni, NiEi, by = "names", sort = FALSE, suffixes = c("_Ni","_NiEi"))
    cbind(Ni_NiEi, FINAL)

  }
)

#' @param model A [DoseRateModel-class] object.
#' @param sigma A [`numeric`]. a factor determining the confidence interval, should be `1` for 68% and `1.96` for 95%
#' @param epsilon A [`numeric`]. an additional uncertainty factor
#' @param use_MC A [`logical`]. enable/disables Monte Carlo simulation for estimation the uncertainty
#' @return A [`data.frame`].
#' @keywords internal
#' @noRd
predict_york <- function(model, spectrum, energy = FALSE,
                         sigma = 1, epsilon = 0, use_MC = FALSE) {
  # Get integration range
  range <- model[["range"]]
  # Get background
  background <- model[["background"]]
  # Integrate spectrum
  if (missing(spectrum)) {
    signals <- model[["data"]]
    signals$value <- signals$signal_value
    signals$error <- signals$signal_error
  } else {
    signals <- signal_integrate(spectrum, background, range = range,
                                energy = energy, simplify = TRUE)
    signals <- as.data.frame(signals)
    signals$names <- rownames(signals)
  }

  # Get linear regression results
  slope <- model[["slope"]]
  intercept <- model[["intercept"]]

  ## calculate dose based on regression model
  gamma_dose <- slope[[1L]] * signals$value + intercept[[1L]]

  if (!use_MC[1L]) {
    gamma_error <- gamma_dose *
      sqrt(
        ((slope[[2L]] * sigma) / slope[[1L]])^2 +
        (signals$error / signals$value)^2 +
        epsilon^2)

  } else {
    ## create sample distributions
    n_MC <- 1e+6
    slope_MC <- stats::rnorm(n_MC, mean = slope[[1L]], sd = slope[[2L]])
    intercept_MC <- stats::rnorm(n_MC, mean = intercept[[1L]], sd = intercept[[2L]])
    signals_MC <- mapply(stats::rnorm, n = n_MC, mean = signals$value, sd = signals$error)


    ## calculate for all runs
    gamma_MC <- vapply(1:ncol(signals_MC), function(x) slope_MC * signals_MC[,x] + intercept_MC, numeric(n_MC))
    gamma_MC_err <- apply(X = gamma_MC, MARGIN = 2, stats::sd) * sigma
    gamma_error <- sqrt((gamma_MC_err/gamma_dose)^2 + epsilon^2) * gamma_dose
  }

  results <- data.frame(
    names = signals$names,
    signal = signals$value,
    signal_err = signals$error,
    dose = gamma_dose,
    dose_err = gamma_error,
    stringsAsFactors = FALSE
  )
  return(results)
}
