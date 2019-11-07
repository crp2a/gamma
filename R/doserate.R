# FIT AND PREDICT DOSE RATE
#' @include AllGenerics.R
NULL

# ========================================================================== Fit
#' @export
#' @rdname doserate
#' @aliases fit_dose,GammaSpectra,GammaSpectrum-method
setMethod(
  f = "fit_dose",
  signature = signature(object = "GammaSpectra", noise = "GammaSpectrum"),
  definition = function(object, noise, range,
                        intercept = TRUE, weights = FALSE,
                        details = NULL, ...) {
    # Integrate background noise
    int_noise <- integrate_signal(noise, range = range, noise = NULL)
    fit_dose(object, int_noise, range = range, intercept = intercept,
             weights = weights, details = details, ...)
  }
)

#' @export
#' @rdname doserate
#' @aliases fit_dose,GammaSpectra,numeric-method
setMethod(
  f = "fit_dose",
  signature = signature(object = "GammaSpectra", noise = "numeric"),
  definition = function(object, noise, range,
                        intercept = TRUE, weights = FALSE,
                        details = NULL, ...) {
    ## Dose rate
    dose_rate <- get_dose(object)
    doses <- data.frame(
      name = rownames(dose_rate),
      dose_value = dose_rate[["value"]],
      dose_error = dose_rate[["error"]],
      stringsAsFactors = FALSE
    )

    zero_dose <- doses$dose_value == 0
    if (any(zero_dose)) {
      names_missing <- names(object)[zero_dose]
      length_missing <- length(names_missing)
      warning(
        sprintf(
          ngettext(
            length_missing,
            "%d spectrum have a dose rate of 0:",
            "%d spectra have a dose rate of 0:"
          ),
          length_missing
        ),
        "\n* ", paste0(names_missing, collapse = "\n* "), call. = FALSE
      )
    }

    # Metadata
    info <- if (is.list(details)) details else list()

    # Signal integration
    signals <- integrate_signal(object, range = range, noise = noise,
                                simplify = TRUE)
    signals <- cbind.data.frame(signals, rownames(signals),
                                stringsAsFactors = FALSE)
    colnames(signals) <- c("signal_value", "signal_error", "name")

    # Fit linear regression
    fit_data <- merge(doses, signals, by = "name", all = FALSE)

    # TODO: check weights!
    fit_weights <- if (weights) 1 / fit_data$dose_error^2 else NULL
    if (intercept) {
      fit_formula <- stats::as.formula("dose_value ~ signal_value")
    } else {
      fit_formula <- stats::as.formula("dose_value ~ 0 + signal_value")
    }
    fit <- stats::lm(formula = fit_formula, data = fit_data,
                     weights = fit_weights)

    methods::new(
      "CalibrationCurve",
      details = info,
      model = fit,
      noise = noise,
      integration = range,
      data = fit_data
    )
  }
)

# ====================================================================== Predict
#' @export
#' @rdname doserate
#' @aliases predict_dose,CalibrationCurve,missing-method
setMethod(
  f = "predict_dose",
  signature = signature(object = "CalibrationCurve", spectrum = "missing"),
  definition = function(object, epsilon = 0, simplify = FALSE, ...) {
    new_data <- methods::as(object@data, "data.frame")
    # Predict dose rate
    do_predict_dose(object, new_data,
                    epsilon = epsilon, simplify = simplify, ...)
  }
)

#' @export
#' @rdname doserate
#' @aliases predict_dose,CalibrationCurve,GammaSpectrum-method
setMethod(
  f = "predict_dose",
  signature = signature(object = "CalibrationCurve", spectrum = "GammaSpectrum"),
  definition = function(object, spectrum, epsilon = 0, simplify = FALSE, ...) {
    spectrum <- methods::as(spectrum, "GammaSpectra")
    predict_dose(object, spectrum, epsilon = epsilon, simplify = simplify, ...)
  }
)

#' @export
#' @rdname doserate
#' @aliases predict_dose,CalibrationCurve,GammaSpectra-method
setMethod(
  f = "predict_dose",
  signature = signature(object = "CalibrationCurve", spectrum = "GammaSpectra"),
  definition = function(object, spectrum, epsilon = 0, simplify = FALSE, ...) {
    spectrum <- methods::as(spectrum, "GammaSpectra")
    # Get noise value and integration range
    noise <- object@noise
    int_range <- object@integration
    # Integrate spectrum
    new_data <- integrate_signal(spectrum, range = int_range, noise = noise,
                                 simplify = TRUE)
    new_data <- cbind.data.frame(new_data, rownames(new_data),
                                 stringsAsFactors = FALSE)
    colnames(new_data) <- c("signal_value", "signal_error", "name")
    # Predict dose rate
    do_predict_dose(object, new_data,
                    epsilon = epsilon, simplify = simplify, ...)
  }
)

do_predict_dose <- function(object, new_data,
                            epsilon = 0, simplify = FALSE, ...) {
  # Validation
  if (!is.data.frame(new_data)) {
    stop("`new_data` must be a data frame.")
  } else {
    variables <- c("signal_value", "signal_error", "name")
    if (!all(variables %in% colnames(new_data)))
      stop("`new_data` is a data.frame, ",
           "but does not have components ",
           paste0(variables, collapse = ", "),
           call. = FALSE)
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
    name = new_data$name,
    signal_value = new_data$signal_value,
    signal_error = new_data$signal_error,
    dose_value = dose_value,
    dose_error = dose_error,
    stringsAsFactors = FALSE
  )
  rownames(results) <- new_data$name
  if (simplify) {
    results
  } else {
    split(results, f = new_data$name)
  }
}
