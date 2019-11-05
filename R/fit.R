# BUILD CALIBRATION CURVE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname fit
#' @aliases fit_dose,GammaSpectra,GammaSpectrum-method
setMethod(
  f = "fit_dose",
  signature = signature(object = "GammaSpectra", noise = "GammaSpectrum"),
  definition = function(object, noise, range = c(200, 2800),
                        intercept = TRUE, weights = FALSE,
                        details = NULL, ...) {
    # Integrate background noise
    int_noise <- integrate_signal(noise, range = range)
    fit_dose(object, int_noise, range = range, intercept = intercept,
             weights = weights, details = details, ...)
  }
)

#' @export
#' @rdname fit
#' @aliases fit_dose,GammaSpectra,numeric-method
setMethod(
  f = "fit_dose",
  signature = signature(object = "GammaSpectra", noise = "numeric"),
  definition = function(object, noise, range = c(200, 2800),
                        intercept = TRUE, weights = FALSE,
                        details = NULL, ...) {
    # Validation
    ## Noise
    n_noise <- length(noise)
    if (n_noise != 2)
      stop(sprintf("`noise` must be a numeric vector of length 2, not %d.",
                   n_noise), call. = FALSE)
    ## Range
    n_range <- length(range)
    if (n_range != 2)
      stop(sprintf("`range` must be a numeric vector of length 2, not %d.",
                   n_range), call. = FALSE)
    ## Dose rate
    dose_rate <- get_dose(object)
    doses <- data.frame(
      reference = rownames(dose_rate),
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
    colnames(signals) <- c("signal_value", "signal_error", "reference")

    # Fit linear regression
    fit_data <- merge(doses, signals, by = "reference", all = FALSE)

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
