# FIT AND PREDICT DOSE RATE
#' @include AllGenerics.R
NULL

# ========================================================================== Fit
#' @export
#' @rdname doserate
#' @aliases fit_dose,GammaSpectra-method
setMethod(
  f = "fit_dose",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, Ni_noise, Ni_range, NiEi_noise, NiEi_range,
                        details = NULL, ...) {
    ## Validation
    if (length(Ni_noise) != 2 || length(NiEi_noise) != 2 ||
        length(Ni_range) != 2 || length(NiEi_range) != 2)
      stop("`*_noise` and `*_range` must be numeric vectors of length 2.",
           call. = FALSE)
    ## Dose rate
    dose_rate <- get_dose(object)
    doses <- data.frame(
      name = rownames(dose_rate),
      dose_rate,
      stringsAsFactors = FALSE
    )

    zero_dose <- doses$gamma_dose == 0
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
    signals <- cbind.data.frame(
      name = names(object),
      live_time = unlist(object[, "live_time"]),
      integrate_signal(object, range = Ni_range, background = Ni_noise,
                       threshold = "Ni", simplify = TRUE),
      integrate_signal(object, range = NiEi_range, background = NiEi_noise,
                       threshold = "NiEi", simplify = TRUE),
      stringsAsFactors = FALSE
    )
    # Fit linear regression
    fit_data <- merge(signals, doses, by = "name", all = FALSE, sort = FALSE)

    Ni_model <- .DoseRateModelNi(
      model = stats::lm(formula = "gamma_dose ~ Ni_signal", data = fit_data),
      background = Ni_noise,
      range = Ni_range
    )
    NiEi_model <- .DoseRateModelNiEi(
      model = stats::lm(formula = "gamma_dose ~ NiEi_signal", data = fit_data),
      background = NiEi_noise,
      range = NiEi_range
    )
    methods::new(
      "CalibrationCurve",
      Ni = Ni_model,
      NiEi = NiEi_model,
      data = fit_data,
      details = info
    )
  }
)

# ====================================================================== Predict
#' @export
#' @rdname doserate
#' @aliases predict_dose,CalibrationCurve,GammaSpectrum-method
setMethod(
  f = "predict_dose",
  signature = signature(object = "CalibrationCurve", spectrum = "GammaSpectrum"),
  definition = function(object, spectrum, threshold = c("Ni", "NiEi"),
                        epsilon = 0, ...) {
    spectrum <- methods::as(spectrum, "GammaSpectra")
    predict_dose(object, spectrum, threshold = threshold,
                 epsilon = epsilon, ...)
  }
)

#' @export
#' @rdname doserate
#' @aliases predict_dose,CalibrationCurve,GammaSpectra-method
setMethod(
  f = "predict_dose",
  signature = signature(object = "CalibrationCurve", spectrum = "GammaSpectra"),
  definition = function(object, spectrum, threshold = c("Ni", "NiEi"),
                        epsilon = 0, ...) {

    # Validation
    threshold <- match.arg(threshold, several.ok = FALSE)

    # Get noise value and integration range
    bkg_noise <- get_noise(object, threshold)
    int_range <- get_range(object, threshold)
    # Integrate spectrum
    int_data <- integrate_signal(
      spectrum,
      range = int_range,
      background = bkg_noise,
      threshold = threshold,
      simplify = TRUE
    )
    int_data <- cbind.data.frame(
      name = rownames(int_data),
      live_time = unlist(spectrum[, "live_time"]),
      int_data,
      stringsAsFactors = FALSE
    )

    # Get linear regression results
    signal_value <- paste0(threshold, "_signal")
    signal_error <- paste0(threshold, "_error")
    fit <- get_model(object, threshold)
    fit_coef <- summary(fit)$coef

    slope <- fit_coef[signal_value, "Estimate"]
    slope_error <- fit_coef[signal_value, "Std. Error"]

    gamma_dose <- stats::predict.lm(fit, int_data[, signal_value, drop = FALSE])

    gamma_error <- gamma_dose *
      sqrt((slope_error / slope)^2 +
             (int_data[[signal_error]] / int_data[[signal_value]])^2 +
             epsilon^2)

    # Generate a warning message if some predicted values do not lie in the
    # dose ~ signal curve range.
    dose_range <- range(object@data$gamma_dose)
    dose_out <- gamma_dose < min(dose_range) | gamma_dose > max(dose_range)
    out <- sum(dose_out)
    if (out != 0) {
      warning(
        sprintf("The following %s lie in the curve range:\n",
                ngettext(out, "value does not", "values do not")),
        paste0("* ", int_data$name[dose_out], collapse = "\n"),
        call. = FALSE
      )
    }

    results <- data.frame(
      int_data,
      gamma_dose = gamma_dose,
      gamma_error = gamma_error,
      stringsAsFactors = FALSE
    )
    rownames(results) <- rownames(int_data)
    return(results)
  }
)
