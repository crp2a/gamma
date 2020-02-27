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
                        alpha = 0.05, details = NULL, ...) {
    # Validation
    # TODO

    # Dose rate
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
    # Fit linear regression (York)
    fit_data <- merge(signals, doses, by = "name", all = FALSE, sort = FALSE)

    Ni_model <- fit_york(fit_data, background = Ni_noise, range = Ni_range,
                         threshold = "Ni", alpha = alpha)
    NiEi_model <- fit_york(fit_data, background = NiEi_noise, range = NiEi_range,
                           threshold = "NiEi", alpha = alpha)
    methods::new(
      "CalibrationCurve",
      Ni = Ni_model,
      NiEi = NiEi_model,
      data = fit_data,
      details = info
    )
  }
)

fit_york <- function(data, background, range,
                     threshold = c("Ni", "NiEi"), alpha = 0.05) {
  # Validation
  if (length(background) != 2 || !is.numeric(background))
    stop("`background` must be a numeric vector of length 2.", call. = FALSE)
  if (length(range) != 2 || !is.numeric(range))
    stop("`range` must be a numeric vector of length 2.", call. = FALSE)
  # Select columns
  threshold <- match.arg(threshold, several.ok = FALSE)
  select <- c(
    paste0(threshold, "_signal"),
    paste0(threshold, "_error"),
    "gamma_dose", "gamma_error"
  )
  data <- data[, select]
  # Fit model
  model <- IsoplotR::york(data, alpha = alpha)
  .DoseRateModel(
    slope = as.numeric(model$b),
    intercept = as.numeric(model$a),
    residuals = data[[3]] - model$b[1] * data[[1]] + model$a[1],
    df = model$df,
    MSWD = model$mswd,
    p_value = model$p.value,
    background = background,
    range = range
  )
}

# ====================================================================== Predict
#' @export
#' @rdname doserate
#' @aliases predict_dose,CalibrationCurve,missing-method
setMethod(
  f = "predict_dose",
  signature = signature(object = "CalibrationCurve", spectrum = "missing"),
  definition = function(object, threshold = c("Ni", "NiEi"), epsilon = 0, ...) {

    predict_york(model = object, data = object@data, threshold = threshold,
                 epsilon = epsilon)
  }
)

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

    predict_york(model = object, data = int_data, threshold = threshold,
                 epsilon = epsilon)
  }
)

#' @param model A \linkS4class{CalibrationCurve} object.
#' @param data A two-columns \code{\link{data.frame}} (returned by
#'  \code{\link{integrate_signal}}).
#' @param threshold A \code{\link{character}} string.
#' @param epsilon A \code{\link{numeric}}.
#' @return A data frame.
#' @keywords internal
#' @noRd
predict_york <- function(model, data, threshold = c("Ni", "NiEi"),
                         epsilon = 0) {

  # Validation
  threshold <- match.arg(threshold, several.ok = FALSE)

  # Get linear regression results
  fit <- get_model(model, threshold)

  signal_value <- paste0(threshold, "_signal")
  signal_error <- paste0(threshold, "_error")

  slope <- fit[1, 1]
  slope_error <- fit[1, 2]
  intercept <- fit[2, 1]
  intercept_error <- fit[2, 2]

  gamma_dose <- slope * data[, signal_value] + intercept

  gamma_error <- gamma_dose *
    sqrt((slope_error / slope)^2 +
           (data[[signal_error]] / data[[signal_value]])^2 +
           epsilon^2)

  # Generate a warning message if some predicted values do not lie in the
  # dose ~ signal curve range.
  dose_range <- range(model@data$gamma_dose)
  dose_out <- gamma_dose < min(dose_range) | gamma_dose > max(dose_range)
  out <- sum(dose_out)
  if (out != 0) {
    warning(
      sprintf("The following %s lie in the curve range:\n",
              ngettext(out, "value does not", "values do not")),
      paste0("* ", data$name[dose_out], collapse = "\n"),
      call. = FALSE
    )
  }

  results <- data.frame(
    data[, c("name", signal_value, signal_error)],
    gamma_dose = gamma_dose,
    gamma_error = gamma_error,
    stringsAsFactors = FALSE
  )
  rownames(results) <- rownames(data)
  return(results)
}
