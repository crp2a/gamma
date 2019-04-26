# BUILD CALIBRATION CURVE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname fit
#' @aliases fit,GammaSpectra,DoseRate,numeric-method
setMethod(
  f = "fit",
  signature = signature(object = "GammaSpectra",
                        doses = "DoseRate", noise = "numeric"),
  definition = function(object, doses, noise,
                        range = c(200, 2800), intercept = TRUE, weights = FALSE,
                        details = NULL, ...) {
    # Validation
    n_noise <- length(noise)
    if (n_noise != 2)
      stop(sprintf("%s must be a numeric vector of length two, not %d.",
                   sQuote("noise"), n_noise))

    # Metadata
    info <- if (is.list(details)) details else list()

    # Signal integration
    signals <- integrateSignal(object, range = range, noise = noise) %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      dplyr::mutate(reference = rownames(.)) %>%
      dplyr::rename(signal_value = "value", signal_error = "error")

    # Fit linear regression
    fit_data <- methods::as(doses, "data.frame") %>%
      dplyr::select(-.data$signal_value, -.data$signal_error) %>%
      dplyr::mutate(reference = as.character(.data$reference)) %>%
      dplyr::inner_join(., signals, by = "reference")

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
      data = methods::as(fit_data, "DoseRate")
    )
  }
)
