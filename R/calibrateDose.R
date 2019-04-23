# BUILD CALIBRATION CURVE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname calibrateDose
#' @aliases calibrateDose,GammaSpectra,list,numeric-method
setMethod(
  f = "calibrateDose",
  signature = signature(object = "GammaSpectra",
                        dose = "list", noise = "numeric"),
  definition = function(object, dose, noise, range,
                        intercept = FALSE, weights = FALSE,
                        details = NULL, ...) {
    # Validation
    if (length(object) != length(dose))
      stop("XXX")
    if (sum(names(dose) %in% names(object)) != length(dose))
      stop("YYY")
    if (is.vector(details)) {
      k <- which(names(details) %in% c("laboratory", "instrument"))
      info <- as.list(details[k])
    } else {
      info <- list(laboratory = NA_character_, instrument = NA_character_)
    }

    # Signal integration
    signals <- integrateSignal(object, range = range, noise = noise) %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      dplyr::mutate(reference = rownames(.)) %>%
      dplyr::rename(signal = "value", signal_error = "error")

    # Fit linear regression
    doses <- dose %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      dplyr::mutate(reference = rownames(.)) %>%
      dplyr::rename(dose = "V1", dose_error = "V2")

    fit_data <- dplyr::inner_join(doses, signals, by = "reference") %>%
      dplyr::select(.data$reference, .data$dose, .data$dose_error,
                    .data$signal, .data$signal_error)

    # TODO: check weights!
    fit_weights <- if (weights) 1 / fit_data$dose_error^2 else NULL
    if (intercept) {
      fit_formula <- stats::as.formula("dose ~ signal")
    } else {
      fit_formula <- stats::as.formula("dose ~ 0 + signal")
    }
    fit <- stats::lm(formula = fit_formula, data = fit_data,
                     weights = fit_weights)

    methods::new(
      "CalibrationCurve",
      instrument = info$instrument,
      laboratory = info$laboratory,
      model = fit,
      noise = noise,
      integration = range,
      data = fit_data
    )
  }
)
