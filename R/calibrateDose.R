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
  definition = function(object, dose, noise, laboratory = "LAB", ...) {
    # Validation
    if (length(object) != length(dose))
      stop("XXX")
    if (sum(names(dose) %in% names(object)) != length(dose))
      stop("YYY")

    # Signal integration
    signals <- integrateSignal(object, noise = noise, ...) %>%
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
    # fit <- stats::lm(dose ~ 0 + signal, data = fit_data, weights = 1 / dose_error^2)
    fit <- stats::lm(dose ~ 0 + signal, data = fit_data)

    methods::new(
      "CalibrationCurve",
      instrument = NA_character_,
      laboratory = laboratory,
      model = fit,
      noise = noise,
      data = fit_data
    )
  }
)
