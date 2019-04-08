# BUILD CALIBRATION CURVE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname calibrate
#' @aliases calibrate,GammaSpectra-method
setMethod(
  f = "calibrate",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, dose, noise, laboratory = "LAB", ...) {

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

    methods::new(
      "CalibrationCurve",
      instrument = NA_character_,
      laboratory = laboratory,
      model = fit,
      noise = NA_real_,
      data = data
    )
  }
)
