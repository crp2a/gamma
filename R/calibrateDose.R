# BUILD CALIBRATION CURVE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname calibrateDose
#' @aliases calibrateDose,GammaSpectra,list,numeric-method
setMethod(
  f = "calibrateDose",
  signature = signature(object = "GammaSpectra",
                        doses = "list", noise = "numeric"),
  definition = function(object, doses, noise, range = c(200, 2800),
                        intercept = TRUE, weights = FALSE,
                        details = NULL, ...) {
    # Validation
    n_object <- length(object)
    n_doses <- length(doses)
    n_noise <- length(noise)
    if (n_object != n_doses)
      stop(sprintf("%s (%d) and %s (%d) must have the same length.",
                   sQuote("object"), n_object, sQuote("doses"), n_doses))
    if (sum(names(doses) %in% names(object)) != length(doses))
      stop(sprintf("Elements of %s and %s must have the same names.",
                   sQuote("object"), sQuote("doses")))
    i_doses <- lengths(doses) == rep(2, n_doses)
    if (!all(i_doses))
      stop(sprintf("%s must be a list of length-two numeric vectors, you should check %s.",
                   sQuote("doses"), paste(sQuote(names(doses)[!i_doses]), collapse = ", ")))
    if (n_noise != 2)
      stop(sprintf("%s must be a numeric vector of length two, not %d.",
                   sQuote("noise"), n_noise))

    # TODO: pas propre
    info <- list(laboratory = NA_character_, instrument = NA_character_)
    if (is.vector(details)) {
      k <- which(names(details) %in% c("laboratory", "instrument"))
      info <- as.list(details[k])
    }

    # Signal integration
    signals <- integrateSignal(object, range = range, noise = noise) %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      dplyr::mutate(reference = rownames(.)) %>%
      dplyr::rename(signal = "value", signal_error = "error")

    # Fit linear regression
    doses <- doses %>%
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
