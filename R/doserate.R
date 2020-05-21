# FIT AND PREDICT DOSE RATE
#' @include AllGenerics.R
NULL

# ========================================================================== Fit
#' @export
#' @rdname doserate
#' @aliases fit_dose,GammaSpectra,GammaSpectrum,matrix-method
setMethod(
  f = "fit_dose",
  signature = signature(object = "GammaSpectra", background = "GammaSpectrum",
                        doses = "matrix"),
  definition = function(object, background, doses, range_Ni, range_NiEi,
                        alpha = 0.05, details = NULL, ...) {
    doses <- as.data.frame(doses)
    fit_dose(object, background, doses,
             range_Ni = range_Ni, range_NiEi = range_NiEi,
             alpha = alpha, details = details)
  }
)

#' @export
#' @rdname doserate
#' @aliases fit_dose,GammaSpectra,data.frame-method
setMethod(
  f = "fit_dose",
  signature = signature(object = "GammaSpectra", background = "GammaSpectrum",
                        doses = "data.frame"),
  definition = function(object, background, doses, range_Ni, range_NiEi,
                        alpha = 0.05, details = NULL) {
    # Validation
    if (length(range_Ni) != 2 | length(range_NiEi) != 2)
      stop(sprintf("%s must be of length 2.", sQuote("range_*")), call. = FALSE)
    if (is.null(rownames(doses)))
      stop(sprintf("%s is missing row names.", sQuote("doses")), call. = FALSE)
    doses <- doses[, c(1, 2)]

    # Metadata
    info <- if (is.list(details)) details else list()
    info$date <- Sys.time()

    # Fit linear regression (York)
    Ni <- fit_york(object, background, doses, range = range_Ni,
                   energy = FALSE, alpha = alpha)
    NiEi <- fit_york(object, background, doses, range = range_NiEi,
                     energy = TRUE, alpha = alpha)

    .CalibrationCurve(
      Ni = Ni,
      NiEi = NiEi,
      details = info
    )
  }
)

fit_york <- function(object, background, doses, range,
                     energy = FALSE, alpha = 0.05) {
  # Signal integration
  bkg <- integrate_signal(background, range = range, energy = energy)
  signals <- integrate_signal(object, background = bkg, range = range,
                              energy = energy, simplify = TRUE)
  colnames(signals) <- c("signal_value", "signal_error")

  # Prepare data
  data <- merge(signals, doses, by = 0, all = FALSE, sort = FALSE)
  colnames(data)[1] <- "names"

  # Fit model
  model <- IsoplotR::york(data[, -1], alpha = alpha)

  .DoseRateModel(
    slope = as.numeric(model$b),
    intercept = as.numeric(model$a),
    covariance = model$cov.ab,
    MSWD = model$mswd,
    df = model$df,
    p_value = model$p.value,
    data = data,
    range = range,
    background = bkg
  )
}

# ====================================================================== Predict
#' @export
#' @rdname doserate
#' @aliases predict_dose,CalibrationCurve,missing-method
setMethod(
  f = "predict_dose",
  signature = signature(object = "CalibrationCurve", spectrum = "missing"),
  definition = function(object, sigma = 1, epsilon = 0) {

    Ni <- predict_york(object[["Ni"]],
                       energy = FALSE, sigma = sigma, epsilon = epsilon)

    NiEi <- predict_york(object[["NiEi"]],
                         energy = TRUE, sigma = sigma, epsilon = epsilon)

    merge(Ni, NiEi, by = "names", sort = FALSE, suffixes = c("_Ni","_NiEi"))
  }
)

#' @export
#' @rdname doserate
#' @aliases predict_dose,CalibrationCurve,GammaSpectrum-method
setMethod(
  f = "predict_dose",
  signature = signature(object = "CalibrationCurve", spectrum = "GammaSpectrum"),
  definition = function(object, spectrum, sigma = 1, epsilon = 0) {
    spectrum <- methods::as(spectrum, "GammaSpectra")
    predict_dose(object, spectrum, sigma = sigma, epsilon = epsilon)
  }
)

#' @export
#' @rdname doserate
#' @aliases predict_dose,CalibrationCurve,GammaSpectra-method
setMethod(
  f = "predict_dose",
  signature = signature(object = "CalibrationCurve", spectrum = "GammaSpectra"),
  definition = function(object, spectrum, sigma = 1, epsilon = 0) {

    Ni <- predict_york(object[["Ni"]], spectrum,
                       energy = FALSE, sigma = sigma, epsilon = epsilon)

    NiEi <- predict_york(object[["NiEi"]], spectrum,
                         energy = TRUE, sigma = sigma, epsilon = epsilon)

    merge(Ni, NiEi, by = "names", sort = FALSE, suffixes = c("_Ni","_NiEi"))
  }
)

#' @param model A \linkS4class{DoseRateModel} object.
#' @param sigma A \code{\link{numeric}}.
#' @param epsilon A \code{\link{numeric}}.
#' @return A \code{\link{data.frame}}.
#' @keywords internal
#' @noRd
predict_york <- function(model, spectrum, energy = FALSE,
                         sigma = 1, epsilon = 0) {
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
    signals <- integrate_signal(spectrum, background, range = range,
                                energy = energy, simplify = TRUE)
    signals <- as.data.frame(signals)
    signals$names <- rownames(signals)
  }

  # Get linear regression results
  slope <- model[["slope"]]
  intercept <- model[["intercept"]]

  gamma_dose <- slope[[1L]] * signals$value + intercept[[1L]]

  gamma_error <- gamma_dose *
    sqrt((slope[[2L]] * sigma / slope[[1L]])^2 +
           (signals$error / signals$value)^2 +
           epsilon^2)

  results <- data.frame(
    names = signals$names,
    gamma_dose = gamma_dose,
    gamma_error = gamma_error,
    stringsAsFactors = FALSE
  )
  return(results)
}
