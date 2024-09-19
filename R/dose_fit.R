# FIT DOSE RATE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname doserate
#' @aliases dose_fit,GammaSpectra,GammaSpectrum,matrix-method
setMethod(
  f = "dose_fit",
  signature = signature(object = "GammaSpectra", background = "GammaSpectrumOrNumeric",
                        doses = "matrix"),
  definition = function(object, background, doses, range_Ni, range_NiEi,
                        details = list(authors = "", date = Sys.time())) {
    doses <- as.data.frame(doses)
    methods::callGeneric(object, background, doses, range_Ni = range_Ni,
                         range_NiEi = range_NiEi, details = details)
  }
)

#' @export
#' @rdname doserate
#' @aliases dose_fit,GammaSpectra,data.frame-method
setMethod(
  f = "dose_fit",
  signature = signature(object = "GammaSpectra", background = "GammaSpectrumOrNumeric",
                        doses = "data.frame"),
  definition = function(object, background, doses, range_Ni, range_NiEi,
                        details = list(authors = "", date = Sys.time())) {
    # Validation
    if (length(range_Ni) != 2 | length(range_NiEi) != 2)
      stop(sprintf("%s must be of length 2.", sQuote("range_*")), call. = FALSE)
    if (is.null(rownames(doses)))
      stop(sprintf("%s is missing row names.", sQuote("doses")), call. = FALSE)
    doses <- doses[, c(1, 2)]

    ## check for energy calibration, if some have calibration others are not,
    ## something likely went wrong
    ## with this checks we are certain that after we can expect two cases: no calibration
    ## or calibration
    if (!all(vapply(object, has_calibration, logical(1))) & any(vapply(object, has_calibration, logical(1))))
      stop("You must not mix spectra with and without energy/channel calibration!", call. = FALSE)

    if (all(!vapply(object, has_calibration, logical(1))))
      warning("All spectra without energy calibration. You can proceed but it is not recommended!", call. = FALSE)

    # Metadata
    info <- if (is.list(details)) details else list()

    ## always add sysdata
    if (is.null(info$date))
      info$date <- Sys.time()

    ## if not available, we try to grep the calibration data and
    ## assign it to the object
    if (is.null(info$energy_calibration)) {
      info$energy_calibration <- NA

      ## more complicated, we have calibrations, but they do not match in such case
      ## we set NA again
      cal_1st <- object[[1]]@calibration

      if (all(vapply(object, has_calibration, logical(1))) &&
        all(vapply(object, function(x) identical(cal_1st, x@calibration), logical(1)))) {
        info$energy_calibration <- list(cal_1st)

      } else if (all(vapply(object, has_calibration, logical(1)))){
        ## now we assign all calibrations we have
        info$energy_calibration <- lapply(object, function(x) x@calibration)

      }

      # if not NA set names
      if (!any(is.na(info$energy_calibration[[1]]))) {
        if(length(info$energy_calibration) > 1)
          names(info$energy_calibration) <- unlist(lapply(object, function(x) x@name))
        else
          names(info$energy_calibration) <- "single_calibration"

      }
    }

    # Fit linear regression (York)
    Ni <- fit_york(object, background, doses, range = range_Ni, energy = FALSE)
    NiEi <- fit_york(object, background, doses, range = range_NiEi, energy = TRUE)

    .CalibrationCurve(
      Ni = Ni,
      NiEi = NiEi,
      details = info
    )
  }
)

fit_york <- function(object, background, doses, range, energy = FALSE) {
  # Signal integration if we have a GammaSpectrum-class for the background
  if (inherits(background, "GammaSpectrum"))
    bkg <- signal_integrate(background, range = range, energy = energy)
  else
    bkg <- background

  signals <- signal_integrate(object, background = bkg, range = range,
                              energy = energy, simplify = TRUE)

  # Prepare data
  data <- merge(signals, doses, by = 0, all = FALSE, sort = FALSE)
  colnames(data) <- c("names", "signal_value", "signal_error",
                      "gamma_dose", "gamma_error")

  # Fit model
  model <- IsoplotR::york(data[, -1])
  # fitted <- model$a[[1L]] + data$signal_value * model$b[[1L]]
  # residuals <- data$gamma_dose - fitted
  # names(residuals) <- seq_along(residuals)

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

