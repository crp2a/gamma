# CALIBRATE SPECTRUM (ENERGY)
#' @include AllGenerics.R
NULL

#' @export
#' @rdname calibrate
#' @aliases calibrate,GammaSpectra,list-method
setMethod(
  f = "calibrate",
  signature = signature(object = "GammaSpectra", lines = "list"),
  definition = function(object, lines, ...) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    energy <- lapply(X = spectra, FUN = calibrate, lines = lines, ...)
    names(energy) <- names(object)

    methods::new("GammaSpectra", energy)
  }
)

#' @export
#' @rdname calibrate
#' @aliases calibrate,GammaSpectrum,list-method
setMethod(
  f = "calibrate",
  signature = signature(object = "GammaSpectrum", lines = "list"),
  definition = function(object, lines, ...) {
    # Validation
    if (length(lines) < 2)
      stop(sprintf("You have to provide at least two lines for calibration (not %d).",
                   length(lines)))

    lines_names <- sapply(X = lines, FUN = function(x) {
      !is.null(names(x)) & all(names(x) %in% c("chanel", "energy"))
    })
    if (!all(lines_names))
      stop(sprintf("%s must be a list of length-two numeric vectors with names %s and %s.",
                   sQuote("lines"), sQuote("chanel"), sQuote("energy")))

    # Get spectrum data
    spc_data <- methods::as(object, "data.frame")

    # Adjust spectrum for energy shift
    ## Get corresponding chanels
    fit_data <- as.data.frame(do.call(rbind, lines))
    ## Fit second order polynomial
    fit_poly <- stats::lm(energy ~ stats::poly(chanel, degree = 2, raw = TRUE),
                          data = fit_data)
    ## Predict shifted energy values
    fit_spc <- stats::predict(fit_poly, spc_data[, "chanel", drop = FALSE])

    # Return a new gamma spectrum with adjusted energy
    methods::new(
      "GammaSpectrum",
      hash = object@hash,
      reference = object@reference,
      instrument = object@instrument,
      file_format = object@file_format,
      chanel = spc_data$chanel,
      energy = fit_spc,
      counts = spc_data$counts,
      live_time = object@live_time,
      real_time = object@real_time,
      calibration = fit_poly
    )
  }
)

#' @export
#' @rdname calibrate
#' @aliases calibrate,PeakModel,numeric-method
setMethod(
  f = "calibrate",
  signature = signature(object = "PeakModel", lines = "numeric"),
  definition = function(object, lines, ...) {
    # Get data
    peaks <- object@coefficients
    spectrum <- object@spectrum
    chanels <- round(peaks[, "mean"], digits = 0)

    # Validation
    n_lines <- length(lines)
    n_chanels <- length(chanels)
    if (n_lines != n_chanels)
      stop(sprintf("%s must be of length %d, not %d",
                   sQuote("lines"), n_chanels, n_lines))

    fit_data <- data.frame(energy = lines, chanel = chanels) %>%
      split(., f = 1:nrow(.))
    calibrate(spectrum, fit_data)
  }
)
