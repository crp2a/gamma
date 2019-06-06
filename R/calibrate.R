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

    .GammaSpectra(energy)
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
    n_lines <- length(lines)
    if (n_lines < 2) {
      msg <- "You have to provide at least 2 lines for calibration, not %d."
      stop(sprintf(msg, n_lines), call. = FALSE)
    }
    if (any(lengths(lines) != 2)) {
      msg <- "`lines` must be a list of length-two numeric vectors"
      stop(msg, call. = FALSE)
    }
    lines_names <- vapply(
      X = lines,
      FUN = function(x) {
        !is.null(names(x)) && all(names(x) %in% c("chanel", "energy"))
      },
      FUN.VALUE = logical(1)
    )
    if (!all(lines_names))
      stop("`lines` is a list",
           " but does not have components 'chanel' and 'energy'.",
           call. = FALSE)

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
    .GammaSpectrum(
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
      stop(sprintf("`lines` must be of length %d, not %d", n_chanels, n_lines),
           call. = FALSE)

    fit_data <- data.frame(energy = lines, chanel = chanels) %>%
      split(., f = seq_len(nrow(.)))
    calibrate(spectrum, fit_data)
  }
)
