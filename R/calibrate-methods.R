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
    if (any(lengths(lines) != 2)) {
      msg <- "`lines` must be a list of length-two numeric vectors."
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

    # Adjust spectrum for energy shift
    ## Get corresponding chanels
    fit_data <- as.data.frame(do.call(rbind, lines))
    # Return a new gamma spectrum with adjusted energy
    .calibrate(object, fit_data)
  }
)

#' @export
#' @rdname calibrate
#' @aliases calibrate,GammaSpectrum,PeakPosition-method
setMethod(
  f = "calibrate",
  signature = signature(object = "GammaSpectrum", lines = "PeakPosition"),
  definition = function(object, lines, ...) {
    # Get data
    peaks <- methods::as(lines, "data.frame")
    clean <- stats::na.omit(peaks)
    # Validation
    if (nrow(clean) == 0)
      stop("You must set the corresponding energy (keV).", call. = FALSE)

    # Adjust spectrum for energy shift
    # Return a new gamma spectrum with adjusted energy
    .calibrate(object, clean)
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

    fit_data <- data.frame(energy = lines, chanel = chanels)
    .calibrate(spectrum, fit_data)
  }
)
