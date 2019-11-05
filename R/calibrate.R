# CALIBRATE SPECTRUM (ENERGY)
#' @include AllGenerics.R
NULL

#' @export
#' @rdname calibrate
#' @aliases calibrate_energy,GammaSpectra,list-method
setMethod(
  f = "calibrate_energy",
  signature = signature(object = "GammaSpectra", lines = "list"),
  definition = function(object, lines, ...) {

    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    energy <- lapply(X = spectra, FUN = calibrate_energy, lines = lines, ...)
    names(energy) <- names(object)

    .GammaSpectra(energy)
  }
)

#' @export
#' @rdname calibrate
#' @aliases calibrate_energy,GammaSpectrum,list-method
setMethod(
  f = "calibrate_energy",
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
    do_calibrate_energy(object, fit_data)
  }
)

#' @export
#' @rdname calibrate
#' @aliases calibrate_energy,GammaSpectrum,PeakPosition-method
setMethod(
  f = "calibrate_energy",
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
    do_calibrate_energy(object, clean)
  }
)

# ==============================================================================
#' (Re)Calibrate the Energy Scale
#'
#' @param spectrum A \linkS4class{GammaSpectrum} object.
#' @param lines A \code{\link[=data.frame]{data frame}}.
#' @return Returns a new \linkS4class{GammaSpectrum} object with an
#'  adjusted energy scale.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
do_calibrate_energy <- function(spectrum, lines) {
  # Validation
  n_lines <- nrow(lines)
  if (n_lines < 3) {
    msg <- "You have to provide at least 3 lines for calibration, not %d."
    stop(sprintf(msg, n_lines), call. = FALSE)
  }
  # Get spectrum data
  spc_data <- methods::as(spectrum, "data.frame")

  # Adjust spectrum for energy shift
  ## Fit second order polynomial
  fit_poly <- stats::lm(energy ~ stats::poly(chanel, degree = 2, raw = TRUE),
                        data = lines)
  ## Predict shifted energy values
  fit_spc <- stats::predict(fit_poly, spc_data[, "chanel", drop = FALSE])

  # Return a new gamma spectrum with adjusted energy
  methods::initialize(spectrum, energy = fit_spc, calibration = fit_poly)
}
