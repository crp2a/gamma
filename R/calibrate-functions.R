#' (Re)Calibrate the Energy Scale
#'
#' @param spectrum A \linkS4class{GammaSpectrum} object.
#' @param lines A \code{\link[=data.frame]{data frame}}.
#' @return Returns a new \linkS4class{GammaSpectrum} object with an
#'  adjusted energy scale.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
.calibrate <- function(spectrum, lines) {
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
