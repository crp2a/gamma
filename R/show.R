# SHOW METHODS
#' @include AllClasses.R
NULL

# GammaSpectrum ================================================================
setMethod(
  f = "show",
  signature = "GammaSpectrum",
  definition = function(object) {
    cat("Gamma spectrum:", "\n",
        "  Reference: ", object@reference, "\n",
        "  Instrument: ", object@instrument, "\n",
        "  Date: ", object@date, "\n",
        "  Number of chanels: ", length(object@chanel), "\n",
        "  Energy range (keV): ", paste(range(round(object@energy, 2)),
                                        collapse = "-"),
        sep = "")
  }
)

# GammaSpectra =================================================================
setMethod(
  f = "show",
  signature = "GammaSpectra",
  definition = function(object) {
    n <- length(object)
    spc <- ifelse(n > 1, "spectra", "spectrum")
    cat("A collection of", n, "gamma", spc, sep = " ")
  }
)

# CalibrationCurve =============================================================
setMethod(
  f = "show",
  signature = "CalibrationCurve",
  definition = function(object) {
    sum_up <- summary(object@model)
    cat("Calibration curve:", "\n",
        "  Slope: ", round(stats::coef(object@model), 2), "\n",
        "  Slope error: ", round(sum_up$coef[, "Std. Error"], 2), "\n",
        "  Adjusted R-squared: ", round(sum_up$adj.r.squared, 5),
        sep = " ")
  }
)

# DoseRate =====================================================================
setMethod(
  f = "show",
  signature = "DoseRate",
  definition = function(object) {
    n <- length(object@dose_value)
    est <- ifelse(n > 1, "estimates", "estimate")
    cat("A set of", n, "gamma dose rate", est, sep = " ")
  }
)
