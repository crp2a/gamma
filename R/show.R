# SHOW METHODS
#' @include AllClasses.R
NULL

# GammaSpectrum ================================================================
setMethod(
  f = "show",
  signature = "GammaSpectrum",
  definition = function(object) {
    if (length(object@energy) == 0) {
      E <- "not calibrated"
    } else {
      E <- paste(range(round(object@energy, 2)), collapse = "-")
    }

    cat("Gamma spectrum:", "\n",
        "  Reference: ", object@reference, "\n",
        "  Instrument: ", object@instrument, "\n",
        "  Date: ", as.character(object@date, format = c("%Y-%m-%d")), "\n",
        "  Number of chanels: ", length(object@chanel), "\n",
        "  Energy range (keV): ", E, "\n",
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
    ref <- names(object)
    cat("A collection of ", n, " gamma ", spc, ": ",
        paste(ref, collapse = ", "), "\n",
        sep = "")
  }
)

# CalibrationCurve =============================================================
setMethod(
  f = "show",
  signature = "CalibrationCurve",
  definition = function(object) {
    sum_up <- summary(object@model)
    cat("Calibration curve:", "\n",
        "  Residual standard error: ", round(sum_up$sigma, 2), "\n",
        "  Multiple R-squared: ", round(sum_up$r.squared, 5), "\n",
        "  Adjusted R-squared: ", round(sum_up$adj.r.squared, 5), "\n",
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
    cat("A set of", n, "gamma dose rate", est, "\n", sep = " ")
  }
)
