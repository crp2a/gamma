# SHOW METHODS
#' @include AllClasses.R
NULL

# GammaSpectrum ================================================================
setMethod(
  f = "show",
  signature = "GammaSpectrum",
  definition = function(object) {
    if (length(object) != 0) {
      E <- if (length(object@energy) != 0) {
        paste0(range(round(object@energy, 2)), collapse = " - ")
      } else {
        "not calibrated"
      }
      D <- if (length(object@dose_rate) != 0) {
        paste0(object@dose_rate, collapse = " +/- ")
      } else {
        "not known"
      }
      cat("Gamma spectrum:", "\n",
          "  Reference: ", object@reference, "\n",
          "  Instrument: ", object@instrument, "\n",
          "  Date: ", as.character(object@date), "\n",
          "  Number of chanels: ", length(object@chanel), "\n",
          "  Energy range (keV): ", E, "\n",
          "  Dose rate: ", D, "\n",
          sep = "")
    } else {
      cat("An empty gamma spectrum.\n", sep = "")
    }
  }
)

# GammaSpectra =================================================================
setMethod(
  f = "show",
  signature = "GammaSpectra",
  definition = function(object) {
    n <- length(object)
    if (n != 0) {
      spc <- ngettext(n, "spectrum", "spectra", )
      ref <- names(object)
      cat("A collection of ", n, " gamma ", spc, ": ",
          paste(ref, collapse = ", "), "\n",
          sep = "")
    } else {
      cat("An empty set of gamma spectra.\n", sep = "")
    }
  }
)

# CalibrationCurve =============================================================
setMethod(
  f = "show",
  signature = "CalibrationCurve",
  definition = function(object) {
    if (length(object@model) != 0) {
      sum_up <- summary(object@model)
      cat("Calibration curve:\n",
          "  Details:\n",
          "  - Laboratory:", object@details$laboratory, "\n",
          "  - Instrument:", object@details$instrument, "\n",
          "  - Detector:", object@details$detector, "\n",
          "  - Authors:", object@details$authors, "\n",
          "  - Date:", as.character(object@details$date), "\n",
          "  Model summary:\n",
          "  - Slope:", round(sum_up$coef[2,1], 3), "+/-",
          round(sum_up$coef[2,2], 3), "\n",
          "  - Intercept:", round(sum_up$coef[1,1], 3), "+/-",
          round(sum_up$coef[1,2], 3), "\n",
          "  - Residual standard error:", round(sum_up$sigma, 2), "\n",
          "  - Multiple R-squared:", round(sum_up$r.squared, 5), "\n",
          "  - Adjusted R-squared:", round(sum_up$adj.r.squared, 5), "\n",
          sep = " ")
    } else {
      cat("Calibration curve: no model.\n",
          sep = " ")
    }
  }
)

# DoseRate =====================================================================
setMethod(
  f = "show",
  signature = "DoseRate",
  definition = function(object) {
    n <- length(object@dose_value)
    est <- ngettext(n, "estimate", "estimates")
    cat(n, "gamma dose rate", est, "\n", sep = " ")
    print(methods::as(object, "data.frame"))
  }
)

# PeakPosition =================================================================
setMethod(
  f = "show",
  signature = "PeakPosition",
  definition = function(object) {
    if (length(object@peaks) != 0) {
      n <- nrow(object@peaks)
      pks <- ngettext(n, " peak was ", " peaks were ")
      cat(n, pks, "detected:", "\n",
          "  Position (chanel):\t", paste(object@peaks$chanel, collapse = "\t"), "\n",
          "  Height (count): \t", paste(object@peaks$counts, collapse = "\t"), "\n",
          sep = "")
    } else {
      cat("No peaks were detected.\n", sep = " ")
    }
  }
)

# PeakModel ====================================================================
setMethod(
  f = "show",
  signature = "PeakModel",
  definition = function(object) {
    if (length(object@coefficients) != 0) {
      n <- nrow(object@coefficients)
      pks <- ngettext(n, " peak was ", " peaks were ")
      cat(n, pks, "estimated:\n", sep = "")
      print(object@coefficients)
    } else {
      cat("No peaks parameters were estimated.\n", sep = " ")
    }
  }
)
