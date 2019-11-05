# SHOW METHODS
#' @include AllClasses.R
NULL

# GammaSpectrum ================================================================
setMethod(
  f = "show",
  signature = "GammaSpectrum",
  definition = function(object) {
    if (get_chanels(object) != 0) {
      E <- if (is_calibrated(object)) {
        paste0(range(round(object@energy, 2)), collapse = " - ")
      } else {
        "not calibrated"
      }
      D <- if (length(object@dose_rate) != 0) {
        paste0(object@dose_rate, collapse = " +/- ")
      } else {
        "unknown"
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
    if (length(object@model$coefficients) != 0) {
      sum_up <- summary(object@model)
      coef <- round(sum_up$coef, 5)
      fstat <- round(sum_up$fstatistic, 0)
      if (nrow(coef) > 1) {
        intercept <- paste(coef[1, 1], "+/-", coef[1, 2], sep = " ")
        slope <- paste(coef[2, 1], "+/-", coef[2, 2], sep = " ")
      } else {
        intercept <- "0 (not estimated)"
        slope <- paste(coef[1], "+/-", coef[2], sep = " ")
      }
      cat("Calibration curve:\n",
          "  Date:", as.character(object@details$date), "\n",
          "  Model summary:\n",
          "  - Slope:", slope, "\n",
          "  - Intercept:", intercept, "\n",
          "  - Residual standard error:", round(sum_up$sigma, 2), "\n",
          "  - Multiple R-squared:", round(sum_up$r.squared, 5), "\n",
          "  - Adjusted R-squared:", round(sum_up$adj.r.squared, 5), "\n",
          "  - F-statistic:", fstat[[1]], "on", fstat[[2]], "and", fstat[[3]], "DF",
          sep = " ")
    } else {
      cat("Calibration curve: no model.\n",
          sep = " ")
    }
  }
)

# PeakPosition =================================================================
setMethod(
  f = "show",
  signature = "PeakPosition",
  definition = function(object) {
    peaks <- methods::as(object, "data.frame")
    n <- nrow(peaks)
    if (!all(is.na(peaks))) {
      pks <- ngettext(n, " peak was ", " peaks were ")
      cat(n, pks, "detected:\n", sep = "")
      print(peaks)
    } else {
      cat("No peaks were detected.\n", sep = " ")
    }
  }
)
