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
        paste(range(round(object@energy, 2)), collapse = "-")
      } else {
        "not calibrated"
      }

      cat("Gamma spectrum:", "\n",
          "  Reference: ", object@reference, "\n",
          "  Instrument: ", object@instrument, "\n",
          "  Date: ", as.character(object@date, format = c("%Y-%m-%d")), "\n",
          "  Number of chanels: ", length(object@chanel), "\n",
          "  Energy range (keV): ", E, "\n",
          sep = "")
    } else {
      cat("An empty gamma spectrum\n", sep = "")
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
      spc <- ifelse(n > 1, "spectra", "spectrum")
      ref <- names(object)
      cat("A collection of ", n, " gamma ", spc, ": ",
          paste(ref, collapse = ", "), "\n",
          sep = "")
    } else {
      cat("An empty set of gamma spectra\n", sep = "")
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
      cat("Calibration curve:", "\n",
          "  Residual standard error:", round(sum_up$sigma, 2), "\n",
          "  Multiple R-squared:", round(sum_up$r.squared, 5), "\n",
          "  Adjusted R-squared:", round(sum_up$adj.r.squared, 5), "\n",
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
    est <- ifelse(n > 1, "estimates", "estimate")
    cat("A set of", n, "gamma dose rate", est, "\n", sep = " ")
  }
)

# PeakPosition =================================================================
setMethod(
  f = "show",
  signature = "PeakPosition",
  definition = function(object) {
    if (length(object@peaks) != 0) {
      n <- nrow(object@peaks)
      pks <- ifelse(n > 1, " peaks were ", " peak was ")
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
    if (length(object@peaks) != 0) {
      param <- lapply(X = object@model, FUN = coef) %>%
        do.call(cbind, .) %>%
        t() %>%
        as.data.frame() %>%
        signif(digits = 6)

      n <- nrow(object@peaks)
      pks <- ifelse(n > 1, " peaks were ", " peak was ")
      cat(n, pks, "estimated:", "\n",
          "  Mean (chanel):\t", paste(param$mu, collapse = "\t"), "\n",
          "  Std. dev. (chanel):\t", paste(param$sigma, collapse = "\t"), "\n",
          "  Height (count): \t", paste(param$C, collapse = "\t"), "\n",
          sep = "")
    } else {
      cat("No peaks paramters were estimated.\n", sep = " ")
    }
  }
)
