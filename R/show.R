# SHOW & SUMMARISE METHODS
#' @include AllClasses.R
NULL

# GammaSpectrum ================================================================
setMethod(
  f = "show",
  signature = "GammaSpectrum",
  definition = function(object) {
    if (get_chanels(object) != 0) {
      meta <- summarise(object)
      meta <- paste(colnames(meta), unlist(meta), sep = ": ")
      cat("Gamma spectrum:", paste("* ", meta), sep = "\n")
    } else {
      cat("An empty gamma spectrum.\n", sep = "")
    }
  }
)

#' @rdname summarise
#' @aliases summarise,GammaSpectrum-method
#' @export
setMethod(
  f = "summarise",
  signature = "GammaSpectrum",
  definition = function(object) {
    E <- if (length(object@energy) != 0) {
      paste0(range(round(object@energy, 2)), collapse = " ")
    } else {
      "not calibrated"
    }
    # D <- if (length(object@dose_rate) != 0) {
    #   paste0(object@dose_rate, collapse = " +/- ")
    # } else {
    #   "unknown"
    # }
    cbind.data.frame(
      name = object@name,
      date = as.character(object@date),
      # Instrument = object@instrument,
      `live time` = object@live_time,
      `real time` = object@real_time,
      chanels = length(object@chanel),
      `energy range` = E,
      # `Dose rate` = D,
      stringsAsFactors = FALSE
    )
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
      ref <- get_name(object)
      cat("A collection of ", n, " gamma ", spc, ": ",
          paste(ref, collapse = ", "), "\n",
          sep = "")
    } else {
      cat("An empty set of gamma spectra.\n", sep = "")
    }
  }
)

#' @rdname summarise
#' @aliases summarise,GammaSpectra-method
#' @export
setMethod(
  f = "summarise",
  signature = "GammaSpectra",
  definition = function(object) {
    sum_up <- lapply(X = object, FUN = summarise)
    do.call(rbind, sum_up)
  }
)

# CalibrationCurve =============================================================
setMethod(
  f = "show",
  signature = "CalibrationCurve",
  definition = function(object) {
    if (length(object@model$coefficients) != 0) {
      meta <- summary(object@model)
      coef <- round(meta$coefficients, 5)
      fstat <- round(meta$fstatistic, 0)
      if (nrow(coef) > 1) {
        intercept <- paste0(coef[1, c(1, 2)], collapse = " +/- ")
        slope <- paste0(coef[2, c(1, 2)], collapse = " +/- ")
      } else {
        intercept <- NA_character_
        slope <- paste0(coef[c(1, 2)], collapse = " +/- ")
      }
      cat(
        "Calibration curve:\n",
        "* Date: ", as.character(object@details$date), "\n",
        "* Model summary:\n",
        "  - slope: ", slope, "\n",
        "  - intercept: ", intercept, "\n",
        "  - residual standard error: ", round(meta$sigma, 2), "\n",
        "  - multiple R-squared: ", round(meta$r.squared, 5), "\n",
        "  - adjusted R-squared: ", round(meta$adj.r.squared, 5), "\n",
        "  - F-statistic: ", fstat[[1]], " on ", fstat[[2]], " and ",
        fstat[[3]], " DF",
        sep = ""
      )
    } else {
      cat("Calibration curve: no model.\n", sep = " ")
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
