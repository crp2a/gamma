# SHOW & SUMMARISE METHODS
#' @include AllClasses.R
NULL

# ================================================================ GammaSpectrum
setMethod(
  f = "show",
  signature = "GammaSpectrum",
  definition = function(object) {
    if (get_nchanels(object) != 0) {
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
    cbind.data.frame(
      name = object@name,
      date = as.character(object@date),
      live_time = object@live_time,
      real_time = object@real_time,
      chanels = length(object@chanel),
      energy_min = ifelse(length(object@energy) != 0, min(object@energy), NA),
      energy_max = ifelse(length(object@energy) != 0, max(object@energy), NA),
      stringsAsFactors = FALSE
    )
  }
)

# ================================================================= GammaSpectra
setMethod(
  f = "show",
  signature = "GammaSpectra",
  definition = function(object) {
    n <- length(object)
    if (n != 0) {
      spc <- ngettext(n, "spectrum", "spectra", )
      ref <- get_names(object)
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

# ================================================================ DoseRateModel
setMethod(
  f = "show",
  signature = "DoseRateModel",
  definition = function(object) {
    cat("Dose vs signal linear model:\n", sep = "")
  }
)

# ============================================================= CalibrationCurve
setMethod(
  f = "show",
  signature = "CalibrationCurve",
  definition = function(object) {
    details <- object[["details"]]
    info <- vapply(
      X = details,
      FUN = function(x) paste0(x, collapse = ", "),
      FUN.VALUE = character(1)
    )
    info <- paste0("* ", names(details), ": ", info, "\n")
    cat("Calibration curve:\n", info, sep = "")
  }
)

# ================================================================= PeakPosition
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
