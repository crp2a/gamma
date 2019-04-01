# ACCESSORS
#' @include AllClasses.R
NULL

# GammaSpectrum ================================================================
#' @export
#' @rdname GammaSpectrum
#' @aliases [[,GammaSpectrum-method
setMethod(
  f = "[[",
  signature = "GammaSpectrum",
  definition = function(x, i) {
    i <- match.arg(i, choices = methods::slotNames("GammaSpectrum"),
                   several.ok = FALSE)
    data <- methods::slot(x, i)
    return(data)
  }
)

# GammaSpectra ================================================================
#' @export
#' @rdname GammaSpectra
#' @aliases [,GammaSpectra-method
setMethod(
  f = "[",
  signature = "GammaSpectra",
  definition = function(x, i, j) {
    spc_list <- x@.Data
    names(spc_list) <- names(x) # Fix names

    if (missing(i)) {
      i <- seq_along(spc_list)
    } else {
      if (is.null(i)) i <- seq_along(spc_list)
      if (is.character(i) | is.factor(i)) i <- which(names(spc_list) %in% i)
      if (is.numeric(i)) i <- as.integer(i)
    }

    # Select spectra
    items <- spc_list[i]

    if (!missing(j)) {
      # Select slot
      slot <- lapply(X = items, FUN = "[[", j)
      # if (!drop)
      #   slot <- as.data.frame(slot, fix.empty.names = TRUE)
      return(slot)
    } else {
      methods::new("GammaSpectra", items)
    }
  }
)

#' @export
#' @rdname GammaSpectra
#' @aliases [[,GammaSpectra-method
setMethod(
  f = "[[",
  signature = "GammaSpectra",
  definition = function(x, i) {
    if (is.character(i) | is.factor(i)) i <- which(names(x) %in% i)
    if (is.numeric(i)) i <- as.integer(i)
    item <- x@.Data[[i]]
    return(item)
  }
)

# CalibrationCurve =============================================================
#' @export
#' @rdname CalibrationCurve
#' @aliases [[,CalibrationCurve-method
setMethod(
  f = "[[",
  signature = "CalibrationCurve",
  definition = function(x, i) {
    i <- match.arg(i, choices = methods::slotNames("CalibrationCurve"),
                   several.ok = FALSE)
    data <- methods::slot(x, i)
    return(data)
  }
)

# DoseRate =====================================================================
#' @export
#' @rdname DoseRate
#' @aliases [[,DoseRate-method
setMethod(
  f = "[[",
  signature = "DoseRate",
  definition = function(x, i) {
    i <- match.arg(i, choices = methods::slotNames("DoseRate"),
                   several.ok = FALSE)
    data <- methods::slot(x, i)
    return(data)
  }
)
