# ACCESSORS
#' @include AllClasses.R
NULL

# ================================================================ GammaSpectrum
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

#' @export
#' @rdname GammaSpectrum
#' @aliases length,GammaSpectrum-method
setMethod(
  f = "length",
  signature = "GammaSpectrum",
  definition = function(x) {
    length(x@chanel)
  }
)

#' @export
#' @rdname extract
#' @aliases getDoseRate,GammaSpectrum-method
setMethod(
  f = "getDoseRate",
  signature = "GammaSpectrum",
  definition = function(object) object@dose_rate
)

#' @export
#' @rdname extract
#' @aliases setDoseRate,GammaSpectrum-method
setMethod(
  f = "setDoseRate<-",
  signature = "GammaSpectrum",
  definition = function(object, value) {
    if (!is.numeric(value) || length(value) != 2)
      stop("`value` must be a length-two numeric vector.", call. = FALSE)

    names(value) <- c("value", "error")
    object@dose_rate <- value
    methods::validObject(object)
    object
  }
)

# ================================================================= GammaSpectra
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

#' @export
#' @rdname extract
#' @aliases getDoseRate,GammaSpectra-method
setMethod(
  f = "getDoseRate",
  signature = "GammaSpectra",
  definition = function(object) {
    spc <- methods::S3Part(object, strictS3 = TRUE)
    dose_rate <- lapply(
      X = spc,
      FUN = function(x) methods::slot(x, "dose_rate")
    )
    if (length(unlist(dose_rate)) == 0)
      stop("No dose rate available for these spectra.", call. = FALSE)

    dose_mtx <- do.call(rbind, dose_rate) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("value", "error"))

    as.matrix(dose_mtx)
  }
)

#' @export
#' @rdname extract
#' @aliases setDoseRate,GammaSpectra-method
setMethod(
  f = "setDoseRate<-",
  signature = "GammaSpectra",
  definition = function(object, value) {
    # Validation
    if (!is.list(value))
      stop("`value` must be a list.", call. = FALSE)
    length_value <- length(value)
    length_object <- length(object)
    if (any(lengths(value) != rep(2, length_value)))
      stop("`value` must be a list of length-two numeric vectors.",
           call. = FALSE)

    names_doses <- names(value)
    names_object <- names(object)
    if (is.null(names_doses)) {
      if (length_value != length_object) {
        stop(sprintf("`value` must be of length %d (not %d), ",
                     length_object, length_value),
             "otherwise each element of `value` must be named.", call. = FALSE)
      } else {
        mapply(FUN = methods::`slot<-`, object = object, value = value,
               MoreArgs = list(name = "dose_rate"), SIMPLIFY = FALSE)
      }
    } else {
      sub_object <- object[names_doses]
      if (length(sub_object) == 0)
        stop("Names of `value` do not match.", call. = FALSE)
      mapply(FUN = methods::`slot<-`, object = sub_object, value = value,
             MoreArgs = list(name = "dose_rate"), SIMPLIFY = FALSE)
    }

    methods::validObject(object)
    object
  }
)

# ============================================================= CalibrationCurve
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

# ======================================================================== Peaks
#' @export
#' @rdname PeakPosition
#' @aliases [[,PeakPosition-method
setMethod(
  f = "[[",
  signature = "PeakPosition",
  definition = function(x, i) {
    i <- match.arg(i, choices = methods::slotNames("PeakPosition"),
                   several.ok = FALSE)
    data <- methods::slot(x, i)
    return(data)
  }
)
#' @export
#' @rdname PeakModel
#' @aliases [[,PeakModel-method
setMethod(
  f = "[[",
  signature = "PeakModel",
  definition = function(x, i) {
    i <- match.arg(i, choices = methods::slotNames("PeakModel"),
                   several.ok = FALSE)
    data <- methods::slot(x, i)
    return(data)
  }
)
