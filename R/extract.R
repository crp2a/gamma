# ACCESSORS
#' @include AllClasses.R
NULL

# ================================================================ GammaSpectrum
#' @export
#' @rdname GammaSpectrum-class
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
#' @rdname GammaSpectrum-class
#' @aliases length,GammaSpectrum-method
setMethod(
  f = "length",
  signature = "GammaSpectrum",
  definition = function(x) {
    max(length(x@chanel), length(x@energy))
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
#' @rdname GammaSpectra-class
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
#' @rdname GammaSpectra-class
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

    names_object <- names(object)

    if (is.matrix(value) || is.data.frame(value)) {
      names_doses <- rownames(value)
      if (length(names_doses) != 0) {
        index <- stats::na.omit(match(names_object, names_doses))
        if (length(index) == 0)
          stop("Names of `value` do not match.", call. = FALSE)
        value <- value[index, ]
        names_doses <- names_doses[index]
      }
      doses_ls <- split(x = value, f = names_doses)
      doses_num <- lapply(X = doses_ls, FUN = as.numeric)
    }

    mapply(FUN = methods::`slot<-`, object = object, value = doses_num,
             MoreArgs = list(name = "dose_rate"), SIMPLIFY = FALSE)

    methods::validObject(object)
    object
  }
)

# ============================================================= CalibrationCurve
#' @export
#' @rdname CalibrationCurve-class
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
#' @rdname PeakPosition-class
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
#' @rdname extract
#' @aliases getEnergy,PeakPosition-method
setMethod(
  f = "getEnergy",
  signature = "PeakPosition",
  definition = function(object) object@energy
)

#' @export
#' @rdname extract
#' @aliases setEnergy,PeakPosition-method
setMethod(
  f = "setEnergy<-",
  signature = "PeakPosition",
  definition = function(object, value) {
    if (!is.atomic(value) || !is.numeric(value))
      stop("`value` must be a numeric vector.", call. = FALSE)

    index <- which(!is.na(value))
    object@energy <- value
    methods::validObject(object)
    object
  }
)

#' @export
#' @rdname PeakModel-class
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
