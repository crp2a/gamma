# ACCESSORS
#' @include AllClasses.R
NULL

# ================================================================ GammaSpectrum
#' @export
#' @rdname subset
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
#' @rdname access
#' @aliases get_hash,GammaSpectrum-method
setMethod(
  f = "get_hash",
  signature = "GammaSpectrum",
  definition = function(object) object@hash
)

#' @export
#' @rdname access
#' @aliases get_length,GammaSpectrum-method
setMethod(
  f = "get_chanels",
  signature = "GammaSpectrum",
  definition = function(object) length(object@chanel)
)

#' @export
#' @rdname access
#' @aliases get_energy,GammaSpectrum-method
setMethod(
  f = "get_energy",
  signature = "GammaSpectrum",
  definition = function(object) range(object@energy)
)

#' @export
#' @rdname access
#' @aliases get_dose,GammaSpectrum-method
setMethod(
  f = "get_dose",
  signature = "GammaSpectrum",
  definition = function(object) object@dose_rate
)

#' @export
#' @rdname access
#' @aliases set_dose,GammaSpectrum-method
setMethod(
  f = "set_dose<-",
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
#' @rdname subset
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
#' @rdname subset
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
#' @rdname access
#' @aliases get_hash,GammaSpectra-method
setMethod(
  f = "get_hash",
  signature = "GammaSpectra",
  definition = function(object) {
    vapply(object, FUN = get_hash, FUN.VALUE = character(1))
  }
)

#' @export
#' @rdname access
#' @aliases get_length,GammaSpectra-method
setMethod(
  f = "get_chanels",
  signature = "GammaSpectra",
  definition = function(object) {
    vapply(object, FUN = get_chanels, FUN.VALUE = numeric(1))
  }
)

#' @export
#' @rdname access
#' @aliases get_energy,GammaSpectra-method
setMethod(
  f = "get_energy",
  signature = "GammaSpectra",
  definition = function(object) {
    vapply(object, FUN = get_energy, FUN.VALUE = numeric(2))
  }
)

#' @export
#' @rdname access
#' @aliases get_dose,GammaSpectra-method
setMethod(
  f = "get_dose",
  signature = "GammaSpectra",
  definition = function(object) {
    dose <- vapply(object, FUN = get_dose, FUN.VALUE = numeric(2))
    dose <- as.data.frame(t(dose))
    colnames(dose) <- c("value", "error")
    dose
  }
)

#' @export
#' @rdname access
#' @aliases set_dose,GammaSpectra-method
setMethod(
  f = "set_dose<-",
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
#' @rdname subset
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
#' @rdname subset
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
#' @rdname access
#' @aliases get_chanels,PeakPosition-method
setMethod(
  f = "get_chanels",
  signature = "PeakPosition",
  definition = function(object) length(object@chanel)
)

#' @export
#' @rdname access
#' @aliases get_energy,PeakPosition-method
setMethod(
  f = "get_energy",
  signature = "PeakPosition",
  definition = function(object) object@energy
)

#' @export
#' @rdname access
#' @aliases set_energy,PeakPosition-method
setMethod(
  f = "set_energy<-",
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
