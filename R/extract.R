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
#' @aliases get_names,GammaSpectrum-method
setMethod(
  f = "get_names",
  signature = "GammaSpectrum",
  definition = function(object) object@name
)

#' @export
#' @rdname access
#' @aliases set_names,GammaSpectrum-method
setMethod(
  f = "set_names<-",
  signature = "GammaSpectrum",
  definition = function(object, value) {
    object@name <- value
    methods::validObject(object)
    object
  }
)

#' @export
#' @rdname access
#' @aliases get_time,GammaSpectrum-method
setMethod(
  f = "get_time",
  signature = "GammaSpectrum",
  definition = function(object, type = c("live", "real")) {
    type <- match.arg(type, several.ok = FALSE)
    time <- switch (
      type,
      live = object@live_time,
      real = object@real_time
    )
    time
  }
)

#' @export
#' @rdname access
#' @aliases get_chanels,GammaSpectrum-method
setMethod(
  f = "get_chanels",
  signature = "GammaSpectrum",
  definition = function(object) object@chanel
)

#' @export
#' @rdname access
#' @aliases get_nchanels,GammaSpectrum-method
setMethod(
  f = "get_nchanels",
  signature = "GammaSpectrum",
  definition = function(object) length(object@chanel)
)

#' @export
#' @rdname access
#' @aliases get_counts,GammaSpectrum-method
setMethod(
  f = "get_counts",
  signature = "GammaSpectrum",
  definition = function(object) object@count
)

#' @export
#' @rdname access
#' @aliases get_rates,GammaSpectrum-method
setMethod(
  f = "get_rates",
  signature = "GammaSpectrum",
  definition = function(object) object@rate
)

#' @export
#' @rdname access
#' @aliases get_energy,GammaSpectrum-method
setMethod(
  f = "get_energy",
  signature = "GammaSpectrum",
  definition = function(object) object@energy
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

#' @export
#' @rdname access
#' @aliases range_energy,GammaSpectrum-method
setMethod(
  f = "range_energy",
  signature = "GammaSpectrum",
  definition = function(object) range(object@energy)
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
#' @aliases get_names,GammaSpectra-method
setMethod(
  f = "get_names",
  signature = "GammaSpectra",
  definition = function(object) {
    vapply(object, FUN = get_names, FUN.VALUE = character(1))
  }
)

#' @export
#' @rdname access
#' @aliases set_name,GammaSpectra-method
setMethod(
  f = "set_names<-",
  signature = "GammaSpectra",
  definition = function(object, value) {
    names(object) <- value
    mapply(FUN = methods::`slot<-`, object = object, value = value,
           MoreArgs = list(name = "name"), SIMPLIFY = FALSE)

    methods::validObject(object)
    object
  }
)

#' @export
#' @rdname access
#' @aliases get_time,GammaSpectra-method
setMethod(
  f = "get_time",
  signature = "GammaSpectra",
  definition = function(object, type = c("live", "real")) {
    vapply(object, FUN = get_time, FUN.VALUE = numeric(1))
  }
)

#' @export
#' @rdname access
#' @aliases get_chanels,GammaSpectra-method
setMethod(
  f = "get_chanels",
  signature = "GammaSpectra",
  definition = function(object, simplify = FALSE) {
    chanels <- lapply(object, FUN = get_chanels)
    if (simplify) chanels <- cbind(chanels)
    chanels
  }
)

#' @export
#' @rdname access
#' @aliases get_nchanels,GammaSpectra-method
setMethod(
  f = "get_nchanels",
  signature = "GammaSpectra",
  definition = function(object) {
    vapply(object, FUN = get_nchanels, FUN.VALUE = numeric(1))
  }
)

#' @export
#' @rdname access
#' @aliases get_counts,GammaSpectra-method
setMethod(
  f = "get_counts",
  signature = "GammaSpectra",
  definition = function(object, simplify = FALSE) {
    counts <- lapply(object, FUN = get_counts)
    if (simplify) counts <- cbind(counts)
    counts
  }
)

#' @export
#' @rdname access
#' @aliases get_rates,GammaSpectra-method
setMethod(
  f = "get_rates",
  signature = "GammaSpectra",
  definition = function(object, simplify = FALSE) {
    rates <- lapply(object, FUN = get_rates)
    if (simplify) rates <- cbind(rates)
    rates
  }
)

#' @export
#' @rdname access
#' @aliases get_energy,GammaSpectra-method
setMethod(
  f = "get_energy",
  signature = "GammaSpectra",
  definition = function(object, simplify = FALSE) {
    energy <- lapply(object, FUN = get_energy)
    if (simplify) energy <- cbind(energy)
    energy
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
    colnames(dose) <- c("gamma_dose", "gamma_error")
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
    if (is.matrix(value) || is.data.frame(value)) {
      object_names <- names(object)
      value <- data.matrix(value)
      if (ncol(value) >= 2) {
        doses <- value[, c(1, 2)]
        doses_names <- rownames(doses)
        # Match by names
        index <- stats::na.omit(match(object_names, doses_names))
        if (length(index) != 0 && length(index) == length(object)) {
          doses <- doses[index, ]
          doses_names <- doses_names[index]
        } else {
          stop("Names of `value` do not match.", call. = FALSE)
        }
      } else {
        stop("`value` must have at least 2 columns.", call. = FALSE)
      }

      doses_ls <- split(x = doses, f = doses_names)
      doses_num <- lapply(X = doses_ls, FUN = as.numeric)

      mapply(FUN = methods::`slot<-`, object = object, value = doses_num,
             MoreArgs = list(name = "dose_rate"), SIMPLIFY = FALSE)

      methods::validObject(object)
      return(object)
    } else {
      stop("`value` must be a matrix or a data.frame.", call. = FALSE)
    }
  }
)

#' @export
#' @rdname access
#' @aliases range_energy,GammaSpectra-method
setMethod(
  f = "range_energy",
  signature = "GammaSpectra",
  definition = function(object) {
    energy <- vapply(object, FUN = range_energy, FUN.VALUE = numeric(2))
    energy <- as.data.frame(t(energy))
    colnames(energy) <- c("min", "max")
    energy
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

#' @export
#' @rdname access
#' @aliases get_model,CalibrationCurve-method
setMethod(
  f = "get_model",
  signature = "CalibrationCurve",
  definition = function(object, threshold = c("Ni", "NiEi")) {
    threshold <- match.arg(threshold, several.ok = FALSE)
    x <- methods::slot(object, threshold)
    y <- rbind(x@slope, x@intercept)
    dimnames(y) <- list(c("slope", "intercept"), c("value", "error"))
    y
  }
)

#' @export
#' @rdname access
#' @aliases get_noise,CalibrationCurve-method
setMethod(
  f = "get_noise",
  signature = "CalibrationCurve",
  definition = function(object, threshold = c("Ni", "NiEi")) {
    threshold <- match.arg(threshold, several.ok = FALSE)
    x <- methods::slot(object, threshold)
    x@background
  }
)

#' @export
#' @rdname access
#' @aliases get_range,CalibrationCurve-method
setMethod(
  f = "get_range",
  signature = "CalibrationCurve",
  definition = function(object, threshold = c("Ni", "NiEi")) {
    threshold <- match.arg(threshold, several.ok = FALSE)
    x <- methods::slot(object, threshold)
    x@range
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
#' @aliases get_hash,PeakPosition-method
setMethod(
  f = "get_hash",
  signature = "PeakPosition",
  definition = function(object) object@hash
)

#' @export
#' @rdname access
#' @aliases get_chanels,PeakPosition-method
setMethod(
  f = "get_chanels",
  signature = "PeakPosition",
  definition = function(object) object@chanel
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
    if (length(value) == 1)
      value <- rep(value, length(object@chanel))
    object@energy <- value
    methods::validObject(object)
    object
  }
)
