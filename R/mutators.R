# GETTERS AND SETTERS
#' @include AllClasses.R AllGenerics.R
NULL

# ====================================================================== Getters
# ---------------------------------------------------------------- GammaSpectrum
#' @export
#' @rdname mutator
#' @aliases length,GammaSpectrum-method
setMethod(
  f = "length",
  signature = "GammaSpectrum",
  definition = function(x) length(x@chanel)
)

#' @export
#' @rdname mutator
#' @aliases get_hash,GammaSpectrum-method
setMethod(
  f = "get_hash",
  signature = "GammaSpectrum",
  definition = function(x) x@hash
)

#' @export
#' @rdname mutator
#' @aliases get_names,GammaSpectrum-method
setMethod(
  f = "get_names",
  signature = "GammaSpectrum",
  definition = function(x) x@name
)

#' @export
#' @rdname mutator
#' @aliases get_livetime,GammaSpectrum-method
setMethod(
  f = "get_livetime",
  signature = "GammaSpectrum",
  definition = function(x) x@live_time
)

#' @export
#' @rdname mutator
#' @aliases get_realtime,GammaSpectrum-method
setMethod(
  f = "get_realtime",
  signature = "GammaSpectrum",
  definition = function(x) x@real_time
)

#' @export
#' @rdname mutator
#' @aliases get_chanels,GammaSpectrum-method
setMethod(
  f = "get_chanels",
  signature = "GammaSpectrum",
  definition = function(x) x@chanel
)

#' @export
#' @rdname mutator
#' @aliases get_counts,GammaSpectrum-method
setMethod(
  f = "get_counts",
  signature = "GammaSpectrum",
  definition = function(x) x@count
)

#' @export
#' @rdname mutator
#' @aliases get_rates,GammaSpectrum-method
setMethod(
  f = "get_rates",
  signature = "GammaSpectrum",
  definition = function(x) x@rate
)

#' @export
#' @rdname mutator
#' @aliases get_energy,GammaSpectrum-method
setMethod(
  f = "get_energy",
  signature = "GammaSpectrum",
  definition = function(x) x@energy
)

#' @export
#' @rdname mutator
#' @aliases range_energy,GammaSpectrum-method
setMethod(
  f = "range_energy",
  signature = "GammaSpectrum",
  definition = function(x, na.rm = FALSE) {
    if (length(x@energy) == 0) return(c(NA, NA))
    range(x@energy, na.rm = na.rm)
  }
)

#' @export
#' @rdname mutator
#' @aliases range_chanels,GammaSpectrum-method
setMethod(
  f = "range_chanels",
  signature = "GammaSpectrum",
  definition = function(x, na.rm = FALSE) {
    if (length(x@chanel) == 0) return(c(NA, NA))
    range(x@chanel, na.rm = na.rm)
  }
)

# ----------------------------------------------------------------- GammaSpectra
#' @export
#' @rdname mutator
#' @aliases get_hash,GammaSpectra-method
setMethod(
  f = "get_hash",
  signature = "GammaSpectra",
  definition = function(x) {
    vapply(x, FUN = get_hash, FUN.VALUE = character(1))
  }
)

#' @export
#' @rdname mutator
#' @aliases get_names,GammaSpectra-method
setMethod(
  f = "get_names",
  signature = "GammaSpectra",
  definition = function(x) {
    vapply(x, FUN = get_names, FUN.VALUE = character(1))
  }
)

#' @export
#' @rdname mutator
#' @aliases get_livetime,GammaSpectra-method
setMethod(
  f = "get_livetime",
  signature = "GammaSpectra",
  definition = function(x) {
    vapply(x, FUN = get_livetime, FUN.VALUE = numeric(1))
  }
)

#' @export
#' @rdname mutator
#' @aliases get_realtime,GammaSpectra-method
setMethod(
  f = "get_realtime",
  signature = "GammaSpectra",
  definition = function(x) {
    vapply(x, FUN = get_realtime, FUN.VALUE = numeric(1))
  }
)

#' @export
#' @rdname mutator
#' @aliases get_chanels,GammaSpectra-method
setMethod(
  f = "get_chanels",
  signature = "GammaSpectra",
  definition = function(x) {
    lapply(x, FUN = get_chanels)
  }
)

#' @export
#' @rdname mutator
#' @aliases get_counts,GammaSpectra-method
setMethod(
  f = "get_counts",
  signature = "GammaSpectra",
  definition = function(x) {
    lapply(x, FUN = get_counts)
  }
)

#' @export
#' @rdname mutator
#' @aliases get_rates,GammaSpectra-method
setMethod(
  f = "get_rates",
  signature = "GammaSpectra",
  definition = function(x) {
    lapply(x, FUN = get_rates)
  }
)

#' @export
#' @rdname mutator
#' @aliases get_energy,GammaSpectra-method
setMethod(
  f = "get_energy",
  signature = "GammaSpectra",
  definition = function(x) {
    lapply(x, FUN = get_energy)
  }
)

#' @export
#' @rdname mutator
#' @aliases range_energy,GammaSpectra-method
setMethod(
  f = "range_energy",
  signature = "GammaSpectra",
  definition = function(x, na.rm = FALSE) {
    energy <- vapply(x, FUN = range_energy, FUN.VALUE = numeric(2),
                     na.rm = na.rm)
    energy <- t(energy)
    colnames(energy) <- c("min", "max")
    energy
  }
)

#' @export
#' @rdname mutator
#' @aliases range_chanels,GammaSpectra-method
setMethod(
  f = "range_chanels",
  signature = "GammaSpectra",
  definition = function(x, na.rm = FALSE) {
    chanels <- vapply(x, FUN = range_chanels, FUN.VALUE = numeric(2),
                      na.rm = na.rm)
    chanels <- t(chanels)
    colnames(chanels) <- c("min", "max")
    chanels
  }
)

# ----------------------------------------------------------------- PeakPosition
#' @export
#' @rdname mutator
#' @aliases get_hash,PeakPosition-method
setMethod(
  f = "get_hash",
  signature = "PeakPosition",
  definition = function(x) x@hash
)

#' @export
#' @rdname mutator
#' @aliases get_chanels,PeakPosition-method
setMethod(
  f = "get_chanels",
  signature = "PeakPosition",
  definition = function(x) x@chanel
)

#' @export
#' @rdname mutator
#' @aliases get_energy,PeakPosition-method
setMethod(
  f = "get_energy",
  signature = "PeakPosition",
  definition = function(x) x@energy
)

# ====================================================================== Setters
# ---------------------------------------------------------------- GammaSpectrum
#' @export
#' @rdname mutator
#' @aliases set_names,GammaSpectrum-method
setMethod(
  f = "set_names<-",
  signature = "GammaSpectrum",
  definition = function(x, value) {
    x@name <- as.character(value)
    methods::validObject(x)
    x
  }
)

# ----------------------------------------------------------------- GammaSpectra
#' @export
#' @rdname mutator
#' @aliases set_name,GammaSpectra-method
setMethod(
  f = "set_names<-",
  signature = "GammaSpectra",
  definition = function(x, value) {
    names(x) <- value
    mapply(FUN = methods::`slot<-`, object = x, value = value,
           MoreArgs = list(name = "name"), SIMPLIFY = FALSE)
    methods::validObject(x)
    x
  }
)

# ----------------------------------------------------------------- PeakPosition
#' @export
#' @rdname mutator
#' @aliases set_energy,PeakPosition,numeric-method
setMethod(
  f = "set_energy<-",
  signature = c(x = "PeakPosition", value = "numeric"),
  definition = function(x, value) {
    # Keep only complete cases
    k <- if (anyNA(value)) which(!is.na(value)) else seq_along(value)
    x@energy <- value[k]
    x@chanel <- x@chanel[k]
    methods::validObject(x)
    x
  }
)

