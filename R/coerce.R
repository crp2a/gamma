# COERCION
#' @include AllClasses.R AllGenerics.R
NULL

# To matrix ====================================================================
## GammaSpectrum ---------------------------------------------------------------
setAs(
  from = "GammaSpectrum",
  to = "matrix",
  def = function(from) {
    cbind(
      channel = if (length(from@channel) != 0) from@channel else NA_real_,
      energy = if (length(from@energy) != 0) from@energy else NA_real_,
      count = if (length(from@count) != 0) from@count else NA_real_,
      rate = if (length(from@rate) != 0) from@rate else NA_real_
    )
  }
)

#' @export
#' @method as.matrix GammaSpectrum
as.matrix.GammaSpectrum <- function(x, ...) methods::as(x, "matrix")

#' @export
#' @rdname coerce
#' @aliases as.matrix,GammaSpectrum-method
setMethod("as.matrix", "GammaSpectrum", as.matrix.GammaSpectrum)

## PeakPosition ----------------------------------------------------------------
setAs(
  from = "PeakPosition",
  to = "matrix",
  def = function(from) {
    cbind(
      channel = if (length(from@channel) != 0) from@channel else NA_real_,
      energy_observed = if (length(from@energy_observed) != 0) from@energy_observed else NA_real_,
      energy_expected = if (length(from@energy_expected) != 0) from@energy_expected else NA_real_
    )
  }
)

#' @export
#' @method as.matrix PeakPosition
as.matrix.PeakPosition <- function(x, ...) methods::as(x, "matrix")

#' @export
#' @rdname coerce
#' @aliases as.matrix,PeakPosition-method
setMethod("as.matrix", "PeakPosition", as.matrix.PeakPosition)

# To data.frame ================================================================
## GammaSpectrum ---------------------------------------------------------------
setAs(
  from = "GammaSpectrum",
  to = "data.frame",
  def = function(from) {
    m <- as.matrix(from)
    m <- as.data.frame(m, stringsAsFactors = FALSE)
    m
  }
)

#' @export
#' @method as.data.frame GammaSpectrum
as.data.frame.GammaSpectrum <- function(x, ...) {
  methods::as(x, "data.frame")
}

#' @export
#' @rdname coerce
#' @aliases as.data.frame,GammaSpectrum-method
setMethod("as.data.frame", "GammaSpectrum", as.data.frame.GammaSpectrum)

## GammaSpectra ----------------------------------------------------------------
as_long <- function(from) {
  df_list <- lapply(X = from, FUN = "as", Class = "data.frame")
  df_nrow <- vapply(X = df_list, FUN = nrow, FUN.VALUE = integer(1))
  df_long <- do.call(rbind, df_list)

  ## Keep original ordering
  name <- rep(names(df_list), times = df_nrow)
  df_long$name <- factor(name, levels = unique(name))
  rownames(df_long) <- NULL

  return(df_long)
}

## PeakPosition ----------------------------------------------------------------
setAs(
  from = "PeakPosition",
  to = "data.frame",
  def = function(from) {
    m <- as.matrix(from)
    m <- as.data.frame(m, stringsAsFactors = FALSE)
    m
  }
)

#' @export
#' @method as.data.frame PeakPosition
as.data.frame.PeakPosition <- function(x, ...) {
  methods::as(x, "data.frame")
}

#' @export
#' @rdname coerce
#' @aliases as.data.frame,PeakPosition-method
setMethod("as.data.frame", "PeakPosition", as.data.frame.PeakPosition)

# To list ======================================================================
## PeakPosition ----------------------------------------------------------------
setAs(
  from = "PeakPosition",
  to = "list",
  def = function(from) {
    x <- as.matrix(from)

    ## we opt for the observed energy as to be taken,
    ## the others are just provided
    list(
      channel = x[, "channel"],
      energy = x[, "energy_observed"],
      energy_observed = x[, "energy_observed"],
      energy_expected = x[, "energy_expected"]
    )
  }
)

#' @export
#' @method as.list PeakPosition
as.list.PeakPosition <- function(x, ...) {
  methods::as(x, "list")
}

#' @export
#' @rdname coerce
#' @aliases as.list,PeakPosition-method
setMethod("as.list", "PeakPosition", as.list.PeakPosition)

# To GammaSpectra ==============================================================
setAs(
  from = "GammaSpectrum",
  to = "GammaSpectra",
  def = function(from) .GammaSpectra(list(from))
)

setAs(
  from = "list",
  to = "GammaSpectra",
  def = function(from) {
    k <- lapply(X = from, FUN = get_names)
    names(from) <- unlist(k)
    .GammaSpectra(from)
  }
)

#' @export
#' @rdname coerce
#' @aliases as_spectra,GammaSpectrum-method
setMethod(
  f = "as_spectra",
  signature = c(x = "GammaSpectrum"),
  definition = function(x) {
    methods::as(x, "GammaSpectra")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_spectra,list-method
setMethod(
  f = "as_spectra",
  signature = c(x = "list"),
  definition = function(x) {
    methods::as(x, "GammaSpectra")
  }
)

# To PeakPosition ==============================================================
setAs(
  from = "list",
  to = "PeakPosition",
  def = function(from) {

    ## check list entry names; the must match our expectations
    from <- from[1:2]
    names(from) <-  tolower(names(from))
    if (!all(names(from) %in% c("channel", "energy")))
      stop("Coercion failed because of list-element name mismatch. Allowed are 'channel' and 'energy'!", call. = FALSE)

    ## create peak position object
    .PeakPosition(
      hash = "<man_coercion_list2PeakPosition>",
      channel = from$channel,
      energy_observed = from$energy,
      energy_expected = from$energy
    )
  }
)

#' @export
#' @rdname coerce
#' @aliases as_peaks,list-method
setMethod(
  f = "as_peaks",
  signature = c(x = "list"),
  definition = function(x) {
    methods::as(x, "PeakPosition")
  }
)
