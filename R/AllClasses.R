# CLASSES DEFINITION AND INITIALIZATION
#' @include gamma.R
NULL

# Class Unions =================================================================
setClassUnion("LmOrNull", c("lm", "NULL"))

# DEFINITION ===================================================================
#' An S4 class to represent a gamma sectrum
#'
#' Represents a single spectrum of a gamma ray spectrometry measurement.
#' @slot hash A \code{\link{character}} string giving the 32-byte MD5 hash of
#'  the imported file.
#' @slot reference A \code{\link{character}} string the measurement reference.
#' @slot date A \code{\link{POSIXct}} element giving the measurement date and
#'  time.
#' @slot instrument A \code{\link{character}} string giving the instrument name.
#' @slot file_format A \code{\link{character}} string giving the format of the
#'  imported file.
#' @slot live_time A \code{\link{numeric}} value.
#' @slot real_time A \code{\link{numeric}} value.
#' @slot chanel A \code{\link{integer}} vector giving the channel number.
#'  Numeric values are coerced to integer as by \code{\link{as.integer}}
#'  (and hence truncated towards zero).
#' @slot energy A \code{\link{numeric}} vector giving the gamma ray's energy
#'  (in keV).
#' @slot counts A \code{\link{numeric}} vector giving the counts number for
#'  each channel. Numeric values are coerced to integer as by
#'  \code{\link{as.integer}} (and hence truncated towards zero).
#' @slot rate A \code{\link{numeric}} vector the count rate (in 1/s) for
#'  each channel.
#' @slot calibration A \code{\link[stats:lm]{linear model}} used for energy
#'  scale calibration (see \code{\link{calibrate}}).
#' @slot dose_rate A length-two \code{\link{numeric}} vector giving the dose
#'  rate and corresponding error.
#' @param x An object of class \code{GammaSpectrum}.
#' @param i A length-one \code{\link{character}} vector specifying the element
#'  to extract or replace (see below). Character sring will be matched to the
#'  names of the slots.
#' @section Access:
#' In the code snippets below, \code{x} is a \code{GammaSpectrum} object.
#' \describe{
#'  \item{\code{length(x)}}{Get the number of chanels in \code{x}.}
#' }
#' @section Coerce:
#' In the code snippets below, \code{x} is a \code{GammaSpectrum} object.
#' \describe{
#'  \item{\code{as(x, "matrix")}}{Coerces \code{x} to a \code{\link{matrix}}.}
#'  \item{\code{as(x, "data.frame")}}{Coerces \code{x} to a
#'  \code{\link[=data.frame]{data frame}}.}
#' }
#' @section Subset:
#' In the code snippets below, \code{x} is a \code{GammaSpectrum} object.
#' \describe{
#'  \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'  subscript \code{i}. \code{i} is a \code{character} vector
#'  of length one.}
#' }
#' @seealso \linkS4class{GammaSpectra}, \linkS4class{BaseLine}
#' @example inst/examples/ex-GammaSpectrum.R
#' @author N. Frerebeau
#' @docType class
#' @rdname GammaSpectrum
#' @aliases GammaSpectrum-class
.GammaSpectrum <- setClass(
  Class = "GammaSpectrum",
  slots = c(
    hash = "character",
    reference = "character",
    date = "POSIXct",
    instrument = "character",
    file_format = "character",
    chanel = "integer",
    energy = "numeric",
    counts = "numeric",
    rate = "numeric",
    live_time = "numeric",
    real_time = "numeric",
    calibration = "LmOrNull",
    dose_rate = "numeric"
  )
)
setClassUnion("GammaSpectrumOrNull", c("GammaSpectrum", "NULL"))

#' An S4 class to represent a collection of gamma sectra
#'
#' Represents a collection of spectra of gamma ray spectrometry measurements.
#' @param x An object of class \code{GammaSpectra}.
#' @param i,j Indices specifying elements to extract or replace (see below).
#'  Indices are \code{\link{numeric}} or \code{\link{character}} vectors or
#'  empty (\code{\link{missing}}) or \code{\link{NULL}}.
#'  Numeric values are coerced to integer as by \code{\link{as.integer}}
#'  (and hence truncated towards zero). Character vectors will be matched to
#'  the names of the object.
#' @details
#'  This class extends the base \code{\link{list}} and can only contains
#'  \linkS4class{GammaSpectrum} objects.
#' @section Access:
#' In the code snippets below, \code{x} is a \code{GammaSpectra} object.
#' \describe{
#'  \item{\code{length(x)}}{Get the number of elements in \code{x}.}
#'  \item{\code{names(x)}}{Get the names of the elements.}
#' }
#' @section Subset:
#' In the code snippets below, \code{x} is a \code{GammaSpectra} object.
#' \describe{
#'  \item{\code{x[i]}}{Extracts the elements selected by subscript \code{i}.
#'   \code{i} can be \code{missing} or \code{NULL}, \code{numeric} or
#'   \code{character} vector or a \code{factor}.
#'   Returns a new \code{GammaSpectra} object.}
#'  \item{\code{x[i, j]}}{Like the above but allows to select a slot thru
#'   \code{j} (see examples). \code{j} is a \code{character} vector of
#'   length one. Returns a \code{list}.}
#'  \item{\code{x[[i]]}}{Extracts the elements selected by subscript \code{i}.
#'   \code{i} can be a \code{numeric} or \code{character} vector
#'   of length one. Returns the corresponding \linkS4class{GammaSpectrum} object.}
#' }
#' @seealso \linkS4class{GammaSpectrum}.
#' @example inst/examples/ex-GammaSpectra.R
#' @author N. Frerebeau
#' @docType class
#' @rdname GammaSpectra
#' @aliases GammaSpectra-class
.GammaSpectra <- setClass(
  Class = "GammaSpectra",
  contains = "list"
)

#' An S4 class to represent a spectrum baseline
#'
#' @note This class extends the \linkS4class{GammaSpectrum} class.
#' @seealso \linkS4class{GammaSpectrum}.
#' @example inst/examples/ex-baseline.R
#' @author N. Frerebeau
#' @docType class
#' @rdname BaseLine
#' @aliases BaseLine-class
.BaseLine <- setClass(
  Class = "BaseLine",
  # TODO
  # slots = c(
  #   method = "character"
  # ),
  contains = "GammaSpectrum"
)

#' An S4 class to represent a calibration curve
#'
#' @slot details A \code{\link{list}} of metadata.
#' @slot model A \code{\link[stats:lm]{linear model}} specifying the calibration
#'  curve.
#' @slot noise A length-two \code{\link{numeric}} vector giving the noise value
#'  and error (see \code{\link{integrateSignal}}).
#' @slot integration A length-two \code{\link{numeric}} vector giving the energy
#'  range to integrate within (see \code{\link{integrateSignal}}).
#' @slot data A \code{\link[=data.frame]{data frame}} giving the data used for
#'  linear model fitting.
#' @param x An object of class \code{CalibrationCurve}.
#' @param i A length-one \code{\link{character}} vector specifying the element
#'  to extract or replace (see below). Character sring will be matched to the
#'  names of the slots.
#' @section Subset:
#' In the code snippets below, \code{x} is a \code{CalibrationCurve} object.
#' \describe{
#'  \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'  subscript \code{i}. \code{i} is a \code{character} vector of length one.}
#' }
#' @author N. Frerebeau
#' @docType class
#' @rdname CalibrationCurve
#' @aliases CalibrationCurve-class
.CalibrationCurve <- setClass(
  Class = "CalibrationCurve",
  slots = c(
    details = "list",
    model = "LmOrNull",
    noise = "numeric",
    integration = "numeric",
    data = "data.frame"
  )
)

#' An S4 class to represent a set peaks
#'
#' @slot model A list of \code{\link[stats:nls]{nonlinear models}} giving the
#'  fitted model for each peak.
#' @slot coefficients A \code{\link{numeric}} matrix giving the peak parameters.
#' @slot spectrum A \linkS4class{GammaSpectrum} object.
#' @slot baseline A \linkS4class{BaseLine} object.
#' @param x An object of class \code{PeakModel}.
#' @param i A length-one \code{\link{character}} vector specifying the element
#'  to extract or replace (see below). Character sring will be matched to the
#'  names of the slots.
#' @section Subset:
#' In the code snippets below, \code{x} is a \code{PeakModel} object.
#' \describe{
#'  \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'  subscript \code{i}. \code{i} is a \code{character} vector
#'  of length one.}
#' }
#' @author N. Frerebeau
#' @docType class
#' @rdname PeakModel
#' @aliases PeakModel-class
.PeakModel <- setClass(
  Class = "PeakModel",
  slots = c(
    model = "list",
    coefficients = "matrix",
    spectrum = "GammaSpectrum",
    baseline = "BaseLine"
  )
)

#' An S4 class to represent a set of peaks
#'
#' @slot method A \code{\link{character}} string specifying the method used for
#'  peak detection.
#' @slot noise A length one \code{\link{numeric}} vector giving the noise
#'  threshold.
#' @slot window A length one \code{\link{numeric}} vector giving the half-window
#'  size.
#' @slot peaks A four columns \code{\link{numeric}} matrix giving the
#'  peak positions.
#' @slot spectrum A \linkS4class{GammaSpectrum} object.
#' @slot baseline A \linkS4class{BaseLine} object.
#' @param x An object of class \code{PeakPosition}.
#' @param i A length-one \code{\link{character}} vector specifying the element
#'  to extract or replace (see below). Character sring will be matched to the
#'  names of the slots.
#' @section Subset:
#' In the code snippets below, \code{x} is a \code{PeakPosition} object.
#' \describe{
#'  \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'  subscript \code{i}. \code{i} is a \code{character} vector
#'  of length one.}
#' }
#' @author N. Frerebeau
#' @docType class
#' @rdname PeakPosition
#' @aliases PeakPosition-class
.PeakPosition <- setClass(
  Class = "PeakPosition",
  slots = list(
    method = "character",
    noise = "numeric",
    window = "numeric",
    peaks = "matrix",
    spectrum = "GammaSpectrum",
    baseline = "BaseLine"
  )
)

# INITIALIZATION ===============================================================
## GammaSpectrum ---------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "GammaSpectrum",
  definition = function(.Object, hash, reference, date, instrument, file_format,
                        chanel, energy, counts, live_time, real_time,
                        calibration) {
    if (!missing(hash)) .Object@hash <- hash
    if (!missing(reference)) .Object@reference <- reference
    if (!missing(date)) .Object@date <- date else .Object@date <- Sys.time()
    if (!missing(instrument)) .Object@instrument <- instrument
    if (!missing(file_format)) .Object@file_format <- file_format
    if (!missing(chanel)) .Object@chanel <- as.integer(chanel)
    if (!missing(energy)) .Object@energy <- energy
    if (!missing(counts)) .Object@counts <- as.integer(counts)
    if (!missing(counts) & !missing(live_time))
      .Object@rate <- counts / live_time
    if (!missing(live_time)) .Object@live_time <- live_time
    if (!missing(real_time)) .Object@real_time <- real_time
    if (!missing(calibration)) .Object@calibration <- calibration

    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(class(.Object), " instance initialized.")
    }
    return(.Object)
  }
)
## BaseLine --------------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "BaseLine",
  definition = function(.Object, ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(class(.Object), " instance initialized.")
    }
    return(.Object)
  }
)
## GammaSpectra ----------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "GammaSpectra",
  definition = function(.Object, ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    # Get spectrum references
    spc_list <- .Object@.Data
    spc_ref <- sapply(X = spc_list, FUN = "[[", i = "reference") %>%
      as.character() %>%
      make.unique()
    names(spc_list) <- spc_ref
    .Object@.Data <- spc_list
    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(class(.Object), " instance initialized.")
    }
    return(.Object)
  }
)
## CalibrationCurve ------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "CalibrationCurve",
  definition = function(.Object, details, model, noise, integration, data) {

    info <- list(
      laboratory = character(0), instrument = character(0),
      detector = character(0), authors = character(0)
    )
    if (!missing(details)) {
      if (is.list(details) & length(details) != 0) {
        info_fields <- c("laboratory", "instrument", "detector", "authors")
        k <- which(names(details) %in% info_fields)
        info <- details[k]
      }
    }
    info$date <- Sys.time()
    .Object@details <- info
    if (!missing(model)) .Object@model <- model
    if (!missing(noise)) .Object@noise <- noise
    if (!missing(integration)) .Object@integration <- integration
    if (!missing(data)) .Object@data <- data

    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(class(.Object), " instance initialized.")
    }
    return(.Object)
  }
)
## PeakModel -------------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "PeakModel",
  definition = function(.Object, model, coefficients, spectrum, baseline) {
    if (!missing(model)) .Object@model <- model
    if (!missing(coefficients)) .Object@coefficients <- coefficients
    if (!missing(spectrum)) .Object@spectrum <- spectrum
    if (!missing(baseline)) .Object@baseline <- baseline

    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(class(.Object), " instance initialized.")
    }
    return(.Object)
  }
)
## PeakPosition ----------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "PeakPosition",
  definition = function(.Object, method, noise, window, peaks, spectrum,
                        baseline) {
    if (!missing(method)) .Object@method <- method
    if (!missing(noise)) .Object@noise <- noise
    if (!missing(window)) .Object@window <- window
    if (!missing(peaks)) .Object@peaks <- peaks
    if (!missing(spectrum)) .Object@spectrum <- spectrum
    if (!missing(baseline)) .Object@baseline <- baseline

    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(class(.Object), " instance initialized.")
    }
    return(.Object)
  }
)
