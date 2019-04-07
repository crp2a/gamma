# CLASSES DEFINITION AND INITIALIZATION
#' @include gamma.R
NULL

#
setOldClass(Classes = "nls")

# DEFINITION ===================================================================
#' An S4 class to represent a gamma sectrum
#'
#' Represents a single spectrum of a gamma ray spectrometry measurement.
#' @slot reference A \code{\link{character}} string the measurement reference.
#' @slot date A \code{\link{character}} string giving the measurement date.
#' @slot instrument A \code{\link{character}} string the instrument name.
#' @slot file_format A \code{\link{character}} string.
#' @slot live_time A \code{\link{numeric}} value.
#' @slot real_time A \code{\link{numeric}} value.
#' @slot chanel A \code{\link{numeric}} vector.
#' @slot energy A \code{\link{numeric}} vector.
#' @slot counts A \code{\link{numeric}} vector.
#' @slot rate A \code{\link{numeric}} vector.
#' @param x An object of class \code{GammaSpectrum}.
#' @param i A length-one \code{\link{character}} vector specifying the element
#'  to extract or replace (see below). Character sring will be matched to the
#'  names of the slots.
#' @section Methods:
#' \describe{
#'  \item{estimateBaseline}{Estimate the baseline of a \code{GammaSpectrum}
#'  object. See \code{\link{estimateBaseline}} for details.}
#'  \item{estimateDoseRate}{Estimate the in-situ gamma dose rate of a
#'  \code{GammaSpectrum} object. See \code{\link{estimateDoseRate}} for details.}
#'  \item{findPeaks}{Look for local maxima to extract peaks out of a
#'  \code{GammaSpectrum} object. See \code{\link{findPeaks}} for details.}
#'  \item{removeBaseline}{estimate and remove the baseline of a
#'  \code{GammaSpectrum} object. See \code{\link{removeBaseline}} for details.}
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
setClass(
  Class = "GammaSpectrum",
  slots = c(
    reference = "character",
    date = "character",
    instrument = "character",
    file_format = "character",
    chanel = "numeric",
    energy = "numeric",
    counts = "numeric",
    rate = "numeric",
    live_time = "numeric",
    real_time = "numeric"
  )
)

#' An S4 class to represent a spectrum baseline
#'
#' @seealso \linkS4class{GammaSpectrum}.
#' @example inst/examples/ex-BaseLine.R
#' @author N. Frerebeau
#' @docType class
#' @rdname BaseLine
#' @aliases BaseLine-class
setClass(
  Class = "BaseLine",
  # slots = c(
  #   method = "character"
  # ),
  contains = "GammaSpectrum"
)

#' An S4 class to represent a collection of gamma sectra
#'
#' Represents a collection of spectra of gamma ray spectrometry measurements.
#' @param x An object of class \code{GammaSpectrum}.
#' @param i,j Indices specifying elements to extract or replace (see below).
#'  Indices are \code{\link{numeric}} or \code{\link{character}} vectors or
#'  empty (\code{\link{missing}}) or \code{\link{NULL}}.
#'  Numeric values are coerced to integer as by \code{\link{as.integer}}
#'  (and hence truncated towards zero). Character vectors will be matched to
#'  the names of the object.
#' @details
#'  This class extends the base \code{\link{list}} and can only contains
#'  \linkS4class{GammaSpectrum} objects.
#' @section Methods:
#' \describe{
#'  \item{estimateBaseline}{Estimate the baseline of each gamma spectrum in a
#'  \code{GammaSpectra} object. See \code{\link{estimateBaseline}} for details.}
#'  \item{estimateDoseRate}{Estimate the in-situ gamma dose rate of each gamma
#'  spectrum in a \code{GammaSpectra} object. See \code{\link{estimateDoseRate}}
#'  for details.}
#'  \item{findPeaks}{Look for local maxima to extract peaks out of each gamma
#'  spectrum in a \code{GammaSpectra} object. See \code{\link{findPeaks}} for
#'  details.}
#'  \item{removeBaseline}{Estimate and remove the baseline of each gamma
#'  spectrum in a \code{GammaSpectra} object. See \code{\link{removeBaseline}}
#'  for details.}
#' }
#' @section Access:
#' In the code snippets below, \code{x} is a \code{GammaSpectra} object.
#' \describe{
#'  \item{\code{length(x)}}{Get the number of elements in \code{x}.}
#'  \item{\code{names(x)}, \code{names(x) <- value}}{Get or set the names of the
#'   elements according to \code{value}.}
#' }
#' @section Coerce:
#' In the code snippets below, \code{x} is a \code{GammaSpectra} object.
#' \describe{
#'  \item{\code{as(x, "list")}}{Coerces \code{x} to a \code{\link{list}}.}
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
setClass(
  Class = "GammaSpectra",
  contains = "list"
)

#' An S4 class to represent a set peaks
#'
#' @author N. Frerebeau
#' @docType class
#' @rdname PeakPosition
#' @aliases PeakPosition-class
setClass(
  Class = "PeakPosition",
  slots = list(
    method = "character",
    noise = "numeric",
    window = "numeric",
    peaks = "data.frame",
    spectrum = "GammaSpectrum"
  )
)

#' An S4 class to represent a set peaks
#'
#' @author N. Frerebeau
#' @docType class
#' @rdname PeakModel
#' @aliases PeakModel-class
setClass(
  Class = "PeakModel",
  slots = c(
    model = "nls",
    peaks = "data.frame",
    spectrum = "GammaSpectrum"
  )
)

#' An S4 class to represent a calibration curve
#'
#' @param x An object of class \code{CalibrationCurve}.
#' @param i A length-one \code{\link{character}} vector specifying the element
#'  to extract or replace (see below). Character sring will be matched to the
#'  names of the slots.
#' @section Subset:
#' In the code snippets below, \code{x} is a \code{CalibrationCurve} object.
#' \describe{
#'  \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'  subscript \code{i}. \code{i} is a \code{character} vector
#'  of length one.}
#' }
#' @author N. Frerebeau
#' @docType class
#' @rdname CalibrationCurve
#' @aliases CalibrationCurve-class
setClass(
  Class = "CalibrationCurve",
  slots = c(
    model = "lm",
    data = "data.frame"
  )
)

#' An S4 class to represent a gamma dose rate
#'
#' @param x An object of class \code{DoseRate}.
#' @param i A length-one \code{\link{character}} vector specifying the element
#'  to extract or replace (see below). Character sring will be matched to the
#'  names of the slots.
#' @section Coerce:
#' In the code snippets below, \code{x} is a \code{DoseRate} object.
#' \describe{
#'  \item{\code{as(x, "matrix")}}{Coerces \code{x} to a \code{\link{matrix}}.}
#'  \item{\code{as(x, "data.frame")}}{Coerces \code{x} to a
#'  \code{\link[=data.frame]{data frame}}.}
#' }
#' @section Subset:
#' In the code snippets below, \code{x} is a \code{DoseRate} object.
#' \describe{
#'  \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'  subscript \code{i}. \code{i} is a \code{character} vector
#'  of length one.}
#' }
#' @author N. Frerebeau
#' @docType class
#' @rdname DoseRate
#' @aliases DoseRate-class
setClass(
  Class = "DoseRate",
  slots = c(
    reference = "character",
    dose_value = "numeric",
    dose_error = "numeric",
    signal_value = "numeric",
    signal_error = "numeric"
  )
)

# INITIALIZATION ===============================================================
## GammaSpectrum ---------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "GammaSpectrum",
  definition = function(.Object, reference, date, instrument, file_format,
                        chanel, energy, counts, live_time, real_time) {
    if (!missing(reference)) .Object@reference <- reference
    if (!missing(date)) .Object@date <- date
    if (!missing(instrument)) .Object@instrument <- instrument
    if (!missing(file_format)) .Object@file_format <- file_format
    if (!missing(chanel)) .Object@chanel <- chanel
    if (!missing(energy)) .Object@energy <- energy
    if (!missing(counts)) {
      .Object@counts <- counts
      .Object@rate <- counts / live_time
    }
    if (!missing(live_time)) .Object@live_time <- live_time
    if (!missing(real_time)) .Object@real_time <- real_time

    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(paste(class(.Object), "instance initialized.", sep = " "))
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
      message(paste(class(.Object), "instance initialized.", sep = " "))
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
    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(paste(class(.Object), "instance initialized.", sep = " "))
    }
    return(.Object)
  }
)
## CalibrationCurve ------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "CalibrationCurve",
  definition = function(.Object, model, data) {
    if (!missing(model)) .Object@model <- model
    if (!missing(data)) .Object@data <- data

    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(paste(class(.Object), "instance initialized.", sep = " "))
    }
    return(.Object)
  }
)
## DoseRate --------------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "DoseRate",
  definition = function(.Object, reference, dose_value, dose_error,
                        signal_value, signal_error) {
    if (!missing(reference)) .Object@reference <- reference
    if (!missing(dose_value)) .Object@dose_value <- dose_value
    if (!missing(dose_error)) .Object@dose_error <- dose_error
    if (!missing(signal_value)) .Object@signal_value <- signal_value
    if (!missing(signal_error)) .Object@signal_error <- signal_error

    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(paste(class(.Object), "instance initialized.", sep = " "))
    }
    return(.Object)
  }
)

# VALIDATION ===================================================================
## GammaSpectrum ---------------------------------------------------------------
setValidity(
  Class = "GammaSpectrum",
  method = function(object) {
    reference <- object@reference
    date <- object@date
    instrument <- object@instrument
    file_format <- object@file_format
    chanel <- object@chanel
    energy <- object@energy
    counts <- object@counts
    rate <- object@rate
    live_time <- object@live_time
    real_time <- object@real_time
    message <- c()

    if (length(reference) > 1)
      message <- c(message, "'reference' must be a single character string.")
    if (length(date) > 1)
      message <- c(message, "'date' must be a single character string.")
    if (length(instrument) > 1)
      message <- c(message, "'instrument' must be a single character string.")
    if (length(file_format) > 1)
      message <- c(message, "'file_format' must be a single character string.")
    if (length(live_time) > 1)
      message <- c(message, "'live_time' must be a single numeric value.")
    if (length(real_time) > 1)
      message <- c(message, "'real_time' must be a single numeric value.")

    x <- lengths(list(chanel, energy, counts))
    if (!isEqual(x))
      message <- c(message, "'chanel', 'energy' and 'counts' must have the same length.")

    if (length(message) != 0) {
      stop(paste(message, collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)
## GammaSpectra ----------------------------------------------------------------
setValidity(
  Class = "GammaSpectra",
  method = function(object) {
    data <- object@.Data
    message <- c()

    # slot: .Data
    if (length(data) != 0) {
      class <- unlist(lapply(X = data, FUN = is, class2 = "GammaSpectrum"))
      if (sum(!class) != 0) {
        message <- c(message, "All elements must be of class 'GammaSpectrum'.")
      }
    }

    if (length(message) != 0) {
      stop(paste(message, collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)
## DoseRate ---------------------------------------------------------------
setValidity(
  Class = "DoseRate",
  method = function(object) {
    reference <- object@reference
    dose_value <- object@dose_value
    dose_error <- object@dose_error
    signal_value <- object@signal_value
    signal_error <- object@signal_error
    message <- c()

    if (length(reference) != 0)
      if (anyNA(reference))
        message <- c(message, "'reference' missing values were detected.")

    if (length(dose_value) != 0)
      if (anyNA(dose_value) | any(is.infinite(dose_value)))
        message <- c(message, "'dose_value' infinite or missing values were detected.")

    if (length(dose_error) != 0)
      if (anyNA(dose_error) | any(is.infinite(dose_error)))
        message <- c(message, "'dose_error' infinite or missing values were detected.")

    if (length(signal_value) != 0)
      if (anyNA(signal_value) | any(is.infinite(signal_value)))
        message <- c(message, "'signal_value' infinite or missing values were detected.")

    if (length(signal_error) != 0)
      if (anyNA(signal_error) | any(is.infinite(signal_error)))
        message <- c(message, "'signal_error' infinite or missing values were detected.")

    x <- lengths(list(reference, dose_value, dose_error, signal_value, signal_error))
    if (!isEqual(x))
      message <- c(message, "All slots must have the same length.")

    if (length(message) != 0) {
      stop(paste(message, collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)
