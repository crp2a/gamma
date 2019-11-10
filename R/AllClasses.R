# CLASSES DEFINITION AND INITIALIZATION

# Class Unions =================================================================
setClassUnion("LmOrNull", c("lm", "NULL"))

# DEFINITION ===================================================================
#' An S4 Class to Represent a Gamma Sectrum
#'
#' Represents a single spectrum of a gamma ray spectrometry measurement.
#' @slot hash A \code{\link{character}} string giving the 32-byte MD5 hash of
#'  the imported file.
#' @slot name A \code{\link{character}} string the measurement reference.
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
#' @slot count A \code{\link{numeric}} vector giving the counts number for
#'  each channel. Numeric values are coerced to integer as by
#'  \code{\link{as.integer}} (and hence truncated towards zero).
#' @slot rate A \code{\link{numeric}} vector the count rate (in 1/s) for
#'  each channel.
#' @slot calibration A \code{\link[stats:lm]{linear model}} used for energy
#'  scale calibration (see \code{\link{calibrate_energy}}).
#' @slot dose_rate A length-two \code{\link{numeric}} vector giving the dose
#'  rate and corresponding error.
#' @section Access:
#' In the code snippets below, \code{x} is a \code{GammaSpectrum} object.
#' \describe{
#'  \item{\code{get_hash(x)}}{Get the MD5 hash of the raw data file.}
#'  \item{\code{get_chanels(x)}}{Get the number of chanels in \code{x}.}
#'  \item{\code{get_energy(x)}}{Get the energy range of \code{x}.}
#'  \item{\code{get_dose(x)}}{Get the dose rate of \code{x}.}
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
#'  of length one and will be matched to the name of the slots.}
#' }
#' @note This class retains copy construction.
#' @seealso \linkS4class{GammaSpectra}, \linkS4class{BaseLine}
#' @example inst/examples/ex-GammaSpectrum.R
#' @author N. Frerebeau
#' @docType class
#' @family class
#' @aliases GammaSpectrum-class
.GammaSpectrum <- setClass(
  Class = "GammaSpectrum",
  slots = c(
    hash = "character",
    name = "character",
    date = "POSIXct",
    instrument = "character",
    file_format = "character",
    chanel = "integer",
    energy = "numeric",
    count = "numeric",
    rate = "numeric",
    live_time = "numeric",
    real_time = "numeric",
    calibration = "lm",
    dose_rate = "numeric"
  ),
  prototype = list(
    hash = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    name = "unknown",
    date = Sys.time(),
    instrument = "unknown",
    file_format = "unknown",
    chanel = integer(0),
    energy = numeric(0),
    count = numeric(0),
    rate = numeric(0),
    live_time = numeric(0),
    real_time = numeric(0),
    calibration = stats::lm(0 ~ 0),
    dose_rate = numeric(2)
  )
)
setClassUnion("GammaSpectrumOrNull", c("GammaSpectrum", "NULL"))

#' An S4 Class to Represent a Collection of Gamma Sectra
#'
#' Represents a collection of spectra of gamma ray spectrometry measurements.
#' @param x An object of class \code{GammaSpectra}.
#' @details
#'  This class extends the base \code{\link{list}} and can only contains
#'  \linkS4class{GammaSpectrum} objects.
#' @section Access:
#' In the code snippets below, \code{x} is a \code{GammaSpectra} object.
#' \describe{
#'  \item{\code{length(x)}}{Get the number of elements in \code{x}.}
#'  \item{\code{names(x)}}{Get the names of the elements of \code{x}.}
#'  \item{\code{get_hash(x)}}{Get the MD5 hash of the raw data file.}
#'  \item{\code{get_chanels(x)}}{Get the number of chanels of \code{x}.}
#'  \item{\code{get_energy(x)}}{Get the energy range of \code{x}.}
#'  \item{\code{get_dose(x)}}{Get the dose rates of \code{x}.}
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
#'   of length one. Returns the corresponding \linkS4class{GammaSpectrum}
#'   object.}
#' }
#' @seealso \linkS4class{GammaSpectrum}.
#' @example inst/examples/ex-GammaSpectra.R
#' @author N. Frerebeau
#' @docType class
#' @family class
#' @aliases GammaSpectra-class
.GammaSpectra <- setClass(
  Class = "GammaSpectra",
  contains = "list"
)

#' An S4 Class to Represent a Spectrum Baseline
#'
#' @note This class extends the \linkS4class{GammaSpectrum} class.
#' @seealso \linkS4class{GammaSpectrum}.
#' @example inst/examples/ex-baseline.R
#' @author N. Frerebeau
#' @docType class
#' @family class
#' @aliases BaseLine-class
.BaseLine <- setClass(
  Class = "BaseLine",
  # slots = c(
  #   method = "character"
  # ),
  contains = "GammaSpectrum"
)

#' An S4 class to Represent a Dose Rate Calibration Curve
#'
#' @slot details A \code{\link{list}} of metadata.
#' @slot model A \code{\link[stats:lm]{linear model}} specifying the calibration
#'  curve.
#' @slot noise A length-two \code{\link{numeric}} vector giving the noise value
#'  and error (see \code{\link{integrate_signal}}).
#' @slot integration A length-two \code{\link{numeric}} vector giving the energy
#'  range to integrate within (see \code{\link{integrate_signal}}).
#' @slot data A \code{\link[=data.frame]{data frame}} giving the data used for
#'  linear model fitting.
#' @section Subset:
#' In the code snippets below, \code{x} is a \code{CalibrationCurve} object.
#' \describe{
#'  \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'  subscript \code{i}. \code{i} is a \code{character} vector of length one.}
#' }
#' @author N. Frerebeau
#' @docType class
#' @family class
#' @aliases CalibrationCurve-class
.CalibrationCurve <- setClass(
  Class = "CalibrationCurve",
  slots = c(
    details = "list",
    model = "lm",
    noise = "numeric",
    integration = "numeric",
    data = "data.frame"
  ),
  prototype = list(
    details = list(
      laboratory = "unknown",
      instrument = "unknown",
      detector = "unknown",
      authors = "unknown",
      date = Sys.time()
    ),
    model = stats::lm(0 ~ 0),
    noise = numeric(0),
    integration = numeric(0),
    data = data.frame()
  )
)

#' An S4 Class to Represent a Set of Peaks
#'
#' @slot hash TODO.
#' @slot noise_method A \code{\link{character}} string specifying the method
#'  used for peak detection.
#' @slot noise_threshold A length one \code{\link{numeric}} vector giving the
#'  noise threshold.
#' @slot window A length one \code{\link{numeric}} vector giving the half-window
#'  size.
#' @slot chanel TODO.
#' @slot energy TODO.
#' @section Subset:
#' In the code snippets below, \code{x} is a \code{PeakPosition} object.
#' \describe{
#'  \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'  subscript \code{i}. \code{i} is a \code{character} vector
#'  of length one.}
#' }
#' @note This class retains copy construction.
#' @author N. Frerebeau
#' @docType class
#' @family class
#' @aliases PeakPosition-class
.PeakPosition <- setClass(
  Class = "PeakPosition",
  slots = list(
    hash = "character",
    noise_method = "character",
    noise_threshold = "numeric",
    window = "integer",
    chanel = "integer",
    energy = "numeric"
  ),
  prototype = list(
    hash = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    noise_method = "unknown",
    noise_threshold = numeric(0),
    window = integer(0),
    chanel = integer(0),
    energy = numeric(0)
  )
)

# INITIALIZATION ===============================================================
## GammaSpectrum ---------------------------------------------------------------
# /!\ initialize() GammaSpectrum retains copy construction
setMethod(
  f = "initialize",
  signature = "GammaSpectrum",
  definition = function(
    .Object, ..., hash = .Object@hash, name = .Object@name,
    date = .Object@date, instrument = .Object@instrument,
    file_format = .Object@file_format, chanel = .Object@chanel,
    energy = .Object@energy, count = .Object@count, rate = .Object@rate,
    live_time = .Object@live_time, real_time = .Object@real_time,
    calibration = .Object@calibration
  ) {

    if (length(date) == 0)
      date <- Sys.time()
    if (length(rate) == 0 && length(count) > 0 && length(live_time) == 1)
      rate <- count / live_time

    methods::callNextMethod(
      .Object, ..., hash = hash, name = name, date = date,
      instrument = instrument, file_format = file_format, chanel = chanel,
      energy = energy, count = count, rate = rate, live_time = live_time,
      real_time = real_time, calibration = calibration
    )
  }
)
## GammaSpectra ----------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "GammaSpectra",
  definition = function(.Object, ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    # Get spectrum names
    spc_list <- .Object@.Data
    spc_ref <- make.unique(vapply(
      X = spc_list,
      FUN = "[[",
      FUN.VALUE = character(1),
      i = "name"
    ), sep = "_")
    names(spc_list) <- spc_ref
    .Object@.Data <- spc_list

    methods::validObject(.Object)
    return(.Object)
  }
)
## PeakPosition ----------------------------------------------------------------
# /!\ initialize() PeakPosition retains copy construction
setMethod(
  f = "initialize",
  signature = "PeakPosition",
  definition = function(
    .Object, ..., hash = .Object@hash, noise_method = .Object@noise_method,
    noise_threshold = .Object@noise_threshold, window = .Object@window,
    chanel = .Object@chanel, energy = .Object@energy
  ) {
    methods::callNextMethod(
      .Object, ..., hash = hash, noise_method = noise_method,
      noise_threshold = noise_threshold, window = window, chanel = chanel,
      energy = energy
    )
  }
)
## CalibrationCurve ------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "CalibrationCurve",
  definition = function(.Object, details, model, noise, integration, data) {

    if (!missing(details)) info <- details else info <- list()
    info$date <- Sys.time()
    .Object@details <- info
    if (!missing(model)) .Object@model <- model
    if (!missing(noise)) .Object@noise <- noise
    if (!missing(integration)) .Object@integration <- integration
    if (!missing(data)) .Object@data <- data

    methods::validObject(.Object)
    return(.Object)
  }
)
