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
#'  \item{\code{get_names(x)}, \code{set_names(x) <- value}}{Retrieves or sets
#'   the name of \code{x} according to \code{value}.}
#'  \item{\code{get_chanels(x)}}{Get the number of chanels in \code{x}.}
#'  \item{\code{get_energy(x)}}{Get the energy range of \code{x}.}
#'  \item{\code{get_dose(x)}}{Get the dose rate of \code{x}.}
#' }
#' @section Coerce:
#' In the code snippets below, \code{x} is a \code{GammaSpectrum} object.
#' \describe{
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

#' An S4 Class to Represent a Collection of Gamma Sectra
#'
#' Represents a collection of spectra of gamma ray spectrometry measurements.
#' @details
#'  This class extends the base \code{\link{list}} and can only contains
#'  \linkS4class{GammaSpectrum} objects.
#' @section Access:
#' In the code snippets below, \code{x} is a \code{GammaSpectra} object.
#' \describe{
#'  \item{\code{length(x)}}{Get the number of elements in \code{x}.}
#'  \item{\code{get_names(x)}, \code{set_names(x) <- value}}{Retrieves or sets
#'   the names of \code{x} according to \code{value}.}
#'  \item{\code{get_hash(x)}}{Get the MD5 hash of the raw data files.}
#'  \item{\code{get_chanels(x)}}{Get the number of chanels.}
#'  \item{\code{get_energy(x)}}{Get the energy ranges.}
#'  \item{\code{get_dose(x)}}{Get the dose rates.}
#' }
#' @section Coerce:
#' In the code snippets below, \code{x} is a \code{GammaSpectra} object.
#' \describe{
#'  \item{\code{as(x, "data.frame")}}{Coerces \code{x} to a long
#'  \code{\link[=data.frame]{data frame}}.}
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
#' @slot model A \code{\link[stats:lm]{linear model}} specifying the calibration
#'  curve.
#' @slot background A length-two \code{\link{numeric}} vector giving the
#'  background noise value and error (see \code{\link{integrate_signal}}).
#' @slot range A length-two \code{\link{numeric}} vector giving the energy
#'  range to integrate within (see \code{\link{integrate_signal}}).
#' @slot Ni A \code{DoseRateModelNi} object.
#' @slot NiEi A \code{DoseRateModelNi} object.
#' @slot data A \code{\link{data.frame}} giving the data used for
#'  linear model fitting.
#' @slot details A \code{\link{list}} of length-one vector giving the curve
#'  metadata.
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

#' @rdname CalibrationCurve-class
.DoseRateModel <- setClass(
  Class = "DoseRateModel",
  slots = c(
    model = "lm",
    background = "numeric",
    range = "numeric"
  ),
  contains = "VIRTUAL"
)
#' @rdname CalibrationCurve-class
.DoseRateModelNi <- setClass(
  Class = "DoseRateModelNi",
  contains = "DoseRateModel"
)
#' @rdname CalibrationCurve-class
.DoseRateModelNiEi <- setClass(
  Class = "DoseRateModelNiEi",
  contains = "DoseRateModel"
)
#' @rdname CalibrationCurve-class
.CalibrationCurve <- setClass(
  Class = "CalibrationCurve",
  slots = c(
    Ni = "DoseRateModelNi",
    NiEi = "DoseRateModelNiEi",
    data = "data.frame",
    details = "list"
  )
)

#' An S4 Class to Represent a Set of Peaks
#'
#' @slot hash A \code{\link{character}} string giving the 32-byte MD5 hash of
#'  the imported spectrum file.
#' @slot noise_method A \code{\link{character}} string specifying the method
#'  used for peak detection.
#' @slot noise_threshold A length one \code{\link{numeric}} vector giving the
#'  noise threshold.
#' @slot window A length one \code{\link{numeric}} vector giving the half-window
#'  size.
#' @slot chanel A \code{\link{integer}} vector giving the channel number.
#'  Numeric values are coerced to integer as by \code{\link{as.integer}}
#'  (and hence truncated towards zero).
#' @slot energy A \code{\link{numeric}} vector giving the gamma ray's energy
#'  (in keV).
#' @section Access:
#' In the code snippets below, \code{x} is a \code{PeakPosition} object.
#' \describe{
#'  \item{\code{get_hash(x)}}{Get the MD5 hash of the raw data file.}
#'  \item{\code{get_chanels(x)}}{Get the chanels of \code{x}.}
#'  \item{\code{get_energy(x)}, \code{set_energy(x) <- value}}{Retrieves or sets
#'   the energy scale of \code{x} according to \code{value}.}
#' }
#' @section Coerce:
#' In the code snippets below, \code{x} is a \code{PeakPosition} object.
#' \describe{
#'  \item{\code{as(x, "data.frame")}}{Coerces \code{x} to a
#'  \code{\link[=data.frame]{data frame}}.}
#' }
#' @section Subset:
#' In the code snippets below, \code{x} is a \code{PeakPosition} object.
#' \describe{
#'  \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'  subscript \code{i}. \code{i} is a \code{character} vector
#'  of length one and will be matched to the name of the slots.}
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
    calibration = .Object@calibration, dose_rate = .Object@dose_rate
  ) {

    if (length(date) == 0)
      date <- Sys.time()
    if (length(chanel) != 0)
      chanel <- as.integer(chanel)
    if (length(rate) == 0 && length(count) > 0 && length(live_time) == 1)
      rate <- count / live_time

    methods::callNextMethod(
      .Object, ..., hash = hash, name = name, date = date,
      instrument = instrument, file_format = file_format, chanel = chanel,
      energy = energy, count = count, rate = rate, live_time = live_time,
      real_time = real_time, calibration = calibration, dose_rate = dose_rate
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
    if (length(chanel) != 0)
      chanel <- as.integer(chanel)

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
  definition = function(.Object, Ni, NiEi, data, details) {

    info <- if (!missing(details)) details else list()
    info$date <- Sys.time()
    .Object@details <- info
    if (!missing(Ni)) .Object@Ni <- Ni
    if (!missing(NiEi)) .Object@NiEi <- NiEi
    if (!missing(data)) .Object@data <- data

    methods::validObject(.Object)
    return(.Object)
  }
)
