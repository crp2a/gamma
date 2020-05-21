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
#' @section Access:
#' In the code snippets below, \code{x} is a \code{GammaSpectrum} object.
#' \describe{
#'  \item{\code{length(x)}}{Get number of chanel in \code{x}.}
#'  \item{\code{get_hash(x)}}{Get the MD5 hash of the raw data file.}
#'  \item{\code{get_names(x)}, \code{set_names(x) <- value}}{Retrieves or sets
#'   the name of \code{x} according to \code{value}.}
#'  \item{\code{get_chanels(x)}}{Get the number of chanels in \code{x}.}
#'  \item{\code{get_counts(x)}}{Get the counts of \code{x}.}
#'  \item{\code{get_energy(x)}}{Get the energy range of \code{x}.}
#'  \item{\code{get_rates(x)}}{Get the count rates of \code{x}.}
#' }
#' @section Coerce:
#' In the code snippets below, \code{x} is a \code{GammaSpectrum} object.
#' \describe{
#'  \item{\code{as.matrix)}}{Coerces \code{x} to a \code{\link{matrix}}.}
#'  \item{\code{as.data.frame)}}{Coerces \code{x} to a \code{\link{data.frame}}.}
#' }
#' @section Subset:
#' In the code snippets below, \code{x} is a \code{GammaSpectrum} object.
#' \describe{
#'  \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'  subscript \code{i}. \code{i} is a \code{character} vector
#'  of length one and will be matched to the name of the slots.}
#' }
#' @note This class retains copy construction.
#' @example inst/examples/ex-GammaSpectrum.R
#' @author N. Frerebeau
#' @family spectrum class
#' @docType class
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
    calibration = "LmOrNull"
  ),
  prototype = list(
    hash = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    name = character(0),
    date = Sys.time(),
    instrument = character(0),
    file_format = character(0),
    chanel = integer(0),
    energy = numeric(0),
    count = numeric(0),
    rate = numeric(0),
    live_time = numeric(0),
    real_time = numeric(0),
    calibration = NULL
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
#'  \item{\code{lengths(x)}}{Get the number of chanels in each element of
#'   \code{x}.}
#'  \item{\code{get_names(x)}, \code{set_names(x) <- value}}{Retrieves or sets
#'   the names of \code{x} according to \code{value}.}
#'  \item{\code{get_hash(x)}}{Get the MD5 hash of the raw data files.}
#'  \item{\code{get_chanels(x)}}{Get the number of chanels in each element of
#'  \code{x}.}
#'  \item{\code{get_counts(x)}}{Get the counts of each element of \code{x}.}
#'  \item{\code{get_energy(x)}}{Get the energy range of each element of
#'  \code{x}.}
#'  \item{\code{get_rates(x)}}{Get the count rates of each element of \code{x}.}
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
#' @example inst/examples/ex-GammaSpectra.R
#' @author N. Frerebeau
#' @family spectrum class
#' @docType class
#' @aliases GammaSpectra-class
.GammaSpectra <- setClass(
  Class = "GammaSpectra",
  contains = "list"
)

#' An S4 Class to Represent a Spectrum Baseline
#'
#' @note This class extends the \linkS4class{GammaSpectrum} class.
#' @example inst/examples/ex-baseline.R
#' @author N. Frerebeau
#' @family spectrum class
#' @docType class
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
#' @slot Ni A \linkS4class{DoseRateModel} object.
#' @slot NiEi A \linkS4class{DoseRateModel} object.
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
#' @family calibration class
#' @docType class
#' @name CalibrationCurve-class
#' @rdname CalibrationCurve-class
NULL

#' @aliases DoseRateModel-class
#' @rdname CalibrationCurve-class
.DoseRateModel <- setClass(
  Class = "DoseRateModel",
  slots = c(
    slope = "numeric",
    intercept = "numeric",
    residuals = "numeric",
    df = "numeric",
    MSWD = "numeric",
    p_value = "numeric",
    background = "numeric",
    range = "numeric"
  )
)

#' @aliases DoseRateModel-class
#' @rdname CalibrationCurve-class
.CalibrationCurve <- setClass(
  Class = "CalibrationCurve",
  slots = c(
    Ni = "DoseRateModel",
    NiEi = "DoseRateModel",
    data = "data.frame",
    details = "list"
  )
)

# ================================================================= PeakPosition
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
#'  \item{\code{matrix)}}{Coerces \code{x} to a \code{\link{matrix}}.}
#'  \item{\code{as.data.frame)}}{Coerces \code{x} to a \code{\link{data.frame}}.}
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
#' @family calibration class
#' @docType class
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
    noise_method = character(0),
    noise_threshold = numeric(0),
    window = integer(0),
    chanel = integer(0),
    energy = numeric(0)
  )
)
