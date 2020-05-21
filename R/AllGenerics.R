# GENERIC METHODS
#' @include AllClasses.R
NULL

# Add S4 dispatch to base S3 generic
setGeneric("length")

# ======================================================================= Coerce
#' Coerce
#'
#' @param x An object to be coerced.
#' @param row.names \code{NULL} or a \code{\link{character}} vector giving the
#'  row names for the data frame. Missing values are not allowed.
#' @param optional A \code{\link{logical}} scalar (see
#'  \code{\link[base]{as.data.frame}}).
#' @param make.names A \code{\link{logical}} scalar (see
#'  \code{\link[base]{as.data.frame}}).
#' @param stringsAsFactors A \code{\link{logical}} scalar: should the character
#'  vector be converted to a \code{\link{factor}}?
#' @param ... Currently not used.
#' @return A coerced object.
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family class
#' @name coerce
#' @rdname coerce
NULL

# ====================================================================== Extract
#' Get or Set Parts of an Object
#'
#' Getters and setters to extract or replace parts of an object.
#' @param x An object from which to get or set element(s).
#' @param value A possible value for the element(s) of \code{x}.
#' @param na.rm A \code{\link{logical}} scalar: should \code{\link{NA}} be
#'  omitted?
#' @param ... Currently not used.
#' @return
#'  An object of the same sort as \code{x} with the new values assigned.
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name mutator
#' @rdname mutator
#' @aliases get set
NULL

#' @rdname mutator
#' @aliases get_hash-method
setGeneric(
  name = "get_hash",
  def = function(x) standardGeneric("get_hash")
)

#' @rdname mutator
#' @aliases get_names-method
setGeneric(
  name = "get_names",
  def = function(x) standardGeneric("get_names")
)

#' @rdname mutator
#' @aliases set_names-method
setGeneric(
  name = "set_names<-",
  def = function(x, value) standardGeneric("set_names<-")
)

#' @rdname mutator
#' @aliases get_livetime-method
setGeneric(
  name = "get_livetime",
  def = function(x) standardGeneric("get_livetime")
)

#' @rdname mutator
#' @aliases get_realtime-method
setGeneric(
  name = "get_realtime",
  def = function(x) standardGeneric("get_realtime")
)

#' @rdname mutator
#' @aliases get_chanels-method
setGeneric(
  name = "get_chanels",
  def = function(x) standardGeneric("get_chanels")
)

#' @rdname mutator
#' @aliases get_counts-method
setGeneric(
  name = "get_counts",
  def = function(x) standardGeneric("get_counts")
)

#' @rdname mutator
#' @aliases get_rates-method
setGeneric(
  name = "get_rates",
  def = function(x) standardGeneric("get_rates")
)

#' @rdname mutator
#' @aliases get_energy-method
setGeneric(
  name = "get_energy",
  def = function(x) standardGeneric("get_energy")
)

#' @rdname mutator
#' @aliases set_energy-method
setGeneric(
  name = "set_energy<-",
  def = function(x, value) standardGeneric("set_energy<-")
)

#' @rdname mutator
#' @aliases range_chanels-method
setGeneric(
  name = "range_chanels",
  def = function(x, ...) standardGeneric("range_chanels")
)

#' @rdname mutator
#' @aliases range_energy-method
setGeneric(
  name = "range_energy",
  def = function(x, ...) standardGeneric("range_energy")
)

#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to
#'  replace element(s).
#' @param i,j Indices specifying elements to extract or replace. Indices are
#'  \code{\link{numeric}}, \code{\link{integer}} or \code{\link{character}}
#'  vectors or empty (missing) or \code{NULL}. Numeric values are coerced to
#'  \code{\link{integer}} as by \code{\link{as.integer}} (and hence truncated
#'  towards zero). Character vectors will be matched to the name of the
#'  elements. An empty index (a comma separated blank) indicates that all
#'  entries in that dimension are selected.
# @param drop A \code{\link{logical}} scalar: should the result be coerced to
#  the lowest possible dimension? This only works for extracting elements,
#  not for the replacement.
#' @return
#'  A subsetted object.
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name subset
#' @rdname subset
NULL

# ==================================================================== Operators
#' Common Operations on Matrix Objects
#'
#' Performs common operations on \code{GammaSpectrum} objects.
#' @param x,e1,e2 An object (typically a \linkS4class{GammaSpectrum} object).
#' @param digits A length-one \code{\link{numeric}} vector giving the
#'  number of digits to be used in \code{\link{round}} or \code{\link{signif}}.
#' @param na.rm A \code{\link{logical}} scalar: should missing values
#'  (including \code{NaN}) be omitted from the calculations?
#' @param ... Further arguments passed to or from methods.
#' @section Group Generics:
#'  \linkS4class{GammaSpectrum} objects have support for S4 group generic
#'  functionality to operate within elements across objects:
#'  \describe{
#'   \item{\code{Arith}}{"\code{+}", "\code{-}", "\code{*}", "\code{^}",
#'   "\code{\%\%}", "\code{\%/\%}", "\code{/}"}
#'   \item{\code{Compare}}{"\code{==}", "\code{>}", "\code{<}", "\code{!=}",
#'   "\code{<=}", "\code{>=}"}
#'   \item{\code{Logic}}{"\code{&}", "\code{|}"}
#'   \item{\code{Math}}{"\code{abs}", "\code{sign}", "\code{sqrt}",
#'   "\code{ceiling}", "\code{floor}", "\code{trunc}", "\code{cummax}",
#'   "\code{cummin}", "\code{cumprod}", "\code{cumsum}", "\code{log}",
#'   "\code{log10}", "\code{log2}", "\code{log1p}", "\code{acos}",
#'   "\code{acosh}", "\code{asin}", "\code{asinh}", "\code{atan}",
#'   "\code{atanh}", "\code{exp}", "\code{expm1}", "\code{cos}",
#'   "\code{cosh}", "\code{cospi}", "\code{sin}", "\code{sinh}",
#'   "\code{sinpi}", "\code{tan}", "\code{tanh}", "\code{tanpi}",
#'   "\code{gamma}", "\code{lgamma}", "\code{digamma}", "\code{trigamma}"}
#'   \item{\code{Math2}}{"\code{round}", "\code{signif}"}
#'   \item{\code{Ops}}{"\code{Arith}", "\code{Compare}", "\code{Logic}"}
#'   \item{\code{Summary}}{"\code{min}", "\code{max}", "\code{range}",
#'   "\code{prod}", "\code{sum}", "\code{any}", "\code{all}"}
#'  }
#' @example inst/examples/ex-operators.R
#' @author N. Frerebeau
#' @docType methods
#' @family operator
#' @name operator
#' @rdname operator
NULL

# ================================================================= Energy scale
#' Energy Scale Calibration
#'
#' Calibrates the energy scale of a gamma spectrum.
#' @param object A \linkS4class{GammaSpectrum} or
#'  \linkS4class{GammaSpectra} object.
#' @param lines A \linkS4class{PeakPosition} object or a \code{\link{list}} of
#'  length two. If a \code{list} is provided, each element must be a named
#'  numeric vector giving the observed peak position ("\code{chanel}") and the
#'  corresponding expected "\code{energy}" value (in keV).
#' @param ... Currently not used.
#' @return
#'  \code{calibrate_energy} returns a \linkS4class{GammaSpectrum} object.
#'
#'  \code{is_calibrated} returns a \code{\link{logical}} vector.
#' @example inst/examples/ex-energy.R
#' @author N. Frerebeau
#' @docType methods
#' @family energy
#' @name energy
#' @rdname energy
NULL

#' @rdname energy
#' @aliases calibrate_energy-method
setGeneric(
  name = "calibrate_energy",
  def = function(object, lines, ...) standardGeneric("calibrate_energy")
)

#' @rdname energy
setGeneric(
  name = "is_calibrated",
  def = function(object) standardGeneric("is_calibrated")
)

# ===================================================================== Baseline
#' Baseline Estimation and Removal
#'
#' @param object A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra}
#'  object.
#' @param method A \code{\link{character}} string specifying the method to be
#'  used for baseline estimation (see details).
#' @param LLS A \code{\link{logical}} scalar: should the LLS operator be applied
#'  on \code{x} before employing SNIP algorithm? Only used if
#'  \code{method} is "\code{SNIP}".
#' @param decreasing A \code{\link{logical}} scalar: should a decreasing
#'  clipping window be used? Only used if \code{method} is "\code{SNIP}".
#' @param k An \code{\link{integer}} value giving the numerber of iterations.
#'  Only used if \code{method} is "\code{SNIP}".
#' @param ... Extra parameters passed to \code{estimate_baseline}.
#' @details
#'  The following methods are availble for baseline estimation:
#'  \describe{
#'   \item{SNIP}{Sensitive Nonlinear Iterative Peak clipping algorithm.}
#'  }
#' @return
#'  \code{estimate_baseline} returns a \linkS4class{BaseLine} object.
#'
#'  \code{remove_baseline} returns a \linkS4class{GammaSpectrum} or
#'  \linkS4class{GammaSpectra} object (same as \code{object}).
#' @references
#'  Morháč, M., Kliman, J., Matoušek, V., Veselský, M. & Turzo, I. (1997).
#'  Background elimination methods for multidimensional gamma-ray spectra.
#'  \emph{Nuclear Instruments and Methods in Physics Research Section A:
#'  Accelerators, Spectrometers, Detectors and Associated Equipment}, 401(1),
#'  p. 113-132.
#'  DOI: \href{https://doi.org/10.1016/S0168-9002(97)01023-1}{10.1016/S0168-9002(97)01023-1}
#'
#'  Morháč, M. & Matoušek, V. (2008). Peak Clipping Algorithms for Background
#'  Estimation in Spectroscopic Data. \emph{Applied Spectroscopy}, 62(1), p. 91-106.
#'  DOI: \href{https://doi.org/10.1366/000370208783412762}{10.1366/000370208783412762}
#'
#'  Ryan, C. G., Clayton, E., Griffin, W. L., Sie, S. H. & Cousens, D. R.
#'  (1988). SNIP, a statistics-sensitive background treatment for the
#'  quantitative analysis of PIXE spectra in geoscience applications.
#'  \emph{Nuclear Instruments and Methods in Physics Research Section B:
#'  Beam Interactions with Materials and Atoms}, 34(3), p. 396-402.
#'  DOI: \href{https://doi.org/10.1016/0168-583X(88)90063-8}{10.1016/0168-583X(88)90063-8}
#' @example inst/examples/ex-baseline.R
#' @author N. Frerebeau
#' @docType methods
#' @family signal processing
#' @name baseline
#' @rdname baseline
NULL

#' @rdname baseline
#' @aliases estimate_baseline-method
setGeneric(
  name = "estimate_baseline",
  def = function(object, ...) standardGeneric("estimate_baseline")
)

#' @rdname baseline
#' @aliases remove_baseline-method
setGeneric(
  name = "remove_baseline",
  def = function(object, ...) standardGeneric("remove_baseline")
)

# ========================================================= Dose rate prediction
#' Dose Rate Estimation
#'
#' \code{fit_dose} builds a calibration curve for gamma dose rate estimation.
#'
#' \code{predict_dose} predicts in-situ gamma dose rate.
#' @param object A \linkS4class{GammaSpectra} or \linkS4class{CalibrationCurve}
#'  object.
#' @param Ni_noise,NiEi_noise A length-two \code{\link{numeric}} vector giving
#'  the background noise integration value and error, respectively.
#' @param Ni_range,NiEi_range A length-two \code{\link{numeric}} vector giving
#'  the energy range to integrate within (in keV).
#' @param alpha A \code{\link{numeric}} value giving the cutoff value for
#'  confidence intervals (see \code{\link[IsoplotR]{york}}).
#' @param details A \code{\link{list}} of length-one vector specifying
#'  additional informations about the instrument for which the curve is built.
#' @param spectrum An optional \linkS4class{GammaSpectrum} or
#'  \linkS4class{GammaSpectra} object in which to look for variables with which
#'  to predict. If omitted, the fitted values are used.
#' @param threshold A \code{\link{character}} sting specifying the threshold
#'  to be used. It must be one of \code{"Ni"} (default) or \code{"NiEi"}
#'  (see details).
#' @param epsilon A \code{\link{numeric}} value giving an extra error term
#'  introduced by the calibration of the energy scale of the spectrum.
# @param simplify A \code{\link{logical}} scalar: should the result be
#  simplified to a matrix? If \code{FALSE} (default), returns a list.
#' @param ... Currently not used.
#' @return
#'  \code{fit_dose} returns a \linkS4class{CalibrationCurve} object.
#'
#'  \code{predict_dose} returns a \code{\link{data.frame}} with the following
#'  columns:
#'  \describe{
#'   \item{name}{(\code{\link{character}}) the name of the spectra.}
#'   \item{*_signal}{(\code{\link{numeric}}) the integrated signal value
#'   (according to the value of \code{threshold}; see
#'   \code{\link{integrate_signal}}).}
#'   \item{*_error}{(\code{\link{numeric}}) the integrated signal error value
#'   (according to the value of \code{threshold}; see
#'   \code{\link{integrate_signal}}).}
#'   \item{gamma_signal}{(\code{\link{numeric}}) the predicted gamma dose rate.}
#'   \item{gamma_error}{(\code{\link{numeric}}) the predicted gamma dose rate
#'   error.}
#'  }
#' @seealso \link{integrate_signal}
#' @references
#'  Mercier, N. & Falguères, C. (2007). Field Gamma Dose-Rate Measurement with
#'  a NaI(Tl) Detector: Re-Evaluation of the "Threshold" Technique.
#'  \emph{Ancient TL}, 25(1), p. 1-4.
#'
#'  York, D., Evensen, N. M., Martínez, M. L. & De Basabe Delgado, J. (2004).
#'  Unified Equations for the Slope, Intercept, and Standard Errors of the Best
#'  Straight Line. \emph{American Journal of Physics}, 72(3), p. 367-75.
#'  DOI: \href{https://doi.org/10.1119/1.1632486}{10.1119/1.1632486}.
#' @example inst/examples/ex-doserate.R
#' @author N. Frerebeau
#' @docType methods
#' @family dose rate
#' @name doserate
#' @rdname doserate
NULL

#' @rdname doserate
#' @aliases fit_dose-method
setGeneric(
  name = "fit_dose",
  def = function(object, background, doses, ...) standardGeneric("fit_dose")
)

#' @rdname doserate
#' @aliases predict_dose-method
setGeneric(
  name = "predict_dose",
  def = function(object, spectrum, ...) standardGeneric("predict_dose")
)

# ==================================================================== Integrate
#' Signal Integration
#'
#' @param object A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra}
#'  object.
#' @param background A \linkS4class{GammaSpectrum} object.
#' @param range A length-two \code{\link{numeric}} vector giving the energy
#'  range to integrate within (in keV).
#' @param energy A \code{\link{logical}} scalar: TODO?
#' @param simplify A \code{\link{logical}} scalar: should the result be
#'  simplified to a matrix? The default value, \code{FALSE}, returns a list.
#' @param ... Currently not used.
#' @details
#'  It assumes that each spectrum is calibrated in energy.
#' @return
#'  If \code{simplify} is \code{FALSE} (the default) returns a
#'  \code{\link{list}} of numeric vectors (the signal value and its error),
#'  else returns a \code{\link{matrix}}.
#' @references
#'  Guérin, G. & Mercier, M. (2011). Determining Gamma Dose Rates by Field Gamma
#'  Spectroscopy in Sedimentary Media: Results of Monte Carlo Simulations.
#'  \emph{Radiation Measurements}, 46(2), p. 190-195.
#'  DOI: \href{https://www.doi.org/10.1016/j.radmeas.2010.10.003}{10.1016/j.radmeas.2010.10.003}.
#'
#'  Mercier, N. & Falguères, C. (2007). Field Gamma Dose-Rate Measurement with
#'  a NaI(Tl) Detector: Re-Evaluation of the "Threshold" Technique.
#'  \emph{Ancient TL}, 25(1), p. 1-4.
#' @author N. Frerebeau
#' @docType methods
#' @family signal processing
#' @rdname integrate
#' @aliases integrate_signal-method
setGeneric(
  name = "integrate_signal",
  def = function(object, background, ...) standardGeneric("integrate_signal")
)

# ======================================================================== Peaks
#' Peaks
#'
#' Finds local maxima in sequential data.
#' @param object A \linkS4class{GammaSpectrum} or \linkS4class{PeakPosition}
#'  object.
#' @param method A \code{\link{character}} string specifying the method to be
#'  used for background noise estimation (see below).
#' @param SNR An \code{\link{integer}} giving the signal-to-noise-ratio for
#'  peak detection (see below).
#' @param span An \code{\link{integer}} giving the half window size (in number
#'  of chanels). If \code{NULL}, 5\% of the number of chanels is used as the
#'  half window size.
#' @param ... Extra parameters to be passed to internal methods.
#' @details
#'  A local maximum has to be the highest one in the given window and has to be
#'  higher than \eqn{SNR \times noise}{SNR * noise} to be recognized as peak.
#'
#'  The following methods are availble for noise estimation:
#'  \describe{
#'   \item{MAD}{Median Absolute Deviation.}
#'  }
#' @return An object of class \linkS4class{PeakPosition}.
#' @example inst/examples/ex-peaks.R
#' @author N. Frerebeau
#' @docType methods
#' @family signal processing
#' @rdname peaks
#' @aliases find_peaks-method
setGeneric(
  name = "find_peaks",
  def = function(object, ...) standardGeneric("find_peaks")
)

# ========================================================================= Plot
#' Plot
#'
#' @param x,y Objects to be plotted.
#' @param xaxis,yaxis A \code{\link{character}} string specifying the data to
#'  be plotted along each axis. It must be one of "\code{energy}" or
#'  "\code{chanel}" (\code{x} axis) and "\code{counts}" or "\code{rate}"
#'  (\code{y} axis). Any unambiguous substring can be given.
#' @param select A \code{\link{numeric}} or \code{\link{character}} vector
#'  giving the selection of the spectrum that are drawn.
#' @param facet A \code{\link{logical}} scalar: should a matrix of panels
#'  defined by spectrum be drawn?
#' @param threshold A \code{\link{character}} sting specifying the threshold
#'  to be used. It must be one of \code{"Ni"} (default) or \code{"NiEi"}
#'  (see details).
#' @param ... Currently not used.
#' @return
#'  A \code{\link[ggplot2]{ggplot}} object.
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family IO
#' @name plot
#' @rdname plot
#' @aliases plot-method
if (!isGeneric("plot")) {
  setGeneric(
    name = "plot",
    def = function(x, y, ...) standardGeneric("plot")
  )
}

# ============================================================== Read/write data
#' Data Input
#'
#' Reads a gamma ray spectrum file.
#' @param file A \code{\link{character}} string giving the path of files to be
#'  imported.
#' @param extensions A \code{\link{character}} vector specifying the possible
#'  file extensions. It must be one or more of "\code{cnf}", "\code{tka}".
#' @param ... Extra parameters to be passed to
#'  \code{\link[rxylib]{read_xyData}}.
#' @note
#'  \emph{Only supports Canberra CNF and TKA files.}
#' @return
#'  A \linkS4class{GammaSpectra} object if more than one spectrum are imported
#'  at once, else a \linkS4class{GammaSpectrum} object.
#' @seealso \link[rxylib]{read_xyData}
#' @example inst/examples/ex-read.R
#' @author N. Frerebeau
#' @docType methods
#' @family IO
#' @rdname read
#' @aliases read-method
setGeneric(
  name = "read",
  def = function(file, ...) standardGeneric("read")
)

# ======================================================================== Slice
#' Choose Chanels by Position
#'
#' Choose chanels by position.
#' @param object A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra}
#'  object.
#' @param ... \code{\link{integer}} values giving the chanels of the
#'  spectrum to be kept/dropped (see below). Numeric values are coerced to
#'  integer as by \code{\link{as.integer}} (and hence truncated towards zero).
#' @details
#'  Either positive values to keep, or negative values to drop, should be
#'  provided. The values provided must be either all positive or all negative.
#'
#'  If no value is provided, an attempt is made to define the number
#'  of channels to skip at the beginning of the spectrum. This drops all
#'  channels before the highest count maximum. This is intended to deal with the
#'  artefact produced by the rapid growth of random background noise towards low
#'  energies.
#' @return
#'  A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra} object.
#' @author N. Frerebeau
#' @example inst/examples/ex-slice.R
#' @docType methods
#' @family signal processing
#' @rdname slice
#' @aliases slice_signal-method
setGeneric(
  name = "slice_signal",
  def = function(object, ...) standardGeneric("slice_signal")
)

# ==================================================================== Smoothing
#' Smooth
#'
#' Smoothes intensities.
#' @param object A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra}
#'  object.
#' @param method A \code{\link{character}} string specifying the smoothing
#'  method to be used. It must be one of "\code{unweighted}" (default),
#'  "\code{weighted}" or "\code{savitzky}" (see details).
#'  Any unambiguous substring can be given.
#' @param m An odd \code{\link{integer}} giving the number of adjacent
#'  points to be used.
#' @param p An \code{\link{integer}} giving the polynomial degree.
#'  Only used if \code{method} is "\code{savitzky}".
#' @param ... Currently not used.
#' @details
#'  The following smoothing methods are available:
#'  \describe{
#'   \item{unweighted}{Unweighted sliding-average or rectangular smooth.
#'   It replaces each point in the signal with the average of \eqn{m} adjacent
#'   points.}
#'   \item{weighted}{Weighted sliding-average or triangular smooth.
#'   It replaces each point in the signal with the weighted mean of \eqn{m}
#'   adjacent points.}
#'   \item{savitzky}{Savitzky-Golay filter. This method is based on the
#'   least-squares fitting of polynomials to segments of \eqn{m} adjacent
#'   points.}
#'  }
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the spectrum for which a complete \eqn{m}-width smooth cannot be
#'  calculated. To prevent data loss, progressively smaller smooths are used at
#'  the ends of the spectrum if \code{method} is \code{unweighted} or
#'  \code{weighted}. If the Savitzky-Golay filter is used, the original
#'  \eqn{(m - 1) / 2} points at the ends of the spectrum are preserved.
#' @return
#'  A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra} object.
#' @references
#'  Gorry, P. A. (1990). General Least-Squares Smoothing and Differentiation by
#'  the Convolution (Savitzky-Golay) Method. \emph{Analytical Chemistry}, 62(6),
#'  p. 570-573.
#'  DOI: \href{https://doi.org/10.1021/ac00205a007}{10.1021/ac00205a007}.
#'
#'  Savitzky, A. & Golay, M. J. E. (1964). Smoothing and Differentiation of
#'  Data by Simplified Least Squares Procedures. \emph{Analytical Chemistry},
#'  36(8), p. 1627-1639.
#'  DOI: \href{https://doi.org/10.1021/ac60214a047}{10.1021/ac60214a047}.
#' @author N. Frerebeau
#' @example inst/examples/ex-smooth.R
#' @docType methods
#' @family signal processing
#' @rdname smooth
#' @aliases smooth_signal-method
setGeneric(
  name = "smooth_signal",
  def = function(object, ...) standardGeneric("smooth_signal")
)

# ==================================================================== Stabilize
#' Transform Intensities
#'
#' @param object A \linkS4class{GammaSpectrum} object.
#' @param transformation A \code{\link{function}} that takes a numeric vector as
#'  argument and returns a numeric vector.
#' @param ... Extra arguments to be passed to \code{transformation}.
#' @return A new \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra}
#'  object with transformed intensities.
#' @author N. Frerebeau
#' @docType methods
#' @family signal processing
#' @rdname stabilize
#' @aliases stabilize_signal-method
setGeneric(
  name = "stabilize_signal",
  def = function(object, ...) standardGeneric("stabilize_signal")
)

# ==================================================================== Summarize
#' Summarize
#'
#' @param object A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra}
#' object.
#' @param ... Currently not used.
#' @return A \code{\link{data.frame}}.
#' @author N. Frerebeau
#' @example inst/examples/ex-summarise.R
#' @docType methods
#' @family IO
#' @rdname summarise
#' @aliases summarise-method
setGeneric(
  name = "summarise",
  def = function(object, ...) standardGeneric("summarise")
)
