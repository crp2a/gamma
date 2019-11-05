# GENERIC METHODS
#' @include AllClasses.R
NULL

# ====================================================================== Extract
#' Get or Set Parts of an Object
#'
#' Getters and setters to extract or replace parts of an object.
#' @param object An object from which to get or set element(s).
#' @param value A possible value for the element(s) of \code{object} (see
#'  below).
#' @return
#'  An object of the same sort as \code{object} with the new values assigned.
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name access
#' @rdname access
#' @aliases get set
NULL

#' @export
#' @rdname access
setGeneric(
  name = "get_hash",
  def = function(object) standardGeneric("get_hash")
)

#' @export
#' @rdname access
setGeneric(
  name = "get_chanels",
  def = function(object) standardGeneric("get_chanels")
)

#' @export
#' @rdname access
setGeneric(
  name = "get_dose",
  def = function(object) standardGeneric("get_dose")
)
#' @export
#' @rdname access
setGeneric(
  name = "set_dose<-",
  def = function(object, value) standardGeneric("set_dose<-")
)

#' @export
#' @rdname access
setGeneric(
  name = "get_energy",
  def = function(object) standardGeneric("get_energy")
)
#' @export
#' @rdname access
setGeneric(
  name = "set_energy<-",
  def = function(object, value) standardGeneric("set_energy<-")
)

# ------------------------------------------------------------------------------
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

# =================================================================== Predicates
#' Predicates
#'
#' @param object An object from which to get or set element(s).
#' @return
#'  A \code{\link{logical}} vector.
#' @author N. Frerebeau
#' @docType methods
#' @family predicates
#' @name predicates
#' @rdname predicates
NULL

#' @export
#' @rdname predicates
setGeneric(
  name = "is_calibrated",
  def = function(object) standardGeneric("is_calibrated")
)

# ===================================================== Energy scale calibration
#' Spectrum calibration
#'
#' Calibrate the energy scale of a gamma spectrum.
#' @param object A \linkS4class{GammaSpectrum} or
#'  \linkS4class{GammaSpectra} object.
#' @param lines A list of or a \code{\link{numeric}} vector.
#'  If a \code{list} is provided, each element must be a named length-two
#'  numeric vector giving the observed peak position (chanel) and the
#'  corresponding expected energy value (in keV). If \code{lines} is a
#'  numeric \code{vector}, each element must be an expected energy value.
#' @param ... Currently not used.
#' @details
#'  TODO
#' @return
#'  A \linkS4class{GammaSpectrum} object.
#' @example inst/examples/ex-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @family energy
#' @rdname calibrate
#' @aliases calibrate_energy-method
setGeneric(
  name = "calibrate_energy",
  def = function(object, lines, ...) standardGeneric("calibrate_energy")
)

# ===================================================================== Baseline
#' Baseline estimation and removal
#'
#' @param object A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra}
#'  object.
#' @param method A \code{\link{character}} string specifying the method to be
#'  used for baseline estimation (see details).
#' @param LLS A \code{\link{logical}} scalar: should the LLS operator be applied
#'  on \code{x} before employing SNIP algorithm?
#' @param decreasing A \code{\link{logical}} scalar: should a decreasing
#'  clipping window be used? Only used if \code{method} is \code{SNIP}.
#' @param k An \code{\link{integer}} value giving the numerber of iterations.
#'  Only used if \code{method} is \code{SNIP}.
#' @param ... Extra parameters passed to \code{estimateBaseline}.
#' @details
#'  The following methods are availble for baseline estimation:
#'  \describe{
#'   \item{SNIP}{Sensitive Nonlinear Iterative Peak clipping algorithm
#'   (see references).}
#'  }
#' @return
#'  A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra} object (same as
#'  \code{object}).
#' @references
#'  Morháč, M., Kliman, J., Matoušek, V., Veselský, M. and Turzo, I. (1997).
#'  Background elimination methods for multidimensional gamma-ray spectra.
#'  \emph{Nuclear Instruments and Methods in Physics Research Section A:
#'  Accelerators, Spectrometers, Detectors and Associated Equipment}, 401(1),
#'  p. 113-132.
#'  DOI: \href{https://doi.org/10.1016/S0168-9002(97)01023-1}{10.1016/S0168-9002(97)01023-1}
#'
#'  Morháč, M. and Matoušek, V. (2008). Peak Clipping Algorithms for Background
#'  Estimation in Spectroscopic Data. \emph{Applied Spectroscopy}, 62(1), p. 91-106.
#'  DOI: \href{https://doi.org/10.1366/000370208783412762}{10.1366/000370208783412762}
#'
#'  Ryan, C. G., Clayton, E., Griffin, W. L., Sie, S. H. and Cousens, D. R.
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
#' Builds a calibration curve for gamma dose rate estimation.
#' @param object An object of class \linkS4class{GammaSpectra}.
#' @param noise A \code{\link{list}} of two numeric values giving the noise
#'  value and error.
#' @param range A length-two \code{\link{numeric}} vector giving the energy
#'  range to integrate within (in keV).
#' @param intercept A \code{\link{logical}} scalar: should the intercept of the
#'  curve be estimated?
#' @param weights A \code{\link{logical}} scalar: should weights be used in the
#'  fitting process. If \code{TRUE}, the inverse of the squared dose errors are
#'  used.
#' @param details A list of \code{\link{character}} vector specifying additional
#'  informations about the instrument for which the curve is built.
#' @param ... Currently not used.
#' @details
#'  TODO
#' @return A \linkS4class{CalibrationCurve} object.
#' @seealso \link{integrate_signal}
#' @example inst/examples/ex-doserate.R
#' @author N. Frerebeau
#' @family dose rate
#' @docType methods
#' @rdname fit
#' @aliases fit_dose-method
setGeneric(
  name = "fit_dose",
  def = function(object, noise, ...) standardGeneric("fit_dose")
)

#' Gamma dose rate
#'
#' Predict in-situ gamma dose rate.
#' @param object A \linkS4class{CalibrationCurve} object.
#' @param spectra An optional \linkS4class{GammaSpectra} object in
#'  which to look for variables with which to predict. If omitted, the fitted
#'  values are used.
#' @param epsilon A \code{\link{numeric}} value giving the error introduced by
#'  the energy calibration of the spectrum.
#' @param simplify A \code{\link{logical}} scalar: should the result be
#'  simplified to a matrix? If \code{FALSE} (default), returns a list.
#' @param ... Currently not used.
#' @return
#'  If \code{simplify} is \code{FALSE} returns a list of length-two numeric
#'  vectors (default), else returns a matrix.
#' @example inst/examples/ex-doserate.R
#' @author N. Frerebeau
#' @family dose rate
#' @docType methods
#' @rdname predict
#' @aliases predict_dose-method
setGeneric(
  name = "predict_dose",
  def = function(object, ...) standardGeneric("predict_dose")
)

# ==================================================================== Integrate
#' Signal integration
#'
#' @param object A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra}
#'  object.
#' @param range A length-two \code{\link{numeric}} vector giving the energy
#'  range to integrate within (in keV).
#' @param noise A length-two \code{\link{numeric}} vector giving the noise
#'  value and error, respectively (see details).
#' @param NiEi A \code{\link{logical}} scalar.
#'  Change this only if you know what you are doing.
#' @param simplify A \code{\link{logical}} scalar: should the result be
#'  simplified to a matrix? The default value, \code{FALSE}, returns a list.
#' @param ... Currently not used.
#' @details
#'  TODO
#'
#'  It assumes that each spectrum is calibrated in energy.
#' @return
#'  If \code{simplify} is \code{FALSE} returns a list of length-two
#'  numeric vectors (default), else returns a matrix.
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
#' @family signal processing
#' @docType methods
#' @rdname integrate
#' @aliases integrate_signal-method
setGeneric(
  name = "integrate_signal",
  def = function(object, range, noise, ...) standardGeneric("integrate_signal")
)

# ======================================================================== Peaks
#' Peaks
#'
#' Finds local maxima in sequential data.
#' @param object A \linkS4class{GammaSpectrum} or \linkS4class{PeakPosition}
#'  object.
#' @param method A \code{\link{character}} string specifying the methode to be
#'  used for background noise estimation (see below).
#' @param SNR An \code{\link{integer}} giving the signal-to-noise-ratio for
#'  peak detection (see below).
#' @param span An \code{\link{integer}} giving the half window size (in number
#'  of chanels). If \code{NULL}, 5\% of the number of chanels is used as the
#'  half window size.
#' @param ... Extra parameters to be passed to internal methods.
#' @section Peak detection:
#'  A local maximum has to be the highest one in the given window and has to be
#'  higher than \eqn{SNR \times noise}{SNR * noise} to be recognized as peak.
#' @section Peak fitting:
#'  TODO
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
#' @param ... Currently not used.
#' @return
#'  A \code{\link[ggplot2]{ggplot}} object.
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
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
#' Data input
#'
#' Reads a gamma ray spectrum file.
#' @param file A \code{\link{character}} string giving the path of files to be
#'  imported.
#' @param extensions A \code{\link{character}} vector specifying the possible
#'  file extensions. It must be one or more of "\code{cnf}", "\code{tka}".
#' @param skip An \code{\link{integer}} vector or a \code{\link{logical}} scalar
#'  (see details). If \code{NULL} (the default) or \code{FALSE}, all data are
#'  imported.
#' @param ... Extra parameters to be passed to
#'  \code{\link[rxylib]{read_xyData}}.
#' @details
#'  If \code{skip} is not \code{NULL}, several channels are skipped during
#'  import to retain only a part of the spectrum.
#'  If \code{skip} is an \code{integer} vector, the corresponding chanels of the
#'  data file will be skipped.
#'  If \code{skip} is \code{TRUE}, an attempt is made to define the number of
#'  channels to skip at the beginning of the spectrum. This skips all channels
#'  before the highest count maximum. This is intended to deal with the artefact
#'  produced by the rapid growth of random background noise towards low
#'  energies.
#' @note
#'  \emph{Only supports Canberra CNF and TKA files.}
#' @return
#'  A \linkS4class{GammaSpectra} object if more than one spectrum are imported
#'  at once, else a \linkS4class{GammaSpectrum} object.
#' @seealso \link[rxylib]{read_xyData}
#' @example inst/examples/ex-read.R
#' @author N. Frerebeau
#' @docType methods
#' @rdname read
#' @aliases read-method
setGeneric(
  name = "read",
  def = function(file, ...) standardGeneric("read")
)

# =================================================================== Simulation
#' Simulate a Gamma-Ray Spectrum
#'
#' Rough simulation of a gamma-ray spectrum.
#' @param K,Th,U A length-one \code{\link{numeric}} vector giving the K-U-Th
#'  ratio that caracterize a natural spectrum.
#' @param energy A length-two \code{\link{numeric}} vector giving the energy
#'  range (in keV).
#' @param n An \code{\link{integer}} giving the number of chanel.
#' @param ... Currently not used.
#' @return A \linkS4class{GammaSpectrum} object.
#' @author N. Frerebeau
#' @docType methods
#' @rdname simulate_spectrum
#' @aliases simulate_spectrum-method
setGeneric(
  name = "simulate_spectrum",
  def = function(K, U, Th, ...) standardGeneric("simulate_spectrum")
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
#'  Only used if \code{method} is \code{savitzky}.
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
#'  Savitzky, A. and Golay, M. J. E. (1964). Smoothing and Differentiation of
#'  Data by Simplified Least Squares Procedures. \emph{Analytical Chemistry},
#'  36(8), p. 1627-1639.
#'  DOI: \href{https://doi.org/10.1021/ac60214a047}{10.1021/ac60214a047}.
#' @author N. Frerebeau
#' @family signal processing
#' @example inst/examples/ex-smooth.R
#' @docType methods
#' @rdname smooth_signal
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
#' @return A new \linkS4class{GammaSpectrum} object with transformed
#'  intensities.
#' @author N. Frerebeau
#' @family signal processing
#' @docType methods
#' @rdname stabilize_signal
#' @aliases stabilize_signal-method
setGeneric(
  name = "stabilize_signal",
  def = function(object, ...) standardGeneric("stabilize_signal")
)
