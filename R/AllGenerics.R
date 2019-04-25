# GENERIC METHODS
#' @include AllClasses.R
NULL

#' Calibration
#'
#' Builds a calibration curve for gamma dose rate estimation.
#' @param object An object of class \linkS4class{GammaSpectra}.
#' @param doses A \code{\link{list}} of length-two numeric vectors giving the
#'  dose rate values and errors, respectively, for each element of
#'  \code{object}. Elements of the list will be matched by names to the
#'  references of the spectra.
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
#' @return An object of class \linkS4class{CalibrationCurve}.
#' @seealso \link{estimateDoseRate}, \link{integrateSignal}
#' @example inst/examples/ex-doserate.R
#' @author N. Frerebeau
#' @docType methods
#' @rdname calibrateDose
#' @aliases calibrateDose-method
setGeneric(
  name = "calibrateDose",
  def = function(object, doses, noise, ...) standardGeneric("calibrateDose")
)

#' Spectrum calibration
#'
#' Calibrate the energy scale of a gamma spectrum.
#' @param object An object of class \linkS4class{GammaSpectrum},
#'  \linkS4class{GammaSpectra} or \linkS4class{PeakModel}.
#' @param lines A list of or a \code{\link{numeric}} vector.
#'  If a \code{list} is provided, each element must be a named length-two
#'  numeric vector giving the observed peak position (chanel) and the
#'  corresponding expected energy value (in keV). If \code{lines} is a
#'  numeric \code{vector}, each element must be an expected energy value.
#' @param ... Currently not used.
#' @details
#'  TODO
#' @return An object of class \linkS4class{GammaSpectrum}.
#' @example inst/examples/ex-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @rdname calibrate
#' @aliases calibrate-method
setGeneric(
  name = "calibrate",
  def = function(object, lines, ...) standardGeneric("calibrate")
)

#' Baseline estimation and removal
#'
#' @param object A \code{\link{numeric}} vector.
#' @param method A \code{\link{character}} string specifying the method to be
#'  used for baseline estimation.
#' @param ... Extra parameters passed to internal functions.
#' @return
#'  \code{estimateBaseline} and \code{removeBaseline} return a
#'  \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra} object (same as
#'  \code{object}).
#' @references
#'  Morháč, M., Kliman, J., Matoušek, V., Veselský, M. and Turzo, I. (1997).
#'  Background elimination methods for multidimensional gamma-ray spectra.
#'  \emph{Nuclear Instruments and Methods in Physics Research Section A:
#'  Accelerators, Spectrometers, Detectors and Associated Equipment}, 401(1), p. 113-132.
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
#' @name processBaseline
#' @rdname processBaseline
NULL

#' @rdname processBaseline
#' @aliases estimateBaseline-method
setGeneric(
  name = "estimateBaseline",
  def = function(object, ...) standardGeneric("estimateBaseline")
)

#' @rdname processBaseline
#' @aliases removeBaseline-method
setGeneric(
  name = "removeBaseline",
  def = function(object, ...) standardGeneric("removeBaseline")
)

#' Gamma dose rate
#'
#' Estimates in-situ gamma dose rate.
#' @param object An object of class \linkS4class{GammaSpectra}.
#' @param curve An object of class \linkS4class{CalibrationCurve}.
#' @param epsilon A \code{\link{numeric}} value.
#' @param ... Currently not used.
#' @example inst/examples/ex-doserate.R
#' @author N. Frerebeau
#' @docType methods
#' @rdname estimateDoseRate
#' @aliases estimateDoseRate-method
setGeneric(
  name = "estimateDoseRate",
  def = function(object, curve, ...) standardGeneric("estimateDoseRate")
)

#' Peaks
#'
#' Finds local maxima in sequential data.
#' @param object An object of class \linkS4class{GammaSpectrum} or
#'  \linkS4class{PeakPosition}.
#' @param peaks A \code{\link{numeric}} vector giving the starting peak postions
#'  for the nonlinear model fitting (see below).
#' @param scale A \code{\link{character}} string specifying the scale of
#'  \code{peaks}. It must be one of "\code{chanel}" (the default) or
#'  "\code{energy}".
#' @param bounds A \code{\link{numeric}} vector giving the
#'  parameters bounds (in percent), replicated to be of length three (see
#'  below). If \code{NULL} (the default), all parameters are assumed to be
#'  unconstrained.
#' @param method A \code{\link{character}} string specifying the methode to be
#'  used for background noise estimation (see below).
#' @param SNR An \code{\link{integer}} giving the signal-to-noise-ratio for
#'  peak detection (see below).
#' @param span An \code{\link{integer}} giving the half window size (in number
#'  of chanels). If \code{NULL}, 5\% of the number of chanels is used as the
#'  half window size.
#' @param ... Extra parameters passed to \code{\link{estimateBaseline}}.
#' @section Peak detection:
#'  A local maximum has to be the highest one in the given window and has to be
#'  higher than \eqn{SNR \times noise}{SNR * noise} to be recognized as peak.
#' @section Peak fitting:
#'  TODO
#' @return
#'  \code{findPeaks} returns an object of class \linkS4class{PeakPosition}.
#'
#'  \code{fitPeaks} returns an object of class \linkS4class{PeakModel}.
#' @example inst/examples/ex-peaks.R
#' @seealso \link{estimateBaseline}
#' @author N. Frerebeau
#' @docType methods
#' @name peaks
#' @rdname peaks
NULL

#' @rdname peaks
#' @aliases findPeaks-method
setGeneric(
  name = "findPeaks",
  def = function(object, ...) standardGeneric("findPeaks")
)

#' @rdname peaks
#' @aliases fitPeaks-method
setGeneric(
  name = "fitPeaks",
  def = function(object, peaks, ...) standardGeneric("fitPeaks")
)

#' Signal integration
#'
#' @param object An object of class \linkS4class{GammaSpectrum} or
#'  \linkS4class{GammaSpectra}.
#' @param range A length two \code{\link{numeric}} vector giving the energy
#'  range to integrate within (in keV).
#' @param noise A length-two \code{\link{numeric}} vector giving the noise
#'  value and error, respectively (see details).
#' @param NiEi A \code{\link{logical}} scalar.
#'  Change this only if you know what you are doing.
#' @param ... Currently not used.
#' @param simplify A \code{\link{logical}} scalar: should the result be
#'  simplified to a matrix? The default value, \code{FALSE}, returns a list.
#' @details
#'  TODO
#'
#'  It assumes that each spectrum is calibrated in energy.
#' @return
#'  If \code{simplify} is \code{FALSE}, then returns a list length-two
#'  numeric vectors (default), else returns a matrix.
#' @author N. Frerebeau
#' @docType methods
#' @rdname integrateSignal
#' @aliases integrateSignal-method
#' @keywords internal
setGeneric(
  name = "integrateSignal",
  def = function(object, ...) standardGeneric("integrateSignal")
)

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
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @name plot
#' @rdname plot
#' @aliases plot-method
if (!isGeneric("plot"))
  setGeneric(name = "plot", def = function(x, y, ...) standardGeneric("plot"))

#' Data input
#'
#' Reads a gamma ray spectrum file.
#' @param file A \code{\link{character}} string giving the path of files to be
#'  imported.
#' @param extensions A \code{\link{character}} vector specifying the possible
#'  file extensions. It must be one or more of "\code{cnf}", "\code{tka}".
#' @param skip A \code{\link{numeric}} vector giving the chanels of the data
#'  file to skip. If \code{TRUE}, it will try to automatically process the
#'  spectrum: all channels before the first maximum will be skipped.
#' @param ... Extra parameters passed to \code{\link[rxylib]{read_xyData}}.
#' @note
#'  \emph{Only supports Canberra CNF and TKA files.}
#' @return
#'  An object of class \linkS4class{GammaSpectra}.
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
