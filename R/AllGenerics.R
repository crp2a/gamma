# GENERIC METHODS
#' @include AllClasses.R
NULL

#' Calibration
#'
#' Builds a calibration curve for gamma dose rate estimation.
#' @param object An object of class \linkS4class{GammaSpectra}.
#' @param dose A \code{\link{list}} of length-two numeric vector.
#' @param noise A \code{\link{list}} of numeric values.
#' @param laboratory A \code{\link{character}} string giving the laboratory
#'  name.
#' @param ... Extra parameters passed to \code{\link{integrateSignal}}.
#' @seealso \link{estimateDoseRate}
#' @example inst/examples/ex-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @rdname calibrate
#' @aliases calibrate-method
setGeneric(
  name = "calibrate",
  def = function(object, ...) standardGeneric("calibrate")
)

#' Baseline estimation and removal
#'
#' @param object A \code{\link{numeric}} vector.
#' @param method TODO.
#' @param ... Extra parameters passed to other methods.
#' @return
#'  \code{estimateBaseline} returns a four columns data frame or a list
#'  of data frame.
#'
#'  \code{removeBaseline} returns a modified \linkS4class{GammaSpectrum} or
#'  \linkS4class{GammaSpectra} object (same as \code{object}, with background
#'  removed).
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
#' @example inst/examples/ex-BaseLine.R
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
#' @param noise A \code{\link{list}} of numeric values.
#' @param ... Extra parameters passed to \code{\link{integrateSignal}}.
#' @example inst/examples/ex-calibrate.R
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
#' @param object An object of class \linkS4class{GammaSpectrum}.
#' @param peaks XXX.
#' @param method A \code{\link{character}} string.
#' @param SNR An \code{\link{integer}}.
#' @param span An \code{\link{integer}} giving the half window size (in number
#'  of chanels). If \code{NULL}, 5\% of the number of chanels is used as the
#'  half window size.
#' @param ... Extra parameters passed to internal functions.
#' @details
#'  A local maximum has to be the highest one in the given window and has to be
#'  higher than \eqn{SNR \times noise}{SNR * noise} to be recognized as peak.
#' @return
#'  \code{findPeaks} returns an object of class \linkS4class{PeakPosition}.
#'
#'  \code{fitPeaks} returns an object of class \linkS4class{PeakModel}.
#' @example inst/examples/ex-BaseLine.R
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
#' @param object An object of class \linkS4class{GammaSpectrum}.
#' @param range A length two \code{\link{numeric}} vector giving the energy
#'  range to integrate within.
#' @param peaks A \code{\link{numeric}} vector.
#' @param noise A \code{\link{list}} of numeric values.
#' @param span An \code{\link{integer}} giving the half window size for peak
#'  searching (see \code{\link{findPeaks}}).
#' @param ... Extra parameters passed to \code{\link{findPeaks}}.
#' @details
#'  TODO
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
#'  be plotted along each axis.
#' @param select A \code{\link{numeric}} or \code{\link{character}} vector
#'  giving the selection of the spectrum that are drawn.
#' @param facet A \code{\link{logical}} scalar: should a matrix of panels
#'  defined by spectrum be drawn?
#' @param ... Currently not used.
#' @details
#'  TODO
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
#' @param ... Extra parameters passed to \code{\link[rxylib]{read_xyData}}.
#' @details
#'  \emph{Only works with Canberra CNF.}
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
