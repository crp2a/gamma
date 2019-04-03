# GENERIC METHODS
#' @include AllClasses.R
NULL

#' Calibration
#'
#' Builds a calibration curve.
#' @param object An object of class \linkS4class{GammaSpectra}.
#' @param dose A \code{\link{list}} of length-two numeric vector.
#' @param noise A \code{\link{list}} of numeric values.
#' @param ... Extra parameters passed to \code{\link{integrateSignal}}.
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
#' @param LLS A \code{\link{logical}} scalar: should the LLS operator be applied
#'  on \code{x} before employing SNIP algorithm?
#' @param ... Extra parameters passed to other methods.
#' @return
#'  \code{estimateBaseline} returns a four columns data frame or a list
#'  of data frame.
#'
#'  \code{removeBaseline} returns a \linkS4class{GammaSpectrum} or a
#'  \linkS4class{GammaSpectra} object (same as \code{object}).
#' @references
#'  Morhác, M., Kliman, J., Matoucek, V., Veselský, M. and Turzo, I. (1997).
#'  Background elimination methods for multidimensional gamma-ray spectra.
#'  \emph{NIM}, A401 (1), 113-132.
#'  DOI: \href{https://doi.org/10.1016/S0168-9002(97)01023-1}{10.1016/S0168-9002(97)01023-1}
#'
#'  Ryan, C. G., Clayton, E., Griffin, W. L., Sie, S. H. and Cousens, D. R.
#'  (1988). SNIP, a statistics-sensitive background treatment for the
#'  quantitative analysis of PIXE spectra in geoscience applications.
#'  \emph{NIM}, B34 (3), 396-402.
#'  DOI: \href{https://doi.org/10.1016/0168-583X(88)90063-8}{10.1016/0168-583X(88)90063-8}
#' @example inst/examples/ex-baseline.R
#' @author N. Frerebeau
#' @docType methods
#' @name baseline
#' @rdname baseline
NULL

#' @rdname baseline
#' @aliases estimateBaseline-method
setGeneric(
  name = "estimateBaseline",
  def = function(object, ...) standardGeneric("estimateBaseline")
)

#' @rdname baseline
#' @aliases removeBaseline-method
setGeneric(
  name = "removeBaseline",
  def = function(object, ...) standardGeneric("removeBaseline")
)

#' Gamma dose rate
#'
#' Estimates gamma dose rate.
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

#' Find peaks
#'
#' Finds local maxima in sequential data.
#' @param object An object of class \linkS4class{GammaSpectrum}.
#' @param span A \code{\link{numeric}} giving the half window size. If
#'  \code{NULL}, 5\% of the number of chanels is used as the half window size.
#' @param ... Currently not used.
#' @details
#'  A local maximum have to be the highest one in the given window to be
#'  recognized as peak.
#' @return
#'  A three columns data frame or a list of data frame.
#' @note
#'  Adapted from Stasia Grinberg's \href{https://github.com/stas-g/findPeaks}{algorithm}.
#' @example inst/examples/ex-baseline.R
#' @author N. Frerebeau
#' @docType methods
#' @rdname findPeaks
#' @aliases findPeaks-method
setGeneric(
  name = "findPeaks",
  def = function(object, ...) standardGeneric("findPeaks")
)

#' Signal integration
#'
#' @param object An object of class \linkS4class{GammaSpectrum}.
#' @param peaks A \code{\link{numeric}} vector.
#' @param noise A \code{\link{list}} of numeric values.
#' @param m An \code{\link{integer}}.
#' @param ... Currently not used.
# @example inst/examples/ex-calibrate.R
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
#' @param select A \code{\link{numeric}} or \code{\link{character}} vector
#'  giving the selection of the spectrum that are drawn.
#' @param facet A \code{\link{logical}} scalar: should a matrix of panels
#'  defined by case/sample be drawn?
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
#' Reads a gamma dose rate file.
#' @param file A \code{\link{character}} string giving the path of files to be
#'  imported.
#' @param ... Extra parameters passed to \code{\link[rxylib]{read_xyData}}.
#' @details
#'  TODO
#' @return
#'  An object of class \linkS4class{GammaSpectra}.
#' @note
#'  \emph{Only works with Canberra CNF.}
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
