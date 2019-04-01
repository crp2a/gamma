# GENERIC METHODS
#' @include AllClasses.R
NULL

#' Data adjust
#'
#' @param object An object of class \linkS4class{GammaSpectra}.
#' @param curve An object of class \linkS4class{CalibrationCurve}.
#' @param peaks A \code{\link{numeric}} vector.
#' @param noise A \code{\link{list}} of numeric values.
#' @param m An \code{\link{integer}}.
#' @param ... Currently not used.
#' @example inst/examples/ex-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @rdname adjust
#' @aliases adjust-method
setGeneric(
  name = "adjust",
  def = function(object, curve, ...) standardGeneric("adjust")
)

#' Calibration
#'
#' Builds a calibration curve.
#' @param object An object of class \linkS4class{GammaSpectra}.
#' @param dose TODO.
#' @param peaks A \code{\link{numeric}} vector.
#' @param noise A \code{\link{list}} of numeric values.
#' @param m An \code{\link{integer}}.
#' @param ... Currently not used.
#' @example inst/examples/ex-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @rdname calibrate
#' @aliases calibrate-method
setGeneric(
  name = "calibrate",
  def = function(object, ...) standardGeneric("calibrate")
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
