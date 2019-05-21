#' CEREGE Calibration Curve #1
#'
#' @details
#' \tabular{ll}{
#'  \strong{Laboratory:} \tab CEREGE \cr
#'  \strong{Instrument:} \tab Canberra Inspector 1000 \cr
#'  \strong{Detector:} \tab NaI \cr
#'  \strong{Authors:} \tab CEREGE Luminescence Team
#' }
#' @section Changelog:
#'  \describe{
#'   \item{v0.1.1}{Curve creation.
#'   Model summary: slope: 0.018 +/- 0; intercept: -34.342 +/- 49.303; residual
#'   standard error: 34.56; multiple R-squared: 0.99775;
#'   adjusted R-squared: 0.997}
#'  }
#' @examples
#' # Load the curve
#' utils::data("AIX1", package = "gamma")
#' AIX1
#'
#' # See how to replicate the curve
#' utils::vignette("AIX1#1", package = "gamma")
"AIX1"

#' CRP2A Calibration Curve #1
#'
#' @details
#' \tabular{ll}{
#'  \strong{Laboratory:} \tab IRAMAT-CRP2A (UMR 5060) \cr
#'  \strong{Instrument:} \tab Canberra Inspector 1000 \cr
#'  \strong{Detector:} \tab LaBr \cr
#'  \strong{Authors:} \tab CRP2A Luminescence Team
#' }
#' @section Changelog:
#'  \describe{
#'   \item{v0.1.1}{Edit the energy scale of the calibration spectra.
#'   Model summary: slope: 0.032 +/- 0; intercept: -41.173 +/- 20.847;
#'   residual standard error: 20.83; multiple R-squared: 0.99916;
#'   adjusted R-squared: 0.999.}
#'   \item{v0.1.0}{Curve creation.
#'   Model summary: slope: 0.032 +/- 0; intercept: -41.329 +/- 20.922;
#'   residual standard error: 20.91; multiple R-squared: 0.99916;
#'   adjusted R-squared: 0.99899.}
#'  }
#' @examples
#' # Load the curve
#' utils::data("BDX1", package = "gamma")
#' BDX1
#'
#' # See how to replicate the curve
#' utils::vignette("CRP2A#1", package = "gamma")
"BDX1"
