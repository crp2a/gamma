# ============================================================ Internal datasets
#' Nuclear Decay Data
#'
#' An internal dataset containing the decay data for the following isotopes:
#' 232-Th, 235-U, 238-U and 40-K.
#' @format
#'  A \eqn{193 \times 13}{193 x 13} \code{\link[=data.frame]{data frame}}
#'  with the following columns (absolute errors):
#'  \describe{
#'    \item{decay_chain}{}
#'    \item{isotope}{}
#'    \item{occurrence}{}
#'    \item{occurrence_error}{}
#'    \item{post_radon}{}
#'    \item{half_life}{}
#'    \item{half_life_error}{}
#'    \item{energy}{Energy in keV.}
#'    \item{energy_error}{}
#'    \item{gamma_intensity}{}
#'    \item{gamma_intensity_error}{}
#'    \item{counts_chain}{}
#'    \item{counts_chain_error}{}
#'  }
#' @source
#'  \href{IAEA Live Chart of Nuclides}{https://www-nds.iaea.org/relnsd/vcharthtml/VChartHTML.html}.
#'  Accessed 2019-06-07.
#' @keywords internal
".decay"

#' Lanthanum Decay Data
#'
#' An internal dataset containing the decay data for the Lanthanum isotopes.
#' @format
#'  A \eqn{2094 \times 11}{2094 x 11} \code{\link[=data.frame]{data frame}}
#'  with the following columns (absolute errors):
#'  \describe{
#'    \item{decay_chain}{}
#'    \item{isotope}{}
#'    \item{occurrence}{}
#'    \item{occurrence_error}{}
#'    \item{post_radon}{}
#'    \item{half_life}{}
#'    \item{half_life_error}{}
#'    \item{energy}{Energy in keV.}
#'    \item{energy_error}{}
#'    \item{gamma_intensity}{}
#'    \item{gamma_intensity_error}{}
#'  }
#' @source
#'  \href{IAEA Live Chart of Nuclides}{https://www-nds.iaea.org/relnsd/vcharthtml/VChartHTML.html}.
#'  Accessed 2019-06-07.
#' @keywords internal
".decay_La"

# ==============================================================================
#' Clermont Reference Data
#'
#' @usage data("clermont")
#' @format TODO
#' @source
#'  Guérin, G., Mercier, N. & Adamiec, G. (2011). Dose-Rate Conversion Factors:
#'  Update. \emph{Ancient TL}, 29(1), p. 5-8.
#'
#'  Miallier, D., Guérin, G., Mercier, N., Pilleyre, T. & Sanzelle, S.
#'  (2009). The Clermont Radiometric Reference Rocks: A Convenient Tool
#'  for Dosimetric Purposes. \emph{Ancient TL}, 27(2), p. 37-44.
"clermont"

#' CEREGE Calibration Curve #1
#'
#' @usage data("AIX1")
#' @details
#' \tabular{ll}{
#'  \strong{Laboratory:} \tab CEREGE \cr
#'  \strong{Instrument:} \tab Canberra Inspector 1000 \cr
#'  \strong{Detector:} \tab NaI \cr
#'  \strong{Authors:} \tab CEREGE Luminescence Team
#' }
#' @section Changelog:
#'  \describe{
#'   \item{0.1.1}{Curve creation.
#'   Model summary:
#'   slope: 0.018 +/- 0;
#'   intercept: -34.361 +/- 49.326;
#'   residual standard error: 34.58;
#'   multiple R-squared: 0.99775;
#'   adjusted R-squared: 0.997}
#'  }
#' @examples
#' # Load the curve
#' utils::data("AIX1", package = "gamma")
#' AIX1
#'
#' \dontrun{
#' # See how to replicate the curve
#' utils::vignette("AIX1#1", package = "gamma")
#' }
"AIX1"

#' CRP2A Calibration Curve #1
#'
#' @usage data("BDX1")
#' @details
#' \tabular{ll}{
#'  \strong{Laboratory:} \tab IRAMAT-CRP2A (UMR 5060) \cr
#'  \strong{Instrument:} \tab Canberra Inspector 1000 \cr
#'  \strong{Detector:} \tab LaBr \cr
#'  \strong{Authors:} \tab CRP2A Luminescence Team
#' }
#' @section Changelog:
#'  \describe{
#'   \item{0.1.1}{Edit the energy scale of the reference spectra.
#'   Model summary:
#'   slope: 0.032 +/- 0;
#'   intercept: -41.116 +/- 20.846;
#'   residual standard error: 20.83 on 5 degrees of freedom;
#'   multiple R-squared: 0.99916;
#'   adjusted R-squared: 0.999.}
#'   \item{0.1.0}{Curve creation.
#'   Model summary:
#'   slope: 0.032 +/- 0;
#'   intercept: -41.329 +/- 20.922;
#'   residual standard error: 20.91 on 5 degrees of freedom;
#'   multiple R-squared: 0.99916;
#'   adjusted R-squared: 0.99899.}
#'  }
#' @examples
#' # Load the curve
#' utils::data("BDX1", package = "gamma")
#' BDX1
#'
#' \dontrun{
#' # See how to replicate the curve
#' utils::vignette("CRP2A#1", package = "gamma")
#' }
"BDX1"
