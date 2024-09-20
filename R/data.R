# ============================================================ Internal datasets
#' Nuclear Decay Data
#'
#' An internal dataset containing the decay data for the following isotopes:
#' 232-Th, 235-U, 238-U and 40-K.
#' @format
#'  A [`data.frame`] with the following columns (absolute errors):
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
#'  Nuclides And Isotopes : Chart of the Nuclides.
#' @name decay
#' @rdname decay
#' @keywords datasets internal
# ".decay"
NULL

#' Lanthanum Decay Data
#'
#' An internal dataset containing the decay data for the Lanthanum isotopes.
#' @format
#'  A [`data.frame`] with the following columns (absolute errors):
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
#'  Nuclides And Isotopes : Chart of the Nuclides.
#' @name decay_La
#' @rdname decay_La
#' @keywords datasets internal
# ".decay_La"
NULL

# ==============================================================================
#' Clermont Reference Data
#'
#' @usage data("clermont")
#' @format A [`data.frame`] with tabulated data and conversion factor reference.
#' @source
#'  Guérin, G., Mercier, N. & Adamiec, G. (2011). Dose-Rate Conversion Factors:
#'  Update. *Ancient TL*, 29(1), p. 5-8.
#'
#'  Miallier, D., Guérin, G., Mercier, N., Pilleyre, T. & Sanzelle, S.
#'  (2009). The Clermont Radiometric Reference Rocks: A Convenient Tool
#'  for Dosimetric Purposes. *Ancient TL*, 27(2), p. 37-44.
#' @family datasets
#' @keywords datasets
"clermont"

# ==============================================================================
#' Clermont Reference Data 2024
#'
#' An updated version for the `clermont` dataset with additional columns
#' for the applied dose rate conversion factors and their reference. The dataset
#' also contains gamma dose rate values and uncertainties for different published
#' conversion factors. Please note that the values for the conversion factors
#' applied in the original `clermont` dataset differ slightly due to rounding
#' errors.
#'
#' @usage data("clermont_2024")
#' @format A [`data.frame`] with tabulated data and conversion factor reference.
#' @source
#'  Adamiec, G. & Aitken, M.J. (1998).
#'  Dose-rate conversion factors: update. *Ancient TL*, 16, p. 37–50.
#'
#'  Cresswell, A.J., Carter, J. & Sanderson, D.C.W. (2018). Dose rate
#'  conversion parameters: Assessment of nuclear data.
#'  *Radiation Measurements*, 120, p. 195–201.
#'
#'  Guérin, G., Mercier, N. & Adamiec, G. (2011). Dose-Rate Conversion Factors:
#'  Update. *Ancient TL*, 29(1), p. 5-8.
#'
#'  Liritzis, I., Stamoulis, K., Papachristodoulou, C. & Ioannides, K. (2013).
#'  A re-evaluation of radiation dose-rate conversion factors.
#'  *Mediterranean Archaeology and Archaeometry*, 12, p. 1–15.
#'
#'  Miallier, D., Guérin, G., Mercier, N., Pilleyre, T. & Sanzelle, S.
#'  (2009). The Clermont Radiometric Reference Rocks: A Convenient Tool
#'  for Dosimetric Purposes. *Ancient TL*, 27(2), p. 37-44.
#' @family datasets
#' @keywords datasets
"clermont_2024"

#' CEREGE Calibration Curve (NaI)
#'
#' @usage data(AIX_NaI_1)
#' @format
#' An object of class [CalibrationCurve-class].
#' \tabular{ll}{
#'  **Laboratory** \tab CEREGE \cr
#'  **Instrument** \tab Canberra Inspector 1000 \cr
#'  **Detector** \tab NaI \cr
#'  **Authors** \tab CEREGE Luminescence Team
#' }
#' @examples
#' ## Load the curve
#' data(AIX_NaI_1, package = "gamma")
#' plot(AIX_NaI_1)
#' @family datasets
#' @keywords datasets
"AIX_NaI_1"

#' CRP2A Calibration Curve (LaBr)
#'
#' @usage data(BDX_LaBr_1)
#' @format
#' An object of class [CalibrationCurve-class].
#' \tabular{ll}{
#'  **Laboratory** \tab IRAMAT-CRP2A (UMR 5060) \cr
#'  **Instrument** \tab Canberra Inspector 1000 \cr
#'  **Detector** \tab LaBr \cr
#'  **Authors** \tab CRP2A Luminescence Team
#' }
#' @examples
#' ## Load the curve
#' data(BDX_LaBr_1, package = "gamma")
#' plot(BDX_LaBr_1)
#' @family datasets
#' @keywords datasets
"BDX_LaBr_1"
