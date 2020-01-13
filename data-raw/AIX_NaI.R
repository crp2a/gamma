# Calibration CEREGE NaI#1 =====================================================
## Set dose rates
AIX_NaI_data <- utils::read.table("./data-raw/AIX_NaI.csv",
                                  header = TRUE, sep = ",", dec = ".",
                                  stringsAsFactors = FALSE)
usethis::use_data(AIX_NaI_data, internal = FALSE, overwrite = FALSE)

## Linear regression
# Build calibration curve
AIX_NaI_Ni <- .DoseRateModelNi(
  model = lm(gamma_dose ~ Ni_signal, data = AIX_NaI_data),
  background = c(1.40046863931121, 0.0190632603473346),
  range = c(200, 2800)
)
AIX_NaI_NiEi <- .DoseRateModelNiEi(
  model = lm(gamma_dose ~ NiEi_signal, data = AIX_NaI_data),
  background = c(1108.05656611974, 0.536218076993667),
  range = c(200, 2800)
)
AIX_NaI_curve <- .CalibrationCurve(
  Ni = AIX_NaI_Ni,
  NiEi = AIX_NaI_NiEi,
  data = AIX_NaI_data,
  details = list(
    laboratory = "CEREGE",
    instrument = "InSpector 1000",
    detector = "NaI",
    authors = "CEREGE Luminescence Team"
  )
)
usethis::use_data(AIX_NaI_curve, internal = FALSE, overwrite = FALSE)
