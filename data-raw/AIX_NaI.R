# Calibration CEREGE NaI#1 =====================================================
## Set dose rates
AIX_NaI_data <- utils::read.table("./data-raw/AIX_NaI.csv",
                                  header = TRUE, sep = ",", dec = ".",
                                  stringsAsFactors = FALSE)
usethis::use_data(AIX_NaI_data, internal = FALSE, overwrite = FALSE)

## Linear regression
# Build calibration curve
Ni_model <- IsoplotR::york(AIX_NaI_data[, c(3,4,7,8)], alpha = alpha)
AIX_NaI_Ni <- .DoseRateModel(
  slope = as.numeric(Ni_model$b),
  intercept = as.numeric(Ni_model$a),
  residuals = AIX_NaI_data[, 7] - Ni_model$b[1] * AIX_NaI_data[, 3] + Ni_model$a[1],
  df = Ni_model$df,
  MSWD = Ni_model$mswd,
  p_value = Ni_model$p.value,
  background = c(1.40046863931121, 0.0190632603473346),
  range = c(200, 2800)
)
NiEi_model <- IsoplotR::york(AIX_NaI_data[, c(5,6,7,8)], alpha = alpha)
AIX_NaI_NiEi <- .DoseRateModel(
  slope = as.numeric(NiEi_model$b),
  intercept = as.numeric(NiEi_model$a),
  residuals = AIX_NaI_data[, 7] - NiEi_model$b[1] * AIX_NaI_data[, 3] + Ni_model$a[1],
  df = NiEi_model$df,
  MSWD = NiEi_model$mswd,
  p_value = NiEi_model$p.value,
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
