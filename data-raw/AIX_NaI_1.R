# Calibration CEREGE NaI#1 =====================================================
## Set dose rates
AIX_NaI_1_data <- utils::read.table("./data-raw/AIX_NaI_1.csv",
                                  header = TRUE, sep = ",", dec = ".",
                                  stringsAsFactors = FALSE)
# usethis::use_data(AIX_NaI_1_data, internal = FALSE, overwrite = FALSE)

## Linear regression
# Build calibration curve
Ni_data <- AIX_NaI_1_data[, c(1, 3, 4, 7, 8)]
colnames(Ni_data) <- c("names", "signal_value", "signal_error",
                       "gamma_dose", "gamma_error")
Ni_model <- IsoplotR::york(Ni_data[, -1], alpha = 0.05)

AIX_NaI_1_Ni <- .DoseRateModel(
  slope = as.numeric(Ni_model$b),
  intercept = as.numeric(Ni_model$a),
  covariance = Ni_model$cov.ab,
  MSWD = Ni_model$mswd,
  df = Ni_model$df,
  p_value = Ni_model$p.value,
  data = Ni_data,
  range = c(200, 2800),
  background = c(1.40046863931121, 0.0190632603473346)
)

NiEi_data <- AIX_NaI_1_data[, c(1, 5, 6, 7, 8)]
colnames(NiEi_data) <- c("names", "signal_value", "signal_error",
                         "gamma_dose", "gamma_error")
NiEi_model <- IsoplotR::york(NiEi_data[, -1], alpha = 0.05)

AIX_NaI_1_NiEi <- .DoseRateModel(
  slope = as.numeric(NiEi_model$b),
  intercept = as.numeric(NiEi_model$a),
  covariance = NiEi_model$cov.ab,
  MSWD = NiEi_model$mswd,
  df = NiEi_model$df,
  p_value = NiEi_model$p.value,
  data = NiEi_data,
  range = c(200, 2800),
  background = c(1108.05656611974, 0.536218076993667)
)

AIX_NaI_1 <- .CalibrationCurve(
  Ni = AIX_NaI_1_Ni,
  NiEi = AIX_NaI_1_NiEi,
  details = list(
    laboratory = "CEREGE",
    instrument = "InSpector 1000",
    detector = "NaI",
    authors = "CEREGE Luminescence Team",
    date = Sys.time()
  )
)
usethis::use_data(AIX_NaI_1, internal = FALSE, overwrite = FALSE)
