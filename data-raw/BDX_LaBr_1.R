# Calibration CRP2A LaBr#1 =====================================================
## Set dose rates
BDX_LaBr_1_data <- utils::read.table("./data-raw/BDX_LaBr_1.csv",
                                     header = TRUE, sep = ",", dec = ".",
                                     stringsAsFactors = FALSE)
usethis::use_data(BDX_LaBr_1_data, internal = FALSE, overwrite = FALSE)

## Linear regression
# Build calibration curve
Ni_data <- BDX_LaBr_1_data[, c(1, 3, 4, 7, 8)]
colnames(Ni_data) <- c("names", "signal_value", "signal_error",
                       "gamma_dose", "gamma_error")
Ni_model <- IsoplotR::york(Ni_data[, -1], alpha = 0.05)

BDX_LaBr_1_Ni <- .DoseRateModel(
  slope = as.numeric(Ni_model$b),
  intercept = as.numeric(Ni_model$a),
  covariance = Ni_model$cov.ab,
  MSWD = Ni_model$mswd,
  df = Ni_model$df,
  p_value = Ni_model$p.value,
  data = Ni_data,
  range = c(300, 2800),
  background = c(22.607509838216, 0.049712234699046)
)


NiEi_data <- BDX_LaBr_1_data[, c(1, 5, 6, 7, 8)]
colnames(NiEi_data) <- c("names", "signal_value", "signal_error",
                       "gamma_dose", "gamma_error")
NiEi_model <- IsoplotR::york(NiEi_data[, -1], alpha = 0.05)

BDX_LaBr_1_NiEi <- .DoseRateModel(
  slope = as.numeric(NiEi_model$b),
  intercept = as.numeric(NiEi_model$a),
  covariance = NiEi_model$cov.ab,
  MSWD = NiEi_model$mswd,
  df = NiEi_model$df,
  p_value = NiEi_model$p.value,
  data = NiEi_data,
  range = c(165, 2800),
  background = c(25279.6317064543, 1.66234933224363)
)

BDX_LaBr_1_curve <- .CalibrationCurve(
  Ni = BDX_LaBr_1_Ni,
  NiEi = BDX_LaBr_1_NiEi,
  details = list(
    laboratory = "IRAMAT-CRP2A (UMR 5060)",
    instrument = "InSpector 1000",
    detector = "LaBr #1",
    authors = "CRP2A Luminescence Team",
    date = Sys.time()
  )
)
usethis::use_data(BDX_LaBr_1_curve, internal = FALSE, overwrite = FALSE)

## Ni
## Slope: 0.03378 +/- 0.0015 / Intercept: 0.08 +/- 1.8
## MSWD: 0.30 / Probability: 0.91
# IsoplotR::york(BDX_LaBr_1_data[, c(7,8,3,4)])
# IsoplotR::isochron(BDX_LaBr_1_data[, c(7,8,3,4)])

## NiEi
## Slope: 31.85 +/- 1.2 / Intercept: 598 +/- 1500
## MSWD: 0.080 / Probability: 0.995
# IsoplotR::york(BDX_LaBr_1_data[, c(7,8,5,6)])
# IsoplotR::isochron(BDX_LaBr_1_data[, c(7,8,5,6)])
