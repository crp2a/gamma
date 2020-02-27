# Calibration CRP2A LaBr#1 =====================================================
## Set dose rates
BDX_LaBr_1_data <- utils::read.table("./data-raw/BDX_LaBr_1.csv",
                                     header = TRUE, sep = ",", dec = ".",
                                     stringsAsFactors = FALSE)
usethis::use_data(BDX_LaBr_1_data, internal = FALSE, overwrite = FALSE)

## Linear regression
# Build calibration curve
Ni_model <- IsoplotR::york(BDX_LaBr_1_data[, c(3,4,7,8)], alpha = alpha)
BDX_LaBr_1_Ni <- .DoseRateModel(
  slope = as.numeric(Ni_model$b),
  intercept = as.numeric(Ni_model$a),
  residuals = BDX_LaBr_1_data[, 7] - Ni_model$b[1] * BDX_LaBr_1_data[, 3] + Ni_model$a[1],
  df = Ni_model$df,
  MSWD = Ni_model$mswd,
  p_value = Ni_model$p.value,
  background = c(22.607509838216, 0.049712234699046),
  range = c(300, 2800)
)
NiEi_model <- IsoplotR::york(BDX_LaBr_1_data[, c(5,6,7,8)], alpha = alpha)
BDX_LaBr_1_NiEi <- .DoseRateModel(
  slope = as.numeric(NiEi_model$b),
  intercept = as.numeric(NiEi_model$a),
  residuals = BDX_LaBr_1_data[, 7] - NiEi_model$b[1] * BDX_LaBr_1_data[, 3] + Ni_model$a[1],
  df = NiEi_model$df,
  MSWD = NiEi_model$mswd,
  p_value = NiEi_model$p.value,
  background = c(25279.6317064543, 1.66234933224363),
  range = c(165, 2800)
)
BDX_LaBr_1_curve <- .CalibrationCurve(
  Ni = BDX_LaBr_1_Ni,
  NiEi = BDX_LaBr_1_NiEi,
  data = BDX_LaBr_1_data,
  details = list(
    laboratory = "IRAMAT-CRP2A (UMR 5060)",
    instrument = "InSpector 1000",
    detector = "LaBr #1",
    authors = "CRP2A Luminescence Team"
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
