# Calibration CRP2A LaBr#1 =====================================================
## Set dose rates
BDX_LaBr_1_data <- utils::read.table("./data-raw/BDX_LaBr_1.csv",
                                     header = TRUE, sep = ",", dec = ".",
                                     stringsAsFactors = FALSE)
usethis::use_data(BDX_LaBr_1_data, internal = FALSE, overwrite = FALSE)

## Linear regression
# Build calibration curve
BDX_LaBr_1_Ni <- .DoseRateModelNi(
  model = lm(gamma_dose ~ Ni_signal, data = BDX_LaBr_1_data),
  background = c(22.607509838216, 0.049712234699046),
  range = c(300, 2800)
)
BDX_LaBr_1_NiEi <- .DoseRateModelNiEi(
  model = lm(gamma_dose ~ NiEi_signal, data = BDX_LaBr_1_data),
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

# Slope: 0.033 / Intercept: 1.612
# lm(Ni_signal ~ gamma_dose, data = BDX_LaBr_1_data)
# Slope: 31.169 / Intercept: 2006.629
# lm(NiEi_signal ~ gamma_dose, data = BDX_LaBr_1_data)
