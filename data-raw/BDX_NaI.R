# Calibration CRP2A LaBr#2 =====================================================
## Set dose rates
BDX_LaBr_2_data <- utils::read.table("./data-raw/BDX_LaBr_2.csv",
                                     header = TRUE, sep = ",", dec = ".",
                                     stringsAsFactors = FALSE)
usethis::use_data(BDX_LaBr_2_data, internal = FALSE, overwrite = FALSE)

## Linear regression
# Build calibration curve
BDX_LaBr_2_curve <- .CalibrationCurve(
  Ni_model = lm(gamma_dose ~ Ni_signal, data = BDX_LaBr_2_data),
  Ni_noise = c(22.607509838216, 0.049712234699046),
  Ni_range = c(300, 2800),
  NiEi_model = lm(gamma_dose ~ NiEi_signal, data = BDX_LaBr_2_data),
  NiEi_noise = c(25279.6317064543, 1.66234933224363),
  NiEi_range = c(165, 2800),
  data = BDX_LaBr_2_data,
  details = list(
    laboratory = "IRAMAT-CRP2A (UMR 5060)",
    instrument = "InSpector 1000",
    detector = "LaBr #2",
    authors = "CRP2A Luminescence Team"
  )
)
usethis::use_data(BDX_LaBr_2_curve, internal = FALSE, overwrite = FALSE)
