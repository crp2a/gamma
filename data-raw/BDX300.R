# Calibration CRP2A NaI#1 ======================================================
## Set dose rates
name <- c("BRIQUE", "C341", "C347", "LMP", "PEP")
index <- clermont$name %in% name

BDX300_data <- cbind.data.frame(
  name = name,
  live_time = c(
    BRIQUE = 3679.55, C341 = 1825.18, C347 = 2208.61,
    LMP = 1803.06, PEP = 1305.49
  ),
  signal_value = c(
    BRIQUE = 34905.81005, C341 = 14539.20509, C347 = 26709.42854,
    LMP = 11070.65716, PEP = 46536.03257
  ),
  signal_error = c(
    BRIQUE = 4.35612, C341 = 3.99168, C347 = 4.91868,
    LMP = 3.50433, PEP = 8.44510
  ),
  dose_value = clermont$gamma[index],
  dose_error = clermont$gamma_error[index],
  stringsAsFactors = FALSE
)
# fit_data <- rbind.data.frame(BDX300_data,
#                              ZERO = list("ZERO", 0, 0, 0, 0, 0))
usethis::use_data(BDX300_data, internal = FALSE, overwrite = FALSE)

## Linear regression
# Build calibration curve
BDX300_curve <- .CalibrationCurve(
  model = lm(dose_value ~ signal_value, data = BDX300_data),
  noise = c(0, 0.01),
  integration = c(155, 2700),
  data = BDX300_data,
  details = list(
    laboratory = "IRAMAT-CRP2A (UMR 5060)",
    instrument = "InSpector 1000",
    detector = "NaI",
    authors = "CRP2A Luminescence Team"
  )
)
usethis::use_data(BDX300_curve, internal = FALSE, overwrite = FALSE)
