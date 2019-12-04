# Calibration CRP2A LaBr#2 =====================================================
## Set dose rates
name <- c("BRIQUE", "C341", "C347", "GOU", "LMP", "MAZ", "PEP")
index <- clermont$name %in% name

fit_data <- cbind.data.frame(
  name = name,
  live_time = c(BRIQUE = 8473.45, C341 = 976.08, C347 = 703.92, GOU = 1008.58,
                LMP = 908.20, MAZ = 965.86, PEP = 507.07),
  signal_value = c(
    BRIQUE = 64071.58877, C341 = 27974.88571, C347 = 46580.21115,
    GOU = 52645.11600, LMP = 21227.36116, MAZ = 38008.81789, PEP = 80121.51546
  ),
  signal_error = c(
    BRIQUE = 4.59247, C341 = 10.44644, C347 = 14.29817,
    GOU = 12.43433, LMP = 10.12118, MAZ = 11.45285, PEP = 20.39077),
  dose_value = clermont$gamma[index],
  dose_error = clermont$gamma_error[index],
  stringsAsFactors = FALSE
)
# fit_data <- rbind.data.frame(fit_data, ZERO = list("ZERO", 0, 0, 0, 0, 0))

## Linear regression
# Build calibration curve
BDX200 <- .CalibrationCurve(
  model = lm(dose_value ~ signal_value, data = fit_data),
  noise = c(25279.63171, 1.66235),
  integration = c(165, 2800),
  data = fit_data,
  details = list(
    laboratory = "IRAMAT-CRP2A (UMR 5060)",
    instrument = "InSpector 1000",
    detector = "LaBr #2",
    authors = "CRP2A Luminescence Team"
  )
)
usethis::use_data(BDX200, internal = FALSE, overwrite = FALSE)
