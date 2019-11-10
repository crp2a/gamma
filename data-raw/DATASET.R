library(magrittr)

# Milieux Clermont =============================================================
# Conversion factors from Guérin et al. 2011
clermont <- utils::read.table("./data-raw/clermont.csv",
                              header = TRUE, sep = ",", dec = ".") %>%
  dplyr::mutate(
    K = dplyr::case_when(
      is.na(.data$K) ~ .data$K2O * 0.8301,
      TRUE ~ .data$K
    ),
    K_error = dplyr::case_when(
      is.na(.data$K_error) ~ .data$K2O_error * 0.8301,
      TRUE ~ .data$K_error
    )
  ) %>%
  dplyr::mutate(
    gamma = .data$U * 111.6 + .data$Th * 47.96 + .data$K * 249.1,
    gamma_error = sqrt((.data$U_error * 111.6)^2 + (.data$Th_error * 47.96)^2 + (.data$K_error * 249.1)^2)
  )
rownames(clermont) <- clermont[["name"]]
usethis::use_data(clermont, internal = FALSE, overwrite = FALSE)

# Calibration CRP2A1 ===========================================================
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
  dose_error = clermont$gamma_error[index]
)

## Linear regression
# Build calibration curve
BDX100 <- .CalibrationCurve(
  model = lm(dose_value ~ signal_value, data = fit_data),
  noise = c(25279.63171, 1.66235),
  integration = c(165, 2800),
  data = fit_data,
  details = list(
    laboratory = "IRAMAT-CRP2A (UMR 5060)",
    instrument = "InSpector 1000",
    detector = "LaBr",
    authors = "CRP2A Luminescence Team"
  )
)
usethis::use_data(BDX100, internal = FALSE, overwrite = FALSE)

# Decay data ===================================================================
.decay <- utils::read.table("./data-raw/decay.csv",
                            header = TRUE, sep = ";", dec = ",") %>%
  dplyr::filter(.data$gamma_intensity >= 1) %>%
  dplyr::mutate(
    decay_chain = factor(.data$decay_chain, levels = unique(.data$decay_chain)),
    occurrence = .data$occurrence / 100,
    post_radon = as.logical(.data$post_radon),
    counts_chain = .data$gamma_intensity * .data$occurrence,
    counts_chain_error = .data$gamma_intensity_error * .data$occurrence +
      .data$gamma_intensity * .data$occurrence_error
  )

.decay_La <- utils::read.table("./data-raw/decay_La.csv",
                               header = TRUE, sep = ";", dec = ",") %>%
  dplyr::mutate(
    occurrence = .data$occurrence / 100,
    post_radon = as.logical(.data$post_radon)
  )

usethis::use_data(.decay, .decay_La, internal = TRUE, overwrite = FALSE)
