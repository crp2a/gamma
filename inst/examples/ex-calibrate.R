## Import a CNF file for calibration
spc_file <- system.file("extdata/test_TKA.tka", package = "gamma")
(spectrum <- read(spc_file))

## Set peak positions (chanel) and expected energy values
calib_lines <- list(
  chanel = c(86, 495, 879),
  energy = c(238, 1461, 2615)
)

## Correct the energy scale
(spc1 <- calibrate_energy(spectrum, lines = calib_lines))

## Inspect results
plot(spc1, xaxis = "energy", yaxis = "count") +
  ggplot2::geom_vline(xintercept = c(238, 1461, 2614.5), linetype = 3)
