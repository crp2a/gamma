# Import a CNF file for calibration
tka_file <- system.file("extdata/test_TKA.tka", package = "gamma")
(tka_spc <- read(tka_file, skip = TRUE))

## 1. Use observed peak positions
### Set peak positions (chanel) and expected energy values
calib_lines <- list(
  Pb = c(chanel = 86, energy = 238),
  K = c(chanel = 496, energy = 1461),
  Cs = c(chanel = 876, energy = 2614.5)
)
### Correct the energy scale
(spc1 <- calibrate(tka_spc, lines = calib_lines))
### Inspect results
plot(spc1, xaxis = "energy", yaxis = "rate") +
  ggplot2::geom_vline(xintercept = c(238, 1461, 2614.5), linetype = 3) +
  ggplot2::labs(x = "Energy [keV]", y = "Count rate [1/s]")

## 2. Use estimated peak parameters
peaks <- fitPeaks(tka_spc, peaks = c(84, 496, 876))
### Inspect results
plot(peaks)
### Correct the energy scale
(spc2 <- calibrate(peaks, lines = c(238, 1461, 2614.5)))
### Inspect results
plot(spc2, xaxis = "energy", yaxis = "rate") +
  ggplot2::geom_vline(xintercept = c(238, 1461, 2614.5), linetype = 3) +
  ggplot2::labs(x = "Energy [keV]", y = "Count rate [1/s]")
