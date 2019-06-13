# Import a Canberra CNF file
spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
spectrum <- read(spc_file, skip = TRUE)

# Find peaks
(peaks <- findPeaks(spectrum))
plot(spectrum, peaks)

# Fit peaks
## Specify starting positions
(fit <- fitPeaks(spectrum, peaks = c(86, 496, 876)))
plot(fit)
