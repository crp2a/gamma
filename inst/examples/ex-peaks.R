# Import a Canberra CNF file
spc_file <- system.file("extdata/test.cnf", package = "gamma")
spectrum <- read(spc_file, skip = TRUE)

# Find peaks
(peaks <- findPeaks(spectrum))
plot(peaks)

# Fit peaks
## Using previously detected peaks
(fit1 <- fitPeaks(peaks))
plot(fit1)
## Specifying starting positions
(fit2 <- fitPeaks(spectrum, peaks = c(86, 496, 876), scale = "chanel"))
plot(fit2)
