# Import a Canberra CNF file
spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
spectrum <- read(spc_file)

# Find peaks
(peaks <- find_peaks(spectrum))
plot(spectrum, peaks)
