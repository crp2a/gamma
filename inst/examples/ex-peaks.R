## Import a Canberra CNF file
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)

## Find peaks
(pks <- find_peaks(spc))
plot(spc, pks)
