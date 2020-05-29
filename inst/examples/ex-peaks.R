## Import a Canberra CNF file
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)

## Find peaks
(pks <- peaks_find(spc))
plot(spc, pks)
