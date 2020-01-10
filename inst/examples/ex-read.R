## Import a Canberra CNF file
cnf_file <- system.file("extdata/test_LaBr.CNF", package = "gamma")
(cnf_spectrum <- read(cnf_file))

## Import a TKA file
tka_file <- system.file("extdata/test_LaBr.TKA", package = "gamma")
(tka_spectrum <- read(tka_file))

## Import all files in a given directory
## Skip the 35 first chanels
spc_dir <- system.file("extdata/", package = "gamma")
(spectra <- read(spc_dir))
