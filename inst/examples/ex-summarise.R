## Import a Canberra CNF file
cnf_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
(spectrum <- read(cnf_file))

summarise(spectrum)

## Import all CNF files in a given directory
spc_dir <- system.file("extdata/BDX100/calibration", package = "gamma")
(spectra <- read(spc_dir))

summarise(spectra)
