# Import a Canberra CNF file
cnf_file <- system.file("extdata/test.cnf", package = "gamma")
(cnf_spectrum <- read(cnf_file))

# Import a TKA file
tka_file <- system.file("extdata/test.tka", package = "gamma")
(tka_spectrum <- read(tka_file))

# Import all files in a given directory
# Skip the 35 first chanels
spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
(spc_spectra <- read(spc_dir, skip = 1:35))
