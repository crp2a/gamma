# Import a Canberra CNF file
cnf_file <- system.file("extdata/test.cnf", package = "gamma")
(cnf_spectrum <- read(cnf_file))

# Import all CNF files in a given directory
cnf_dir <- system.file("extdata/cerege/", package = "gamma")
(cnf_spectra <- read(cnf_dir))

# Import a TKA file
tka_file <- system.file("extdata/test.tka", package = "gamma")
(tka_spectrum <- read(tka_file))
