# Import a Canberra CNF file
file <- system.file("extdata/gamma.cnf", package = "gamma")
(spectrum <- read(file))

# Import all CNF files in a given directory
dir <- system.file("extdata/cerege/", package = "gamma")
(spectra <- read(dir))
