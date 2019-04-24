# Import a CNF file
spc_dir <- system.file("extdata/crp2a/", package = "gamma")
spectra <- read(spc_dir)

# Integrate signal (raw)
(int <- integrateSignal(spectra, simplify = TRUE))

# Integrate signal (net)
(int <- integrateSignal(spectra, noise = c(1190, 1), simplify = TRUE))
