# Import CNF files
## Spectra
spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
spectra <- read(spc_dir)
## Background noise
bkg_dir <- system.file("extdata/crp2a/background", package = "gamma")
noise <- read(bkg_dir)

# First, integrate background noise spectrum (raw signal)
(int_noise <- integrateSignal(noise, range = c(200, 2800)))

# Then, integrate spectra and substract background noise value (net signal)
int_spc1 <- integrateSignal(spectra, range = c(200, 2800), noise = int_noise,
                            simplify = F)

# Or, do it in one go
int_spc2 <- integrateSignal(spectra, range = c(200, 2800), noise = noise,
                            simplify = TRUE)

all(int_spc1 == int_spc2)
