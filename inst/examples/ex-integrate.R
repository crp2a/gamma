## Import CNF files
### Spectra
spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
spectra <- read(spc_dir)
spectra <- slice_signal(spectra)

### Background noise
bkg_dir <- system.file("extdata/crp2a/background", package = "gamma")
noise <- read(bkg_dir)
noise <- slice_signal(noise)

## First, integrate background noise spectrum (raw signal)
(int_noise <- integrate_signal(noise, range = c(200, 2800)))

## Then, integrate spectra and substract background noise value (net signal)
int_spc1 <- integrate_signal(spectra, range = c(200, 2800), noise = int_noise,
                             simplify = FALSE)

## Or, do it in one go
int_spc2 <- integrate_signal(spectra, range = c(200, 2800), noise = noise,
                             simplify = TRUE)

all(int_spc1 == int_spc2)
