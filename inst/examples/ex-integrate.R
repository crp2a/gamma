## Import CNF files
### Spectra
spc_dir <- system.file("extdata/BDX100/calibration", package = "gamma")
spc <- read(spc_dir)
spc <- slice_signal(spc)

### Background noise
bkg_dir <- system.file("extdata/BDX100/background", package = "gamma")
bkg <- read(bkg_dir)
bkg <- slice_signal(bkg)

## First, integrate background noise spectrum (raw signal)
## (energy threshold)
(int_bkg <- integrate_signal(bkg, range = c(300, 2800), threshold = "Ni"))

## Then, integrate spectra and substract background noise value (net signal)
(int_spc <- integrate_signal(spc, range = c(300, 2800), background = c(22.61, 0.05),
                             threshold = "Ni", simplify = TRUE))
