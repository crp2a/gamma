# Import CNF files
dir <- system.file("extdata/calib/", package = "gamma")
spectra <- read(dir)

# Plot the spectrum named 'BRIQUE'
plot(spectra, select = "BRIQUE", facet = TRUE) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts") +
  ggplot2::theme_bw()

# Plot the first three spectra
plot(spectra, select = 1:3, facet = TRUE) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts") +
  ggplot2::theme_bw()
