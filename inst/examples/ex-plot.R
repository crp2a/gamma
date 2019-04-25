# Import CNF files
spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
spectra <- read(spc_dir)

# Plot all spectra
plot(spectra, yaxis = "rate", facet = FALSE) +
  ggplot2::labs(x = "Energy [keV]", y = "Count rate [1/s]") +
  ggplot2::theme_bw()

# Plot the spectrum named 'BRIQUE'
plot(spectra, xaxis = "energy", yaxis = "count",
     select = "BRIQUE", facet = TRUE) +
  ggplot2::labs(x = "Energy [keV]", y = "Count") +
  ggplot2::theme_bw()

# Plot the first three spectra
plot(spectra, xaxis = "chanel", yaxis = "rate",
     select = 1:3, facet = TRUE) +
  ggplot2::labs(x = "Chanel", y = "Count rate [1/s]") +
  ggplot2::theme_bw()
