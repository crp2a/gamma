# Import a CNF file
spc_dir <- system.file("extdata/test1.cnf", package = "gamma")
spc_dir <- system.file("extdata/cerege", package = "gamma")
spc <- read(spc_dir)[[5]]

# Estimate baseline
baseline <- estimateBaseline(spc)

# Plot spectrum + baseline
plot(spc, baseline) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts")

# Remove baseline
spc_clean <- removeBaseline(spc)

# Detect peaks
peak_index <- findPeaks(spc_clean, span = NULL)

plot(spc_clean, select = 1:3, facet = TRUE) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts") +
  ggplot2::geom_vline(xintercept = peak_index$energy, colour = "red") +
  ggplot2::theme_bw()
