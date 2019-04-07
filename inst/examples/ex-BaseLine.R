# Import a CNF file
spc_dir <- system.file("extdata/test1.cnf", package = "gamma")
spc <- read(spc_dir)

# Estimate baseline
baseline <- estimateBaseline(spc)

# Plot spectrum + baseline
plot(spc, baseline) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts")

# Remove baseline
spc_clean <- removeBaseline(spc)

# Detect peaks
peak_index <- findPeaks(spc_clean, span = 20)

plot(peak_index) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts") +
  ggplot2::theme_bw()
