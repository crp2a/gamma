# Import a CNF file
spc_dir <- system.file("extdata/test.cnf", package = "gamma")
spc <- read(spc_dir, skip = 1:35)

# Estimate baseline
baseline <- estimateBaseline(spc)

plot(spc, baseline) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts")

# Remove baseline
spc_clean <- removeBaseline(spc)

plot(spc_clean) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts")

# Detect peaks
peak_index <- findPeaks(spc, span = 20)

plot(peak_index) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts") +
  ggplot2::theme_bw()

# Fit peaks
peak_fit <- fitPeaks(spc, peaks = c(238, 1461, 2614.5), scale = "energy")

plot(peak_fit) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts") +
  ggplot2::theme_bw()
