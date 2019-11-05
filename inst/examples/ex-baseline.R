# Import a CNF file
spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
spectrum <- read(spc_file, skip = TRUE)

# Estimate baseline
baseline <- estimate_baseline(spectrum)

plot(spectrum, baseline) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts")

# Remove baseline
spc_clean1 <- remove_baseline(spectrum)
spc_clean2 <- spectrum - baseline

plot(spc_clean1) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts")
