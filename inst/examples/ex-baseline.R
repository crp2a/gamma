# Import a CNF file
spc_file <- system.file("extdata/test.cnf", package = "gamma")
spectrum <- read(spc_file, skip = TRUE)

# Estimate baseline
baseline <- estimateBaseline(spectrum)

plot(spectrum, baseline) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts")

# Remove baseline
spc_clean1 <- removeBaseline(spectrum)
spc_clean2 <- spectrum - baseline

plot(spc_clean1) +
  ggplot2::labs(x = "Energy [keV]", y = "Counts")
