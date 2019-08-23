# Import CNF files
spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
spectrum <- read(spc_file, skip = TRUE)

# Plot spectrum
spc_clean <- removeBaseline(spectrum)
plot(spc_clean)

# Rectangular smooth
spc_unweighted <- smooth(spectrum, method = "unweighted", m = 3)
spc_unweighted_clean <- removeBaseline(spc_unweighted)
plot(spc_unweighted_clean)

# Triangular smooth
spc_weighted <- smooth(spectrum, method = "weighted", m = 5)
spc_weighted_clean <- removeBaseline(spc_weighted)
plot(spc_weighted_clean)

# Savitzky–Golay
spc_savitzky <- smooth(spectrum, method = "savitzky", m = 5, p = 2)
spc_savitzky_clean <- removeBaseline(spc_savitzky)
plot(spc_savitzky_clean)
