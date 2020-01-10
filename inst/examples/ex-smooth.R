# Import CNF files
spc_file <- system.file("extdata/test_LaBr.CNF", package = "gamma")
spectrum <- read(spc_file)
spectrum <- slice_signal(spectrum)

# Plot spectrum
spc_clean <- remove_baseline(spectrum)
plot(spc_clean)

# Rectangular smooth
spc_unweighted <- smooth_signal(spectrum, method = "unweighted", m = 3)
spc_unweighted_clean <- remove_baseline(spc_unweighted)
plot(spc_unweighted_clean)

# Triangular smooth
spc_weighted <- smooth_signal(spectrum, method = "weighted", m = 5)
spc_weighted_clean <- remove_baseline(spc_weighted)
plot(spc_weighted_clean)

# Savitzky–Golay
spc_savitzky <- smooth_signal(spectrum, method = "savitzky", m = 5, p = 2)
spc_savitzky_clean <- remove_baseline(spc_savitzky)
plot(spc_savitzky_clean)
