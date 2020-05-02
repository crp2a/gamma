# Import CNF files
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)
spc <- slice_signal(spc, -c(1:35))

# Plot raw spectrum
spc_clean <- remove_baseline(spc)
plot(spc_clean)

# Rectangular smooth
spc_unweighted <- smooth_signal(spc, method = "unweighted", m = 3)
spc_unweighted_clean <- remove_baseline(spc_unweighted)
plot(spc_unweighted_clean)

# Triangular smooth
spc_weighted <- smooth_signal(spc, method = "weighted", m = 5)
spc_weighted_clean <- remove_baseline(spc_weighted)
plot(spc_weighted_clean)

# Savitzky–Golay
spc_savitzky <- smooth_signal(spc, method = "savitzky", m = 21, p = 2)
spc_savitzky_clean <- remove_baseline(spc_savitzky)
plot(spc_savitzky_clean)
