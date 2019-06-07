library(magrittr)

# Import CNF files
spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
spectrum <- read(spc_file, skip = TRUE)

# Plot spectrum
spectrum %>%
  removeBaseline() %>%
  plot()

# Rectangular smooth
spectrum %>%
  smooth(method = "unweighted", m = 3) %>%
  removeBaseline() %>%
  plot()

# Triangular smooth
spectrum %>%
  smooth(method = "weighted", m = 5) %>%
  removeBaseline() %>%
  plot()

# Savitzky–Golay
spectrum %>%
  smooth(method = "savitzky", m = 5, p = 2) %>%
  removeBaseline() %>%
  plot()
