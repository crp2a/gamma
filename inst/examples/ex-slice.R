## Import CNF files
spc_file <- system.file("extdata/test_LaBr.CNF", package = "gamma")
spectrum <- read(spc_file)

## Plot spectrum
plot(spectrum)

## Slice
sliced <- slice_signal(spectrum, 450:550)
plot(sliced)

sliced <- slice_signal(spectrum)
plot(sliced)
