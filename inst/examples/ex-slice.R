## Import CNF files
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)

## Plot spectrum
plot(spc)

## Slice
sliced <- slice_signal(spc)
plot(sliced)

sliced <- slice_signal(spc, -c(1:35))
plot(sliced)

sliced <- slice_signal(spc, 450:550)
plot(sliced)
