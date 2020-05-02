## Import a CNF file
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)

## Remove the first 35 chanels
spc <- slice_signal(spc, -c(1:35))

## Estimate baseline
baseline <- estimate_baseline(spc)

plot(spc, baseline)

## Remove baseline
spc_clean1 <- remove_baseline(spc)
spc_clean2 <- spc - baseline

plot(spc_clean1)
