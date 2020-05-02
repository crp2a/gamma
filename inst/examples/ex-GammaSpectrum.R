# Import a Canberra CNF file
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
(spc <- read(spc_file))

## Access
get_hash(spc)
get_names(spc)
get_time(spc, "live")
get_time(spc, "real")
get_nchanels(spc)
range_energy(spc)

spc[["date"]]
spc[["instrument"]]
spc[["file_format"]]

# Coerce
spc <- as(spc, "data.frame")
head(spc)
