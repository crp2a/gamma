# Import a Canberra CNF file
spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
(spectrum <- read(spc_file))

## Access
get_hash(spectrum)
get_names(spectrum)
get_chanels(spectrum)
get_energy(spectrum)
get_dose(spectrum)

# Coerce
as(spectrum, "data.frame")

# Subset
spectrum[["hash"]]
spectrum[["name"]]
spectrum[["date"]]
spectrum[["instrument"]]
spectrum[["file_format"]]
spectrum[["live_time"]]
spectrum[["real_time"]]
spectrum[["calibration"]]
