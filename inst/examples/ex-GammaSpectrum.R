# Import a Canberra CNF file
spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
(spectrum <- read(spc_file))

## Access
get_hash(spectrum)
get_chanels(spectrum)
get_energy(spectrum)
get_dose(spectrum)

# Coerce
df <- as(spectrum, "data.frame")
head(df)
tail(df)

# Subset
spectrum[["hash"]]
spectrum[["reference"]]
spectrum[["date"]]
spectrum[["instrument"]]
spectrum[["file_format"]]
spectrum[["live_time"]]
spectrum[["real_time"]]
spectrum[["calibration"]]
