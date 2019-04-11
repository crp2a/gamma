# Import a Canberra CNF file
spc_file <- system.file("extdata/test.cnf", package = "gamma")
(spectrum <- read(spc_file))

# Coerce
df <- as(spectrum, "data.frame")
head(df)

# Subset
spectrum[["reference"]]
spectrum[["instrument"]]
