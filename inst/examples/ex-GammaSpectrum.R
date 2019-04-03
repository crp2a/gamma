# Import a Canberra CNF file
file <- system.file("extdata/test1.cnf", package = "gamma")
(spectrum <- read(file))

# Coerce
df <- as(spectrum, "data.frame")
head(df)

# Subset
spectrum[["reference"]]
spectrum[["instrument"]]
