# Import a Canberra CNF file
file <- system.file("extdata/test1.cnf", package = "gamma")
(spectrum <- read(file)[[1]])

# Coerce
as(spectrum, "data.frame")

# Subset
spectrum[["reference"]]
spectrum[["instrument"]]
