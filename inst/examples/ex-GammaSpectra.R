# Import all CNF files in a given directory
dir <- system.file("extdata/calib/", package = "gamma")
(spectra <- read(dir))

# Access
names(spectra)
length(spectra)

# Coerce
as(spectra, "list")

# Subset
spectra[] # All spectra
spectra[NULL] # All spectra
spectra[1] # The first spectrum
spectra[-6] # Delete the sixth spectrum
spectra[1:3] # The first three spectra
spectra[c(1, 3)] # The first and third spectra
spectra["BRIQUE"] # The spectrum named 'BRIQUE'
spectra[c("BRIQUE", "C347")] # The spectra named 'BRIQUE' and 'C347'
spectra[1:3, "energy"] # The slot 'energy' of the first three spectra
spectra[[1]]
spectra[["BRIQUE"]]
