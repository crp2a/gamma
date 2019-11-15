## Import all CNF files in a given directory
spc_dir <- system.file("extdata/BDX100/calibration", package = "gamma")
(spectra <- read(spc_dir))

## Access
length(spectra)
get_names(spectra)
get_hash(spectra)
get_chanels(spectra)
get_energy(spectra)

## Coerce
\donttest{
as(spectra, "data.frame")
}

## Subset
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
