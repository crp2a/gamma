# TEMPORARY WORKAROUND FOR BRICE
#' @include AllGenerics.R
NULL

#' Temporary import method for csv files
#'
#' @param file The name of the file which the data are to be read from.
#' @param ... Extra parameters passed to \code{\link[utils]{read.table}}
#' @return A \linkS4class{GammaSpectrum} object.
#' @examples
#' csv <- system.file("extdata/T14.csv", package = "gamma")
#'
#' readCSV(csv, dec = ".")
#' @export
readCSV <- function(file, ...) {
  csv <- utils::read.table(file, ...)[, 1]
  counts <- csv
  counts[1:2] <- 0

  file <- system.file("extdata/test1.cnf", package = "gamma")
  cnf <- read(file)

  methods::new(
    "GammaSpectrum",
    reference = "unknown",
    date = "unknown",
    instrument = "unknown",
    file_format = "unknown",
    chanel = seq_along(csv),
    energy = cnf@energy,
    counts = counts,
    rate = counts / csv[1],
    live_time = csv[1],
    real_time = csv[2]
  )
}
