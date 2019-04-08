# READ DATA FILES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname read
#' @aliases read,character-method
setMethod(
  f = "read",
  signature = signature(file = "character"),
  definition = function(file, ...) {

    # If input is a directory and not a single file
    if (!utils::file_test("-f", file)) {
      # Look for cnf files
      cnf_files <- tools::list_files_with_exts(file, exts = "cnf")
      if (length(cnf_files) == 0)
        stop("No spectrum files were fund.")

      file <- cnf_files
    }

    spc <- lapply(X = file, FUN = readCanberraCNF, ...)
    if (length(spc) > 1) {
      spc_ref <- sapply(X = spc, FUN = "[[", i = "reference")
      names(spc) <- spc_ref
      methods::new("GammaSpectra", spc)
    } else {
      spc[[1]]
    }
  }
)

#' Read Canberra CNF file
#'
#' @param file A \code{\link{character}} string giving the path and file to be
#'  imported.
#' @param ... Extra parameters passed to \code{\link[rxylib]{read_xyData}}.
#' @return
#'  An object of class \linkS4class{GammaSpectrum}.
#' @keywords internal
#' @noRd
readCanberraCNF <- function(file, ...) {
  # Read file
  spc_xy <- rxylib::read_xyData(file = file, ..., verbose = getOption("verbose"))
  # Get and check file format
  format <- attr(spc_xy, "format_name")
  if (format != "Canberra CNF")
    stop("Only Canberra CNF files are supported.")

  # Get metadata
  spc_meta <- spc_xy$dataset[[1]]$metadata_block
  date <- as.Date(spc_meta[1, 2], format = c("%a, %Y-%m-%d %H:%M:%S"))
  live_time <- as.numeric(spc_meta[5, 2])
  real_time <- as.numeric(spc_meta[6, 2])

  # Get data
  spc_data <- spc_xy$dataset[[1]]$data_block %>%
    as.data.frame() %>%
    dplyr::mutate(chanel = dplyr::row_number()) %>%
    magrittr::set_colnames(c("energy", "counts", "chanel"))

  # Get instrument name (remove the last word)
  instrument_name <- gsub("\\s*\\w*$", "", names(spc_xy$dataset))

  methods::new(
    "GammaSpectrum",
    reference = tools::file_path_sans_ext(basename(file)),
    date = date,
    instrument = instrument_name,
    file_format = format,
    chanel = spc_data$chanel,
    energy = spc_data$energy,
    counts = spc_data$counts,
    live_time = live_time,
    real_time = real_time
  )
}
