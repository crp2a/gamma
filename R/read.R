# READ DATA FILES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname read
#' @aliases read,character-method
setMethod(
  f = "read",
  signature = signature(file = "character"),
  definition = function(file, extensions = c("cnf", "tka"), skip = NULL, ...) {
    # Validation
    extensions <- match.arg(extensions, several.ok = TRUE)

    # If input is a directory and not a single file
    # Then, look for all files with allowed extensions
    if (!utils::file_test("-f", file)) {
      cnf_files <- tools::list_files_with_exts(file, exts = extensions)
      if (length(cnf_files) == 0)
        stop("No spectrum files were fund.")
      file <- cnf_files
    }

    # Read files
    spc <- lapply(X = file, FUN = function(x, ...) {
      extension <- tools::file_ext(x)
      switch(
        extension,
        cnf = readCanberraCNF(x, skip, ...),
        tka = readCanberraTKA(x, skip, ...)
      )
    }, ...)

    if (length(spc) > 1) {
      # Return a GammaSpectra object
      methods::new("GammaSpectra", spc)
    } else {
      # Return a GammaSpectrum
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
readCanberraCNF <- function(file, skip = NULL, ...) {
  # Read file
  spc_xy <- rxylib::read_xyData(file = file, ..., verbose = getOption("verbose"))
  # Get and check file format
  format <- attr(spc_xy, "format_name")
  if (format != "Canberra CNF")
    stop("Only Canberra CNF files are supported.")

  # Get metadata
  spc_meta <- spc_xy$dataset[[1]]$metadata_block
  date <- as.POSIXct(spc_meta[1, 2], format = c("%a, %Y-%m-%d %H:%M:%S"))
  live_time <- as.numeric(spc_meta[5, 2])
  real_time <- as.numeric(spc_meta[6, 2])

  # Get data
  spc_data <- spc_xy$dataset[[1]]$data_block %>%
    as.data.frame() %>%
    dplyr::mutate(chanel = dplyr::row_number()) %>%
    magrittr::set_colnames(c("energy", "counts", "chanel"))

  # Skip chanels
  if (!is.null(skip)) {
    spc_data <- skipChanels(spc_data, skip = skip)
  }

  # Get instrument name (remove the last word)
  instrument_name <- gsub("\\s*\\w*$", "", names(spc_xy$dataset))

  # Compute 32-bytes MD5 hash
  hash <- as.character(tools::md5sum(file))

  methods::new(
    "GammaSpectrum",
    hash = hash,
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

#' Read Canberra TKA file
#'
#' @param file A \code{\link{character}} string giving the path and file to be
#'  imported.
#' @param ... Currently not used.
#' @return
#'  An object of class \linkS4class{GammaSpectrum}.
#' @keywords internal
#' @noRd
readCanberraTKA <- function(file, skip = NULL, ...) {
  # Read file
  spc_xy <- utils::read.table(file = file)
  # Get and check file format
  format <- "Canberra TKA"

  # Get metadata
  live_time <- as.numeric(spc_xy[1, 1])
  real_time <- as.numeric(spc_xy[2, 1])
  instrument_name <- "unknown"

  # Get data
  spc_data <- data.frame(count = as.numeric(spc_xy[, 1])) %>%
    magrittr::inset(1:2, 1, c(0, 0)) %>%
    dplyr::mutate(chanel = dplyr::row_number()) %>%
    magrittr::set_colnames(c("counts", "chanel"))

  # Skip chanels
  if (!is.null(skip)) {
    spc_data <- skipChanels(spc_data, skip = skip)
  }

  # Compute 32-bytes MD5 hash
  hash <- as.character(tools::md5sum(file))

  methods::new(
    "GammaSpectrum",
    hash = hash,
    reference = tools::file_path_sans_ext(basename(file)),
    instrument = instrument_name,
    file_format = format,
    chanel = spc_data$chanel,
    counts = spc_data$count,
    live_time = live_time,
    real_time = real_time
  )
}

#' Skip chanels
#'
#' @param x A \code{\link[=data.frame]{data frame}}.
#' @param skip A \code{\link{logical}} scalar or a \code{\link{numeric}} vector.
#' @return A \code{\link[=data.frame]{data frame}}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
skipChanels <- function(x, skip) {
  if (is.logical(skip)) {
    skip_chanel <- seq_len(which.max(x$counts))
    x <- x[-skip_chanel, ]
  }
  if (is.numeric(skip)) {
    skip_chanel <- which(x$chanel %in% skip)
    x <- x[-skip_chanel, ]
  }
  return(x)
}
