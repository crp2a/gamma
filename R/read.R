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
    extensions <- match.arg(extensions, several.ok = TRUE) %>% c(., toupper(.))

    # If input is a directory and not a single file
    # Then, look for all files with allowed extensions
    if(!utils::file_test("-f", file)) {
      file_list <- tools::list_files_with_exts(file, exts = extensions)
      if(length(file_list) == 0)
        stop("No spectrum files were fund.", call. = FALSE)
      file <- as.list(file_list)
    }

    if(!is.null(skip)) {
      all_numeric <- all(
        vapply(X = skip, FUN = function(x) is.numeric(x) || is.null(x),
               FUN.VALUE = logical(1))
      )
      all_logical <- all(
        vapply(X = skip, FUN = function(x) is.logical(x) || is.null(x),
               FUN.VALUE = logical(1))
      )
      if(!all_numeric & !all_logical) {
        stop("`skip` must be a list of numeric vectors or logical scalars.",
             call. = FALSE)
      }
    } else {
      skip <- FALSE
    }
    if(is.numeric(skip)) skip <- list(skip)
    if(is.logical(skip)) skip <- as.list(skip)
    n_files <- length(file)
    n_skip <- length(skip)
    if (n_skip != 1 & n_skip != n_files) {
      stop(sprintf("`skip` must be of length 1 or %d, not %d.",
                   n_files, n_skip), call. = FALSE)
    }

    # Read files
    spc <- mapply(FUN = function(x, y, ...) {
      extension <- tolower(tools::file_ext(x))
      switch(
        extension,
        cnf = readCanberraCNF(file = x, skip = y, ...),
        tka = readCanberraTKA(file = x, skip = y, ...)
      )
    }, x = file, y = skip, MoreArgs = list(...))

    if(length(spc) > 1) {
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
  spc_xy <- rxylib::read_xyData(file = file, ..., verbose = FALSE)
  # Get and check file format
  file_format <- attr(spc_xy, "format_name")
  if(file_format != "Canberra CNF")
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
  if(!is.null(skip)) {
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
    file_format = "CNF",
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

  # Get metadata
  live_time <- as.numeric(spc_xy[1, 1])
  real_time <- as.numeric(spc_xy[2, 1])
  instrument_name <- "unknown"

  # Get data
  spc_data <- data.frame(count = as.numeric(spc_xy[, 1])) %>%
    magrittr::inset(c(1, 2), 1, c(0, 0)) %>%
    dplyr::mutate(chanel = dplyr::row_number()) %>%
    magrittr::set_colnames(c("counts", "chanel"))

  # Skip chanels
  if(!is.null(skip)) {
    spc_data <- skipChanels(spc_data, skip = skip)
  }

  # Compute 32-bytes MD5 hash
  hash <- as.character(tools::md5sum(file))

  methods::new(
    "GammaSpectrum",
    hash = hash,
    reference = tools::file_path_sans_ext(basename(file)),
    instrument = instrument_name,
    file_format = "TKA",
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
  # Validation
  if (!is.data.frame(x))
    stop("A data.frame is expected.")

  skip_chanel <- x$chanel
  if (is.logical(skip)) {
    if (skip) {
      skip_chanel <- -seq_len(which.max(x$counts))
    }
  }
  if (is.numeric(skip)) {
    skip_chanel <- -which(x$chanel %in% as.integer(skip))
  }
  if (length(skip_chanel) == 0) {
    skip_chanel <- x$chanel
  }
  if (getOption("verbose")) {
    n <- if (all(skip_chanel %in% x$chanel)) 0 else length(skip_chanel)
    message(sprintf(ngettext(n, "%d chanel skiped.", "%d chanels skiped."), n))
  }
  x[skip_chanel, , drop = FALSE]
}
