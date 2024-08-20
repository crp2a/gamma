# READ DATA FILES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname read
#' @aliases read,character-method
setMethod(
  f = "read",
  signature = signature(file = "character"),
  definition = function(file, extensions = c("cnf", "tka", "spe"), ...) {
    # Validation
    extensions <- match.arg(extensions, several.ok = TRUE)
    extensions <- c(extensions, toupper(extensions))

    # If input is a directory and not a single file
    # Then, look for all files with allowed extensions
    if (!all(utils::file_test("-f", file))) {
      file_list <- tools::list_files_with_exts(file, exts = extensions)
      if (length(file_list) == 0)
        stop("No spectrum files were fund.", call. = FALSE)
      file <- as.list(file_list)
    }

    # Read files
    spc <- lapply(X = file, FUN = function(x, y, ...) {
      extension <- tolower(tools::file_ext(x))
      switch(
        extension,
        cnf = readCanberraCNF(file = x, ...),
        tka = readCanberraTKA(file = x, ...),
        spe = readKromekSPE(file = x, ...)
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
#' @param file A [`character`] string giving the path and file to be imported.
#' @param ... Extra parameters passed to [rxylib::read_xyData()].
#' @return
#'  An object of class [GammaSpectrum-class].
#' @keywords internal
#' @noRd
readCanberraCNF <- function(file, ...) {
  # Read file
  spc_xy <- rxylib::read_xyData(file = file, ..., verbose = FALSE)
  # Get and check file format
  file_format <- attr(spc_xy, "format_name")

  # Get metadata
  spc_meta <- spc_xy$dataset[[1]]$metadata_block
  i_datetime <- grep("date and time", spc_meta$key, value = FALSE)
  i_livetime <- grep("live time", spc_meta$key, value = FALSE)
  i_realtime <- grep("real time", spc_meta$key, value = FALSE)
  i_description <- grep("description", spc_meta$key, value = FALSE)

  date <- as.POSIXct(spc_meta$value[[i_datetime]], format = c("%a, %Y-%m-%d %H:%M:%S"))
  live_time <- as.numeric(spc_meta$value[[i_livetime]])
  real_time <- as.numeric(spc_meta$value[[i_realtime]])

  # Get data
  spc_data <- as.data.frame(spc_xy$dataset[[1]]$data_block)
  # Add a column to store the channel number
  spc_data[["channel"]] <- as.integer(seq_len(nrow(spc_data)))
  colnames(spc_data) <- c("energy", "count", "channel")

  # Get instrument name (remove the last word)
  instrument_name <- gsub("\\s*\\w*$", "", names(spc_xy$dataset))

  # Compute 32-bytes MD5 hash
  hash <- as.character(tools::md5sum(file))

  .GammaSpectrum(
    hash = hash,
    name = tools::file_path_sans_ext(basename(file)),
    date = date,
    instrument = instrument_name,
    file_format = "CNF",
    channel = as.integer(spc_data$channel),
    energy = spc_data$energy,
    count = as.integer(spc_data$count),
    live_time = live_time,
    real_time = real_time
  )
}

#' Read Canberra TKA file
#'
#' @param file A [`character`] string giving the path and file to be imported.
#' @param ... Currently not used.
#' @return
#'  An object of class [GammaSpectrum-class].
#' @keywords internal
#' @noRd
readCanberraTKA <- function(file, ...) {
  # Read file
  spc_xy <- utils::read.table(file = file)

  # Get metadata
  live_time <- as.numeric(spc_xy[1, 1])
  real_time <- as.numeric(spc_xy[2, 1])
  instrument_name <- "unknown"

  # Get data
  spc_data <- data.frame(count = as.numeric(spc_xy[, 1]))
  # Remove the first two value (live time and real time)
  spc_data[c(1, 2), 1] <- c(0, 0)
  # Add a column to store the channel number
  spc_data[["channel"]] <- as.integer(seq_len(nrow(spc_data)))
  colnames(spc_data) <- c("counts", "channel")

  # Compute 32-bytes MD5 hash
  hash <- as.character(tools::md5sum(file))

  .GammaSpectrum(
    hash = hash,
    name = tools::file_path_sans_ext(basename(file)),
    instrument = instrument_name,
    file_format = "TKA",
    channel = as.integer(spc_data$channel),
    count = as.integer(spc_data$count),
    live_time = live_time,
    real_time = real_time
  )
}

#' Read Kromek SPE file
#'
#' @param file A [`character`] string giving the path and file to be imported.
#' @param ... currently not used
#' @return
#'  An object of class [GammaSpectrum-class].
#' @keywords internal
#' @noRd
readKromekSPE <- function(file, ...) {
  ## import entire file
  tmp <- readLines(con = file)

  ## search for KROMEK_INFO
  if (length(grep(pattern = "$KROMEK_INFO", x = tmp, fixed = TRUE)) == 0)
    stop("Kromek SPE does not follow implemented definition!", call. = FALSE)

  ## get elements and their position
  el_id <- which(grepl(pattern = "\\$[a-zA-Z].+[^:]", x = tmp))

  ## now read all information into a list
  el_l <- lapply(seq_along(el_id), function(x) {
    ## if limit is reached or the element length is 0 nothing was stored
    if (length(tmp) == x && (el_id[x] + 1) == (el_id[x + 1] - 1))
      return(NA)

    tmp[(el_id[x] + 1):(min(length(tmp), el_id[x + 1] - 1, na.rm = TRUE))]

  })

  ## assign element names but remove the dollar and the :
  names(el_l) <- gsub(pattern = "[$:]", replacement = "", x = tmp[el_id])

  ## extract data
  ## $DATA
  m <- matrix(c(1:length(el_l[["DATA"]][-1]), as.numeric(el_l[["DATA"]][-1])), ncol = 2)
  colnames(m) <- c("CHN", "CNTS")
  el_l[["DATA"]] <- m

  ## $KROMEK_INFO
  l <- el_l[["KROMEK_INFO"]][seq(2, length(el_l[["KROMEK_INFO"]]), 2)]
  names(l) <- gsub(pattern = ":", replacement = "", x = el_l[["KROMEK_INFO"]][seq(1, length(el_l[["KROMEK_INFO"]]), 2)], fixed = TRUE)
  el_l[["KROMEK_INFO"]] <- as.list(l)

  ## get metadata (here we follow the template from before)
  date <- as.POSIXct(el_l[["DATE_MEA"]], format = c("%m/%d/%Y %H:%M:%S"))
  tmp_time <- as.numeric(strsplit(el_l[["MEAS_TIM"]], " ")[[1]])
    live_time <- tmp_time[1]
    real_time <- tmp_time[2]

  ## get data
  spc_data <- as.data.frame(el_l[["DATA"]])

  ## add a column to store the channel number
  spc_data[["channel"]] <- as.integer(seq_len(nrow(spc_data)))
  colnames(spc_data) <- c("energy", "count", "channel")

  # Get instrument name (remove the last word)
  instrument_name <-  paste("Kromek", el_l[["KROMEK_INFO"]][["DETECTOR_TYPE"]])

  # Compute 32-bytes MD5 hash
  hash <- as.character(tools::md5sum(file))

  .GammaSpectrum(
    hash = hash,
    name = tools::file_path_sans_ext(basename(file)),
    date = date,
    instrument = instrument_name,
    file_format = "SPE",
    channel = as.integer(spc_data$channel),
    energy = spc_data$energy,
    count = as.integer(spc_data$count),
    live_time = live_time,
    real_time = real_time
  )
}


