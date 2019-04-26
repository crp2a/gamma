# COERCION
#' @include AllClasses.R
NULL

# To matrix ====================================================================
setAs(
  from = "GammaSpectrum",
  to = "matrix",
  def = function(from) {
    df <- methods::as(from, "data.frame")
    mtx <- as.matrix(df)
    return(mtx)
  }
)

# To data.frame ================================================================
setAs(
  from = "GammaSpectrum",
  to = "data.frame",
  def = function(from) {
    df <- data.frame(
      chanel = if (length(from@chanel) != 0) from@chanel else NA_real_,
      energy = if (length(from@energy) != 0) from@energy else NA_real_,
      counts = if (length(from@counts) != 0) from@counts else NA_real_,
      rate = if (length(from@rate) != 0) from@rate else NA_real_
    )
    return(df)
  }
)
setAs(
  from = "GammaSpectra",
  to = "data.frame",
  def = function(from) {
    df_list <- lapply(X = from, FUN = "as", Class = "data.frame")
    df_long <- dplyr::bind_rows(df_list, .id = "reference")
    return(df_long)
  }
)
setAs(
  from = "DoseRate",
  to = "data.frame",
  def = function(from) {
    df <- data.frame(
      reference = if (length(from@reference) != 0) from@reference else NA_character_,
      dose_value = if (length(from@dose_value) != 0) from@dose_value else NA_real_,
      dose_error = if (length(from@dose_error) != 0) from@dose_error else NA_real_,
      signal_value = if (length(from@signal_value) != 0) from@signal_value else NA_real_,
      signal_error = if (length(from@signal_error) != 0) from@signal_error else NA_real_
    )
    return(df)
  }
)
setAs(
  from = "PeakModel",
  to = "data.frame",
  def = function(from) {
    df <- as.data.frame(from@coefficients)
    return(df)
  }
)
setAs(
  from = "PeakPosition",
  to = "data.frame",
  def = function(from) {
    df <- as.data.frame(from@peaks)
    return(df)
  }
)

# To list ======================================================================
setAs(
  from = "GammaSpectra",
  to = "list",
  def = function(from) {
    ls <- methods::S3Part(from, strictS3 = TRUE, "list")
    return(ls)
  }
)

# To GammaSpectra ==============================================================
setAs(
  from = "GammaSpectrum",
  to = "GammaSpectra",
  def = function(from) {
    methods::new("GammaSpectra", list(from))
  }
)

# To DoseRate ==================================================================
setAs(
  from = "list",
  to = "DoseRate",
  def = function(from) {
    # Validation
    element_names <- c("reference", "dose_value", "dose_error",
                       "signal_value", "signal_error")
    dose_names <- names(from)
    valid_names <- dose_names %in% element_names
    n_valid <- sum(valid_names)
    n_doses <- length(from)
    k_doses <- lengths(from)

    if (is.null(dose_names))
      stop("All elements of the list must be named.")
    if (!isEqual(k_doses))
      stop("All elements of the list must have the same length.")

    if (n_valid == 0)
      stop(sprintf("Names of the list must one or more of: %s.",
                   paste(sQuote(element_names), collapse = ", ")))
    if (n_valid < n_doses)
      warning(sprintf("%d unknown element ignored (%s).",
                      sum(!valid_names), dose_names[!valid_names]))

    methods::new(
      "DoseRate",
      reference = if (length(from$reference) != 0) from$reference else character(0),
      dose_value = if (length(from$dose_value) != 0) from$dose_value else numeric(0),
      dose_error = if (length(from$dose_error) != 0) from$dose_error else numeric(0),
      signal_value = if (length(from$signal_value) != 0) from$signal_value else numeric(0),
      signal_error = if (length(from$signal_error) != 0) from$signal_error else numeric(0)
    )
  }
)
