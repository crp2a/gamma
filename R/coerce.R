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
      rate = if (length(from@rate) != 0) from@rate else NA_real_,
      stringsAsFactors = FALSE
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
  from = "PeakModel",
  to = "data.frame",
  def = function(from) {
    df <- as.data.frame(from@coefficients,
                        stringsAsFactors = FALSE)
    return(df)
  }
)
setAs(
  from = "PeakPosition",
  to = "data.frame",
  def = function(from) {
    df <- as.data.frame(from@peaks,
                        stringsAsFactors = FALSE)
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
