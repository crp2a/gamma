# COERCION
#' @include AllClasses.R
NULL

# To matrix ====================================================================
setAs(
  from = "GammaSpectrum",
  to = "matrix",
  def = function(from) {
    mtx <- cbind(from@chanel, from@energy, from@counts)
    colnames(mtx) <- c("chanel", "energy", "counts")
    return(mtx)
  }
)
setAs(
  from = "DoseRate",
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
    mtx <- methods::as(from, "matrix")
    df <- as.data.frame(mtx)
    return(df)
  }
)
setAs(
  from = "DoseRate",
  to = "data.frame",
  def = function(from) {
    df <- data.frame(
      reference = from@reference,
      dose = from@dose_value, dose_error = from@dose_error,
      signal = from@signal_value, signal_error = from@signal_error
    )
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
