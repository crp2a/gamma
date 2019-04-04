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
    df <- data.frame(
      chanel = from@chanel,
      energy = from@energy,
      counts = from@counts,
      rate = from@rate
    )
    return(df)
  }
)
setAs(
  from = "CalibrationCurve",
  to = "data.frame",
  def = function(from) {
    df <- as.data.frame(from@data)
    return(df)
  }
)
setAs(
  from = "DoseRate",
  to = "data.frame",
  def = function(from) {
    df <- data.frame(
      reference = from@reference,
      dose = from@dose_value,
      dose_error = from@dose_error,
      signal = from@signal_value,
      signal_error = from@signal_error
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
