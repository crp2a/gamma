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

# To list ======================================================================
setAs(
  from = "GammaSpectra",
  to = "list",
  def = function(from) {
    ls <- methods::S3Part(from, strictS3 = TRUE, "list")
    return(ls)
  }
)
