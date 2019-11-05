# COERCION
#' @include AllClasses.R
NULL

# To matrix ====================================================================
setAs(
  from = "GammaSpectrum",
  to = "matrix",
  def = function(from) {
    as.matrix(methods::as(from, "data.frame"))
  }
)

# To data.frame ================================================================
setAs(
  from = "GammaSpectrum",
  to = "data.frame",
  def = function(from) {
    data.frame(
      chanel = if (length(from@chanel) != 0) from@chanel else NA_integer_,
      energy = if (length(from@energy) != 0) from@energy else NA_real_,
      counts = if (length(from@counts) != 0) from@counts else NA_real_,
      rate = if (length(from@rate) != 0) from@rate else NA_real_,
      stringsAsFactors = FALSE
    )
  }
)
setAs(
  from = "GammaSpectra",
  to = "data.frame",
  def = function(from) {
    df_list <- lapply(X = from, FUN = "as", Class = "data.frame")
    df_nrow <- vapply(X = df_list, FUN = nrow, FUN.VALUE = 1L)
    df_long <- do.call(rbind, df_list)
    df_long[["reference"]] <- rep(names(df_list), times = df_nrow)
    return(df_long)
  }
)
setAs(
  from = "PeakPosition",
  to = "data.frame",
  def = function(from) {
    data.frame(
      chanel = if (length(from@chanel) != 0) from@chanel else NA_integer_,
      energy = if (length(from@energy) != 0) from@energy else NA_real_,
      stringsAsFactors = FALSE
    )
  }
)

# To list ======================================================================
setAs(
  from = "GammaSpectrum",
  to = "list",
  def = function(from) {
    list(
      chanel = if (length(from@chanel) != 0) from@chanel else NA_real_,
      energy = if (length(from@energy) != 0) from@energy else NA_real_,
      counts = if (length(from@counts) != 0) from@counts else NA_real_,
      rate = if (length(from@rate) != 0) from@rate else NA_real_
    )
  }
)
setAs(
  from = "GammaSpectra",
  to = "list",
  def = function(from) {
    methods::S3Part(from, strictS3 = TRUE)
  }
)

# To GammaSpectra ==============================================================
setAs(
  from = "list",
  to = "GammaSpectra",
  def = function(from) {
    spc_ref <- make.unique(vapply(
      X = from,
      FUN = "[[",
      FUN.VALUE = character(1),
      i = "reference"
    ))
    names(from) <- spc_ref
    .GammaSpectra(from)
  }
)
setAs(
  from = "GammaSpectrum",
  to = "GammaSpectra",
  def = function(from) {
    .GammaSpectra(list(from))
  }
)
