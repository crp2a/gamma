# COERCION
#' @include AllClasses.R
NULL

# To data.frame ================================================================
setAs(
  from = "GammaSpectrum",
  to = "data.frame",
  def = function(from) {
    data.frame(
      chanel = if (length(from@chanel) != 0) from@chanel else NA_integer_,
      energy = if (length(from@energy) != 0) from@energy else NA_real_,
      count = if (length(from@count) != 0) from@count else NA_real_,
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
    df_nrow <- vapply(X = df_list, FUN = nrow, FUN.VALUE = integer(1))
    df_long <- do.call(rbind, df_list)
    df_long[["name"]] <- rep(names(df_list), times = df_nrow)
    rownames(df_long) <- NULL
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
      count = if (length(from@count) != 0) from@count else NA_real_,
      rate = if (length(from@rate) != 0) from@rate else NA_real_
    )
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
      i = "name"
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
