# FIND PEAKS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname findPeaks
#' @aliases findPeaks,GammaSpectra-method
setMethod(
  f = "findPeaks",
  signature = signature(object = "GammaSpectra"),
  definition = function (object, span = NULL, ...){
    peaks <- lapply(X = object, FUN = findPeaks, span = span, ...)
    names(peaks) <- names(object)
    return(peaks)
  }
)

#' @export
#' @rdname findPeaks
#' @aliases findPeaks,GammaSpectrum-method
setMethod(
  f = "findPeaks",
  signature = signature(object = "GammaSpectrum"),
  definition = function (object, span = NULL, ...){
    # Get count data
    data <- methods::as(object, "data.frame")
    counts <- data$counts

    span <- if (is.null(span)) round(length(counts) * 0.05) else span
    shape <- diff(sign(diff(counts, na.pad = FALSE)))

    index <- sapply(
      X = which(shape < 0),
      FUN = function(i, data, span) {
        n <- length(data)
        z <- i - span + 1
        z <- ifelse(z > 0, z, 1)
        w <- i + span + 1
        w <- ifelse(w < n, w, n)
        if (all(data[c(z:i, (i + 2):w)] <= data[i + 1])) {
          return(i + 1)
        } else {
          return(numeric(0))
        }
      },
      data = counts,
      span = span
    )
    peaks <- data[unlist(index), ]
    rownames(peaks) <- NULL
    return(peaks)
  }
)
