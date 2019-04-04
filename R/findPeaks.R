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
  definition = function (object, method = c("MAD"), SNR = 2, span = NULL, ...){
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    SNR <- as.integer(SNR)

    # Get count data
    data <- methods::as(object, "data.frame")
    counts <- data$counts
    span <- if (is.null(span)) round(length(counts) * 0.05) else span

    shape <- diff(sign(diff(counts, na.pad = FALSE)))
    index_shape <- sapply(
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

    noise <- switch (
      method,
      MAD = MAD(counts, ...)
    )
    index_noise <- index_shape %>%
      unlist() %>%
      subset(., counts[.] >= noise * SNR)

    peaks <- data[index_noise, ]
    rownames(peaks) <- NULL
    return(peaks)
  }
)

#' Median absolute deviation
#'
#' @param x A \code{\link{numeric}} vector.
#' @param k A \code{\link{numeric}} value.
#' @param na.rm A \code{\link{logical}} scalar.
#' @return A \code{\link{numeric}} value.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
MAD <- function(x, k = 1.4826, na.rm = FALSE) {
  k * stats::median(abs(x - stats::median(x, na.rm = na.rm)), na.rm = na.rm)
}
