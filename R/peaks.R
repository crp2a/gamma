# FIND PEAKS
#' @include AllGenerics.R
NULL

# AUTOMATIC PEAK DETECTION =====================================================
#' @export
#' @rdname peaks
#' @aliases find_peaks,GammaSpectrum-method
setMethod(
  f = "find_peaks",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("MAD"), SNR = 2, span = NULL, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    SNR <- as.integer(SNR)[[1L]]

    # Get count data
    spc <- methods::as(object, "data.frame")
    count <- spc[["count"]]
    if (is.null(span)) span <- round(length(count) * 0.05)
    span <- as.integer(span)[[1L]]

    shape <- diff(sign(diff(count, na.pad = FALSE)))
    index_shape <- lapply(
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
      data = count,
      span = span
    )

    noise <- switch (
      method,
      MAD = MAD(count, ...)
    )
    threshold <- noise * SNR
    index_noise <- unlist(index_shape)
    index_subset <- subset(index_noise, count[index_noise] >= threshold)

    pks <- spc[index_subset, ]
    rownames(pks) <- paste0("peak #", seq_len(nrow(pks)))

    .PeakPosition(
      hash = object@hash,
      noise_method = method,
      noise_threshold = threshold,
      window = span,
      chanel = pks[["chanel"]],
      energy = pks[["energy"]]
    )
  }
)
