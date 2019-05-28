# SMOOTH SIGNAL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname smooth
#' @aliases smooth,GammaSpectra-method
setMethod(
  f = "smooth",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, method = c("unweighted", "weighted"),
                        m = 3, ...) {
    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    smoothed <- lapply(
      X = spectra,
      FUN = function(x, method, m, ...) smooth(x, method, m, ...),
      method = method, m = m, ...
    )
    names(smoothed) <- names(object)

    .GammaSpectra(smoothed)
  }
)

#' @export
#' @rdname smooth
#' @aliases smooth,GammaSpectrum-method
setMethod(
  f = "smooth",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("unweighted", "weighted"),
                        m = 3, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    m <- as.integer(m)[1]
    if (m %% 2 == 0)
      stop("`m` must be an odd integer.", call. = FALSE)

    # Index
    k <- (m - 1) / 2
    index_k <- seq_len(k)
    # Get data
    y <- object@counts
    index_y <- seq_along(y)
    index_m <- c(index_k, rep_len(k + 1, length(y) - 2 * k), rev(index_k)) - 1

    # Smooth
    z <- switch (
      method,
      unweighted = mapply(
        FUN = function(i, k, data) mean(data[(i - k):(i + k)]),
        i = index_y, k = index_m,
        MoreArgs = list(data = y)
      ),
      weighted = mapply(
        FUN = function(i, k, data) {
          j <- seq_len(k)
          w <- c(j, k + 1, rev(j))
          stats::weighted.mean(x = data[(i - k):(i + k)], w = w)
        },
        i = index_y, k = index_m,
        MoreArgs = list(data = y)
      ),
      stop(sprintf("There is no such method: '%s'.", method), call. = FALSE)
    )

    tmp <- object
    tmp@counts <- z
    tmp
  }
)


differenciate <- function(object,
                          method = c("central", "forward", "backward")) {
  # Validation
  method <- match.arg(method, several.ok = FALSE)
  # Get data
  x <- object@chanel
  y <- object@counts

  h <- 1
  n <- length(x)
  z <- switch (
    method,
    central = (y[(1 + 2*h):n] - y[1:(n - 2*h)]) / (x[(1 + 2*h):n] - x[1:(n - 2*h)]),
    forward = (y[(1 + h):n] - y[1:(n - h)]) / (x[(1 + h):n] - x[1:(n - h)]),
    backward = "",
    stop(sprintf("There is no such method: '%s'.", method), call. = FALSE)
  )

  tmp <- object
  tmp@chanel <- object@chanel[-c(n-1, n)]
  tmp@counts <- z
  if (length(object@energy) != 0) tmp@energy <- object@energy[-c(n-1, n)]
  if (length(object@rate) != 0) tmp@rate <- object@rate[-c(n-1, n)]
  tmp
}



