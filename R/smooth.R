# SMOOTH SIGNAL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname smooth
#' @aliases smooth,GammaSpectra-method
setMethod(
  f = "smooth",
  signature = signature(object = "GammaSpectra"),
  definition = function(object,
                        method = c("unweighted", "weighted", "savitzky"),
                        m = 3, p = 2) {
    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    smoothed <- lapply(
      X = spectra,
      FUN = function(x, method, m, p) smooth(x, method, m, p),
      method = method, m = m, p = p
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
  definition = function(object,
                        method = c("unweighted", "weighted", "savitzky"),
                        m = 3, p = 2) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    m <- as.integer(m)[1]
    if (m %% 2 == 0)
      stop("`m` must be an odd integer.", call. = FALSE)

    # Get data
    y <- object@counts

    # Smooth
    z <- switch (
      method,
      unweighted = smoothRectangular(y, m),
      weighted = smoothTriangular(y, m),
      savitzky = smoothSavitzky(y, m, p),
      stop(sprintf("There is no such method: '%s'.", method), call. = FALSE)
    )

    methods::initialize(object, counts = z)
  }
)
#' Rectangular Smooth
#'
#' Unweighted sliding-average.
#' @param x A \code{\link{numeric}} vector of observed values to be smoothed.
#' @param m An odd \code{\link{integer}} scalar giving the number of adjacent
#'  points to use.
#' @return A \code{\link{numeric}} vector of the same length as \code{x}.
#' @keywords internal
smoothRectangular <- function(x, m) {
  # Index
  k <- (m - 1) / 2
  index_k <- seq_len(k)
  index_x <- seq_along(x)
  index_m <- c(index_k, rep_len(k + 1, length(x) - 2 * k), rev(index_k)) - 1

  smoothed <- mapply(
    FUN = function(i, k, data) {
      index <- seq(from = i - k, to = i + k, by = 1)
      mean(data[index])
    },
    i = index_x,
    k = index_m,
    MoreArgs = list(data = x)
  )
  smoothed
}
#' Triangular Smooth
#'
#' Weighted sliding-average.
#' @param x A \code{\link{numeric}} vector of observed values to be smoothed.
#' @param m An odd \code{\link{integer}} scalar giving the number of adjacent
#'  points to use.
#' @return A \code{\link{numeric}} vector of the same length as \code{x}.
#' @keywords internal
smoothTriangular <- function(x, m) {
  # Index
  k <- (m - 1) / 2
  index_k <- seq_len(k)
  index_x <- seq_along(x)
  index_m <- c(index_k, rep_len(k + 1, length(x) - 2 * k), rev(index_k)) - 1

  smoothed <- mapply(
    FUN = function(i, k, data) {
      j <- seq_len(k)
      w <- c(j, k + 1, rev(j))
      index <- seq(from = i - k, to = i + k, by = 1)
      stats::weighted.mean(x = data[index], w = w)
    },
    i = index_x, k = index_m,
    MoreArgs = list(data = x)
  )
  smoothed
}
#' Savitzky-Golay Filter
#'
#' \code{smoothSavitzky} smoothes the data using the Savitzky-Golay filter.
#' \code{coefficientSavitzky} computes the Savitzky-Golay convolution
#' coefficients.
#' @param x A \code{\link{numeric}} vector of observed values to be smoothed.
#' @param m An odd \code{\link{integer}} scalar giving the number of adjacent
#'  points to use.
#' @param p An \code{\link{integer}} scalar giving the polynomial degree.
#' @return A \code{\link{numeric}} vector.
#' @keywords internal
smoothSavitzky <- function(x, m, p = 2) {
  k <- (m - 1) / 2
  i <- seq(from = -k, to = k, by = 1)
  j <- seq_along(x) %>% utils::tail(n = -k) %>% utils::head(n = -k)
  conv <- coefficientSavitzky(m, p)

  smoothed <- vapply(
    X = j,
    FUN = function(j, i, conv, data) {
      sum(conv * data[j + i])
    },
    FUN.VALUE = double(1),
    i = i,
    conv = conv,
    data = x
  )
  x[j] <- smoothed
  x
}
#' @rdname smoothSavitzky
#' @keywords internal
coefficientSavitzky <- function(m, p = 2) {
  k <- (m - 1) / 2
  z <- seq(from = -k, to = k, by = 1)
  J <- vapply(X = c(0, p), FUN = function(p, z) z^p, z, FUN.VALUE = double(m))
  (solve(t(J) %*% J) %*% t(J))[1, , drop = TRUE]
}

#
# differenciate <- function(object,
#                           method = c("central", "forward", "backward")) {
#   # Validation
#   method <- match.arg(method, several.ok = FALSE)
#   # Get data
#   x <- object@chanel
#   y <- object@counts
#
#   h <- 1
#   n <- length(x)
#   z <- switch (
#     method,
#     central = (y[(1 + 2*h):n] - y[1:(n - 2*h)]) / (x[(1 + 2*h):n] - x[1:(n - 2*h)]),
#     forward = (y[(1 + h):n] - y[1:(n - h)]) / (x[(1 + h):n] - x[1:(n - h)]),
#     backward = "",
#     stop(sprintf("There is no such method: '%s'.", method), call. = FALSE)
#   )
#
#   tmp <- object
#   tmp@chanel <- object@chanel[-c(n-1, n)]
#   tmp@counts <- z
#   if (length(object@energy) != 0) tmp@energy <- object@energy[-c(n-1, n)]
#   if (length(object@rate) != 0) tmp@rate <- object@rate[-c(n-1, n)]
#   tmp
# }
