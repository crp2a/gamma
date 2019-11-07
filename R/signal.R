# SIGNAL PROCESSING
#' @include AllGenerics.R
NULL

# ======================================================================== Slice
#' @export
#' @rdname slice
#' @aliases slice_signal,GammaSpectrum-method
setMethod(
  f = "slice_signal",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, ...) {
    index <- as.integer(c(...))
    if (length(index) == 0) {
      index <- -seq_len(which.max(object[["count"]]))
    }

    if (all(index > 0) || all(index < 0)) {
      chanel <- object[["chanel"]][index]
      energy <- object[["energy"]][index]
      count <- object[["count"]][index]
      rate <- object[["rate"]][index]
    } else {
      stop("A vector of strictly positive of negative integers is expected.",
           call. = FALSE)
    }

    methods::initialize(object, chanel = chanel, energy = energy,
                        count = count, rate = rate)
  }
)

#' @export
#' @rdname slice
#' @aliases slice_signal,GammaSpectra-method
setMethod(
  f = "slice_signal",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, ...) {
    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    sliced <- lapply(
      X = spectra,
      FUN = slice_signal,
      ...
    )
    names(sliced) <- names(object)

    .GammaSpectra(sliced)
  }
)

# ==================================================================== Stabilize
#' @export
#' @rdname stabilize
#' @aliases stabilize_signal,GammaSpectrum-method
setMethod(
  f = "stabilize_signal",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, transformation, ...) {
    count <- transformation(object[["count"]], ...)
    methods::initialize(object, count = count)
  }
)

#' @export
#' @rdname stabilize
#' @aliases stabilize_signal,GammaSpectra-method
setMethod(
  f = "stabilize_signal",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, transformation, ...) {
    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    stabilized <- lapply(
      X = spectra,
      FUN = stabilize_signal,
      transformation = transformation
    )
    names(stabilized) <- names(object)

    .GammaSpectra(stabilized)
  }
)

# ======================================================================= Smooth
#' @export
#' @rdname smooth
#' @aliases smooth_signal,GammaSpectrum-method
setMethod(
  f = "smooth_signal",
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
    y <- object[["count"]]

    # Smooth
    z <- switch (
      method,
      unweighted = smooth_rectangular(y, m),
      weighted = smooth_triangular(y, m),
      savitzky = smooth_savitzky(y, m, p),
      stop(sprintf("There is no such method: '%s'.", method), call. = FALSE)
    )

    methods::initialize(object, count = z)
  }
)

#' @export
#' @rdname smooth
#' @aliases smooth_signal,GammaSpectra-method
setMethod(
  f = "smooth_signal",
  signature = signature(object = "GammaSpectra"),
  definition = function(object,
                        method = c("unweighted", "weighted", "savitzky"),
                        m = 3, p = 2) {
    spectra <- methods::S3Part(object, strictS3 = TRUE, "list")
    smoothed <- lapply(
      X = spectra,
      FUN = smooth_signal,
      method = method, m = m, p = p
    )
    names(smoothed) <- names(object)

    .GammaSpectra(smoothed)
  }
)

# ------------------------------------------------------------------------------
#' Rectangular Smooth
#'
#' Unweighted sliding-average.
#' @param x A \code{\link{numeric}} vector of observed values to be smoothed.
#' @param m An odd \code{\link{integer}} scalar giving the number of adjacent
#'  points to use.
#' @return A \code{\link{numeric}} vector of the same length as \code{x}.
#' @keywords internal
#' @noRd
smooth_rectangular <- function(x, m) {
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
#' @noRd
smooth_triangular <- function(x, m) {
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
#' \code{smooth_savitzky} smoothes the data using the Savitzky-Golay filter.
#'
#' \code{coef_savitzky} computes the Savitzky-Golay convolution coefficients.
#' @param x A \code{\link{numeric}} vector of observed values to be smoothed.
#' @param m An odd \code{\link{integer}} scalar giving the number of adjacent
#'  points to use.
#' @param p An \code{\link{integer}} scalar giving the polynomial degree.
#' @return A \code{\link{numeric}} vector.
#' @name smooth_savitzky
#' @rdname smooth_savitzky
#' @keywords internal
#' @noRd
NULL

#' @rdname smooth_savitzky
#' @keywords internal
#' @noRd
smooth_savitzky <- function(x, m, p = 2) {
  k <- (m - 1) / 2
  i <- seq(from = -k, to = k, by = 1)
  j <- utils::head(utils::tail(seq_along(x), n = -k), n = -k)
  conv <- coef_savitzky(m, p)

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

#' @rdname smooth_savitzky
#' @keywords internal
#' @noRd
coef_savitzky <- function(m, p = 2) {
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
#   x <- object[["chanel"]]
#   y <- object[["count"]]
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
#   tmp@count <- z
#   if (length(object@energy) != 0) tmp@energy <- object@energy[-c(n-1, n)]
#   if (length(object@rate) != 0) tmp@rate <- object@rate[-c(n-1, n)]
#   tmp
# }
