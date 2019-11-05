# STATISTICS HELPERS

#' MAD
#'
#' Calculates the Median Absolute Deviation (MAD).
#' @param x A \code{\link{numeric}} vector.
#' @param k A \code{\link{numeric}} value.
#' @param na.rm A \code{\link{logical}} scalar.
#' @return A \code{numeric} value.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
MAD <- function(x, k = 1.4826, na.rm = FALSE) {
  k * stats::median(abs(x - stats::median(x, na.rm = na.rm)), na.rm = na.rm)
}

#' FWHM
#'
#' Estimates the Half-Width at Half-Maximum (FWHM) for a given peak.
#' @param x,y A \code{\link{numeric}} vector giving the \eqn{x} and \eqn{y}
#'  coordinates of a set of points. Alternatively, a single argument \eqn{x}
#'  can be provided.
#' @param center A \code{\link{numeric}} value giving the peak position in
#'  \code{x} units.
#' @return A \code{\link{numeric}} value.
#' @details
#'  It tries to get the smallest possible estimate.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
FWHM <- function(x, y, center) {
  if (missing(y)) {
    z <- x
    if (is.list(z)) {
      x <- z[[1]]
      y <- z[[2]]
    }
    if (is.matrix(z) | is.data.frame(z)) {
      x <- z[, 1]
      y <- z[, 2]
    }
  } else {
    if (length(x) != length(y))
      stop("`x` and `y` lengths differ.", call. = FALSE)
  }

  i <- which(x == center)
  peak_height <- y[i]
  scale_for_roots <- y - peak_height / 2
  root_indices <- which(diff(sign(scale_for_roots)) != 0)

  tmp <- sort(c(root_indices, i))
  k <- which(tmp == i)

  root_left <- root_indices[k - 1]
  root_right <- root_indices[k]

  HWHM_left <- x[i] - x[root_left]
  HWHM_right <- x[root_right] - x[i]

  FWHM <- 2 * min(c(HWHM_left, HWHM_right))
  return(FWHM)
}
