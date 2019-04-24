# HELPERS

#' Find nearest value
#'
#' Finds the closest element to a number in a vector.
#' @param x A \code{\link{numeric}} vector to search within.
#' @param value A \code{\link{numeric}} vector giving the value to look for.
#' @return A \code{\link{numeric}} vector of indices.
#' @keywords internal
#' @noRd
findClosest <- function(x, value) {
  # Validation
  if (!is.numeric(x) | !is.numeric(value))
    stop("Numeric vectors are expected.")

  index <- sapply(X = value,
                  FUN = function(i, x) which.min(abs(x - i)),
                  x = x)
  return(index)
}

#' Equality within a vector
#'
#' Checks for equality among all elements of a vector.
#' @param x A \code{\link{numeric}} vector to be checked.
#' @param tolerance A length-one \link{\code{numeric}} vector giving the
#'  tolerance to check within.
#' @param na.rm A \code{\link{logical}} scalar specifying if missing values
#'  (including NaN) should be omitted.
#' @return A \code{\link{logical}}.
#' @keywords internal
#' @noRd
isEqual <- function(x, tolerance = .Machine$double.eps^0.5, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.")
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) <= tolerance
}

#' Positive numbers
#'
#' Checks if an object only contains positive values.
#' @param x A \code{\link{numeric}} object to be checked.
#' @param strict A \code{\link{logical}} scalar.
#' @param na.rm A \code{\link{logical}} scalar specifying if missing values
#'  (including NaN) should be omitted.
#' @return A \code{\link{logical}}.
#' @keywords internal
#' @noRd
isPositive <- function(x, strict = FALSE, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.")

  if (strict) {
    !any(x <= 0, na.rm = na.rm)
  } else {
    !any(x < 0, na.rm = na.rm)
  }
}
