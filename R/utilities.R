# HELPERS

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

#' Integer numbers
#'
#' Checks if an object only contains integer numbers.
#' @param x A \code{\link{numeric}} object to be checked.
#' @param tolerance A length-one \link{\code{numeric}} vector giving the
#'  tolerance to check within.
#' @return A \code{\link{logical}} depending on whether \code{x} contains integer
#'  numbers.
#' @seealso \code{\link[base]{is.integer}}
#' @keywords internal
#' @noRd
isWholeNumber <- function(x, tolerance = .Machine$double.eps^0.5) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.")
  abs(x - round(x)) <= tolerance
}
