# HELPERS

#' Helpers
#'
#' \code{compact} removes elements from a list or vector.
#' \code{detect} xxx.
#'
#' %o% allows for function composition.
#'
#' %||% xxx.
#' @param x An object.
#' @param f,f1,f2 A \code{\link{function}}. In \code{compact} and \code{detect}
#'  \code{f} must be a logical predicate.
#' @param g A \code{\link{function}}.
#' @param lhs An object.
#' @param rhs An object.
#' @details
#'  Adapted from H. Wickham's \emph{Avanced R}.
#' @return
#'  TODO
#' @references
#'  Wickham, H. (2014). \emph{Advanced R}. London: Chapman & Hall. The R Series.
#' @name helpers
#' @keywords internal
NULL

#' @rdname helpers
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) lhs else rhs
}
#' @rdname helpers
`%o%` <- function(f, g) {
  function(...) f(g(...))
}
#' @rdname helpers
compact <- function(f, x) {
  Filter(Negate(f), x)
}
#' @rdname helpers
detect <- function(f, x) {
  vapply(x, f, logical(1))
}
#' @rdname helpers
count <- function(f, x) {
  sum(detect(f, x))
}

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
    stop("Numeric vectors are expected.", call. = FALSE)

  index <- vapply(
    X = value,
    FUN = function(i, x) which.min(abs(x - i)),
    FUN.VALUE = integer(1),
    x = x
  )
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
    stop("A numeric vector is expected.", call. = FALSE)
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) <= tolerance
}
isEmpty <- function(x) {
  length(x) == 0
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
    stop("A numeric vector is expected.", call. = FALSE)

  if (strict) {
    !any(x <= 0, na.rm = na.rm)
  } else {
    !any(x < 0, na.rm = na.rm)
  }
}

#' Error
#'
#' Checks if an object only contains positive values.
#' @param x A \code{\link{numeric}} object to be checked.
#' @param strict A \code{\link{logical}} scalar.
#' @param na.rm A \code{\link{logical}} scalar specifying if missing values
#'  (including NaN) should be omitted.
#' @return A \code{\link{logical}}.
#' @keywords internal
#' @noRd
isError <- function(x) {
  inherits(x, "error")
}
stopCustom <- function(.subclass, message, call = NULL, ...) {
  error <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
  stop(error)
}
