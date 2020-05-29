# ROLLING BALL BASELINE
#' @include AllClasses.R AllGenerics.R
NULL

# @export
# @rdname baseline
# @aliases baseline_rollingball,GammaSpectrum-method
# setMethod(
#   f = "baseline_rollingball",
#   signature = signature(object = "GammaSpectrum"),
#   definition = function(object, ...) {
#     # Get counts
#     x <- get_chanels(object)
#     y <- get_counts(object)
#     # Estimate baseline
#     bsl <- rollingball(x, y)
#
#     # Check baseline
#     if (anyNA(bsl))
#       stop("Failed to estimate the baseline, please check your parameters.",
#            call. = FALSE)
#
#     spc <- methods::initialize(object, count = bsl)
#     methods::as(spc, "BaseLine")
#   }
# )


# rollingball <- function(x, y) {
#
# }
