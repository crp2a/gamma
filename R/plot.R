# PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectrum,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectrum", y = "missing"),
  definition = function(x, ...) {
    spc <- methods::as(x, "data.frame")
    ggplot2::ggplot(spc, ggplot2::aes_string(x = "energy", y = "counts")) +
      ggplot2::geom_line()
  }
)

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectra,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectra", y = "missing"),
  definition = function(x, select = 1, facet = FALSE, ...) {
    # Subset data
    spc <- methods::as(x[select], "list")
    if (length(spc) == 1)
      facet <- FALSE

    # Build long data frame
    spc_ls <- lapply(X = spc, FUN = "as", Class = "data.frame")
    spc_df <- dplyr::bind_rows(spc_ls, .id = "reference")

    if (facet) {
      colour <- NULL
      facet <- ggplot2::facet_wrap(.~reference, nrow = length(spc_ls), scales = "free_y")
    } else {
      colour <- "reference"
      facet <- NULL
    }
    ggplot2::ggplot(data = spc_df,
                    mapping = ggplot2::aes_string(
                      x = "energy", y = "counts",
                      group = "reference", colour = colour)
                    ) +
      ggplot2::geom_line() +
      facet
  }
)

#' @export
#' @rdname plot
#' @aliases plot,CalibrationCurve,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "CalibrationCurve", y = "missing"),
  definition = function(x, ...) {
    # Get data
    data <- x@model$model

    ggplot2::ggplot(data = data,
                    mapping = ggplot2::aes_string(x = "dose", y = "signal")) +
      ggplot2::geom_point() +
      ggplot2::stat_smooth(method = "lm", col = "red",
                           formula = y ~ 0 + x, se = FALSE)
  }
)

#' @export
#' @rdname plot
#' @aliases plot,CalibrationCurve,GammaSpectra-method
setMethod(
  f = "plot",
  signature = signature(x = "CalibrationCurve", y = "GammaSpectra"),
  definition = function(x, y, ...) {
    a <- ""
  }
)
