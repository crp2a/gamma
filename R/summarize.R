# SUMMARISE
#' @include AllClasses.R AllGenerics.R
NULL

# ================================================================ GammaSpectrum
#' @rdname summarise
#' @aliases summarise,GammaSpectrum-method
#' @export
setMethod(
  f = "summarise",
  signature = "GammaSpectrum",
  definition = function(object) {
    energy <- round(range_energy(object), 2)
    cbind.data.frame(
      name = object@name,
      date = as.character(object@date),
      live_time = object@live_time,
      real_time = object@real_time,
      chanels = length(object),
      energy_min = energy[[1L]],
      energy_max = energy[[2L]],
      stringsAsFactors = FALSE
    )
  }
)

# ================================================================= GammaSpectra
#' @rdname summarise
#' @aliases summarise,GammaSpectra-method
#' @export
setMethod(
  f = "summarise",
  signature = "GammaSpectra",
  definition = function(object) {
    sum_up <- lapply(X = object, FUN = summarise)
    do.call(rbind, sum_up)
  }
)

# ================================================================ DoseRateModel
#' @rdname summarise
#' @aliases summarise,DoseRateModel-method
#' @export
setMethod(
  f = "summarise",
  signature = "DoseRateModel",
  definition = function(object) {
    cat("Residuals:\n")
    print(object[["residuals"]])
    cat("\nCoefficients:\n")
    print(matrix(data = c(object[["intercept"]], object[["slope"]]),
                 nrow = 2L, ncol = 2L, byrow = TRUE,
                 dimnames = list(c("Intercept", "Slope"),
                                 c("Estimate", "Std. Error"))))
    cat(sprintf("\nMSWD: %g on %d degrees of freedom, p-value: %g\n",
                object[["MSWD"]], object[["df"]], object[["p_value"]]))
  }
)

# ============================================================= CalibrationCurve
#' @rdname summarise
#' @aliases summarise,CalibrationCurve-method
#' @export
setMethod(
  f = "summarise",
  signature = "CalibrationCurve",
  definition = function(object) {
    cat(sprintf("--- Ni %s\n", paste0(rep("-", options()$width - 7),
                                      collapse = "")), sep = "")
    summarise(object[["Ni"]])
    cat(sprintf("\n--- NiEi %s\n", paste0(rep("-", options()$width - 9),
                                          collapse = "")), sep = "")
    summarise(object[["NiEi"]])
  }
)
