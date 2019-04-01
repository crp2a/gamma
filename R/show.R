# SHOW METHODS
#' @include AllClasses.R
NULL

# GammaSpectrum ================================================================
setMethod(
  f = "show",
  signature = "GammaSpectrum",
  definition = function(object) {
    cat("Gamma spectrum:", "\n",
        "  Reference: ", object@reference, "\n",
        "  Instrument: ", object@instrument, "\n",
        "  Date: ", object@date, "\n",
        "  Number of chanels: ", max(object@chanel), "\n",
        "  Energy range (keV): ", paste(range(round(object@energy, 2)),
                                        collapse = "-"),
        sep = "")
  }
)

# GammaSpectra =================================================================
setMethod(
  f = "show",
  signature = "GammaSpectra",
  definition = function(object) {
    n <- length(object)
    spc <- ifelse(n > 1, "spectra", "spectrum")
    cat("A collection of", n, "gamma", spc, sep = " ")
  }
)
