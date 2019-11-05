# SIMULATE A GAMMA SPECTRUM
#' @include AllGenerics.R
NULL

#' @export
#' @rdname simulate_spectrum
#' @aliases simulate_spectrum,numeric,numeric,numeric-method
setMethod(
  f = "simulate_spectrum",
  signature = signature(K = "numeric", U = "numeric", Th = "numeric"),
  definition = function(K, U, Th, energy = c(1, 3200), n = 1024, ...) {
    # Get data
    n <- as.integer(n)[[1L]]

    avogadro <- 6.022141e23
    # Mass (g)
    mass <- c(K, Th, U, U)
    # Molar mass (g/mol)
    M <- c(40.46, 232.1, 241.1, 241.1)
    # Relative abundance
    abundance <- c(K40 = 0.000117, Th232 = 1,
                   U235 = 0.007204, U238 = 0.992742)
    # Activité
    activity <- c(K40 = 1.760e-17, Th232 = 1.569e-18,
                  U235 = 3.120e-17, U238 = 4.916e-18)
    # Nombre de désintégration par seconde
    D <- (mass * abundance * avogadro / M) * activity

    chain <- .decay[["decay_chain"]]
    k <- .decay[["counts_chain"]] *
      rep(D, times = table(.decay[["decay_chain"]]))

    energy_range <- seq(from = energy[[1L]], to = energy[[2L]], length.out = n)
    count <- lapply(
      X = .decay[["energy"]],
      FUN = function(x) stats::dnorm(energy_range, mean = x, sd = sqrt(x))
    )
    .GammaSpectrum(
      reference = "simulation",
      energy = energy_range,
      # chanel = seq_len(n),
      count = colSums(do.call(rbind, count) * k)
    )
  }
)
