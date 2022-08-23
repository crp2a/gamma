library(magrittr)

# Milieux Clermont =============================================================
# Conversion factors from Guérin et al. 2011
clermont <- utils::read.table("./data-raw/clermont.csv",
                              header = TRUE, sep = ",", dec = ".") |>
  dplyr::mutate(
    K = dplyr::case_when(
      is.na(.data$K) ~ .data$K2O * 0.8301,
      TRUE ~ .data$K
    ),
    K_error = dplyr::case_when(
      is.na(.data$K_error) ~ .data$K2O_error * 0.8301,
      TRUE ~ .data$K_error
    )
  )
rownames(clermont) <- clermont[["name"]]
usethis::use_data(clermont, internal = FALSE, overwrite = FALSE)

# Decay data ===================================================================
.decay <- utils::read.table("./data-raw/decay.csv",
                            header = TRUE, sep = ";", dec = ",") |>
  dplyr::filter(.data$gamma_intensity >= 1) |>
  dplyr::mutate(
    decay_chain = factor(.data$decay_chain, levels = unique(.data$decay_chain)),
    occurrence = .data$occurrence / 100,
    post_radon = as.logical(.data$post_radon),
    counts_chain = .data$gamma_intensity * .data$occurrence,
    counts_chain_error = .data$gamma_intensity_error * .data$occurrence +
      .data$gamma_intensity * .data$occurrence_error
  )

.decay_La <- utils::read.table("./data-raw/decay_La.csv",
                               header = TRUE, sep = ";", dec = ",") |>
  dplyr::mutate(
    occurrence = .data$occurrence / 100,
    post_radon = as.logical(.data$post_radon)
  )

usethis::use_data(.decay, .decay_La, internal = TRUE, overwrite = FALSE)
