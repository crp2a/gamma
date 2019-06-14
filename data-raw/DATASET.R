library(magrittr)

# Milieux Clermont =============================================================
clermont <- utils::read.table("./data-raw/clermont.csv",
                              header = TRUE, sep = ",", dec = ".") %>%
  dplyr::mutate(
    K = dplyr::case_when(
      is.na(.data$K) ~ .data$K2O * 0.8301,
      TRUE ~ .data$K
    ),
    K_error = dplyr::case_when(
      is.na(.data$K_error) ~ .data$K2O_error * 0.8301,
      TRUE ~ .data$K_error
    )
  ) %>%
  dplyr::mutate(
    gamma = .data$U * 111.6 + .data$Th * 47.96 + .data$K * 249.1,
    gamma_error = sqrt((.data$U_error * 111.6)^2 + (.data$Th_error * 47.96)^2 + (.data$K_error * 249.1)^2)
  )
rownames(clermont) <- clermont[["reference"]]
usethis::use_data(clermont, internal = FALSE, overwrite = FALSE)

# Decay data ===================================================================
decay <- utils::read.table("./data-raw/decay.csv",
                           header = TRUE, sep = ";", dec = ",")
decay %<>% dplyr::mutate(
  post_radon = as.logical(.data$post_radon)
)
usethis::use_data(decay, internal = TRUE, overwrite = FALSE)
clermont
