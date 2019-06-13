library(magrittr)

decay <- utils::read.table("./data-raw/decay.csv",
                           header = TRUE, sep = ";", dec = ",")
decay %<>% dplyr::mutate(
  post_radon = as.logical(.data$post_radon)
)
usethis::use_data(decay, internal = TRUE, overwrite = FALSE)
