# Coefficients for computing composite VR12 scales
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(tidyr)
library(purrr)
library(usethis)

VR12_coefs <-
  # system.file("data-raw", "VR12_coefs.csv", package = "anchorUtils") %>%
  file.path("data-raw", "VR12_coefs.csv") %>%
  readr::read_csv(col_types = "ccddddddddddddd") %>%
  tidyr::nest(-Domain, -Mode) %>%
  dplyr::mutate_at("data", ~purrr::map(., purrr::as_vector)) %>%
  tidyr::spread(Domain, data) %>%
  tidyr::nest(-Mode) %>%
  tidyr::spread(Mode, data) %>%
  purrr::flatten() %>%
  purrr::map(purrr::flatten)

usethis::use_data(VR12_coefs, internal = TRUE, overwrite = TRUE)
