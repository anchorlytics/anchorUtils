# Coefficients for computing composite VR12 scales
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(tidyr)
library(purrr)
library(usethis)

VR12_coefs <-
  readr::read_csv(
    file.path("data-raw", "VR12_coefs.csv"),
    col_types = "ccddddddddddddd")

VR12_items <- tail(head(names(VR12_coefs), -1), -2)

usethis::use_data(VR12_coefs, internal = TRUE, overwrite = TRUE)
usethis::use_data(VR12_items, overwrite = TRUE)
