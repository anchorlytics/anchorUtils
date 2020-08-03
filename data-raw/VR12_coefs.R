# Coefficients for computing composite VR12 scales
library(usethis)
library(dplyr)

VR12_coefs <-
  system.file("extdata", "VR12_coefs.csv", package = "anchorUtils") %>%
  readr::read_csv(col_types = "ccddddddddddddd")

VR12_items <-
  names(VR12_coefs) %>%
  head(-1) %>%
  tail(-2)

usethis::use_data(VR12_coefs, overwrite = TRUE, internal = TRUE)
usethis::use_data(VR12_items, overwrite = TRUE)
