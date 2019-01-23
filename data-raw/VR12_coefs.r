# Coefficients for computing composite VR12 scales
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(tidyr)
library(purrr)
library(usethis)

VR12_coefs <-
  # system.file("data-raw", "VR12_coefs.csv", package = "anchorUtils") %>%
  file.path("data-raw", "VR12_coefs.csv") %>%
  read_csv(col_types = "ccddddddddddddd") %>%
  nest(-Domain, -Mode) %>%
  mutate_at("data", ~map(., as_vector)) %>%
  spread(Domain, data) %>%
  nest(-Mode) %>%
  spread(Mode, data) %>%
  flatten() %>%
  map(flatten)

usethis::use_data(VR12_coefs, internal = TRUE, overwrite = TRUE)
