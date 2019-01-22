#' Coefficients for computing composite VR12 scales
VR12_coefs <- read_csv(file.path("data", "VR12_coefs.csv"),
                 col_types = "ccddddddddddddd") %>%
  # interviewer mode only
  filter(Mode == "Phone") %>%
  # reformat to a list of lists:
  select(-Mode) %>%
  nest(-Domain) %>%
  spread(Domain, data) %>%
  map(unlist)

#' Scale VR12 items to 0-100
#' Taken from a [SAS script](https://healthcaredelivery.cancer.gov/seer-mhos/program/pcs_mcs_score.sashttps://healthcaredelivery.cancer.gov/seer-mhos/program/pcs_mcs_score.sas) for SF12:
#'
VR12_scale <- function(.data) {
  dplyr::mutate(
    .data,
    GH1  = c(100, 85, 60, 35, 0)[GH1],
    PF2 = (PF2-1)*50, PF4 = (PF4-1)*50,
    RP2 = (5-RP2)*25, RP3 = (5-RP3)*25,
    RE2 = (5-RE2)*25, RE3 = (5-RE3)*25,
    BP2 = (5-BP2)*25, MH3 = (6-MH3)*20,
    VT2 = (6-VT2)*20, MH4 = (MH4-1)*20,
    SF2 = (SF2-1)*25
  )
}

#' Compute VR12 subscales
#'
#' Given a tibble of VR12 item scores, append two new columns:
#' VR12PCS (physical) and VR12MCS (mental).
#'
#' The input data should have at least the following integer columns:
#' GH1, PF2, PF4, RP2, RP3, RE2, RE3, BP2, MH3, VT2, MH4, SF2.
#' It may have other columns as well.
#' If there is any missing data, imputation should be performed prior to this.
#'
#' @param .data tibble
#' @return tibble with new columns VR12PCS and VR12MCS
#'
#' @export
#' @family VR12
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' VR12_score(data.frame(
#'   GH1 = 2, PF2 = 0, PF4 = 0, RP2 = 3, RP3 = 3, RE2 = 3, RE3 = 3, BP2 = 3,
#'   MH3 = 3, VT2 = 3, MH4 = 0, SF2 = 0))
#'
VR12_score <- function(.data, coefs = VR12_coefs) {
  dplyr::mutate(
    .data,
    CONS = 1,
    VR12PCS = data.matrix(select(., names(VR12$PCS))) %*% VR12$PCS,
    VR12MCS = data.matrix(select(., names(VR12$MCS))) %*% VR12$MCS
  )
}
