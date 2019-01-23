#' Scale VR12 items
#'
#' Given a tibble with columns named according to the VR12 items,
#' scale them from original Likert to a 0 to 100 scale.
#'
#' Taken from a [SAS script](https://healthcaredelivery.cancer.gov/seer-mhos/program/pcs_mcs_score.sas) for SF12.
#'
#' @param .data tibble with VR12 items on Likert scale
#' @return tibble with VR12 items on 0 to 100 scale
#'
#' @importFrom dplyr %>%
#' @examples
#' anchorUtils:::VR12_scale(data.frame(
#'   GH1 = 2:3, PF2 = 0:1, PF4 = 0:1, RP2 = 2:3, RP3 = 2:3, RE2 = 2:3,
#'   RE3 = 2:3, BP2 = 2:3, MH3 = 2:3, VT2 = 2:3, MH4 = 0:1, SF2 = 0:1))
#'
VR12_scale <- function(.data) {
  .data %>%
    dplyr::mutate(
      GH1 = c(100, 85, 60, 35, 0)[GH1],
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
#' Given a tibble with VR12 item scores,
#' create two new columns: VR12PCS (physical) and VR12MCS (mental).
#'
#' If there is any missing data, imputation should be performed prior to this.
#'
#' The default set of scoring coefficients is from the Interviewer form.
#'
#' @param .data tibble
#' @param .vars list of columns, e.g., using dplyr::vars(), holding the VR12
#'   item scores.  Order must match the order in the coefficient list:
#'   by default, GH1, PF2, PF4, RP2, RP3, RE2, RE3, BP2, MH3, VT2, MH4, SF2.
#' @param coefs vector of scoring coefficients
#' @return tibble with new columns VR12PCS and VR12MCS
#'
#' @importFrom dplyr %>%
#' @export
#' @family VR12
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' VR12_score(data.frame(
#'   GH1 = 2:3, PF2 = 0:1, PF4 = 0:1, RP2 = 2:3, RP3 = 2:3, RE2 = 2:3,
#'   RE3 = 2:3, BP2 = 2:3, MH3 = 2:3, VT2 = 2:3, MH4 = 0:1, SF2 = 0:1))
#'
VR12_score <- function(.data, .vars, coefs = VR12_coefs$Phone) {
  .data %>%
    dplyr::mutate(CONS = 1) %>%
    dplyr::mutate_at(names(coefs), as.integer) %>%
    VR12_scale() %>%
    dplyr::mutate(
      VR12PCS = data.matrix(dplyr::select(., names(coefs$PCS))) %*% coefs$PCS,
      VR12MCS = data.matrix(dplyr::select(., names(coefs$MCS))) %*% coefs$MCS
    ) %>%
    dplyr::select(-CONS)
}
