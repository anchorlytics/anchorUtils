#' Scale VR12 items
#'
#' Given a tibble with columns named according to the VR12 items,
#' scale them from original Likert to a 0 to 100 scale.
#'
#' Taken from a [SAS script](https://healthcaredelivery.cancer.gov/seer-mhos/program/pcs_mcs_score.sas) for SF12.
#'
#' @param .data tibble with VR12 items on 1-based integer scale
#' @return tibble with VR12 items on 0 to 100 scale
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' anchorUtils:::VR12_scale(data.frame(
#'   GH1 = 2:3, PF2 = 0:1, PF4 = 0:1, RP2 = 2:3, RP3 = 2:3, RE2 = 2:3,
#'   RE3 = 2:3, BP2 = 2:3, MH3 = 2:3, VT2 = 2:3, MH4 = 0:1, SF2 = 0:1))
#'
VR12_scale <- function(.data) {
  .data %>%
    dplyr::mutate_at("GH1", ~c(100, 85, 60, 35, 0)[.]) %>%
    dplyr::mutate_at("PF2", ~(.-1)*50) %>%
    dplyr::mutate_at("PF4", ~(.-1)*50) %>%
    dplyr::mutate_at("RP2", ~(5-.)*25) %>%
    dplyr::mutate_at("RP3", ~(5-.)*25) %>%
    dplyr::mutate_at("RE2", ~(5-.)*25) %>%
    dplyr::mutate_at("RE3", ~(5-.)*25) %>%
    dplyr::mutate_at("BP2", ~(5-.)*25) %>%
    dplyr::mutate_at("MH3", ~(6-.)*20) %>%
    dplyr::mutate_at("VT2", ~(6-.)*20) %>%
    dplyr::mutate_at("MH4", ~(.-1)*20) %>%
    dplyr::mutate_at("SF2", ~(.-1)*25)
}

#' Compute VR12 component subscale
#'
#' Given a tibble with VR12 item scores, add a column for component VR12 scores.
#'
#' If there is any missing data, imputation should be performed prior to this.
#'
#' @param .data tibble
#' @param .vars list of columns (parsed with tidyselect) holding the VR12
#'   item scores, in the following order:
#'   GH1, PF2, PF4, VRP2, VRP3, VRE2, VRE3, BP2, MH3, VT2, MH4, SF2.
#'   Item scores should be 1-based integers.
#' @param scale either "PCS" (physical component) or "MCS" (mental component)
#' @param mode either "Phone" (default) or "Mail"
#' @return tibble with new column, either "PCS" or "MCS"
#'
#' @export
#' @importFrom dplyr %>%
#' @family VR12
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' VR12_score(data.frame(
#'   GH1 = c(1, 4), PF2 = c(3, 1), PF4 = c(3, 1), RP2 = c(1, 4), RP3 = c(1, 4),
#'   RE2 = c(1, 4), RE3 = c(1, 4), BP2 = c(1, 5), MH3 = c(1, 5), VT2 = c(1, 5),
#'   MH4 = c(6, 1), SF2 = c(5, 1)),
#'   dplyr::everything(), "PCS")
#'
VR12_score <- function(.data, .vars, scale, mode = "Phone") {
  item_names <- dplyr::enquo(.vars)
  # pull copy of dataset
  item_scores <- dplyr::select(.data, !!item_names)
  names(item_scores) <- VR12_items
  item_scores <- item_scores %>%
    dplyr::mutate_all(as.integer) %>%
    dplyr::mutate(CONS = 1)
  # scale to 0-100
  item_scores <- VR12_scale(item_scores)
  # dot-product with coefficients
  coefs <-
    VR12_coefs %>%
    dplyr::filter(Domain == scale, Mode == mode) %>%
    dplyr::select(-Domain, -Mode) %>%
    purrr::as_vector()
  component <- as.vector(data.matrix(item_scores) %*% coefs)
  # merge with original dataset
  .data %>%
    dplyr::bind_cols(!!scale := component)
}
