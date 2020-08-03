#' Scale VR12 items
#'
#' Given a tibble with columns named according to the VR12 items,
#' scale them from original 1-based Likert to a 0 to 100 scale.
#' Reverse-coding of items is handled here.
#'
#' @source [SAS script](https://healthcaredelivery.cancer.gov/seer-mhos/program/pcs_mcs_score.sas)
#'
#' @param .data tibble with at least the following columns:
#'   GH1, PF2, PF4, RP2, RP3, RE2, RE3, BP2, MH3, VT2, MH4, SF2,
#'   with values 1:n.  The columns do not need to be in this order, but do
#'   need to be named exactly this.
#' @return tibble with same columns, but VR12 items scaled to 0-100.
#'
#' @import dplyr
#'
#' @family VR12 utilities
#'
#' @examples
#' anchorUtils:::VR12_scale(data.frame(
#'   GH1 = 2:3, PF2 = 1:2, PF4 = 1:2, RP2 = 2:3, RP3 = 2:3, RE2 = 2:3,
#'   RE3 = 2:3, BP2 = 2:3, MH3 = 2:3, VT2 = 2:3, MH4 = 1:2, SF2 = 1:2))
#'
VR12_scale <- function(.data) {
  .data %>%
    mutate(across(any_of(c("GH1")), ~c(100, 85, 60, 35, 0)[.])) %>%
    # 2-pt scale
    mutate(across(any_of(c("PF2", "PF4")), ~(.-1)*50)) %>%
    # 5-pt reverse
    mutate(across(any_of(c("RP2", "RP3", "RE2", "RE3", "BP2")), ~(5-.)*25)) %>%
    # 6-pt reverse
    mutate(across(any_of(c("MH3", "VT2")), ~(6-.)*20)) %>%
    # 6-pt
    mutate(across(any_of(c("MH4")), ~(.-1)*20)) %>%
    # 5-pt
    mutate(across(any_of(c("SF2")), ~(.-1)*25))
}

#' Compute VR12 component subscale
#'
#' Given a tibble with VR12 item scores, add a column for component VR12 scores.
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
#' @import dplyr
#' @importFrom rlang :=
#'
#' @export
#' @family VR12 utilities
#'
#' @examples
#' VR12_score(data.frame(
#'   GH1 = c(1, 4), PF2 = c(3, 1), PF4 = c(3, 1), RP2 = c(1, 4), RP3 = c(1, 4),
#'   RE2 = c(1, 4), RE3 = c(1, 4), BP2 = c(1, 5), MH3 = c(1, 5), VT2 = c(1, 5),
#'   MH4 = c(6, 1), SF2 = c(5, 1)),
#'   dplyr::everything(), "PCS")
#'
VR12_score <- function(.data, .vars, scale, mode = "Phone") {
  item_names <- enquo(.vars)
  # pull copy of dataset
  item_scores <- select(.data, !!item_names)
  names(item_scores) <- anchorUtils::VR12_items
  item_scores <- item_scores %>%
    mutate(across(everything(), as.integer)) %>%
    mutate(CONS = 1)
  # scale to 0-100
  item_scores <- VR12_scale(item_scores)
  # get scoring coefficients
  coefs <- VR12_coefs
  coefs <- coefs[coefs$Domain == scale & coefs$Mode == mode, -2:-1]
  coefs <- purrr::as_vector(coefs)
  # dot-product with coefficients
  component <- as.vector(data.matrix(item_scores) %*% coefs)
  # merge with original dataset
  .data %>%
    bind_cols(!!scale := component)
}
