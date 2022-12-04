#' @title Identifica golpes e autogolpes
#' @description `r lifecycle::badge("experimental")`
#'
#' Esta função identifica golpes e autogolpes
#' @inheritParams typology
#' @export

add_coups <- function(.data, ...){
  rlang::quos(...) -> .quo_args
  .data |>
    dplyr::group_by(!!.quo_args[[".reg_info"]]) |>
    dplyr::mutate(
      reg_end_type2 = dplyr::if_else(
        dplyr::row_number() == dplyr::n(),
        as.numeric(!!.quo_args[[".reg_end_type"]]),
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      reg_end_type2_fix = dplyr::lag(reg_end_type2),
      coups = dplyr::case_when(
        reg_end_type2_fix %in% 0 ~ "coup",
        reg_end_type2_fix %in% 1 ~ "coup",
        reg_end_type2_fix %in% 2 ~ "autogolpe"
      )
    ) |>
    dplyr::select(
      - dplyr::all_of(
        c(
          "reg_end_type2_fix",
          "reg_end_type2",
          "reg_end_type"
        )
      )
    )
}
