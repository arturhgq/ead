#' @title Define as principais expressões usadas nos calculos da tipologia \code{ead}
#' @inheritParams typology
measures_exprs <- \(.measure, .index) {

  rlang::get_expr(.index) |>
    rlang::as_string() -> .index_string

  rlang::sym(
    paste0("change","_", .index_string)
  ) -> .change_index_sym

  if (.measure == "democratization") {
    rlang::expr(
      !!.change_index_sym >= .threshold
    ) -> .start_epi
    rlang::expr(
      !!.change_index_sym < .events[[".backward"]]
    ) -> .backward
    rlang::expr(
      !!.change_index_sym > .define_epi[[".before_value"]]
    ) -> .before_value
    rlang::expr(
      !!.change_index_sym > .define_epi[[".after_value"]]
    ) -> .after_value
    rlang::expr(
      dplyr::between(
        !!.change_index_sym,
        .define_epi[[".before_value"]],
        .threshold
      )
    ) -> .before_value_interval
    rlang::expr(
      dplyr::between(
        !!.change_index_sym,
        .define_epi[[".after_value"]],
        .threshold
      )
    ) -> .after_value_interval

  } else if(.measure == "autocratization") {
    rlang::expr(
      !!.change_index_sym - .threshold) -> .start_epi_diff
    rlang::expr(
      janitor::round_half_up(!!.start_epi_diff , 5) <= 0
    ) -> .start_epi
    rlang::expr(
      !!.change_index_sym > .events[[".backward"]]
    ) -> .backward
    rlang::expr(
      !!.change_index_sym < .define_epi[[".before_value"]]
    ) -> .before_value
    rlang::expr(
      !!.change_index_sym < .define_epi[[".after_value"]]
    ) -> .after_value
    rlang::expr(
      dplyr::between(
        !!.change_index_sym,
        .threshold,
        .define_epi[[".before_value"]]
      )
    ) -> .before_value_interval
    rlang::expr(
      dplyr::between(
        !!.change_index_sym,
        .threshold,
        .define_epi[[".after_value"]]
      )
    ) -> .after_value_interval
  }

  rlang::expr(
    dplyr::between(
      !!.change_index_sym,
      purrr::pluck(.events, ".forward", 1),
      purrr::pluck(.events, ".forward", 2)
      )
    ) -> .forward

  rlang::expr(
    dplyr::between(
      !!.change_index_sym,
      purrr::pluck(.events, ".stagnant", 1),
      purrr::pluck(.events, ".stagnant", 2)
      )
    ) -> .stagnant

  return(
    list(
      .start_epi = .start_epi,
      .backward = .backward,
      .stagnant = .stagnant,
      .forward = .forward,
      .change_index_sym = .change_index_sym,
      .before_value = .before_value,
      .after_value = .after_value,
      .before_value_interval = .before_value_interval,
      .after_value_interval = .after_value_interval
      )
    )
}

#' @title Define o início do episódio de mudança de regime e
#' expande a série histórica em casos de descontinuidade.
#' @inheritParams typology
#' @param .countries países
#' @param .year série histórica
#' @param .start_epi_expr expressão definida em \code{measures_exprs()} usada
#' para estimar do início dos episódios de mudança de regime
#'
start_epi <- \(
  .data,
  .countries,
  .year,
  .index,
  .threshold,
  .start_epi_expr
  ) {

  rlang::enquo(.index) |>
    rlang::as_name() -> .index_string

  .data |>
    dplyr::group_by({{.countries}}) |>
    tidyr::expand(
      {{.year}} := tidyr::full_seq({{.year}},1)
    ) |>
    dplyr::full_join(.data)  |>
    dplyr::mutate(
      "change_{.index_string}" := {{.index}} - dplyr::lag({{.index}}),
      start_epi = dplyr::if_else(!!.start_epi_expr, 1,0)
      ) |>
    dplyr::ungroup() |>
    suppressMessages()
}

#' @title Agrupamento preliminar dos episódios
#' @inheritParams typology
#' @inheritParams start_epi
#' @note Caracterização dos valores atribuídos à variável "values"
#'
#' \itemize{
#'   \item \code{-1}: indica que a variação no índice de democracia é menor do
#'   que o ponto de corte. Possível evento de reação, estagnação ou avanço.
#'
#'   A função \code{pre_events()} discrimina eventos de avanço, estagnação e de
#'   reação com base nesses casos.
#'
#'   \item \code{0}: sugere a estabilidade do regime.
#'
#'   \item \code{.5}: indica que a variação no índice de democracia alcançou o
#'   \code{.threshold} especificado, porém a observação é precedida ou sucedida
#'   por valores ausentes ou por observações que não atingiram o ponto de corte.
#'
#'   \item \code{1}: sugere a existência de casos de mudança de regime
#'
#'   \item \code{99}: Missing Data entre os agrupamentos que indicam casos de
#'   mudança de regime.
#' }
#'
pre_group <- \(.data, .define_epi, .countries) {

  rlang::exprs(
    lengths <= .define_epi[[".keep"]] &
      values == 0 &
      dplyr::lead(values) == 1 &
      dplyr::lag(values) == 1 ~ -1,
    lengths <= .define_epi[[".keep"]] &
      values == "NA" & dplyr::lead(values) == 1 &
      dplyr::lag(values) == 1 ~ 99,
    lengths <= .define_epi[[".keep"]] &
      values == 0 & dplyr::lead(values) != 1 &
      dplyr::lag(values) != 1 ~ 0,
    lengths >= .define_epi[[".stop"]] &
      values == 0 ~ 0,
    lengths == 1 & values == 1 ~ .5,
    lengths >= 2 & values == 1 ~ 1,
    TRUE ~ NA_real_
    ) -> .pre_group_values

  .data |>
    dplyr::group_by({{.countries}}) |>
    dplyr::mutate(
      start_epi = stringr::str_replace_na(start_epi)
    ) |>
    dplyr::summarise(
      lengths = rle(start_epi)[[1]],
      values = rle(start_epi)[[2]]
      ) |>
    dplyr::mutate(
      values = dplyr::case_when(!!!.pre_group_values)
    ) |>
    dplyr::ungroup() |>
    tibble::rowid_to_column("pre_group") |>
    tidyr::uncount(lengths) |>
    dplyr::rename(pre_group_values = values) |>
    dplyr::select(-{{.countries}}) |>
    dplyr::bind_cols(.data) |>
    dplyr::relocate(pre_group_values, pre_group, .after = "start_epi")
}

#' @title Estende os agrupamentos prévios.
#' @description Redesenha o início e fim dos episódios de acordo com
#' os parâmetros .after, .before, .after_value e .before_value.
#' O objetivo é ampliar a quantidade de casos cobertos.
#' @inheritParams typology
#' @inheritParams start_epi
#' @param .before_value Veja [typology()]
#' @param .after_value Veja [typology()]
#' @param .before_value_interval_expr expressão definida em \code{measures_exprs()}
#' que estima a variação no índice de democracia, inferior ao \code{.threshold},
#' usada para alterar o ano de abertura dos episódios de mudança de regime.
#'  dada
#' @param .after_value_interval_expr expressão definida em \code{measures_exprs()}
#' que estima a variação mínima no índice de democracia, inferior ao \code{.threshold},
#' usada para alterar o ano de encerramento dos episódios de mudança de regime.
extend_group <- \(
  .data,
  .countries,
  .define_epi,
  .threshold,
  .before_value,
  .after_value,
  .before_value_interval_expr,
  .after_value_interval_expr
) {
  .data |>
    dplyr::group_by(pre_group, {{.countries}}) |>
    dplyr::mutate(
      id_within_group_after = dplyr::row_number(),
      id_within_group_before = rev(dplyr::row_number()),
      pre_group_values = dplyr::if_else(
        id_within_group_after <= .define_epi[[".after"]] &
          pre_group_values == 0 &
          !!.after_value_interval_expr,
        true = "after",
        false = as.character(pre_group_values)
        ),
      pre_group_values = dplyr::if_else(
        id_within_group_before <= .define_epi[[".before"]] &
          pre_group_values == 0 &
          !!.before_value_interval_expr,
        true = "before",
        false = as.character(pre_group_values)
        )
      )  |>
    dplyr::ungroup()
}

#' @title Identificação preliminar dos episódios de mudança de regime
#' @inheritParams typology
pre_episodes <- \(.data){

  rlang::exprs(
    pre_group_values == .5 ~ 1,
    pre_group_values == 1 ~ 1,
    pre_group_values == -1 ~ 0,
    pre_group_values == 99 ~ 0,
    pre_group_values == "before" ~ 1,
    pre_group_values == "after" ~ 1,
    TRUE ~ NA_real_
   ) -> .pre_episodes_expr

  .data |>
    dplyr::mutate(
      pre_episodes = dplyr::case_when(
        !!!.pre_episodes_expr
      )
    )
}

#' @title Discrimina eventos menores de estagnação e avanços, bem como eventos de reação.
#' @inheritParams typology
#' @param .backward_expr expressão definida em \code{measures_exprs()} para a estimação
#' dos eventos de reação
#' @param .stagnant_expr expressão definida em \code{measures_exprs()} para a
#' estimação dos eventos de estagnação
#' @param .forward_expr expressão definida em \code{measures_exprs()} para a estimação
#' dos eventos de avanço
pre_events <- \(
  .data,
  .events,
  .backward_expr,
  .stagnant_expr,
  .forward_expr
) {

  rlang::exprs(
    pre_episodes == "0" & !!.forward_expr ~ "forward",
    pre_episodes == "0" & !!.stagnant_expr ~ "stagnant",
    pre_episodes == "0" & !!.backward_expr ~ "backward"
    ) -> .pre_events_expr

  .data |>
    dplyr::mutate(
      pre_events = dplyr::case_when(!!!.pre_events_expr)
    )
}

#' @title Agrupamento final
#' @note Cada grupo corresponde a um episódio de mudança de regime
#' @inheritParams start_epi
final_group <- \(.data, .countries) {
  .data |>
    dplyr::group_by({{.countries}}) |>
    dplyr::mutate(
      pre_episodes = dplyr::if_else(!is.na(pre_episodes), 1 ,0),
      pre_episodes = stringr::str_replace_na(pre_episodes)
    ) |>
    dplyr::summarise(
      lengths = rle(pre_episodes)[[1]],
      values = rle(pre_episodes)[[2]],
      .groups = "drop"
    ) |>
    tibble::rowid_to_column("group") |>
    tidyr::uncount(lengths) |>
    dplyr::rename(group_values = values) |>
    dplyr::select(-{{.countries}}) |>
    dplyr::bind_cols(.data) |>
    dplyr::relocate(group_values, group, .after = "pre_events") |>
    dplyr::mutate(
      group_values = as.numeric(group_values),
      group_values = dplyr::na_if(group_values, 0),
      group_with_events = dplyr::if_else(
        pre_events %in% c("stagnant", "backward"),
        true = pre_events,
        false = as.character(group_values)
      )
    )
}
#' @title Cria as medidas para estimar os episódios de mudança de regime
#' @inheritParams typology
#' @param .measure "autocratization" ou "democratization"
#'
ead_measures <- \(
  .data,
  ...,
  .index,
  .threshold,
  .define_epi,
  .events,
  .measure
){
  print(
    cli::rule(center = glue::glue(" {(.measure)} "))
    )
  cli::cli_progress_step("Criando as medidas...", spinner = TRUE)

  rlang::quos(...) -> .args_quo
  rlang::enquo(.index) -> .index_enquo

  measures_exprs(.measure, .index_enquo) -> .measures_exprs

  start_epi(
    .data,
    !!.args_quo[[".countries"]],
    !!.args_quo[[".year"]],
    !!.index_enquo,
    .threshold,
    .measures_exprs[[".start_epi"]]
    ) |>
    pre_group(
      .define_epi,
      !!.args_quo[[".countries"]]
      ) |>
    extend_group(
      !!.args_quo[[".countries"]],
      .define_epi,
      .threshold,
      .define_epi[[".before_value"]],
      .define_epi[[".after_value"]],
      .measures_exprs[[".before_value_interval"]],
      .measures_exprs[[".after_value_interval"]]
      ) |>
    pre_episodes() |>
    pre_events(
      .events,
      .measures_exprs[[".backward"]],
      .measures_exprs[[".stagnant"]],
      .measures_exprs[[".forward"]]
      ) |>
    final_group(
      !!.args_quo[[".countries"]]
    ) -> ead_measures

  cli::cli_progress_update()
  return(
    ead_measures
  )
}

