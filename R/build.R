#' @title Identifica o início dos agrupamentos
#' @note Cada agrupamento corresponde a um episódio de mudança de regime.
#' @param .data \code{ead_measures} data frame
group_start <- \(.data) {
  .data |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      group_start = dplyr::if_else(
        dplyr::row_number() == 1 &
          group_values == 1,
        true = "begin",
        false = NA_character_
        ),
      group_start = dplyr::if_else(
        pre_group_values == .5 &
          length(group_values) == 1,
        true = "perhaps",
        false = group_start
        )
      ) |>
    dplyr::ungroup()
}

#' @title Identifica o final do agrupamentos
#' @note Cada agrupamento corresponde a um episódio de mudança de regime.
#' @param .data \code{ead_measures} data frame
group_end <- \(.data) {
  .data  |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      group_end = dplyr::if_else(
        dplyr::row_number() == dplyr::n() & group_values == 1,
        true = "end",
        false = NA_character_
        )
      ) |>
    dplyr::ungroup()
}

#' @title Computa a soma acumulada da variação do índice de democracia escolhido
#' @inheritParams typology
cumulative_sum <- \(.data, .index) {
  rlang::enquo(.index) |>
    rlang::as_name() -> .index_string

  rlang::sym(
    paste0("change","_", .index_string)
  ) -> .change_index_sym

  .data |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      group_length = purrr::accumulate(group_values, `+`),
      cumulative = purrr::accumulate(!!.change_index_sym, `+`),
      cumulative = dplyr::if_else(
        is.na(group_length),
        true = NA_real_,
        false = cumulative
        )
      ) |>
    dplyr::ungroup()
}

#' @title Declara o início, o intervalo de duração e o fim dos episódios de mudança de regime
#' @inheritParams typology
#' @inheritParams ead_measures
epi_progress <- \(.data, .define_epi, .measure){

  if (.measure == "autocratization") {
    rlang::expr(
      dplyr::last(cumulative) <= .define_epi[[".cumulative"]]
      ) -> .cumulative_expr
  } else if (.measure == "democratization"){
    rlang::expr(
      dplyr::last(cumulative) >= .define_epi[[".cumulative"]]
      ) -> .cumulative_expr
  }
  .data |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      epi_progress = dplyr::if_else(
        !is.na(group_length) &
          !!.cumulative_expr,
        true = "ongoing",
        false = NA_character_
        ),
      epi_progress = dplyr::if_else(
        !is.na(epi_progress) &
          dplyr::row_number() == 1,
        true = "begin",
        false = epi_progress
        ),
      epi_progress = dplyr::if_else(
        !is.na(epi_progress) &
          dplyr::row_number() == dplyr::n(),
        true = "end",
        false = epi_progress
        )
      ) |>
    dplyr::ungroup()
}

#' @title Identifica episódios de autocratização e democratização
#' @inheritParams typology
#' @inheritParams ead_measures
epi_identify <- \(.data, ..., .index, .define_epi, .measure, .row_update) {
  rlang::quos(
    ...,
    .group_id = group
  ) -> .args_quo

  rlang::enquo(.index) -> .index_enquo

  cli::cli_progress_step("Identificando os episódios...", spinner = TRUE)
  .data |>
    group_start() |>
    group_end() |>
    cumulative_sum(
      !!.index_enquo
    ) |>
    epi_progress(
      .define_epi = .define_epi,
      .measure = .measure
      ) -> .identify
  cli::cli_progress_update()

  cli::cli_progress_step("Atualizando 'Regimes of the World'...", spinner = TRUE)
  .identify |>
    purrr::when(
      isTRUE(.row_update) ~
        row_update(
          .,
          .countries = !!.args_quo[['.countries']],
          .typology_d = !!.args_quo[['.typology_d']],
          .typology_p = !!.args_quo[['.typology_p']],
          .index = !!.index_enquo
        ),
      isFALSE(.row_update) ~
        cli::cli_abort(
          c("a opção `.row_update = FALSE` não foi implementada.",
            "i" = "Solicite a implementação via 'pull request'."
            )
          )
      ) -> .row_update
  cli::cli_progress_update()

  cli::cli_progress_step(
    "Iniciando a codificação...",
    msg_done = "Codificação concluída com sucesso!",
    spinner = TRUE
  )

  .row_update |>
    epi_coding(
      .measure = .measure,
      .countries = .args_quo[['.countries']],
      .group_id = .args_quo[[".group_id"]]
    ) |>
    epi_id(
      .measure = .measure,
      .countries = .args_quo[['.countries']],
      .group_id = .args_quo[[".group_id"]]
    ) -> .return
  cli::cli_progress_update()
  return(.return)
}

#' @inheritParams typology
#' @inheritParams ead_measures
#' @rdname typology
#' @export
epi_build <- function(
    .data, ..., .index, .threshold, .define_epi, .events, .measure, .row_update = TRUE
  ) {
    rlang::quos(...) -> .args_quo

  ead_measures(
    .data,
    ...,
    .index = !!rlang::ensym(.index),
    .threshold = .threshold,
    .define_epi = .define_epi,
    .events = .events,
    .measure = .measure
    ) |>
    epi_identify(
      ...,
      .index = !!rlang::ensym(.index),
      .define_epi = .define_epi,
      .measure = .measure,
      .row_update = .row_update
      ) -> ead_data

  # Exclui as observações adicionadas em {start_epi()} para o calculo da variação
  # acumulada do índice de democracia.

  .data |>
    dplyr::select(
      !!.args_quo[[".countries"]],
      !!.args_quo[[".year"]]
    ) |>
  dplyr::inner_join(ead_data) |>
    suppressMessages()

}
