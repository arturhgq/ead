#' @title Faz pequenos ajustes na tipologia Regimes of the World
#' @description `r lifecycle::badge("stable")`
#'
#' Esta função flexibiliza o ponto de corte usado na codificação de
#' regimes políticos e desconsidera as transições que resultam de variações
#' anuais no Índice de Democracia Eleitoral contidas no intervalo \[-0.05; 0.05\] para todos os
#' casos em que tais episódios sucedem imediatamente um retorno ao regime
#' anterior. Em outros termos, transições de regime com duração de um ano, fruto
#' de variações de 5% no Índice de Democracia Eleitoral são descartadas. Argumento que
#' essas transições devem ser tratadas como ruídos, pois o próprio conceito de
#' regime político pressupõe um mínimo de continuidade no tempo. (Schedler, 2013).
#' @param .data Banco de dados
#' @param ... Variáveis que devem ser especificadas
#' * \code{.countries} = países
#' * \code{typology_d} = ROW classifição dicotômica
#' * \code{typology_p} = ROW classificação policotômica
#' * \code{.index} = Índice de Democracia Eleitoral
#' @export

row_update <- \(.data, ...) {
  rlang::quos(...) -> .args_quo

  .data |>
    dplyr::mutate(
      change_IDE = !!.args_quo[[".index"]] - dplyr::lag(!!.args_quo[[".index"]])
    ) |>
    dplyr::group_by(!!.args_quo[[".countries"]]) |>
    dplyr::mutate(
      regime_old := !!.args_quo[[".typology_p"]],
      regime2_old := !!.args_quo[[".typology_d"]],
      regime_subtype_change = dplyr::case_when(
        regime != dplyr::lag(regime) ~
          glue::glue("{dplyr::lag(regime)}-{regime}"),
        TRUE ~ NA_character_
      ),
      regime2_type_change = dplyr::case_when(
        !!.args_quo[[".typology_d"]] != dplyr::lag(regime2) ~
          glue::glue("{dplyr::lag(regime2)}-{regime2}"
          ),
        TRUE ~ NA_character_
      ),
      !!.args_quo[[".typology_p"]] := dplyr::case_when(
        dplyr::lead(regime) == dplyr::lag(regime) &
          dplyr::between(change_IDE, -0.05, 0.05) ~
          dplyr::lead(regime),
        TRUE ~ regime
      ),
      !!.args_quo[[".typology_d"]] := dplyr::case_when(
        dplyr::lead(regime2) == dplyr::lag(regime2) &
          dplyr::between(change_IDE, -0.05, 0.05) ~
          dplyr::lead(regime2),
        TRUE ~ regime2
      ),
      regime_subtype_change_discarded = dplyr::if_else(
        !!.args_quo[[".typology_p"]]!= regime_old,
        true = TRUE,
        false = NA
      ),
      regime2_type_change_discarded = dplyr::if_else(
        !!.args_quo[[".typology_d"]] != regime2_old,
        true = TRUE,
        false = NA
      )
    ) |>
    dplyr::ungroup()
}
