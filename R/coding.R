#' @title Codifica os episódios de mudança de regime
#' @inheritParams typology
#' @inheritParams ead_measures
#' @inheritParams start_epi
#' @param .group_id agrupamentos dos episódios de mudança de regime
epi_coding <- \(.data, .measure, .countries, .group_id) {
  if (.measure == "autocratization") {
  .data |>
      dplyr::group_by(!!.group_id) |>
      dplyr::mutate(
        backsliding = dplyr::case_when(
          !is.na(epi_progress) &
            dplyr::first(regime2)== 1 &
            dplyr::last(regime2) == 1 ~
            "backsliding"
          ),
        democratic_breakdown = dplyr::case_when(
          !is.na(epi_progress) &
            dplyr::first(regime2) == 1 &
            dplyr::last(regime2) == 0 ~
            "democratic breakdown"
          ),
        autocracy_deepening = dplyr::case_when(
          !is.na(epi_progress) &
            is.na(democratic_breakdown) &
            dplyr::first(regime2) == 0 &
            dplyr::last(regime2) == 0 ~
            "autocracy deepening"
          )
        ) |>
      dplyr::ungroup() -> .first_coding

    # Em seguida,
    # [1] - corrijo a codificação de alguns casos identificados como aprofundamento
    #       autoritário que correspondem a colapsos democráticos.
    # [2] - assumo que os casos de autocratização iniciados no século XIX que continuam no
    #       século XX correspondem a episódios de aprofundamento autoritário.
    # [3] - identifico episódios de colapsos democráticos previstos em RoW que não são
    #       captados pelo algoritmo.

    .first_coding |>
      dplyr::group_by(!!.group_id) |>
      dplyr::mutate(
        democratic_breakdown = dplyr::case_when(
          dplyr::first(regime2_type_change) == "1-0"  &
            autocracy_deepening == "autocracy deepening" ~
            "democratic breakdown",
          epi_progress == "end" & length(group) == 1 &
            regime2_type_change == "1-0" &
            autocracy_deepening == "autocracy deepening" ~
            "democratic breakdown",
          is.na(epi_progress) &
            regime2_type_change == "1-0" ~
            "democratic breakdown",
          TRUE ~ democratic_breakdown
        ),
        autocracy_deepening = dplyr::case_when(
          !is.na(democratic_breakdown) ~
            NA_character_,
          !is.na(epi_progress) &
            dplyr::first(year) <= 1900 &
            dplyr::last(regime2) == 0 ~
            "autocracy deepening",
          TRUE ~ autocracy_deepening
        ),
        ROW_regime_change_aut = dplyr::if_else(
          is.na(epi_progress) &
            regime2_type_change %in% "1-0",
          true = 1L,
          false = NA_integer_
        )
      ) |>
      dplyr::ungroup() -> .second_coding
  } else if(.measure == "democratization") {
    .data |>
      dplyr::group_by(!!.group_id) |>
      dplyr::mutate(
        liberalization = dplyr::case_when(
          !is.na(epi_progress) &
            dplyr::first(regime2)== 0 &
            dplyr::last(regime2) == 0 ~
            "liberalization"
        ),
        democratic_transition = dplyr::case_when(
          !is.na(epi_progress) &
            dplyr::first(regime2) == 0 &
            dplyr::last(regime2) == 1 ~
            "democratic transition"
        ),
        democratic_deepening = dplyr::case_when(
          !is.na(epi_progress) &
            is.na(democratic_transition) &
            dplyr::first(regime2) == 1 &
            dplyr::last(regime2) == 1 ~
            "democratic deepening"
        )
      ) |>
      dplyr::ungroup() -> .first_coding

    # Em seguida,
    # [1] - corrijo a codificação de alguns casos identificados como aprofundamento
    #       democrático que correspondem a transições democráticas.
    # [2] - assumo que os casos de democratização iniciados no século XIX que continuam no
    #       século XX correspondem a episódios de transição democrática ou de liberalizações.
    # [3] - identifico episódios de transições democráticas previstos em RoW que não são
    #       captados pelo algorítmo.
    # [4] - Crio uma variável dummy que identifica as transições de regime captadas
    #       por ROW, mas não pelo algoritmo.
    .first_coding |>
      dplyr::group_by(!!.group_id) |>
      dplyr::mutate(
        democratic_transition = dplyr::case_when(
          dplyr::first(regime2_type_change) == "0-1" &
            democratic_deepening == "democratic deepening" ~
            "democratic transition",
          epi_progress == "end" &
            length(!!.group_id) == 1 &
            regime2_type_change == "0-1" &
            democratic_deepening == "democratic deepening" ~
            "democratic transition",
          is.na(epi_progress) &
            regime2_type_change == "0-1" ~
            "democratic transition",
          !is.na(epi_progress) &
            dplyr::first(year) <= 1900 &
            dplyr::last(regime2) == "1" ~
            "democratic transition",
          TRUE ~ democratic_transition
        ),
        democratic_deepening = dplyr::case_when(
          !is.na(democratic_transition) ~ NA_character_,
          TRUE ~ democratic_deepening),
        liberalization = dplyr::case_when(
          !is.na(epi_progress) &
            dplyr::first(year) <= 1900 &
            dplyr::last(regime2) == "0" ~
            "liberalization",
          TRUE ~ liberalization
        ),
        ROW_regime_change_dem = dplyr::if_else(
          is.na(epi_progress) &
            regime2_type_change %in% "0-1",
          true = 1L,
          false = NA_integer_
        )
      ) |>
      dplyr::ungroup() -> .second_coding
  }

  glue_coding(
    .second_coding,
    .measure = .measure,
    .countries = .countries,
    .group_id = .group_id
  )

}

#' @title Cola os episódios de mudança de regime em uma única coluna
#' @inheritParams typology
#' @inheritParams ead_measures
#' @inheritParams start_epi
#' @inheritParams epi_coding
glue_coding <- \(.data, .measure, .countries, .group_id) {

  if (.measure == "autocratization") {
    .data |>
      tidyr::unite(
        aut,
        backsliding,
        democratic_breakdown,
        autocracy_deepening,
        na.rm = T,
        remove = F
      ) |>
      dplyr::na_if("") -> .third_coding

    # Por fim, eu corrijo a codificação de alguns casos identificados como retrocesso
    # democrático que correspondem a colapsos democráticos.

    rlang::expr(
      !is.na(dplyr::lead(backsliding)) &
        democratic_breakdown == "democratic breakdown"
    ) -> .backsliding_fix_lead_expr
    rlang::expr(
      !is.na(dplyr::lag(backsliding)) &
        democratic_breakdown == "democratic breakdown"
    ) -> .backsliding_fix_lag_expr

    .third_coding |>
      dplyr::group_by(!!.countries) |>
      dplyr::mutate(
        !!.group_id := dplyr::case_when(
          !!.backsliding_fix_lead_expr ~ group + 1L,
          !!.backsliding_fix_lag_expr ~ group - 1L,
          TRUE ~ group
        ),
        epi_progress = dplyr::case_when(
          !!.backsliding_fix_lead_expr ~ "end2",
          !!.backsliding_fix_lag_expr ~ "end2",
          TRUE ~ epi_progress
        )
      ) |>
      dplyr::group_by(!!.group_id) |>
      dplyr::mutate(
        aut = dplyr::case_when(
          !is.na(aut) &
            dplyr::n_distinct(aut) == 2 ~
            "democratic breakdown",
          TRUE ~ aut)) |>
      dplyr::ungroup() -> .final_coding


  } else if(.measure == "democratization") {
    .data |>
      tidyr::unite(
        dem,
        liberalization,
        democratic_transition,
        democratic_deepening,
        na.rm = T,
        remove = F) |>
      dplyr::na_if("") -> .third_coding

    # Por fim, corrijo a codificação de alguns casos identificados como liberalização
    # que correspondem a transições democráticas.

    rlang::expr(
      !is.na(dplyr::lead(liberalization)) &
        democratic_transition == "democratic transition"
    ) -> .lib_fix_lead_expr

    rlang::expr(
      !is.na(dplyr::lag(liberalization)) &
        democratic_transition == "democratic transition"
    ) -> .lib_fix_lag_expr

    .third_coding |>
      dplyr::group_by(!!.countries) |>
      dplyr::mutate(
        !!.group_id := dplyr::case_when(
          !!.lib_fix_lead_expr ~ group + 1L,
          !!.lib_fix_lag_expr ~ group - 1L,
          TRUE ~ group
        ),
        epi_progress = dplyr::case_when(
          !!.lib_fix_lead_expr ~ "end2",
          !!.lib_fix_lag_expr ~ "end2",
          TRUE ~ epi_progress
        )
      ) |>
      dplyr::group_by(!!.group_id) |>
      dplyr::mutate(
        dem = dplyr::case_when(
          !is.na(dem) &
            dplyr::n_distinct(dem) == 2 ~
            "democratic transition",
          TRUE ~ dem
        )
      ) |>
      dplyr::ungroup()
  }
}


#' @title Cria um identificar único para cada episódio de mudança de regime
#'
#' @description Cria um identificar único para cada episódio de mudança de regime.
#' @inheritParams typology
#' @inheritParams ead_measures
#' @inheritParams start_epi
#' @inheritParams epi_coding
epi_id <- \(.data, .measure, .countries, .group_id) {
  if (.measure == "autocratization") {
    .data |>
      dplyr::group_by(!!.group_id) |>
      dplyr::mutate(
        aut_id = dplyr::case_when(
          !is.na(aut) ~
            paste(
              !!.countries,
              dplyr::first(year),
              dplyr::last(year), sep = "_"
            )
        ),
        aut_id = dplyr::case_when(
          is.na(epi_progress) & regime2_type_change == "1-0" ~
            paste(
              !!.countries,
              year,
              year,
              sep = "_"
            ),
          TRUE ~ aut_id
        ),
        aut_events = dplyr::if_else(
          !is.na(pre_events) & !is.na(aut_id),
          pre_events,
          NA_character_
        )
      ) |>
      dplyr::ungroup()
  } else if (.measure == "democratization") {
    .data |>
      dplyr::group_by(!!.group_id) |>
      dplyr::mutate(
        dem_id = dplyr::case_when(
          !is.na(dem) ~
            paste(
              !!.countries, dplyr::first(year),
              dplyr::last(year),
              sep = "_"
            )
        ),
        dem_id = dplyr::case_when(
          is.na(epi_progress) &
            !is.na(dem) &
            regime2_type_change == "0-1" ~
            paste(
              !!.countries,
              year,
              year,
              sep = "_"
            ),
          TRUE ~ dem_id
        ),
        dem_events = dplyr::if_else(
          !is.na(pre_events) & !is.na(dem_id),
          pre_events,
          NA_character_
        )
      ) |>
      dplyr::ungroup()
  }
}
