#' @title Cria a tipologia **Episódios de Autocratização e Democratização**
#'
#' @description `r lifecycle::badge("stable")`
#'
#' \code{typology} detecta e classifica seis tipos de mudança de regime.
#' O algorítmo considera explicitamente que episódios de mudança de regime
#' são interpostos por eventos de reação.
#'
#' Para episódios de autocratização, codifica-se casos de:
#'   * \code{Aprofundamento autoritário}: indica que instituições políticas em
#'   autocracias perderam atributos democráticos
#'
#'   * \code{Colapso democrático}: designa casos de substituição de regimes
#'   democráticos por regimes autoritários
#'
#'   * \code{Retrocesso democrático}: indica episódios de colapso democrático
#'    malsucedidos, isto é, casos em que a coalizão de atores antidemocráticos
#'    não obtiveram sucesso em romper o regime.
#'
#' Para episódios de democratização, codifica-se casos de:
#'   * \code{Aprofundamento democrático}: indica que novos atributos democráticos
#'   foram incorporados em instituições políticas.
#'
#'   * \code{Transição democrática}: designa casos de substituição de regimes
#'   autocráticos por regimes democráticos
#'
#'   * \code{Liberalização}: indica episódios de transições democráticas
#'   malsucedidas, isto é, casos em que a coalizão de atores democráticos
#'   não obtiveram sucesso em encerrar regimes autoritários.
#'
#' A tipologia baseia-se na generalização do terceiro pressuposto da proposta
#' teórica que desenvolvi no capítulo três de minha dissertação para o exame
#' de casos de retrocesso democrático.
#'
#' @param .data dados do projeto *"Varieties of Democracy"*.
#' @param ... Variáveis que devem ser especificadas
#'
#'   * \code{.year} = série histórica
#'   * \code{.countries} = países
#'   * \code{.typology_d} = classificação dicotômica
#'   * \code{.typology_p} = classificação policotômica
#'   * \code{.reg_info} = nome do regime, quando ele começa e quando ele termina
#'   * \code{.reg_end_type} = nome do processo mais importante que resultou no
#'    colapso do regime
#'
#' @param .index Índice de democracia usado para estimar os episódios
#' de mudança de regime.
#'
#'   * \code{IDE}: Índice de Democracia Eleitoral
#'   * \code{IDL}: Índice de Democracia Liberal
#'   * \code{IDI}: Índice de Democracia Igualitária
#'   * \code{IDP}: Índice de Democracia Participativa
#'   * \code{IDD}: Índice de Democracia Deliberativa
#'
#' Por definição, eu adoto o Índice de Democracia Eleitoral (IDE), que mensura
#' os pré-resiquitos institucionais da democracia, conforme foi proposto por
#' Dahl (1971) e revisto em Dahl(1989), Dahl (1998) e em Coppedge et al.(2020).
#'
#' IDE corresponde ao princípio, valor ou aspecto eleitoral da democracia.
#' Os demais índices agregam o Índice de Democracia Eleitoral a componentes
#' específicos da democracia. Por exemplo, o Índice de Democracia Liberal
#' equilave ao: IDE + componente liberal da democracia.
#'
#' Para mais informações, consulte o guia do usuário do projeto v-DEM
#' (Coppedge et al., 2020) e o codebook do banco de dados (Coppedge et al., 2022).
#'
#' @param .aut_parameters,  Veja os parâmetros \code{.threshold},
#' \code{.define_epi} e \code{.events}
#' @param .dem_parameters Veja os parâmetros \code{.threshold},
#' \code{.define_epi} e \code{.events}
#' @param .threshold Define o valor mínimo de variação no
#' índice de democracia para determinar o ínicio de processos de mudança de regime.
#' \code{.threshold} aceita valores entre 0 e 1.
#'
#' Por definição, 0.01(1%) em episódios de democratização e -0.01(-1%) em
#' episódios de autocratização.
#'
#' @param .define_epi Lista de parâmetros que delimitam a extensão dos episódios
#' de mudança de regime.
#'
#'   * \code{.stop}: intervalo temporal que encerra os episódios de
#'   mudança de regime (em anos).
#'
#'   Por definição, quando a variação no índice de democracia for < 0.01
#'   por três anos consecutivos em casos de democratização, e > -0.01 em casos
#'   de autocratização, \code{.stop} encerra os episódios de mudança de
#'   regime. Durante os episódios, variações menores que o \code{.threshold}
#'   indicam: 1) eventos de reação às alterações no status quo ou 2) a escasez de
#'   recursos para a implementação de novas alterações no regime.
#'
#'   * \code{.keep}: número de eventos de estagnação ou reação aceitos
#'   durante episódios de mudança. Por definição, dois eventos.
#'
#'   * \code{.cumulative}: define o valor acumulado mínimo para codificar
#'   um episódio de mudança de regime.
#'
#'   Por definição, 10% (0,1) em democratizações e -10% (-0,1) em autocratizações.
#'
#'   * \code{.after}: quantidade máxima de anos em que o índice de democracia
#'    poderá não atingir o ponto de corte e ainda assim ter esses anos incluídos
#'    no calculo dos episódios de mudança de regime.
#'    Depende de \code{.after_value}.
#'
#'   * \code{.before}: quantidade máxima de anos em que o índice de democracia
#'   poderá não atingir o ponto de corte, mas ainda assim ter esses anos incluídos
#'   no calculo dos episódios de mudança de regime.
#'   Depende de \code{.before_value}.
#'
#'   * \code{.after_value}: variação mínima, inferior ao \code{.threshold},
#'   para extensão do período de encerramento dos episódios.
#'
#'   * \code{.before_value}: variação mínima, inferior ao \code{.threshold},
#'    para extensão do período de abertura dos episódios.
#'
#' @param .events define os eventos de avanço, estagnação ou reação.
#'    * \code{.forward}:
#'
#'    por definição \code{-0.0099 <= .forward <= -0.006} em episódios
#'    de autocratização e \code{0.006 <= .forward <= 0.0099} em episódios de democratização.
#'    \code{.forward} especifica variações menores, mas próximas ao \code{threshold}.
#'
#'    * \code{.stagnant}:
#'
#'    por definição \code{-0.0059 <= .stagnant <= 0.0059} em episódios de
#'    autocratização e de democratização. \code{.stagnant} especifica eventos de estagnação.
#'
#'    * \code{.backward}:
#'
#'    por definição, \code{.backward < 0.0059} em episódios de autocratização
#'    e \code{.backward < - 0.0059} em episódios de democratização. \code{.backward}, por fim,
#'    indica eventos de reação.
#'
#' @param .row_update desconsidera as transições que resultam de variações anuais em
#' \code{IDE} contidas no intervalo \[-0.05; 0.05\] para todos os casos em que
#' tais episódios sucedem imediatamente um retorno ao regime anterior.
#' Em outros termos, transições de regime com duração de um ano, fruto de variações
#' de |5%| em \code{IDE} são descartadas. Argumento que essas transições
#' devem ser tratadas como ruídos, pois o próprio conceito de regime político
#' pressupõe um mínimo de continuidade no tempo (Schedler, 2013).
#' Por definição, \code{TRUE}.
#'
#' Essa operação reduz o número de episódios identificados pela tipologia \code{row}.
#'
#' @param .all_vars Se \code{TRUE}, retorna dois bancos de dados adicionais com
#' todas as variáveis usadas para construir os episódios de democratização e
#' autocratização.
#' @param .add_coups Adiciona episódios de golpes e autogolpes à tipologia
#' @note
#'
#' A tipologia usa dados do projeto Varieties of Democracy (v-DEM) e é inspirada
#' nos estudos de Lührmann et al. (2018), Lührmann e Lindberg (2019),
#' Mainwaring e Bizzaro (2019), Wilson et al. (2020), Maerz et al. (2021) e
#' Haggard e Kaufman (2021); especialmente nas tipologias *Regimes of the World* (row)
#' e *Episodes of Regime Transformation* (ERT).
#'
#' @examples
#' library(ead)
#'
#' typology(
#'   vdem,
#'   .countries = country_name,
#'   .year = year,
#'   .typology_p = regime,
#'   .typology_d = regime2,
#'   .reg_info = reg_info,
#'   .reg_end_type = reg_end_type,
#'   .index = IDE,
#'   .add_coups = TRUE,
#'   .all_vars = TRUE
#' ) -> typology
#' @export
#'
  typology <- \(
    .data,
    ...,
    .index = IDE,
    .aut_parameters = list(
      .threshold = -0.01,
      .define_epi = list(
      .stop = 3,
      .keep = 2,
      .cumulative = -.10,
      .before = 1,
      .after = 1,
      .before_value = -0.009,
      .after_value = -0.009
      ),
      .events = list(
        .forward = c(-0.0099, -0.0060),
        .stagnant = c(-0.0059, 0.0059),
        .backward = 0.0059
        )
      ),
    .dem_parameters = list(
      .threshold = 0.01,
      .define_epi = list(
        .stop = 3,
        .keep = 2,
        .cumulative = .10,
        .before = 1,
        .after = 1,
        .before_value = 0.009,
        .after_value = 0.009
      ),
      .events = list(
        .forward = c(0.0060, 0.0099),
        .stagnant = c(-0.0059, 0.0059),
        .backward = -0.0059
      )
    ),
    .row_update = TRUE,
    .all_vars = FALSE,
    .add_coups = TRUE
  ) {

    dtools::check_r_pipe(.rversion = TRUE)

    Sys.sleep(1)
    epi_build(
      .data,
      ...,
      .index = !!rlang::ensym(.index),
      .threshold = .aut_parameters[['.threshold']],
      .define_epi= .aut_parameters[['.define_epi']],
      .events = .aut_parameters[['.events']],
      .measure = "autocratization",
      .row_update = .row_update
      ) -> .aut
    Sys.sleep(1)

    epi_build(
      .data,
      ...,
      .index = !!rlang::ensym(.index),
      .threshold = .dem_parameters[['.threshold']],
      .define_epi = .dem_parameters[['.define_epi']],
      .events = .dem_parameters[['.events']],
      .measure = "democratization",
      .row_update = .row_update
      ) -> .dem
    Sys.sleep(1)

    rlang::quos(..., .index = !!rlang::enquo(.index)) -> .quo_args
    purrr::map_chr(.quo_args, rlang::as_label) -> .vars_label

    paste0("change","_",.vars_label[['.index']]) -> .change_name

    c("year",
      .vars_label,
      "IDE",
      "change_IDE",
      .change_name,
      "sch_elections",
      "regime_subtype_change",
      "regime2_type_change",
      "ROW_regime_change_aut",
      "ROW_regime_change_dem",
      "aut",
      "aut_events",
      "aut_id",
      "dem",
      "dem_events",
      "dem_id",
      use.names = FALSE
    ) -> .vars

    if (.add_coups) {
      c(.vars, "reg_info", "reg_end_type") -> .vars
    }

    select_ <- function(x) {
      x |>
      dplyr::select(
        dplyr::any_of(.vars)
      )
    }

    list(.dem, .aut) |>
      purrr::map(select_) |>
      purrr::reduce(dplyr::left_join) |>
      tidyr::unite(
        ROW_regime_change,
        ROW_regime_change_aut,
        ROW_regime_change_dem,
        na.rm = T
      ) |>
      tidyr::unite(ead, dem, aut, remove = F, na.rm = T) |>
      dplyr::relocate(.after = aut_id, ead) |>
      dplyr::na_if("") |>
      suppressMessages() -> .ead

    if (.add_coups) {
      .ead |>
        add_coups(
          .reg_info = !!rlang::sym(.vars_label[[".reg_info"]]),
          .reg_end_type = !!rlang::sym(.vars_label[[".reg_end_type"]])
        ) -> .ead
    }

    .ead |>
      dplyr::filter(
        !!.quo_args[[".year"]] >= 1900
      ) -> .ead

    .aut |>
      dplyr::filter(
        !!.quo_args[[".year"]] >= 1900
      ) -> .aut

    .dem |>
      dplyr::filter(
        !!.quo_args[[".year"]] >= 1900
      ) -> .dem

    ead_messages(
      .dem_parameters,
      .aut_parameters,
      .vars_label[['.index']]
    )

    if (.all_vars) {
      list(
        .aut = .aut,
        .dem = .dem,
        .ead = .ead
      )
    } else{
      .ead
    }

  }






