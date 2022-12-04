#' vDEM
#'
#' O banco de dados \code{vdem} contém variáveis originais e tratadas do projeto
#'  *Varieties of Democracy*
#'
#' @format Lista de variáveis
#' \describe{
#'   \item{country_name}{nome do país}
#'   \item{year}{ano}
#'   \item{regime}{Classificação ordinal de regimes políticos (RoW)}
#'   \item{regime2}{Classificação dicotômica de regimes políticos (RoW)}
#'   \item{reg_end_type}{Nome do processo mais importante que resultou no colapso do regime}
#'   \item{reg_info}{Nome do regime, quando ele começa e quando ele termina}
#'   \item{IDE}{Índide de Democracia Eleitoral}
#'   \item{IDL}{Índide de Democracia Liberal}
#'   \item{IDI}{Índide de Democracia Igualitária}
#'   \item{IDP}{Índide de Democracia Participativa}
#'   \item{IDD}{Índice de Democracia Deliberativa}
#' }
#' @source Varieties of Democracy (2022)
"vdem"


#' Tipologia
#'
#' Banco de dados da tipologia
"data"
#' @format Lista de variáveis
#' \describe{
#'   \item{country_name}{nome do país}
#'   \item{year}{ano}
#'   \item{regime}{Classificação ordinal de regimes políticos (RoW)}
#'   \item{regime2}{Classificação dicotômica de regimes políticos (RoW)}
#'   \item{reg_info}{Nome do regime, quando ele começa e quando ele termina}
#'   \item{IDE}{Índide de Democracia Eleitoral}
#'   \item{IDL}{Índide de Democracia Liberal}
#'   \item{IDI}{Índide de Democracia Igualitária}
#'   \item{IDP}{Índide de Democracia Participativa}
#'   \item{change_IDE}{Variação anual do Índice de Democracia Eleitoral}
#'   \item{sch_elections}{Expectativa de eleições}
#'   \item{regime_subtype_change}{Mudança de subtipo de regime}
#'   \item{regime2_type_change}{Mudança de regime}
#'   \item{ROW_regime_change}{Mudança de regime, segundo ROW}
#'   \item{dem}{Episódios de democratização codificados}
#'   \item{dem_events}{Eventos em episódios de democratização}
#'   \item{dem_id}{Identificador do episódio de democratização}
#'   \item{aut}{Episódios de autocratização codificados}
#'   \item{aut_events}{Eventos em episódios de autocratização}
#'   \item{aut_id}{Identificador do episódio de autocratização}
#'   \item{coups}{Golpes de estado e autogolpes}
#' }
