#' @title Imprime no console os parâmetros da tipologia
#' @param .dem_parameters Veja [typology()]
#' @param .aut_parameters Veja [typology()]
#' @param .index_string Índice de democracia
ead_messages <- \(.dem_parameters, .aut_parameters, .index_string) {
  print(cli::rule())
  Sys.sleep(1)
  print(
    cli::boxx(
      "A Tipologia está pronta!",
      float = "center",
      padding = 1,
      margin = 1,
      border_col = "green")
  )
  Sys.sleep(1)
  cli::cli_alert_info("Você construiu a tipologia com as seguintes especificações:\n")
  Sys.sleep(1)
  cli::cli_ul()
  ul <- cli::combine_ansi_styles("bold")

  Sys.sleep(.3)
  cat(ul("Em episódios de autocratização:\n"))
  Sys.sleep(.3)
  cli::cli_li("threshold = {(.aut_parameters$.threshold)}")
  Sys.sleep(.3)
  cli::cli_li("keep = {(.aut_parameters$.define_epi$.keep)}")
  Sys.sleep(.3)
  cli::cli_li("stop = {(.aut_parameters$.define_epi$.stop)}")
  Sys.sleep(.3)
  cli::cli_li("cumulative = {(.aut_parameters$.define_epi$.cumulative)}")

  cat(ul("Em episódios de democratização:\n"))
  cli::cli_ul()
  Sys.sleep(.3)
  cli::cli_li("threshold = {(.dem_parameters$.threshold)}")
  Sys.sleep(.3)
  cli::cli_li("keep = {(.dem_parameters$.define_epi$.keep)}")
  Sys.sleep(.3)
  cli::cli_li("stop = {(.dem_parameters$.define_epi$.stop)}")
  Sys.sleep(.3)
  cli::cli_li("cumulative = {(.dem_parameters$.define_epi$.cumulative)}")
  Sys.sleep(.3)

  Sys.sleep(.3)
  cat(ul("Índice de democracia:\n"))
  cli::cli_li("{(.index_string)}")
}
