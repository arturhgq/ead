## code to prepare `DATASET` dataset goes here

here::here(
  "data-raw",
  "vDEM_v12.csv"
) |>
  read.csv() |>
  dplyr::select(
    country_name,
    year,
    regime = v2x_regime,
    reg_end_type = v2regendtype,
    reg_info = v2reginfo,
    elect_win_ord = v2elasmoff_ord, # Election assume office
    elect_reg = v2x_elecreg, # Electoral regime index
    #HOG_d_elect = v2ex_elechog, # HOG directly elected
    #HOS_d_elect = v2ex_elechos, # HOS directly elected
    HOG_appointment = v2expathhg, # HOS appointment in practice
    HOS_appointment = v2expathhs, # HOG appointment in practice
    IDE = v2x_polyarchy,  # Electoral democracy index
    IDL = v2x_libdem, # Liberal democracy index
    IDI = v2x_egaldem, # Egalitarian democracy index
    IDP = v2x_partipdem, # Participatory democracy index
    IDD = v2x_delibdem, # Deliberative democracy index
    IDE_freexp = v2x_freexp_altinf, # freedom of expression
    IDE_freeassoc = v2x_frassoc_thick, # freedom of association thick
    IDE_suffr = v2x_suffr, # suffrage
    IDE_freefair = v2xel_frefair, # clean elections
    IDE_freefair_ord = v2elfrfair_ord,
    IDE_elecoff = v2x_elecoff, # elected officials
    LBC = v2x_liberal, # Liberal component index
    IDL_rights = v2xcl_rol, # equality before the law and individual liberties
    IDL_jconstraits = v2x_jucon, # judicial constraints on the executive
    IDL_lconstraits = v2xlg_legcon # legislative constraints on the executive
  ) |>
  dplyr::mutate(
    regime2 = dplyr::recode(
      regime,
      `0` = 0,
      `1` = 0,
      `2` = 1,
      `3` = 1
    ),
    sch_elections = dplyr::case_when(
      IDE_freefair_ord %in% c(3,4) &
        IDE_suffr == 1 &
        elect_reg == 1 &
        elect_win_ord == 2
      ~ "democratic elections",
      IDE_freefair_ord %in% c(2,1,0) &
        elect_reg == 1 ~ "non-democratic elections",
      IDE_suffr < 1 &
        elect_reg == 1 ~ "non-democratic elections",
      elect_win_ord %in% c(0,1) &
        elect_reg == 1 ~ "non-democratic elections",
      elect_reg == 0 ~ "unforeseen elections"
    )
  ) |>
  dplyr::group_by(country_name) |>
  dplyr::mutate(
    sch_elections = zoo::na.locf(sch_elections, maxgap = 5, na.rm = FALSE)
  ) |>
  dplyr::ungroup() -> vdem

ead::ead_typology(
  vdem,
  .countries = country_name,
  .year = year,
  .typology_p = regime,
  .typology_d = regime2,
  .reg_info = reg_info,
  .reg_end_type = reg_end_type,
  .index = IDE
) -> data

usethis::use_data(vdem, data, overwrite = TRUE)



# Executive electoral regime index (A) (v2xex_elecreg)
# Chief executive no longer elected (D) (v2x_hosinter)
# HOS = HOG? (v2exhoshog)
# HOG appointed by HOS (D) (v2ex_hosconhog)
