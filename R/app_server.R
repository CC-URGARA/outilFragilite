#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  mod_accueil_server("accueil")
  mod_tab_score_server("tab_score")
  mod_score_list_etab_server("score_list_etab")
  mod_score_focus_etab_server("score_focus_etab")
  mod_carto_server("carto")
  mod_comparateur_server("comparateur")
  mod_export_server("export")
  mod_param_server("param")
}
