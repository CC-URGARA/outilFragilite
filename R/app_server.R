#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom golem invoke_js
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  #Javascript hide etab focus tant qu'un étab n'a pas été selectionné
  invoke_js(fun = "hide", 'a[data-value="focus-tab-etab"]')

  observeEvent(eventExpr = r_global$etab_FINESS, handlerExpr = {
    invoke_js(fun = "show", 'a[data-value="focus-tab-etab"]')
  })

  # initialisation du r_global
  r_global <- reactiveValues(
    tab_indic = tab_indic,
    tab_indic_control = tab_indic_control,
    tab_context = tab_context,
    tab_context_control = tab_context_control,
    res_fragilite = res_fragilite,
    etab_FINESS = NULL
    )

  #Appel serveurs
  mod_accueil_server("accueil", r_global = r_global)
  mod_tab_score_server("tab_score", r_global = r_global)
  mod_score_list_etab_server("score_list_etab", r_global = r_global)
  mod_score_focus_etab_server("score_focus_etab", r_global = r_global)
  mod_carto_server("carto", r_global = r_global)
  mod_comparateur_server("comparateur", r_global = r_global)
  mod_export_server("export", r_global = r_global)
  mod_param_server("param", r_global = r_global)
}
