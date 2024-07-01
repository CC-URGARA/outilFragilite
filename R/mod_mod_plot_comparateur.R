#' mod_plot_comparateur UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mod_plot_comparateur_ui <- function(id, EG, RS){
  ns <- NS(id)
  tagList(
    box(title = paste0(RS, " (", EG, ")"), width = 6,
        collapsible = TRUE,
        plotOutput(outputId = ns("plot"))
    )
  )
}

#' mod_plot_comparateur Server Functions
#'
#' @noRd
mod_mod_plot_comparateur_server <- function(id, r_global, EG){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot = renderPlot({
      fiche_etab = r_global$res_fragilite$list_data_fiche_etab[[EG]]
      plot_famille_indic(fiche_etab$tab_score_etab)
    })
  })
}

## To be copied in the UI
# mod_mod_plot_comparateur_ui("mod_plot_comparateur_1")

## To be copied in the server
# mod_mod_plot_comparateur_server("mod_plot_comparateur_1", r_global = r_global, EG = )
