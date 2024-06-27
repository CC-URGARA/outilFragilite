#' tab_score UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom DT DTOutput renderDT datatable
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_score_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Tableau de score")
    ),
    p("Le tableau suivant décrit pour chaque indicateur les classes définies et le score associé."),
    DTOutput(ns("tab_score"))
  )
}

#' tab_score Server Functions
#'
#' @noRd
mod_tab_score_server <- function(id, r_global = r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tab_score <- renderDT({
      r_global$res_fragilite$table_score_clean |>
        DT_theme(theme = "top_filter", col_disable_filter = c(1, 2))
    })

  })
}

## To be copied in the UI
# mod_tab_score_ui("tab_score_1")

## To be copied in the server
# mod_tab_score_server("tab_score_1")
