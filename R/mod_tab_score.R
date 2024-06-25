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
mod_tab_score_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tab_score <- renderDT({
      random_table(ncol = 5, nrow = 95) |>
        DT_theme(theme = "top_filter")
    })

  })
}

## To be copied in the UI
# mod_tab_score_ui("tab_score_1")

## To be copied in the server
# mod_tab_score_server("tab_score_1")
