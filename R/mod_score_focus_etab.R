#' score_focus_etab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT renderDT DTOutput
mod_score_focus_etab_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Résumé de l'établissement : XXXX")
    ),
    div(
      tabsetPanel(type = "pills",
                  tabPanel(title = "Table score",
                           DTOutput(outputId = ns("tab_score"))),
                  tabPanel(title = "Description de l'établissement",
                           DTOutput(outputId = ns("tab_descript"))),
                  tabPanel(title = "Synthèse",
                           fluidRow(
                             box(title = "Famille de fragilité",width = 6,
                                 plotOutput(outputId = ns("plot_famille"))),
                             box(title = "Indicateurs",width = 6,
                                 plotOutput(outputId = ns("plot_indicateurs")))
                           ))
      )
    )
  )
}

#' score_focus_etab Server Functions
#'
#' @noRd
mod_score_focus_etab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tab_score <- renderDT({
      random_table(ncol = 7, nrow = 75)
      })

    output$tab_descript <- renderDT({
      random_table(ncol = 2, nrow = 10)
    })
    output$plot_indicateurs <- renderPlot(random_ggplot())
    output$plot_famille <- renderPlot(random_ggplot())

  })
}

## To be copied in the UI
# mod_score_focus_etab_ui("score_focus_etab_1")

## To be copied in the server
# mod_score_focus_etab_server("score_focus_etab_1")
