#' score_list_etab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_score_list_etab_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Résumé par établissement")
    ),
    div(class = "text-box",
        h2("Filtres"),
        fluidRow(
          column(width = 4,
                 selectizeInput(inputId = ns("region"),
                                label = "Région : ", choices = NULL)),
          column(width = 4,
                 selectizeInput(inputId = ns("stjr"),
                                label = "Statut juridique : ", choices = NULL)),
          column(width = 4,
                 selectizeInput(inputId = ns("var_affich"),
                                label = "Variables affichées", choices = NULL))
        ),
        fluidRow(
          column(width = 4,
                 selectizeInput(inputId = ns("dpt"),
                                label = "Département : ", choices = NULL)),
          column(width = 4,
                 actionButton(inputId = ns("bttn_maj"), label = "Mettre à jour la sélection"))
        )
    ),
    DTOutput(ns("tab_etab"))
  )
}

#' score_list_etab Server Functions
#'
#' @noRd
mod_score_list_etab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$tab_etab <- renderDT({
      random_table(ncol = 5, nrow = 95) |>
        DT_theme(theme = "minimal")
    })

  })
}

## To be copied in the UI
# mod_score_list_etab_ui("score_list_etab_1")

## To be copied in the server
# mod_score_list_etab_server("score_list_etab_1")
