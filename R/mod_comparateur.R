#' comparateur UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comparateur_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Comparateur d'établissements")
    ),

    div(class = "text-box",
        h2("Filtres"),
        fluidRow(
          column(width = 4,
                 selectizeInput(inputId = ns("finess"),
                                label = "FINESS :", choices = NULL))
        ),
        fluidRow(
          column(width = 4,
                 selectizeInput(inputId = ns("RS"),
                                label = "Raison sociale :", choices = NULL))
        ),
        fluidRow(
          column(width = 4,
                 actionButton(inputId = "bttn_comparaison", label = "Comparer les établissements"))
        )
    ),
    fluidRow(
      box(title = "Etab X", width = 6,
          plot(1)),
      box(title = "Etab X", width = 6,
          plot(1)),
      box(title = "Etab X", width = 6,
          plot(1)),
      box(title = "Etab X", width = 6,
          plot(1))
    )
  )
}

#' comparateur Server Functions
#'
#' @noRd
mod_comparateur_server <- function(id, r_global = r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_comparateur_ui("comparateur_1")

## To be copied in the server
# mod_comparateur_server("comparateur_1")
