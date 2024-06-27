#' accueil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_accueil_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Accueil")
    ),
    box(id = ns("contexte"), title = "Contexte",
        width = 12, collapsible = T,
        random_text(nwords = 100)),
    box(id = ns("objectif"), title = "Objectif",
        width = 12, collapsible = T,
        random_text(nwords = 15)),
    box(id = ns("sources-donnees"), title = "Sources de données",
        width = 12, collapsible = T,
        tags$ul(
          tags$li("a"),
          tags$li("b"),
          tags$li("c"),
          tags$li("d"),
          tags$li("e"))),
    box(id = ns("mode-emploi"), title = "Mode d'emploi",
        width = 12, collapsible = T,
        h3("titre 1"),
        p(random_text(nwords = 150)),
        h3("titre 2"),
        p(random_text(nwords = 120))),
  )
}

#' accueil Server Functions
#'
#' @noRd
mod_accueil_server <- function(id, r_global = r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_accueil_ui("accueil_1")

## To be copied in the server
# mod_accueil_server("accueil_1")
