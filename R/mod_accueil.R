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
    div(id = ns("contexte"), class = "text-box",
        h2("Contexte :"),
        p(random_text(nwords = 100)),
        style = "background:#d9d8d8;"),#grey
    div(id = ns("objectif"), class = "text-box",
        h2("Objectif :"),
        p(random_text(nwords = 15)),
        style = "background:#aecf38;"),#green
    div(id = ns("sources-donnees"), class = "text-box",
        h2("Sources de données :"),
        tags$ul(
          tags$li("a"),
          tags$li("b"),
          tags$li("c"),
          tags$li("d"),
          tags$li("e")),
        style = "background:#1c6fad;"),#blue
    div(id = ns("mode-emploi"), class = "text-box",
        h2("Mode d'emploi :"),
        h3("titre 1"),
        p(random_text(nwords = 150)),
        h3("titre 2"),
        p(random_text(nwords = 120)),
        style = "background:#93cddd;")#light blue
  )
}

#' accueil Server Functions
#'
#' @noRd
mod_accueil_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_accueil_ui("accueil_1")

## To be copied in the server
# mod_accueil_server("accueil_1")
