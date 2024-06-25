#' carto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leaflet addTiles leafletOutput renderLeaflet
mod_carto_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Cartographie de la fragilité")
    ),
    leafletOutput(outputId = ns("carto"), height = 1200)
    )
}

#' carto Server Functions
#'
#' @noRd
mod_carto_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$carto = renderLeaflet({
      leaflet() |>
        addTiles()
    })
  })
}

## To be copied in the UI
# mod_carto_ui("carto_1")

## To be copied in the server
# mod_carto_server("carto_1")
