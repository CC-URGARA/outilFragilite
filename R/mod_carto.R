#' carto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_carto_ui <- function(id){
  ns <- NS(id)
  tagList(
 h1("cartographie")
  )
}

#' carto Server Functions
#'
#' @noRd
mod_carto_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_carto_ui("carto_1")

## To be copied in the server
# mod_carto_server("carto_1")
