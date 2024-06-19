#' param UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_param_ui <- function(id){
  ns <- NS(id)
  tagList(
 h1("paramètres avancés")
  )
}

#' param Server Functions
#'
#' @noRd
mod_param_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_param_ui("param_1")

## To be copied in the server
# mod_param_server("param_1")
