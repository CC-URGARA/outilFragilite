#' tab_score UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_score_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' tab_score Server Functions
#'
#' @noRd 
mod_tab_score_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_tab_score_ui("tab_score_1")
    
## To be copied in the server
# mod_tab_score_server("tab_score_1")
