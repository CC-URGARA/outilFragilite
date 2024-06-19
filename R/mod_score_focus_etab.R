#' score_focus_etab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_score_focus_etab_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' score_focus_etab Server Functions
#'
#' @noRd 
mod_score_focus_etab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_score_focus_etab_ui("score_focus_etab_1")
    
## To be copied in the server
# mod_score_focus_etab_server("score_focus_etab_1")
