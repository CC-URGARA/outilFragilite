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
 
  )
}
    
#' score_list_etab Server Functions
#'
#' @noRd 
mod_score_list_etab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_score_list_etab_ui("score_list_etab_1")
    
## To be copied in the server
# mod_score_list_etab_server("score_list_etab_1")
