#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_export_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Export")
    ),
    div(class = "text-box",
        h2("Paramètres d'export"),
        selectizeInput(inputId = ns("region"),
                       label = "Région : ", choices = NULL),
        actionButton(inputId = ns("bttn_pdf"), label = "Générer un rapport pdf"),
        actionButton(inputId = ns("bttn_xlsx"), label = "Exporter les données au format Excel")
    )

  )
}

#' export Server Functions
#'
#' @noRd
mod_export_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_export_ui("export_1")

## To be copied in the server
# mod_export_server("export_1")
