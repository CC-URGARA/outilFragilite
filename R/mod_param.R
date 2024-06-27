#' param UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput renderDT
mod_param_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Paramètres avancés")
    ),
    div(id = ns("warning-param"), class = "text-box",
        h2("Attention "),
        p("Cet onglet est réservé aux utilisateurs avancés. Une mauvaise utilisation peut rendre invalides les résultats de l'outil."),
        style = "background:#e18d8d;"),#light red
    DTOutput(outputId = ns("tab_param_indic"))
  )
}

#' param Server Functions
#'
#' @noRd
mod_param_server <- function(id, r_global = r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tab_param_indic <- renderDT(expr = {
      datatable(shinipsum::random_table(nrow = 10, ncol = 4), editable = list(target = "column", disable = list(columns = 0:3)),
                options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE,
                               pageLength = Inf, bInfo = FALSE, autoWidth = TRUE),
                rownames = FALSE, escape = TRUE,
                selection = "none")
    })

  })
}

## To be copied in the UI
# mod_param_ui("param_1")

## To be copied in the server
# mod_param_server("param_1")
