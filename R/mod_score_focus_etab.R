#' score_focus_etab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT renderDT DTOutput
mod_score_focus_etab_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "header",
        uiOutput(outputId = ns("page_title"))
    ),
    div(
      h3("Données à afficher :"),
      tabsetPanel(type = "pills",
                  tabPanel(title = "Table score",
                           DTOutput(outputId = ns("tab_score_etab"))),
                  tabPanel(title = "Description de l'établissement",
                           DTOutput(outputId = ns("tab_info_etab"))),
                  tabPanel(title = "Synthèse",
                           fluidRow(
                             box(title = "Famille de fragilité",width = 12,
                                 plotOutput(outputId = ns("plot_famille")))),
                           fluidRow(
                             box(title = "Indicateurs",width = 12,
                                 plotOutput(outputId = ns("plot_indicateurs")),
                                 p("*Le score des indicateur est affiché en pourcentage du score maximal"))
                           ))
      )
    )
  )
}

#' score_focus_etab Server Functions
#'
#' @noRd
mod_score_focus_etab_server <- function(id, r_global = r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #initialisation du r_local
    r_local <- reactiveValues(
      fiche_etab = NULL
    )

    #Maj de la fiche etab selon FINESS_etab
    observeEvent(eventExpr = r_global$etab_FINESS, handlerExpr = {
      r_local$fiche_etab = r_global$res_fragilite$list_data_fiche_etab[[r_global$etab_FINESS]]
    })

    output$tab_score_etab <- renderDT({
      r_local$fiche_etab$tab_score_etab |>
        DT_theme(theme = "minimal")
    })

    output$tab_info_etab <- renderDT({
      r_local$fiche_etab$tab_info_etab |>
        DT_theme(theme = "minimal")
    })

    output$plot_indicateurs <- renderPlot({
      plot_indic(r_local$fiche_etab$tab_score_etab)
    })
    output$plot_famille <- renderPlot({
      plot_famille_indic(r_local$fiche_etab$tab_score_etab)
    })

    output$page_title <- renderUI({
      h1(paste0("Etablissement : ", r_local$fiche_etab$nom_etab))
    })

  })
}

## To be copied in the UI
# mod_score_focus_etab_ui("score_focus_etab_1")

## To be copied in the server
# mod_score_focus_etab_server("score_focus_etab_1")
