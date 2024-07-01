#' comparateur UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput updatePickerInput
mod_comparateur_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Comparateur d'établissements")
    ),

    div(class = "text-box",
        h2("Filtres"),
        fluidRow(
          column(width = 4,
                 pickerInput(inputId = ns("EG"),
                             label = "FINESS géographique :",
                             choices = NULL, multiple = TRUE,
                             options = pickerInputOptions_custom()))
        ),
        fluidRow(
          column(width = 4,
                 pickerInput(inputId = ns("RS"),
                             label = "Raison sociale :",
                             choices = NULL, multiple = TRUE,
                             options = pickerInputOptions_custom()))
        ),
        fluidRow(
          column(width = 4,
                 actionButton(inputId = ns("bttn_comparaison"), label = "Comparer les établissements"))
        )
    ),
    fluidRow(
      uiOutput(outputId = ns("boxes_comp"))
    )
  )
}

#' comparateur Server Functions
#'
#' @noRd
#' @importFrom purrr walk
mod_comparateur_server <- function(id, r_global = r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #Initialisation de r_local
    r_local = reactiveValues(
      list_EG = NULL,
      list_RS = NULL,
    )

    #Update des input avec les FINESS et RS dans la base du score
    observeEvent(eventExpr = r_global$res_fragilite, handlerExpr = {
      order = order(r_global$res_fragilite$table_score_EG_clean$EG)#uniformise l'ordre

      updatePickerInput(inputId = "EG",
                        choices = r_global$res_fragilite$table_score_EG_clean$EG[order])
      updatePickerInput(inputId = "RS",
                        choices = r_global$res_fragilite$table_score_EG_clean$RS[order])
    })

    #Update des input RS/EG pour correspondre l'un à l'autre
    ##EG->RS
    observeEvent(eventExpr = input$EG, handlerExpr = {
      RS_selected = r_global$res_fragilite$table_score_EG_clean$RS[res_fragilite$table_score_EG_clean$EG %in% input$EG]
      if(!setequal(RS_selected, input$RS)){
        #Update uniquement si différent (évite le ping pong) EG->RS->EG
        #qui referme le menu à chaque click sinon
        updatePickerInput(inputId = "RS",
                          selected = RS_selected)
      }

    })

    ##RS->EG
    observeEvent(eventExpr = input$RS, handlerExpr = {
      # browser()
      EG_selected = r_global$res_fragilite$table_score_EG_clean$EG[res_fragilite$table_score_EG_clean$RS %in% input$RS]
      if(!setequal(EG_selected, input$EG)){
        #Update uniquement si différent (évite le ping pong) RS->EG->RS
        #qui referme le menu à chaque click sinon
        updatePickerInput(inputId = "EG",
                          selected = EG_selected)
      }
    })

    ##Update de r_local sur click bouton
    observeEvent(eventExpr = input$bttn_comparaison, handlerExpr = {
      r_local$list_EG = input$EG
      r_local$list_RS = input$RS
    })

    #Outputs
    ##UI output qui appel UI plot comparaison
    output$boxes_comp <- renderUI({
      req(r_local$list_EG)
      tagList(
        lapply(seq_len(length(r_local$list_EG)), function(i){
          mod_mod_plot_comparateur_ui(id = ns(paste0("mod_plot_comparateur_", r_local$list_EG[i])),
                                      EG = r_local$list_EG[i], RS = r_local$list_RS[i])
        })
      )
    })
    ##Appel un serveur pour chaque UI
    #Note : Un walk est utilisé à la place d'une boucle "for" car avec la boucle
    #seule la dernière valeur (i max) est utilisé dans chaque serveur pour EG.
    observeEvent(eventExpr = input$bttn_comparaison, handlerExpr = {
      walk(seq_len(length(r_local$list_EG)), function(i){#une boucle for ne fonctionne pas, la dernière valeur est tilisée dans chaque serveur
        mod_mod_plot_comparateur_server(paste0("mod_plot_comparateur_", r_local$list_EG[i]),
                                        r_global = r_global, EG = r_local$list_EG[i])
      })
    })
  })
}

## To be copied in the UI
# mod_comparateur_ui("comparateur_1")

## To be copied in the server
# mod_comparateur_server("comparateur_1")
