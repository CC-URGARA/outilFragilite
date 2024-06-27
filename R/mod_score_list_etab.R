#' score_list_etab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput updatePickerInput
mod_score_list_etab_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Résumé par établissement")
    ),
    div(class = "text-box",
        h2("Filtres"),
        fluidRow(
          column(width = 4,
                 pickerInput(inputId = ns("region"), label = "Région",
                             choices = NULL, multiple = TRUE,
                             options = pickerInputOptions_custom()
                 )
          ),
          column(width = 4,
                 pickerInput(inputId = ns("stjr"), label = "Statut juridique",
                             choices = NULL, multiple = TRUE,
                             options = pickerInputOptions_custom())
          ),
          column(width = 4,
                 pickerInput(inputId = ns("var_affich"), label = "Variables affichées",
                             choices = NULL, multiple = TRUE,
                             options = pickerInputOptions_custom())
          )),
        fluidRow(
          column(width = 4,
                 pickerInput(inputId = ns("dpt"),
                             label = "Département : ", choices = NULL,
                             multiple = TRUE,
                             options = pickerInputOptions_custom())
          ),
          column(width = 4,
                 actionButton(inputId = ns("bttn_maj"), label = "Mettre à jour la sélection"))
        )
    ),
    DTOutput(ns("tab_etab"))
  )
}

#' score_list_etab Server Functions
#'
#' @importFrom dplyr filter pull
#'
#' @noRd
mod_score_list_etab_server <- function(id, r_global = r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #init r_local
    r_local <- reactiveValues(
      tab_filtered = NULL
    )

    #Update des input sur la base
    ##Initialisation input + table
    observeEvent(eventExpr = r_global$res_fragilite, handlerExpr = {
      #update des input
      updatePickerInput(inputId = "region",
                        choices = unique(r_global$res_fragilite$table_score_EG_clean$Région),
                        selected = unique(r_global$res_fragilite$table_score_EG_clean$Région))

      updatePickerInput(inputId = "stjr",
                        choices = unique(r_global$res_fragilite$table_score_EG_clean$stjr),
                        selected = unique(r_global$res_fragilite$table_score_EG_clean$stjr))

      updatePickerInput(inputId = "var_affich",
                        choices = names(r_global$res_fragilite$table_score_EG_clean),
                        selected = c("EG", "RS", "Région", "dpt",
                                     "Glob_score", "Glob_score_rang",
                                     "Etablissement_score", "Etablissement_score_rang",
                                     "Zone_score", "Zone_score_rang"))

      #init de la table par défaut
      r_local$tab_filtered <- r_global$res_fragilite$table_score_EG_clean |>
        select(all_of(c("EG", "RS", "Région", "dpt",
                        "Glob_score", "Glob_score_rang",
                        "Etablissement_score", "Etablissement_score_rang",
                        "Zone_score", "Zone_score_rang")))
    })

    ##Update de la liste des dpt selon la région (réactivité directe)
    observeEvent(eventExpr = input$region, ignoreNULL = FALSE, handlerExpr = {
      list_dpt_region = tab_dpt_region |>
        filter(Région %in% input$region) |>
        pull(dpt_lab)

      updatePickerInput(inputId = "dpt",
                        choices = sort(list_dpt_region),
                        selected = sort(list_dpt_region))
    })

    #Update etab_FINESS onclick table
    observeEvent(eventExpr = input$tab_etab_cell_clicked, handlerExpr = {
      req(input$tab_etab_cell_clicked$row)#la val par défaut de cet input est truthy (named list)
      #update
      r_global$etab_FINESS <- r_local$tab_filtered$EG[input$tab_etab_cell_clicked$row]
      #Lien vers focus etab
      invoke_js(fun = "clickon", 'a[data-value="focus-tab-etab"]')
    })

    #Filtrage de la table selon les inputs on click button
    observeEvent(eventExpr = input$bttn_maj, ignoreNULL = TRUE,
                 handlerExpr = {
                   r_local$tab_filtered = r_global$res_fragilite$table_score_EG_clean |>
                     filter_table_score_EG_clean(region_searched = input$region, dpt_searched = input$dpt,
                                                 stjr_searched = input$stjr, var_affich_searched = input$var_affich)
                 })

    #Outputs
    output$tab_etab <- renderDT({
      r_local$tab_filtered |>
        DT_theme(theme = "minimal")
    })

  })
}

## To be copied in the UI
# mod_score_list_etab_ui("score_list_etab_1")

## To be copied in the server
# mod_score_list_etab_server("score_list_etab_1")
