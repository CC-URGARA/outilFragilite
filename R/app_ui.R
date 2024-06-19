#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinipsum
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # fluidPage(
    #   navb
    #   h1("outilFragilite")
    # )
    dashboardPage(
      header = dashboardHeader(
        title = "Outil fragrilité"
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          menuItem("Accueil", icon = NULL, tabName  = "accueil"),
          menuItem("Tableau de score", icon = NULL, tabName  = "tab-de-score"),
          menuItem("Score par établissement", icon = NULL,
                   menuSubItem("Liste établissement", tabName  = "list-tab-etab"),
                   menuSubItem("Etablissement séléctionné", tabName  = "focus-tab-etab")),
          menuItem("Cartographie", icon = NULL, tabName  = "carto"),
          menuItem("Comparateur", icon = NULL, tabName  = "comparateur"),
          menuItem("Export", icon = NULL, tabName  = "export"),
          br(),br(),
          menuItem("Paramètres avancés", icon = NULL, tabName  = "param")
        )
      ),
      body = dashboardBody(
        tabItems(
          tabItem(tabName = "accueil",
                  mod_accueil_ui("accueil")),
          tabItem(tabName = "tab-de-score",
                  mod_tab_score_ui("tab_score")),
          tabItem(tabName = "list-tab-etab",
                  mod_score_list_etab_ui("score_list_etab")),
          tabItem(tabName = "focus-tab-etab",
                  mod_score_focus_etab_ui("score_focus_etab")),
          tabItem(tabName = "carto",
                  mod_carto_ui("carto")),
          tabItem(tabName = "comparateur",
                  mod_comparateur_ui("comparateur")),
          tabItem(tabName = "export",
                  mod_export_ui("export")),
          tabItem(tabName = "param",
                  mod_param_ui("param"))
        )

      ))
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "outilFragilite"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

