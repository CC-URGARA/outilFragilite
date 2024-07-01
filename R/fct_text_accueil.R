#' text_accueil
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom shiny div tags
#'
#' @noRd
texte_accueil <- function(type){
  if(type == "Contexte"){
    res = div(paste("Outil mis en place dans le cadre du plan d'action de consolidation de l'offre",
                  "de médecine d'urgences afin de répondre au besoin d'aide à la décision."))
  }

  if(type == "Objectif"){
    res = div(paste("Faciliter la détection d'établissements en situtation de fragilité",
                  "et l'identifiation de leviers d'action possibles."))
  }

  if(type == "Sources"){
    res = div("Les sources de données suivantes on été utilisée pour générer cet outil :",
            tags$ul(
              tags$li("Résumé de Passages aux Urgences (RPU) - 2022"),
              tags$li("Base de l'Accessibilité Potentielle Localisée (APL) - 2021"),
              tags$li("Base Statistique Annuelle des Etablissements de santé (SAE) - 2022"),
              tags$li("Programme de Médicalisation des Systèmes d’Information (PMSI) - 2021-2023"),
              tags$li("Système National de Données de Santé (SNDS) - 2021-2023"),
              tags$li("Openstreetmap")
            )
    )
  }

  if(type == "Guide"){
    res = div("")
  }

  return(res)
}
