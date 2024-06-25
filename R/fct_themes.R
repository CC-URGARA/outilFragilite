# Contient les fonctions de theme (ggplot, datatable) ---------------------


#' DT_theme
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
DT_theme <- function(tab, theme = "minimal"){
  if(theme[1] == "minimal"){
    tab_clean <- datatable(tab,
                           options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE,
                                          pageLength = Inf, bInfo = FALSE, autoWidth = TRUE),
                           rownames = FALSE, escape = FALSE,
                           selection = "none")
    return(tab_clean)
  }

  if(theme[1] == "top_filter"){
    tab_clean <- datatable(tab,
                           filter = "top",
                           options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE,
                                          pageLength = Inf, bInfo = FALSE, autoWidth = TRUE),
                           rownames = FALSE, escape = FALSE,
                           selection = "none")
    return(tab_clean)
  }

  return(datatable(tab))
}
