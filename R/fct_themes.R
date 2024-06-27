# Contient les fonctions de theme (ggplot, datatable) ---------------------


#' DT_theme
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
DT_theme <- function(tab, theme = "minimal", col_disable_filter = NULL){
  if(theme[1] == "minimal"){
    tab_clean <- datatable(tab,
                           options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE,
                                          pageLength = Inf, bInfo = FALSE, autoWidth = TRUE,
                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')),
                           rownames = FALSE, escape = FALSE,
                           selection = "none")
    return(tab_clean)
  }

  if(theme[1] == "top_filter"){
    tab_clean <- datatable(tab,
                           filter = "top",
                           options = list(paging = FALSE, lengthChange = FALSE,
                                          dom = "t",#retire le champ recherche mais garde les filtres
                                          pageLength = Inf, bInfo = FALSE, autoWidth = TRUE,
                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'),
                                          ordering = FALSE,
                                          columnDefs = list(
                                            list(targets = col_disable_filter, searchable = FALSE)
                                          )),
                           rownames = FALSE, escape = FALSE,
                           selection = "none")
    return(tab_clean)
  }

  return(datatable(tab))
}
