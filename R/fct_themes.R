# Contient les fonctions de theme (ggplot, datatable) ---------------------


#' DT_theme
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom DT formatStyle datatable
#'
#' @noRd
DT_theme <- function(tab, theme = "minimal", col_disable_filter = NULL, clickable = FALSE){
  #Paramètres par défaut
  searching = FALSE
  paging = TRUE
  lengthChange = FALSE
  pageLength = 50
  bInfo = TRUE
  autoWidth = TRUE
  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')#a tester en ligne
  rownames = FALSE
  escape = FALSE
  selection = "none"
  columnDefs = list()
  filter = "none"
  dom = "lftipr"
  ordering = TRUE

  if(theme[1] == "top_filter"){
    filter = "top"
    dom = "t"
    ordering = FALSE
    columnDefs = list(
      list(targets = col_disable_filter, searchable = FALSE)
    )
  }

  tab_clean <- datatable(tab,
                         filter = filter,
                         options = list(searching = searching, paging = paging, lengthChange = lengthChange,
                                        pageLength = pageLength, bInfo = bInfo, autoWidth = autoWidth,
                                        language = language, columnDefs = columnDefs, dom = dom, ordering = ordering),
                         rownames = rownames, escape = escape,
                         selection = selection)

  if(clickable){
    tab_clean = tab_clean |>
      formatStyle(columns = names(tab), cursor = "pointer")
  }

  return(tab_clean)
}
