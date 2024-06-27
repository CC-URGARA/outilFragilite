#' filter_table_score_EG_clean
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter select all_of
#'
#' @noRd
filter_table_score_EG_clean <- function(tab, region_searched, dpt_searched,
                                        stjr_searched, var_affich_searched){
  tab_filtered = tab |>
    filter(Région %in% region_searched,
           dpt %in% dpt_searched,
           stjr %in% stjr_searched) |>
    arrange(Etablissement_score_rang) |>
    select(all_of(var_affich_searched))
  return(tab_filtered)
}
