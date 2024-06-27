#' plot_famille_indic
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom stringr str_detect str_extract str_wrap
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous scale_x_discrete scale_fill_manual theme element_text coord_cartesian
#'
#' @noRd
plot_famille_indic <- function(tab_score_etab){
  tab_plot = tab_score_etab |>
    filter(bg %in% "Famille") |>
    transmute(
      Famille = str_extract(Indicateur, "(?<=Total : ).*") |>
        str_wrap(width = 15),
      Score = as.numeric(Score),
      Type = Type
    )

  plot = ggplot(tab_plot, aes(x = Famille, y = Score, fill = Type)) +
    geom_col() +
    scale_y_continuous(breaks = seq(0, 100, by = 10)) +
    scale_x_discrete(name = "Famille de fragilité") +
    scale_fill_manual(name = "Type de fragilité :",
                      values = c("#000091", "#e1000f")) +
    coord_cartesian(ylim = c(0, 100)) +
    theme_pubclean()
  return(plot)
}

#' plot_indic
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom stringr str_detect str_extract str_wrap
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous scale_x_discrete scale_fill_discrete theme element_text coord_cartesian
#' @importFrom ggpubr theme_pubclean
#' @importFrom dplyr filter group_by mutate transmute
#'
#' @noRd
plot_indic <- function(tab_score_etab){
  tab_plot = tab_score_etab |>
    filter(bg %in% "Indicateur") |>
    group_by(Famille) |>
    mutate(max_score = 100/n()) |>
    transmute(
      Indicateur = fct_inorder(str_wrap(fct_abrv_string_3(Indicateur), width = 15)),
      Score = round(as.numeric(Score)/max_score*100),
      Famille = Famille
    )

  plot = ggplot(tab_plot, aes(x = Indicateur, y = Score, fill = Famille)) +
    geom_col() +
    scale_y_continuous(name = "Score (%)", breaks = seq(0, 100, by = 10)) +
    scale_x_discrete(name = "Indicateur") +
    scale_fill_discrete(name = "Famille de fragilité :") +
    coord_cartesian(ylim = c(0, 100)) +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
  return(plot)
}




fct_abrv_string_3 <- function(string){
  str_replace_all(string, "(?<=[^\\s]{3})\\w+", ".")
}
fct_abrv_string_3("bobéy le grand")
