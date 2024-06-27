## code to prepare every dataset goes here
library(tidyverse)
library(rUrgAra)
devtools::load_all(".")

#Ouverture des données
tab_indic = read.csv2("../../Axe 3 1.5/app/donnees/tab_indicateurs.csv", na.strings = c("", "NA")) %>%
  as_tibble() %>%
  mutate_all(as.character) %>%
  mutate(EG = clean_FINESS(EG)) %>%
  mutate_at(vars(-EG, -RS), as.numeric)
tab_indic_control = read.csv2("../../Axe 3 1.5/app/donnees/tab_indicateurs_control.csv", encoding = "latin1", na.strings = "") %>%
  as_tibble() %>%
  mutate_all(as.character)

tab_context = read.csv2("../../Axe 3 1.5/app/donnees/tab_var_context.csv") %>%
  as_tibble() %>%
  mutate_all(as.character) %>%
  mutate(EG = clean_FINESS(EG))
tab_context_control = read.csv2("../../Axe 3 1.5/app/donnees/tab_var_context_control.csv", encoding = "latin1", na.strings = "") %>%
  as_tibble() %>%
  mutate_all(as.character)


res_fragilite <- compute_frailty(tab_indic = tab_indic, tab_indic_control = tab_indic_control,
                                 tab_context = tab_context, tab_context_control = tab_context_control,
                                 sort_frailty_by = "Etablissement")



# Export des bases --------------------------------------------------------

usethis::use_data(tab_indic, overwrite = TRUE)
usethis::use_data(tab_indic_control, overwrite = TRUE)
usethis::use_data(tab_context, overwrite = TRUE)
usethis::use_data(tab_context_control, overwrite = TRUE)
usethis::use_data(res_fragilite, overwrite = TRUE)
