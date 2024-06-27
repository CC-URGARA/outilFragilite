## code to prepare `tab_dpt_region` dataset goes here
library(tidyverse)

tab_dpt_region <- read.csv2("data-raw/2024_04_19_departements_france.csv", encoding = "latin1")
tab_dpt_region <- tab_dpt_region |>
  mutate(dpt_lab = paste(dpt_num, dpt_nom, sep = " - "))

usethis::use_data(tab_dpt_region, overwrite = TRUE)
