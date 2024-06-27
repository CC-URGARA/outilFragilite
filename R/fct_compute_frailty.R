# Contient toutes les fonctions relative au calcul de la fragilité --------

#' compute_frailty
#'
#' @description
#' Fonction principale réalisant le calcule de la fragilité et l'export de la table excel.
#'
#' @param tab_indic data.frame
#' @param tab_indic_control data.frame
#' @param tab_context data.frame
#' @param tab_context_control data.frame
#' @param sort_frailty_by Sur quel type de fragilité classer les résultats initialement
#'
#' @return Retourne une liste contenant toutes les infos nécessaires à la création du rapport
#'
#' @importFrom dplyr select
#'
#' @noRd
compute_frailty <- function(tab_indic, tab_indic_control,
                            tab_context, tab_context_control,
                            sort_frailty_by){
  #S'assure que l'ordre des variables dans la table controle est idem à la table indic
  tab_indic = tab_indic %>% select(EG, RS, tab_indic_control$var_indic)

  #Discrétisation des variables
  tab_indic_discr = fct_discretize(tab_indic = tab_indic, tab_indic_control = tab_indic_control)

  #Calcule de la table de score de fragilité
  table_score = fct_frailty_table(tab_indic_discr = tab_indic_discr,
                                  tab_indic_control = tab_indic_control)

  #Calcule de la table de score par établissement
  table_score_EG_clean = fct_frailty_per_EG(tab_indic = tab_indic,
                                            tab_indic_discr = tab_indic_discr,
                                            tab_indic_control = tab_indic_control,
                                            tab_context = tab_context,
                                            tab_context_control = tab_context_control,
                                            sort_frailty_by = sort_frailty_by,
                                            table_score = table_score)

  #Calcule d'une fiche par établissement
  list_data_fiche_etab = fct_sheet_per_EG(table_score_EG_clean = table_score_EG_clean,
                                          tab_indic_control = tab_indic_control,
                                          tab_context_control = tab_context_control,
                                          table_score = table_score)

  #Nettoyage du tableau de score afin de pouvoir l'afficher
  table_score_clean = fct_clean_table_score(table_score = table_score,
                                            tab_indic_control = tab_indic_control)

  #Return
  return(list(
    table_score_EG_clean = table_score_EG_clean,
    list_data_fiche_etab = list_data_fiche_etab,
    table_score_clean = table_score_clean
  ))
}

# Functions internes à compute_frailty -------------------------------------------------------

#' fct_discretize
#'
#' @description
#' Discrétise les indicateurs selon la table de controle#'
#'
#' @param tab_indic data.frame
#' @param tab_indic_control data.frame
#'
#' @return Retourne la base indicateurs avec une nouvelle colonne par indicateur contenant l'indicateur discritisé et nommée nomIndic_class
#'
#' @noRd
#' @importFrom dplyr filter pull all_of select rename_at vars inner_join
#' @importFrom purrr imap_dfr
#' @importFrom arules discretize
#' @importFrom stringr str_split_1
fct_discretize <- function(tab_indic, tab_indic_control){
  #Sélection des indicateurs déjà discrets, a discrétisés sur des seuils déterminés ou automatiquement
  indic_discret_auto <- tab_indic_control %>% filter(!is.na(class_nb)) %>% pull(var_indic)
  indic_discret_manu <- tab_indic_control %>% filter(!is.na(class_bornes)) %>% pull(var_indic)

  #cas 1 : discrétisation automatique
  tab_indic_discr_auto = tab_indic %>%
    select(EG, RS, all_of(indic_discret_auto)) %>%
    imap_dfr(
      function(.x, .y){
        if(.y %in% c("EG", "RS")){
          return(.x)
        } else {
          res = discretize(.x, breaks = as.numeric(tab_indic_control$class_nb[tab_indic_control$var_indic == .y]),
                           method = "cluster", nstart = 1000, iter.max = 100, infinity = T)
          return(res)
        }
      }) %>%
    rename_at(.vars = vars(-"EG", -"RS"), .funs = function(x){
      paste0(x, "_class")
    })

  #cas 2 : discrétisation sur seuil prédéterminé
  tab_indic_discr_manu = tab_indic %>%
    select(EG, RS, all_of(indic_discret_manu)) %>%
    imap_dfr(
      function(.x, .y){
        if(.y %in% c("EG", "RS")){
          return(.x)
        } else {
          bornes = str_split_1(tab_indic_control$class_bornes[tab_indic_control$var_indic == .y], pattern = "\\|") %>%
            as.numeric()
          res = cut(round(.x, 3),
                    breaks = bornes,
                    include.lowest = T, right = F)
          return(res)
        }
      }) %>%
    rename_at(.vars = vars(-"EG", -"RS"), .funs = function(x){
      paste0(x, "_class")
    })

  #Merge des deux cas
  tab_indic_discr = inner_join(tab_indic_discr_auto, tab_indic_discr_manu, by = c("EG", "RS"))

  if(!identical(dim(tab_indic_discr), dim(tab_indic))){
    stop("Un problème a eu lieu durant la discrétisation")
  }

  #Merge de la base discrète avec la base continue
  res = inner_join(tab_indic, tab_indic_discr, by = c("EG", "RS"))
  if(nrow(res) != nrow(tab_indic)){
    stop("Un problème a eu lieu durant la discrétisation")
  }
  return(res)
}



#' fct_frailty_table
#'
#' @param tab_indic_discr data.frame
#' @param tab_indic_control data.frame
#'
#' @return data.frame
#'
#' @noRd
#' @import dplyr
#' @importFrom purrr set_names map imap
#' @importFrom stats median sd
fct_frailty_table <- function(tab_indic_discr, tab_indic_control){
  #Création d'une liste nommée contenant un niveau pour chaque famille
  #et un vecteur du nom des indicateurs dans chaque niveau
  list_group = unique(tab_indic_control$famille) %>%
    purrr::set_names() %>%
    map(function(fam){
      res = tab_indic_control %>% filter(famille %in% fam) %>% pull(var_indic)
      return(res)
    })

  #Calcule du score de chaque catégorie
  tab_score = imap(list_group, function(group_var, nom_groupe_var){#Pour chaque groupe de variable
    tab_sous_groupe = tab_indic_discr %>%
      select(starts_with(group_var))

    #chaque var un score sur 100/n_var du groupe est calculé
    #Le score est centré par la médiane du groupe minimal et réduit par l'écart-type de l'indicateur
    tab_score_groupe = map(group_var, function(var){#Pour chaque variable du groupe
      var_class = paste0(var, "_class")#Nom de la variable contenant la class
      sd_var = tab_sous_groupe %>% pull(var) %>% sd(na.rm = T)
      med_var_grp_temp = tab_sous_groupe %>%#médiane par classe
        select(current_var = {var},
               current_var_class = {var_class}) %>%
        na.omit() %>%
        group_by(current_var_class) %>%
        summarise(med = median(current_var, na.rm = T)) %>%
        na.omit()

      direction_indic <- tab_indic_control$direction_fragilite[tab_indic_control$var_indic == var]
      if(direction_indic %in% "Négative"){#Indicateur corrélé négativement à la fragilité => centré sur max et ramené au min
        med_var_grp = med_var_grp_temp %>%
          mutate(
            var = var,
            score = (med-max(med, na.rm = T))/sd_var,#centré sur le min et standardisé par sd glob
            score_std = round(score/min(score, na.rm = T)*100/length(group_var), 1)) %>%#rapporté à 100/nb var dans le groupe
          rename("modalite" = "current_var_class")
      } else if(direction_indic %in% "Positive"){#Indicateur corrélé positivement à la fragilité => centré sur min et ramené au max
        med_var_grp = med_var_grp_temp %>%
          mutate(
            var = var,
            score = (med-min(med, na.rm = T))/sd_var,#centré sur le min et standardisé par sd glob
            score_std = round(score/max(score, na.rm = T)*100/length(group_var), 1)) %>%#rapporté à 100/nb var dans le groupe
          rename("modalite" = current_var_class)
      } else {
        stop("Valeur inattendue pour 'direction_fragilite' dans le calcule du score")
      }

      return(med_var_grp)
    }) %>%
      bind_rows() %>%
      return(tab_score_groupe)
  }) %>%
    bind_rows

  #Ajout d'infos sup nécessaires plus tard (bornes, famille, type etc.)
  tab_score_complete = tab_score %>%
    mutate(class = paste(var, modalite, sep = "_"),
           borne_min = as.numeric(str_extract(modalite, "(?<=\\[).*(?=,)")),
           borne_max = as.numeric(str_extract(modalite, "(?<=,).*(?=[\\]\\)])"))) %>%
    group_by(var) %>%
    mutate(rang_modalite = paste0(rank(-score_std), "/", length(score_std))) %>%
    ungroup %>%
    left_join(tab_indic_control[,c("var_indic", "lab_indic", "famille", "type", "ponderation")],
              by = c("var" = "var_indic"))

  return(tab_score_complete)
}



#' fct_frailty_per_EG
#'
#' @description
#' Calcule la table de score par établissement (export Excel).
#'
#'
#' @param tab_indic data.frame
#' @param tab_indic_discr data.frame
#' @param tab_indic_control data.frame
#' @param tab_context data.frame
#' @param tab_context_control data.frame
#' @param sort_frailty_by character
#' @param table_score data.frame
#'
#' @return data.frame
#'
#' @noRd
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr imap
#' @importFrom stringr str_detect str_remove
#' @importFrom glue glue
#' @importFrom stats median
fct_frailty_per_EG = function(tab_indic, tab_indic_discr, tab_indic_control,
                              tab_context, tab_context_control, sort_frailty_by,
                              table_score = table_score){
  #Une ligne par EG, une colonne par indicateur (+EG et RS) et le score dans les cases
  tab_score_EG <- imap(tab_indic, function(var_val, var_name){
    if(var_name %in% c("EG", "RS") |
       str_detect(var_name, "_class$")){
      return(var_val)
    } else {
      tab_score_var = table_score %>%
        filter(var == var_name)

      var_val_score <- sapply(var_val, function(x){
        if(is.na(x) | nrow(tab_score_var) == 0){return(NA)} else{
          return(tab_score_var$score_std[tab_score_var$borne_min <= x & tab_score_var$borne_max > x])
        }
      })
      #les cas manquants sont remplacés par la médiane des établissement
      med_etab = round(median(var_val_score, na.rm = T))
      var_val_score = if_else(is.na(var_val_score), med_etab, var_val_score)

      return(var_val_score)
    }
  }) %>%
    as_tibble %>%
    rename_at(.vars = vars(-"EG", -"RS"), .funs = function(x){
      paste0(x, "_score")
    })

  #Ajout de la valeur et classe de l'indicateur
  tab_score_EG = tab_score_EG %>%
    inner_join(tab_indic_discr, by = c("EG", "RS"))

  #Ajout des infos supp de contextualisation
  tab_score_EG <- tab_context %>%
    mutate(across(c(tab_context_control$var_context[tab_context_control$type_object %in% c("real", "integer")]),
                  as.numeric)) %>%
    inner_join(tab_score_EG, by = c("EG", "RS"))

  # Calcules des scores et rangs type/famille
  ## Score : Famille
  tab_score_Famille_long = tab_score_EG %>%
    select(EG, RS, ends_with("_score")) %>%#select var concernées
    pivot_longer(cols = c(-"EG", -"RS"),#Un ligne par étab/indic
                 names_to = "var", values_to = "score") %>%
    mutate(var = str_remove(var, "_score$")) %>%#Permet match sur var avec la table des familles
    left_join(#merge pour ajout de la famille d'indicateur
      select(table_score, var, Famille = famille, Type = type) %>%
        unique(),
      by = "var"
    ) %>%
    group_by(EG, RS, Famille, Type) %>%
    summarise(score = sum(score)) %>%#sous-score = somme de la famille
    ungroup

  #Remise au bon formet et merge avec la table de score
  tab_score_Famille = tab_score_Famille_long %>%
    select(-Type) %>%
    pivot_wider(names_from = Famille, values_from = score) %>%
    rename_with(.cols = c(-EG, -RS), ~paste0(.x, "_score"))

  tab_score_EG <- left_join(tab_score_EG, tab_score_Famille, by = c("EG", "RS"))

  ## Score : type (calculé sur moyenne des familles => utilisation de tab_score_Famille_long)
  tab_score_Type = tab_score_Famille_long %>%
    group_by(EG, RS, Type) %>%
    summarise(score = round(mean(score), 1)) %>%
    pivot_wider(names_from = Type, values_from = score, names_glue = "{Type}_score")

  tab_score_EG <- left_join(tab_score_EG, tab_score_Type, by = c("EG", "RS"))

  ## Score : global
  tab_score_Glob = tab_score_Famille_long %>%
    group_by(EG, RS) %>%
    summarise(Glob_score = round(mean(score), 1))

  tab_score_EG <- left_join(tab_score_EG, tab_score_Glob, by = c("EG", "RS"))

  ##rang : type/Famille/glob/indic
  tab_score_EG = tab_score_EG %>%
    mutate(across(ends_with("_score"),
                  ~rank(-.x, ties.method = "max"),
                  .names = "{.col}_rang"))

  ##Ordre d'apparition des colonnes/lignes
  tab_score_EG_clean = tab_score_EG %>%
    select(EG, RS,
           all_of(tab_context_control$var_context),
           Glob_score, Glob_score_rang,
           starts_with(unique(tab_indic_control$type)),
           starts_with(unique(tab_indic_control$famille)),
           starts_with(tab_indic_control$var_indic),
    ) %>%
    arrange(desc(across(all_of(glue("{sort_frailty_by}_score")))))
  return(tab_score_EG_clean)
}



#' fct_sheet_per_EG
#'
#' @description
#' Résume et mets en forme toutes les informations à afficher dans le rapport par centre
#'
#'
#' @param table_score_EG_clean data.frame
#' @param tab_indic_control data.frame
#' @param tab_context_control data.frame
#' @param table_score data.frame
#'
#' @return Retourne une liste contenant par EG : un tableau de score et un tableau de contexte
#'
#' @noRd
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr set_names
#' @importFrom stringr str_replace_all str_detect str_remove
#' @importFrom forcats fct_na_value_to_level fct_inorder
#' @importFrom stats setNames
fct_sheet_per_EG <- function(table_score_EG_clean,
                             tab_indic_control,
                             tab_context_control,
                             table_score){
  ##Listing des données par étab
  tab_rang_modalites = table_score %>%
    select(var, Classe = modalite, Rang = rang_modalite, score_std)

  list_data_fiche_etab = table_score_EG_clean$EG %>%
    purrr::set_names() %>%
    lapply(function(FINESS){
      #Sélection des données de l'étab actif
      tab_etab = table_score_EG_clean %>% filter(EG == FINESS)

      #Info générales
      FINESS_complet = clean_FINESS(FINESS)
      dpt = substr(FINESS_complet, 1, 2)
      nom_etab = tab_etab$RS

      #extraction d'info à afficher sur la page etab contextualisation
      tab_info_etab = tab_etab %>%
        select(all_of(tab_context_control$var_context)) %>%
        #mise en forme
        mutate(across(c(tab_context_control$var_context[tab_context_control$type_object == "integer"]),
                      ~format(round(as.numeric(.x)), big.mark = " ")),
               across(c(tab_context_control$var_context[tab_context_control$type_object == "real"]),
                      ~format(round(as.numeric(.x), 2)))) %>%
        mutate_all(as.character) %>%
        pivot_longer(cols = everything(),
                     names_to = "Variable",
                     values_to = "Valeur") %>%
        mutate(#recodage
          Variable = str_replace_all(Variable,
                                     setNames(tab_context_control$lab_context, tab_context_control$var_context))
        )

      #Info pour la page table de score
      ##Pour chaque indicateur extraction du score, de sa classe et de sa valeur
      tab_val_score = tab_etab %>%
        select(starts_with(tab_indic_control$var_indic),
               -ends_with("_rang")) %>%
        mutate_all(as.character) %>%
        pivot_longer(cols = everything(),
                     names_to = "Variable", values_to = "value") %>%
        mutate(type_val = if_else(str_detect(Variable, "_score$"), "Score",
                                  if_else(str_detect(Variable, "_class$"), "Classe", "Valeur")),
               Variable = str_remove(Variable, "(_score$)|(_class$)")) %>%
        pivot_wider(names_from = type_val, values_from = value) %>%
        mutate(across(c(Score, Valeur),
                      ~round(as.numeric(.x), 2)))

      #merge pour créer la table du centre avec une ligne par indicateur
      tab_indic = left_join(tab_val_score, tab_rang_modalites, by = c("Variable" = "var", "Classe")) %>%
        left_join(tab_indic_control, by = c("Variable" = "var_indic")) %>%
        select(Indicateur = lab_indic, Valeur, Classe, Rang, Score, Type = type, Famille = famille) %>%
        mutate(Classe = fct_clean_interval(Classe),
               Valeur = if_else(is.na(Valeur), "NA/NC", as.character(Valeur)),
               Rang = if_else(is.na(Rang), "X", Rang),
               Score = if_else(is.nan(Score), "XX", as.character(Score)),
               bg = "Indicateur"#contrôle la couleur de l'arrière plan dans le tableau du rapport
        )

      #création table avec une ligne par famille
      tab_tot_famille = tab_etab %>%
        select(starts_with(unique(tab_indic_control$famille)) &
                 (ends_with("_rang") | ends_with("_score"))) %>%
        pivot_longer(cols = everything(), names_to = "Famille") %>%
        mutate(#var nécessaires au pivot wider
          type_val = if_else(str_detect(Famille, "_rang$"), "Rang", "Score"),
          Famille = str_remove(Famille, "_rang$")
        ) %>%
        pivot_wider(names_from = type_val, values_from = value) %>%
        mutate(
          Famille = str_remove(Famille, "_score$"),
          Indicateur = paste0("Total : ", unique(Famille)),
          Valeur = "X",
          Classe = "X",
          Rang = paste0(Rang, "/", nrow(table_score_EG_clean)),
          Score = as.character(Score),
          bg = "Famille"
        ) %>%
        left_join(tab_indic_control %>%#merge avec le type
                    select(Famille = famille,
                           Type = type) %>%
                    unique(),
                  by = "Famille")

      #création table avec une ligne par type et une ligne pour le total
      tab_tot_type = tab_etab %>%
        select(starts_with(c("Glob", unique(tab_indic_control$type))) &
                 (ends_with("_rang") | ends_with("_score"))) %>%
        pivot_longer(cols = everything(), names_to = "name") %>%
        mutate(#var nécessaires au pivot wider
          type_val = if_else(str_detect(name, "_rang$"), "Rang", "Score"),
          name = str_remove(name, "_rang$")
        ) %>%
        pivot_wider(names_from = type_val, values_from = value) %>%
        mutate(
          name = str_remove(name, "_score$"),
          Type = if_else(name == "Glob", NA_character_, name),
          Indicateur = if_else(name == "Glob", "Total global",
                               paste0("Total : ", unique(Type))),
          Valeur = "X",
          Classe = "X",
          Rang = paste0(Rang, "/", nrow(table_score_EG_clean)),
          Score = as.character(Score),
          bg = if_else(name == "Glob", "Glob", "Type"),
          Famille = NA
        ) %>%
        select(Indicateur, Valeur, Classe, Rang, Score, Famille, Type, bg)
      #merge des tab var, famille et tot
      tab = bind_rows(tab_indic, tab_tot_famille, tab_tot_type) %>%
        mutate(Indicateur = fct_inorder(Indicateur),
               Famille = fct_inorder(Famille),
               Type = fct_inorder(Type)) %>%
        arrange(Type, Famille, Indicateur) %>%
        mutate(across(c(Type, Famille), ~fct_na_value_to_level(.x, "")))

      return(list(
        tab_info_etab = tab_info_etab,
        nom_etab = nom_etab,
        dpt = dpt,
        tab_score_etab = tab
      ))
    })

  return(list_data_fiche_etab)
}




#' fct_clean_table_score
#'
#' @param table_score data.frame
#' @param tab_indic_control data.frame
#'
#' @return data.frame
#'
#' @noRd
#' @import dplyr
fct_clean_table_score <- function(table_score, tab_indic_control){
  table_score_clean <- table_score %>%
    mutate(var = factor(var, levels = tab_indic_control$var_indic),
           type = factor(type, levels = unique(tab_indic_control$type))) %>%
    arrange(type, var, rang_modalite) %>%
    select(Indicateur = lab_indic, Classe = modalite, Score = score_std, Type = type, Famille = famille) %>%
    mutate(Classe = fct_clean_interval(as.character(Classe)),
           Score = if_else(is.na(Score), "XX", as.character(Score)),
           Famille = factor(Famille))
  return(table_score_clean)
}

