#' clean_FINESS
#'
#' @description adds a heading 0 to FINESS codes when needed
#'
#' @return vecteur de caractères contenant le FINESS avec un "0" au début si nécessaire
#'
#' @noRd
#'
clean_FINESS <- function(char){
    if (!is.character(char)) {
      stop("char must be a character vector.")
    }
    if (any(!nchar(char) %in% c(8, 9))) {
      stop("FINESS should be 9 or 8 characters. Some occurences are different.")
    }
    char = if_else(nchar(char) == 8, paste0("0", char), as.character(char))
    return(char)
}



#' fct_clean_interval
#' @description
#' transformation d'un interval en un format >, <
#'
#'
#' @param interval un vecteur
#'
#' @return un vecteur
#'
#' @noRd
#' @importFrom dplyr if_else
#' @importFrom stringr str_extract
fct_clean_interval <- function(interval){
  sapply(interval, function(current_interval){
    if(is.na(current_interval)){return("X")}

    borne_min = round(as.numeric(str_extract(current_interval, "(?<=\\[).*(?=,)")), 2)
    borne_max = round(as.numeric(str_extract(current_interval, "(?<=,).*(?=[\\]\\)])")), 2)

    left = str_extract(current_interval, "^.")
    right = str_extract(current_interval, ".$")

    if(!left%in%c("(", "[") | !right%in%c(")", "]")) stop("L'interval n'a pas le bon format")

    if(is.infinite(borne_min)){
      symb_inegalite = if_else(right == "]", "\u2264", "<")#≤
      return(paste0(symb_inegalite, " ", borne_max))
    } else if(is.infinite(borne_max)){
      symb_inegalite = if_else(left == "[", "\u2265", ">")
      return(paste0(symb_inegalite, " ", borne_min))
    } else {
      return(current_interval)
    }
  })
}

#' fct_add_newline
#'
#' @description
#' Ajoute un retour à la ligne dans une chaine de charactère dépassant max_length caractères.
#'
#'
#' @param char un vecteur de caractère
#' @param max_length la longueurs maximal d'une ligne
#'
#' @return un vecteur de caractères
#'
#' @noRd
#' @importFrom stringr str_detect str_split str_sub
#' @importFrom dplyr lag
#' @importFrom stats na.omit
fct_add_newline = function(char, max_length){
  n_return = nchar(char)%/%max_length
  if(n_return > 0){
    space_pos = which(str_detect(str_split(char, "", simplify = T), " "))#position des espaces
    space_pos_modulo = space_pos %/% max_length
    space_sub_pos = space_pos[space_pos_modulo!=lag(space_pos_modulo, 1)] %>% na.omit()
    char_spaced = char
    for(pos in space_sub_pos){
      stringr::str_sub(char_spaced, pos, pos) <- "\n"
    }
    return(char_spaced)
  } else {
    return(char)
  }
}
