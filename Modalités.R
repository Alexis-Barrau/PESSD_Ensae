### Ce script permet de réaliser les statistiques descriptives d'ensemble donnant ###
### le profil global des répondants dans la base ###

#Lecture des packages
library(haven)
library(tidyverse)

#Lecture de la base
base <- read_dta("LYCEES_base.dta")
base <- base %>% mutate_if(is.labelled, as_factor)

#Sélection des variables
var <- base %>%
  select(genre,
         niveau)

#Décompte des modalités
resultats <- lapply(var, function(x) {
  effectif <- table(x)
  frequence <- prop.table(effectif)
  
  data.frame(
    Modalité = names(effectif),
    Effectif = as.vector(effectif),
    Fréquence = round(as.vector(frequence), 3)
  )
})

#Affichage des modalités
for(nom_var in names(resultats)) {
  cat("\nVariable :", nom_var, "\n")
  print(resultats[[nom_var]])
}
