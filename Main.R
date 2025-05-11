### Script qui permet de dérouler le projet, se référer aux divers scripts pour le détail et les résutats intermédiaires

# Nettoyage de l'environnement
rm(list = ls())

#Lecture des packages
source("0_packages.R")

#Lecture de la base
base <- read_dta("LYCEES_base.dta")

#création des variables de facteurs
source("1b_creation_indices.R")

# Transformation des variables labellisées en facteurs si nécessaire
base <- base %>% mutate_if(is.labelled, as_factor)
