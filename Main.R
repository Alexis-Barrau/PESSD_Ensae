### Script qui permet de dérouler le projet, se référer aux divers scripts pour le détail et les résutats intermédiaires

# Nettoyage de l'environnement
rm(list = ls())

#Lecture des packages
source("0_packages.R")

#Lecture de la base
base <- read_dta("LYCEES_base.dta")

#création des variables de facteurs
source("1_creation_indices.R")

#cStatistiques descriptives sur les indicateurs
source("2_stats_desc.R")

#Réalisation des régressions
source("3_regression.R")

