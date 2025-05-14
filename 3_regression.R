########################################################################
# Titre         3 regression
# Description   Ce script vise à réaliser les régressions de nos variables
#               d'intérêt sur nos indicateurs ainsi qu'un ensemble de contrôle.
#               On procède aussi à quelques prétraitements :
#               - La création d'une indicatrice pour les lycéens issues de
#               catégories populaires à partir de la PCS ménage
#               (issue de la profession déclarée des parents)
#               - La création d'une indicatrice pour les lycéens dont au moins 
#               un des parents est titulaire d'un diplôme du supérieur
#               - L'exclusion des individus ne déclarant pas de genre binaire
#               (en raison d'un effectif trop faible)
########################################################################


# Création de la variable classe_pop
base <- base %>%
  mutate(classe_pop = case_when(
    men_pcs %in% 5:8 ~ 1,
    !is.na(men_pcs) ~ 0,
    TRUE ~ NA_real_
  ))
table(base$classe_pop)

# Création de la variable dipsup
base$dipsup <- NA_integer_
base$dipsup[base$dipl_pere == 5 & base$dipl_mere == 5] <- 1 # Père et Mère tous les deux diplômés du supérieur
base$dipsup[(base$dipl_pere == 5 | base$dipl_mere == 5) & is.na(base$dipsup)] <- 1 # Père OU Mère diplômé du supérieur mais pas les deux
base$dipsup[base$dipl_pere %in% c(1, 2, 6, NA) & base$dipl_mere %in% c(1, 2, 6, NA)] <- 0 # Père ET mère dipl inf à bac ou NA/Ne sait pas
base$dipsup[is.na(base$dipl_pere) & is.na(base$dipl_mere)] <- NA # On remet en NA si Père ET Mere sont NA
base$dipsup[(base$dipl_pere %in% c(3, 4) | base$dipl_mere %in% c(3, 4)) & is.na(base$dipsup)] <- 0 # Père ou mère dipl bac ou sup court et aucun diplomé de plus
table(base$dipsup)

# Suppression des genre inconnu
base <- base[base$genre != 3,]
     
# Régression 
base <- base %>%
  mutate(across(c(genre, niveau, classe_pop, dipsup), as.factor))
model_sensib <- lm(sensib_env ~ ident_pol * comp_polit + genre + niveau + classe_pop + dipsup, data = base)
model_renonc <- lm(passage_action ~ sensib_env * comp_polit + genre + niveau + classe_pop + dipsup, data = base)
model_action <- lm(renonc_ecolo ~ sensib_env * comp_polit + genre + niveau + classe_pop + dipsup, data = base)

# Vecteur de labels dans le bon ordre et sans nom
labels <- c(
  "Position politique",
  "Sensibilité écolo.",
  "Compétence perçue",
  "Homme",
  "2nde Pro.",
  "1ère Générale",
  "1ère Technologique",
  "1ère Pro.",
  "Term. Générale",
  "Term. Technologique",
  "Term. Pro.",
  "CAP",
  "Classes populaires",
  "Parents dip. sup.long",
  "Pos. pol. x compétence",
  "Sensib. eco. x compétence"
)

# Appel sans nommage (ordre implicite)
stargazer(model_sensib, model_renonc, model_action,
          type = "latex",
          title = "Effet de la compétence politique autoperçue et du positionnement gauche-droite sur l'engagement écologique",
          report = "vc*",
          covariate.labels = labels,
          out = "Figures/reg_sensib_ecolo.tex",
          escape = FALSE)
