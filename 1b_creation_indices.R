########################################################################
# Titre         1 CREATION DES INDICES
# Description   Ce script vise à créer les indicateurs synthétiques qui
#               seront utilisés dans les régressions :
#               1.1 sensibilité environnementales : 
#               1.2 passage à l'action :
#               1.3 compétence politique : 
#               1.4 gauchisme : 
#               1.5 renoncement pour des raisons écologiques
#               A chaque fois, on vérifie le caractère unidimensionnel de 
#               l'indice avant de lancer une ACP (avec imputation des 
#               valeurs manquantes via le package missMDA). Les coordonnées
#               sur le 1er axe factoriel forment l'indice et sont ajoutés à la base
########################################################################

dir.create("Figures")

# Rename utiles dans toutes la suite
base <- base %>%
  rename(
    Progressiste = q30a,
    Conservateur = q30b,
    Antiraciste = q30c,
    Religieux = q30d,
    Féministe = q30e,
    Libéral = q30f,
    Individualiste = q30g,
    Républicain = q30h,
    Communiste = q30i,
    Patriote = q30j,
    Anticapitaliste = q30k,
    Nationaliste = q30l,
    Écologiste = q30m,
    Traditionaliste = q30n,
    Anarchiste = q30o,
    Royaliste = q30p,
    Apolitique = q30q,
    Rien = q30r
  )

# Réagencement dans un ordre ordinale 'Non' < 'Je ne sais pas' < 'Oui' des q24
base <- base %>%
  mutate(
    q24a = as.numeric(q24a),
    q24a = recode(q24a, `2` = 3, `3` = 2, `4` = 2),
    q24b = as.numeric(q24b),
    q24b = recode(q24b, `2` = 3, `3` = 2, `4` = 2)
  )

# 1.1 SENSIBILITE ENVIRONNEMENTALE ---------------------------------------------

sensib_env = c("q10a", "q10b", "q10c", "q10d", "q10e", "q10f")

# Valeurs manquantes
base %>% 
  select(all_of(sensib_env)) %>%
  summarise(across(everything(), ~mean(is.na(.))*100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  ggplot(aes(x = reorder(variable, -pct_na), y = pct_na)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Valeurs manquantes sur les variables de l'indice 'sensibilité environnementale'", y = "% manquantes", x = "")
ggsave("Figures/1.1a_sensib_env_valeurs_manquantes.png")

# Imputation 
imputation <- as.data.frame(round(imputePCA(base[, sensib_env], ncp = 1)$completeObs))
#~ Distrib avant/apres
par(mfrow = c(2, 3))  # 2 lignes, 3 colonnes
all_vals <- as.numeric(names(table(base[[sensib_env[1]]]))) # liste des valeurs prise
for (var in sensib_env) {
  barplot(rbind(table(base[[var]]), table(imputation[[var]])),
          beside = TRUE,
          names.arg = all_vals,
          col = c("grey70", "steelblue"),
          main = var,
          legend.text = c("Avant", "Après"))
}
dev.print(device = png, file = "Figures/1.1b_sensib_env_imputation.png", width = 800)
base[, sensib_env] <- imputation
rm(imputation)

# Unidimensionnalité
KMO(base[, sensib_env]) # Avec une valeur de 0.7, l'utilisation d'une analyse factorielle est acceptable
bartlett.test(base[, sensib_env]) #On vérifie avec un test de Barlett

# ACP
pca_sensib_env = PCA(base[, c(sensib_env,"q11")], scale.unit = TRUE, graph = TRUE, quanti.sup=7:7)
ggsave("Figures/1.1c_sensib_env_pca_cercle.png")
fviz_eig(pca_sensib_env)
ggsave("Figures/1.1d_sensib_env_pca_scree_plot.png")

# Ajout des coordonnées sur le premier axe factoriel à la base
base$sensib_env = pca_sensib_env$ind$coord[,1]

# Distrib de l'indice pour verif de cohérence 
# Créer un data frame long pour ggplot
df_long <- base %>%
  pivot_longer(cols = all_of(sensib_env), names_to = "variable", values_to = "modalite")

# Boxplot : distribution de l’indice par modalité et par variable
ggplot(df_long, aes(x = as.factor(modalite), y = sensib_env)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = "Modalité", y = "Indice de sensibilité environnementale",
       title = "Distribution de l'indice selon les modalités des variables Q10") +
  theme_minimal()
ggsave("Figures/1.1e_sensib_env_distrib_indice.png")
rm(df_long)



# 1.2 PASSAGE A L'ACTION -------------------------------------------------------
passage_action = c("q22a", "q22b", "q22c", "q22d", "q22e", "q22f", "q22g")

# Valeurs manquantes
base %>% 
  select(all_of(passage_action)) %>%
  summarise(across(everything(), ~mean(is.na(.))*100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  ggplot(aes(x = reorder(variable, -pct_na), y = pct_na)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Valeurs manquantes sur les variables de l'indice 'passage à l'action", y = "% manquantes", x = "")
ggsave("Figures/1.2a_passage_action_valeurs_manquantes.png")

# Imputation 
imputation <- as.data.frame(round(imputePCA(base[, passage_action], ncp = 1)$completeObs))
#~ Distrib avant/apres
par(mfrow = c(2, 4))  # 2 lignes, 3 colonnes
all_vals <- as.numeric(names(table(base[[passage_action[1]]]))) # liste des valeurs prise
for (var in passage_action) {
  barplot(rbind(table(base[[var]]), table(imputation[[var]])),
          beside = TRUE,
          names.arg = all_vals,
          col = c("grey70", "steelblue"),
          main = var,
          legend.text = c("Avant", "Après"))
}
dev.print(device = png, file = "Figures/1.2b_passage_action_imputation.png", width = 800)
base[, passage_action] <- imputation
rm(imputation)

# Unidimensionnalité
KMO(base[, passage_action]) # Avec une valeur de 0.7, l'utilisation d'une analyse factorielle est acceptable
bartlett.test(base[, passage_action]) #On vérifie avec un test de Barlett

# ACP
pca_passage_action = PCA(base[, c(passage_action,"q11")], scale.unit = TRUE, graph = TRUE, quanti.sup=8:8)
ggsave("Figures/1.2c_passage_action_pca_cercle.png")
fviz_eig(pca_passage_action)
ggsave("Figures/1.2d_passage_action_pca_scree_plot.png")

# Ajout des coordonnées sur le premier axe factoriel à la base
base$passage_action = pca_passage_action$ind$coord[,1]

# Distrib de l'indice pour verif de cohérence 
# Créer un data frame long pour ggplot
df_long <- base %>%
  pivot_longer(cols = all_of(passage_action), names_to = "variable", values_to = "modalite")

# Boxplot : distribution de l’indice par modalité et par variable
ggplot(df_long, aes(x = as.factor(modalite), y = passage_action)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = "Modalité", y = "Indice de passage à l'action",
       title = "Distribution de l'indice selon les modalités des variables q22") +
  theme_minimal()
ggsave("Figures/1.2e_passage_action_distrib_indice.png")
rm(df_long)



# 1.3 COMPETENCE POLITIQUE -----------------------------------------------------

comp_polit = c("q26a", "q26b", "q26c", "q26d", "q26e")

# Valeurs manquantes
base %>% 
  select(all_of(comp_polit)) %>%
  summarise(across(everything(), ~mean(is.na(.))*100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  ggplot(aes(x = reorder(variable, -pct_na), y = pct_na)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Valeurs manquantes sur les variables de l'indice 'compétence politique'", y = "% manquantes", x = "")
ggsave("Figures/1.3a_comp_polit_valeurs_manquantes.png")

# Imputation 
imputation <- as.data.frame(round(imputePCA(base[, comp_polit], ncp = 1)$completeObs))
#~ Distrib avant/apres
par(mfrow = c(2, 3))  # 2 lignes, 3 colonnes
all_vals <- as.numeric(names(table(base[[comp_polit[1]]]))) # liste des valeurs prise
for (var in comp_polit) {
  barplot(rbind(table(base[[var]]), table(imputation[[var]])),
          beside = TRUE,
          names.arg = all_vals,
          col = c("grey70", "steelblue"),
          main = var,
          legend.text = c("Avant", "Après"))
}
dev.print(device = png, file = "Figures/1.3b_comp_polit_imputation.png", width = 800)
base[, comp_polit] <- imputation
rm(imputation)

# Unidimensionnalité
KMO(base[, comp_polit]) # Avec une valeur de 0.7, l'utilisation d'une analyse factorielle est acceptable
bartlett.test(base[, comp_polit]) #On vérifie avec un test de Barlett

# ACP
pca_comp_polit = PCA(base[, c(comp_polit)], scale.unit = TRUE, graph = TRUE)
ggsave("Figures/1.3c_comp_polit_pca_cercle.png")
fviz_eig(pca_comp_polit)
ggsave("Figures/1.3d_comp_polit_pca_scree_plot.png")

# Ajout des coordonnées sur le premier axe factoriel à la base
base$comp_polit = pca_comp_polit$ind$coord[,1]

# Distrib de l'indice pour verif de cohérence 
# Créer un data frame long pour ggplot
df_long <- base %>%
  pivot_longer(cols = all_of(comp_polit), names_to = "variable", values_to = "modalite")

# Boxplot : distribution de l’indice par modalité et par variable
ggplot(df_long, aes(x = as.factor(modalite), y = comp_polit)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = "Modalité", y = "Indice de compétence politique",
       title = "Distribution de l'indice selon les modalités des variables Q26") +
  theme_minimal()
ggsave("Figures/1.3e_comp_polit_distrib_indice.png")
rm(df_long)




# 1.4 IDENTITE POLITIQUE -------------------------------------------------------

ident_pol = c("q31a", "q31c", "q31d", "q31e", "q31f")

# Valeurs manquantes
base %>% 
  select(all_of(ident_pol)) %>%
  summarise(across(everything(), ~mean(is.na(.))*100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  ggplot(aes(x = reorder(variable, -pct_na), y = pct_na)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Valeurs manquantes sur les variables de l'indice 'identité politique'", y = "% manquantes", x = "")
ggsave("Figures/1.4a_ident_pol_valeurs_manquantes.png")

# Imputation 
imputation <- as.data.frame(round(imputePCA(base[, ident_pol], ncp = 1)$completeObs))
#~ Distrib avant/apres
par(mfrow = c(2, 3))  # 2 lignes, 3 colonnes
all_vals <- as.numeric(names(table(base[[ident_pol[1]]]))) # liste des valeurs prise
for (var in ident_pol) {
  barplot(rbind(table(base[[var]]), table(imputation[[var]])),
          beside = TRUE,
          names.arg = all_vals,
          col = c("grey70", "steelblue"),
          main = var,
          legend.text = c("Avant", "Après"))
}
dev.print(device = png, file = "Figures/1.4b_ident_pol_imputation.png", width = 800)
base[, ident_pol] <- imputation
rm(imputation)

# Unidimensionnalité
KMO(base[, ident_pol]) # Avec une valeur de 0.7, l'utilisation d'une analyse factorielle est acceptable
bartlett.test(base[, ident_pol]) #On vérifie avec un test de Barlett

# ACP
pca_ident_pol = PCA(base[, c(ident_pol,"Progressiste",  "Conservateur",  "Antiraciste",  "Religieux",  "Féministe",  "Libéral",  "Individualiste",  "Républicain",  "Communiste",  "Patriote",  "Anticapitaliste",  "Nationaliste",  "Écologiste",  "Traditionaliste",  "Anarchiste",  "Royaliste",  "Apolitique",  "Rien", "q27a")], scale.unit = TRUE, graph = TRUE, quali.sup=6:24)
# Issu on améliore la visualisation du cercle des correlations en ajoutant des variables supplementaire pr mieux interpreter
mod_qualisup <- as.data.frame(pca_ident_pol$quali.sup$coord)
mod_qualisup$label <- rownames(mod_qualisup)
p <- fviz_pca_var(pca_ident_pol, col.var = "black", repel = TRUE)
p + 
  geom_point(data = mod_qualisup, aes(x = Dim.1, y = Dim.2), color = "red") +
  geom_text(data = mod_qualisup, aes(x = Dim.1, y = Dim.2, label = label),
            color = "red", vjust = -0.5)
ggsave("Figures/1.4c_ident_pol_pca_cercle.png")
fviz_eig(pca_ident_pol)
ggsave("Figures/1.4d_ident_pol_pca_scree_plot.png")
rm(mod_qualisup, p)

# Ajout des coordonnées sur le premier axe factoriel à la base
base$ident_pol = pca_ident_pol$ind$coord[,1]

# Distrib de l'indice pour verif de cohérence 
# Créer un data frame long pour ggplot
df_long <- base %>%
  pivot_longer(cols = all_of(ident_pol), names_to = "variable", values_to = "modalite")

# Boxplot : distribution de l’indice par modalité et par variable
ggplot(df_long, aes(x = as.factor(modalite), y = ident_pol)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = "Modalité", y = "Indice d'identité politique",
       title = "Distribution de l'indice selon les modalités des variables Q31") +
  theme_minimal()
ggsave("Figures/1.4e_ident_pol_distrib_indice.png")
rm(df_long)



# 1.5 RENONCEMENT ECOLOGIQUE ---------------------------------------------------

renonc_ecolo = c("q20i_elv", "q20l_elv", "q20m_elv", "q24a", "q24b")


# Valeurs manquantes
base %>% 
  select(all_of(renonc_ecolo)) %>%
  summarise(across(everything(), ~mean(is.na(.))*100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  ggplot(aes(x = reorder(variable, -pct_na), y = pct_na)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Valeurs manquantes sur les variables de l'indice 'sensibilité environnementale'", y = "% manquantes", x = "")
ggsave("Figures/1.5a_renonc_ecolo_valeurs_manquantes.png")

# Imputation 
imputation <- as.data.frame(round(imputePCA(base[, renonc_ecolo], ncp = 1)$completeObs))
#~ Distrib avant/apres
par(mfrow = c(2, 3))  # 2 lignes, 3 colonnes
for (var in renonc_ecolo) {
  all_vals <- as.numeric(names(table(base[[var]]))) # liste des valeurs prise
  barplot(rbind(table(base[[var]]), table(imputation[[var]])),
          beside = TRUE,
          names.arg = all_vals,
          col = c("grey70", "steelblue"),
          main = var,
          legend.text = c("Avant", "Après"))
}
dev.print(device = png, file = "Figures/1.5b_renonc_ecolo_imputation.png", width = 800)
base[, renonc_ecolo] <- imputation
rm(imputation)

# Unidimensionnalité
KMO(base[, renonc_ecolo]) # Avec une valeur de 0.7, l'utilisation d'une analyse factorielle est acceptable
bartlett.test(base[, renonc_ecolo]) #On vérifie avec un test de Barlett

# ACP
pca_renonc_ecolo = PCA(base[, c(renonc_ecolo,"q11")], scale.unit = TRUE, graph = TRUE, quanti.sup=6:6)
ggsave("Figures/1.5c_renonc_ecolo_pca_cercle.png")
fviz_eig(pca_renonc_ecolo)
ggsave("Figures/1.5d_renonc_ecolo_pca_scree_plot.png")

# Ajout des coordonnées sur le premier axe factoriel à la base
base$renonc_ecolo = pca_renonc_ecolo$ind$coord[,1]

# Distrib de l'indice pour verif de cohérence 
# Créer un data frame long pour ggplot
df_long <- base %>%
  pivot_longer(cols = all_of(renonc_ecolo), names_to = "variable", values_to = "modalite")

# Boxplot : distribution de l’indice par modalité et par variable
ggplot(df_long, aes(x = as.factor(modalite), y = renonc_ecolo)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = "Modalité", y = "Indice de sensibilité environnementale",
       title = "Distribution de l'indice selon les modalités des variables Q10") +
  theme_minimal()
ggsave("Figures/1.5e_renonc_ecolo_distrib_indice.png")
rm(df_long)
write_dta(base, "LYCEES_base_aug.dta")
