########################################################################
# Titre         2 DESCRIPTIVES STATISTICSS
# Description   Ce script produit un graphique visant à mieux comprendre
#               les relations entre les 5 indicateurs créés. Il produit : 
#               - une matrice des corrélation
#               - un graphique réprésentant l'action (collective via passage_action
#                 ou individuelle via renonc_ecolo) en fonction de la sensibilité
#                 écologique. A chaque fois, deux courbes sont tracées : 
#                 l'une en bleu pour les lycéens qui se sentent politiquement 
#                 compétents (comp_polit>mean+1SD) et l'autre en rouge pour ceux qui 
#                 se sentent incompétents (comp_polit>mean-1SD) 
########################################################################

base$action <- as.integer(base$q22a == 4 | base$q22b == 4 | base$q22c == 4)


# 2.1 Matrice de corrélation ---------------------------------------------------
indics <- c("sensib_env", "renonc_ecolo", "passage_action", "comp_polit", "ident_pol")


# Graphique simple
mat_corr <- cor(base[indics])
png("Figures/2.1_Matrice_correlation_simple.png", width = 800, height = 800)
corrplot(mat_corr, method = "color", type = "upper", 
         addCoef.col = "black", tl.cex = 0.9, number.cex = 0.8)
dev.off()



# Graphique sophistiqué
results <- data.frame()

for (xvar in indics) {
  for (yvar in indics) {
    
    if (xvar == yvar) next  # on saute les cas diagonaux
    
    temp <- base %>%
      mutate(
        xval = .data[[xvar]],
        yval = .data[[yvar]],
        decile = ntile(xval, 10)
      )
    
    decile_medians <- temp %>%
      group_by(decile) %>%
      summarise(x_median = median(xval), .groups = "drop")
    
    summary <- temp %>%
      group_by(decile) %>%
      summarise(
        moy = mean(yval),
        se = sd(yval) / sqrt(n()),
        ci99 = 2.576 * se,
        .groups = "drop"
      ) %>%
      left_join(decile_medians, by = "decile") %>%
      mutate(xvar = xvar, yvar = yvar)
    
    results <- bind_rows(results, summary)
  }
}

# Corrélations
cor_matrix <- expand.grid(xvar = indics, yvar = indics, stringsAsFactors = FALSE) %>%
  filter(xvar != yvar) %>%
  rowwise() %>%
  mutate(r = cor(base[[xvar]], base[[yvar]])) %>%
  ungroup()

results <- left_join(results, cor_matrix, by = c("xvar", "yvar"))

# Fond des facettes
facet_bg <- distinct(results, xvar, yvar, r) %>%
  mutate(abs_r = abs(r))

# Graphique
ggplot(results, aes(x = x_median, y = moy)) +
  # Fond par r
  geom_rect(data = facet_bg,
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = abs_r),
            inherit.aes = FALSE, alpha = 0.2) +
  
  # Courbes
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.3) +
  geom_ribbon(aes(ymin = moy - ci99, ymax = moy + ci99), fill = "grey80", alpha = 0.3) +
  geom_line(color = "black") +
  
  # r affiché
  geom_text(
    data = facet_bg,
    aes(label = paste0("r = ", round(r, 2))),
    x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
    inherit.aes = FALSE, size = 3
  ) +
  
  # Facet
  facet_grid(yvar ~ xvar, drop = FALSE, switch = "both") +
  scale_fill_gradient(low = "white", high = "grey40", guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    strip.placement = "outside"
  )
ggsave("Figures/2.1_Matrice_correlation_sophistiquee.png")
rm(results, facet_bg, cor_matrix, decile_medians, mat_corr, summary, temp)


# 2.2 Effet du politique sur le lien sensibilité ecolo <> passage à l'action ---
# Paramètres
depvar_list <- c("renonc_ecolo_z", "passage_action_z")
modvar_list <- c("comp_polit_z", "ident_pol_z")
expvar <- "sensib_env_z"

# Initialiser le tableau de résultats
results <- data.frame()

# Boucle sur chaque combinaison
for (dep in depvar_list) {
  for (mod in modvar_list) {
    
    temp <- base %>%
      mutate(
        dep = .data[[dep]],
        modvec = .data[[mod]],
        expvec = .data[[expvar]],
        group = case_when(
          modvec < mean(modvec) - sd(modvec) ~ "Faible",
          modvec > mean(modvec) + sd(modvec) ~ "Elevé",
          TRUE ~ NA_character_
        ),
        decile = ntile(expvec, 10)
      )
    
    decile_medians <- temp %>%
      group_by(decile) %>%
      summarise(xval = median(expvec), .groups = "drop")
    
    summary_data <- temp %>%
      filter(!is.na(group)) %>%
      group_by(group, decile) %>%
      summarise(
        moy_action = mean(dep),
        n = n(),
        se = sd(dep) / sqrt(n),
        ci95 = 1.96 * se,
        .groups = "drop"
      ) %>%
      left_join(decile_medians, by = "decile") %>%
      mutate(
        depvar_label = dep,
        modvar_label = mod,
        group_label = case_when(
          mod == "ident_pol_z" & group == "Faible" ~ "Droite",
          mod == "ident_pol_z" & group == "Elevé" ~ "Gauche",
          mod == "comp_polit_z" & group == "Faible" ~ "Incompétents",
          mod == "comp_polit_z" & group == "Elevé" ~ "Compétents"
        )
      )
    
    
    results <- bind_rows(results, summary_data)
  }
}

# Forcer l’ordre des labels
results <- results %>%
  mutate(
    depvar_label = factor(depvar_label, levels = depvar_list),
    modvar_label = factor(modvar_label, levels = modvar_list),
    group_label = factor(group_label, levels = c("Compétents", "Incompétents", "Gauche", "Droite"))
  )
scale_color = c(
  "Gauche"       = "#f72585",  # bleu
  "Droite"       = "#194fff",  # rouge
  "Compétents"   = "#f7b62b",  # vert
  "Incompétents" = "#46c4b4"   # orange
)

# Filtrer les deux sous-données
res_renonc <- results %>% filter(depvar_label == "renonc_ecolo_z")
res_action <- results %>% filter(depvar_label == "passage_action_z")

# Graphe 1
p1 <- ggplot(res_renonc, aes(x = xval, y = moy_action, color = group_label, group = group_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = moy_action - ci95, ymax = moy_action + ci95, fill = group_label),
              alpha = 0.2, color = NA) +
  facet_wrap(~ modvar_label, nrow = 1) +
  scale_color_manual(values = scale_color) + 
  scale_fill_manual(values = scale_color) +
  labs(title = "Renoncement écologique", x = "Sensibilité écologique", y = "Renoncement écologique", color = "Sentiment\n de compétence et\n position politique", fill = "Sentiment\n de compétence et\n position politique") +
  theme_minimal() +
  theme(strip.text = element_blank())

# Graphe 2
p2 <- ggplot(res_action, aes(x = xval, y = moy_action, color = group_label, group = group_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = moy_action - ci95, ymax = moy_action + ci95, fill = group_label),
              alpha = 0.2, color = NA) +
  facet_wrap(~ modvar_label, nrow = 1) +
  scale_color_manual(values = scale_color) + 
  scale_fill_manual(values = scale_color) +
  labs(title = "Passage à l'action", x = "Sensibilité écologique", y = "Passage à l'action", color = "Sentiment\n de compétence et\n position politique", fill = "Sentiment\n de compétence et\n position politique") +
  theme_minimal() +
  theme(strip.text = element_blank())


p1 / p2
ggsave("Figures/2.2_Action_ftc_sensibilité.png")
rm(results, res_renonc, res_action, summary_data, temp, decile_medians, p1, p2)