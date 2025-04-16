### Ce script vise à construire nos variables dépendantes et explicatives
### On procède via des analyses factorielles (ACP)

## Question 10 accord avec des affirmations relatives à la lutte écologique
# Les sous-questions q10g et q10h n'ont pas été posées dans l'un des établissements, donc on les exclut

#On regarde les NA aux sous-questions
sum(is.na(base$q10a))
sum(is.na(base$q10b))
sum(is.na(base$q10c))
sum(is.na(base$q10d))
sum(is.na(base$q10e))
sum(is.na(base$q10f))

sum(rowSums(is.na(base[, c("q10a", "q10b", "q10c", "q10d", "q10e", "q10f")])) > 0)
#le nombre de NA est limité, on vérifie qu'ils sont réparties équitablement entre les lycées
q10NA <- base %>% subset(rowSums(is.na(base[, c("q10a", "q10b", "q10c", "q10d", "q10e", "q10f")])) > 0)
table(q10NA$ville)

#On crée une base sans NA
base_q10 <- base %>% subset(rowSums(is.na(base[, c("q10a", "q10b", "q10c", "q10d", "q10e", "q10f")])) == 0)


#On effectue un test de KMO pour savoir si une analyse factorielle est pertinente
KMO(select(base_q10, q10a, q10b, q10c, q10d, q10e, q10f))
# Avec une valeur de 0.7, l'utilisation d'une analyse factorielle est acceptable
#On vérifie avec un test de Barlett
bartlett.test(select(base_q10, q10a, q10b, q10c, q10d, q10e, q10f))

# Il est donc possible de faire une analyse factorielle

pca_result <- PCA(select(base_q10, q10a, q10b, q10c, q10d, q10e, q10f), scale.unit = TRUE, graph = TRUE)

fviz_eig(pca_result)

#On récupère les coordonnées
base_q10$f1_q10 <- pca_result$ind$coord[,1]
base_q10$f2_q10 <- pca_result$ind$coord[,2]
base_q10$f3_q10 <- pca_result$ind$coord[,3]

#Et on remet ça dans notre base en ne gardant que le premier qui est le seul à se démarquer
base <- merge(base, base_q10[, c("id", "f1_q10")], by = "id", all.x = TRUE)





## Question 22 positionnement formes d'engagement

#On regarde les NA aux sous-questions
sum(is.na(base$q22a))
sum(is.na(base$q22b))
sum(is.na(base$q22c))
sum(is.na(base$q22d))
sum(is.na(base$q22e))
sum(is.na(base$q22f))
sum(is.na(base$q22g))

sum(rowSums(is.na(base[, c("q22a", "q22b", "q22c", "q22d", "q22e", "q22f", "q22g")])) > 0)
#le nombre de NA est limité, on vérifie qu'ils sont réparties équitablement entre les lycées
q22NA <- base %>% subset(rowSums(is.na(base[, c("q22a", "q22b", "q22c", "q22d", "q22e", "q22f", "q22g")])) > 0)
table(q22NA$ville)

#On crée une base sans NA
base_q22 <- base %>% subset(rowSums(is.na(base[, c("q22a", "q22b", "q22c", "q22d", "q22e", "q22f", "q22g")])) == 0)


#On effectue un test de KMO pour savoir si une analyse factorielle est pertinente
KMO(select(base_q22, q22a, q22b, q22c, q22d, q22e, q22f, q22g))
# Avec une valeur de 0.7, l'utilisation d'une analyse factorielle est acceptable
#On vérifie avec un test de Barlett
bartlett.test(select(base_q22, q22a, q22b, q22c, q22d, q22e, q22f, q22g))

pca_result <- PCA(select(base_q22, q22a, q22b, q22c, q22d, q22e, q22f, q22g), scale.unit = TRUE, graph = TRUE)

fviz_eig(pca_result)

#On récupère la coordonnée sur le premier facteur mais aussi le deuxième qui semble 
base_q22$f1_q22 <- pca_result$ind$coord[,1] 
base_q22$f2_q22 <- pca_result$ind$coord[,2]


#Et on remet ça dans notre base
base <- merge(base, base_q22[, c("id", "f1_q22", "f2_q22")], by = "id", all.x = TRUE)





## Question 26 : sentiment de compétence politique / intérêt
# On exclut la sous question f qui porte manifestement sur autre chose

#On regarde les NA aux sous-questions
sum(is.na(base$q26a))
sum(is.na(base$q26b))
sum(is.na(base$q26c))
sum(is.na(base$q26d))
sum(is.na(base$q26e))

sum(rowSums(is.na(base[, c("q26a", "q26b", "q26c", "q26d", "q26e")])) > 0)
#le nombre de NA est limité, on vérifie qu'ils sont réparties équitablement entre les lycées
q26NA <- base %>% subset(rowSums(is.na(base[, c("q26a", "q26b", "q26c", "q26d", "q26e")])) > 0)
table(q26NA$ville)

#On crée une base sans NA
base_q26 <- base %>% subset(rowSums(is.na(base[, c("q26a", "q26b", "q26c", "q26d", "q26e")])) == 0)


#On effectue un test de KMO pour savoir si une analyse factorielle est pertinente
KMO(select(base_q26, q26a, q26b, q26c, q26d, q26e))
# Avec une valeur de 0.86, l'utilisation d'une analyse factorielle est acceptable
#On vérifie avec un test de Barlett
bartlett.test(select(base_q26, q26a, q26b, q26c, q26d, q26e))

# Il est donc possible de faire une analyse factorielle

pca_result <- PCA(select(base_q26, q26a, q26b, q26c, q26d, q26e), scale.unit = TRUE, graph = TRUE)

fviz_eig(pca_result)

#On récupère les coordonnées
base_q26$f1_q26 <- pca_result$ind$coord[,1]

#Et on remet ça dans notre base en ne gardant que le premier qui est le seul à se démarquer
base <- merge(base, base_q26[, c("id", "f1_q26")], by = "id", all.x = TRUE)





## Question 31 accord avec des affirmations politiques

#On regarde les NA aux sous-questions
sum(is.na(base$q31a))
sum(is.na(base$q31b))
sum(is.na(base$q31c))
sum(is.na(base$q31d))
sum(is.na(base$q31e))
sum(is.na(base$q31f))

sum(rowSums(is.na(base[, c("q31a", "q31b", "q31c", "q31d", "q31e", "q31f")])) > 0)
#le nombre de NA plus élevé...
q31NA <- base %>% subset(rowSums(is.na(base[, c("q31a", "q31b", "q31c", "q31d", "q31e", "q31f")])) > 0)
table(q31NA$ville)

#On crée une base sans NA
base_q31 <- base %>% subset(rowSums(is.na(base[, c("q31a", "q31b", "q31c", "q31d", "q31e", "q31f")])) == 0)


#On effectue un test de KMO pour savoir si une analyse factorielle est pertinente
KMO(select(base_q31, q31a, q31b, q31c, q31d, q31e, q31f))
# Avec une valeur de 0.7, l'utilisation d'une analyse factorielle est acceptable
#On vérifie avec un test de Barlett
bartlett.test(select(base_q31, q31a, q31b, q31c, q31d, q31e, q31f))

# Il est donc possible de faire une analyse factorielle, je crois...

pca_result <- PCA(select(base_q31, q31a, q31b, q31c, q31d, q31e, q31f), scale.unit = TRUE, graph = TRUE)

fviz_eig(pca_result)

#On récupère les coordonnées
base_q31$f1_q31 <- pca_result$ind$coord[,1]
base_q31$f2_q31 <- pca_result$ind$coord[,2]

#Et on remet ça dans notre base en ne gardant que le premier qui est le seul à se démarquer
base <- merge(base, base_q31[, c("id", "f1_q31")], by = "id", all.x = TRUE)



###
#On termine en regardant ce que nos NA cumulés donnent...
sum(rowSums(is.na(base[, c("f1_q10", "f1_q22", "f1_q26", "f1_q31")])) > 0)
baseNA <- base %>% subset(rowSums(is.na(base[, c("f1_q10", "f1_q22", "f1_q26", "f1_q31")])) > 0)
table(baseNA$ville)

# Donc on a quand même près d'un quart de l'échantillon qu'on perd...
# =>  Si je ne me trompe pas, c'est 1079 individus qui ont au moins un NA sur les 4 questions étudiées, mais qui peuvent
# avoir répondu aux autres

# Nettoyage de la mémoire

rm(base_q10, base_q22, base_q26, base_q31, baseNA, pca_result, q10NA, q22NA, q26NA, q31NA)
gc()


# #Tentative de projeter q10 et q22 ensemble, ainsi que q26 et q31
# id_q10_q22=intersect(base_q10$id, base_q22$id)
# base_q10q22=base %>%
#   filter(id %in% id_q10_q22)
# 
# KMO(select(base_q10q22, q10a, q10b, q10c, q10d, q10e, q10f, q22a, q22b, q22c, q22d, q22e, q22f, q22g))
# bartlett.test(select(base_q10q22, q10a, q10b, q10c, q10d, q10e, q10f, q22a, q22b, q22c, q22d, q22e, q22f, q22g))
# pca_result <- PCA(select(base_q10q22, q10a, q10b, q10c, q10d, q10e, q10f, q22a, q22b, q22c, q22d, q22e, q22f, q22g), scale.unit = TRUE, graph = TRUE)
# fviz_eig(pca_result)
# 
# 
# 
# id_q26_q31=intersect(base_q26$id, base_q31$id)
# base_q26q31=base %>%
#   filter(id %in% id_q26_q31)
# 
# KMO(select(base_q26q31, q26a, q26b, q26c, q26d, q26e, q31a, q31b, q31c, q31d, q31e, q31f))
# bartlett.test(select(base_q26q31, q26a, q26b, q26c, q26d, q26e, q31a, q31b, q31c, q31d, q31e, q31f))
# pca_result <- PCA(select(base_q26q31, q26a, q26b, q26c, q26d, q26e, q31a, q31b, q31c, q31d, q31e, q31f), scale.unit = TRUE, graph = TRUE)
# fviz_eig(pca_result)
# 
# 
# 
# #Ajout de l'orientation politique déclarée (q27a)
# 
# #Création de nouvelles variables pour séparer gauche-droite, ni gauche-ni droite et NSP
# base_q10q22_q27a=base_q10q22 %>%
#   filter(is.na(q27a)==FALSE) %>%                        #240 NA
#   mutate(G_D=case_when(q27a %in% c(1,2,3,4,5) ~ q27a,
#                        TRUE ~ mean(base$q27a[base$q27a %in% 1:5])), #Pour les individus ayant répondu 6 ou 7, on impute par la moyenne du sous groupe 1-5
#          niG_niD=case_when(q27a==6 ~ 1,
#                            TRUE ~ 0),
#          NSP_GD=case_when(q27a==7 ~1,
#                           TRUE ~ 0))
# 
# base_q26q31_q27a=base_q26q31 %>%
#   filter(is.na(q27a)==FALSE) %>%                        #221 NA
#   mutate(G_D=case_when(q27a %in% c(1,2,3,4,5) ~ q27a,
#                        TRUE ~ mean(base$q27a[base$q27a %in% 1:5])), #Pour les individus ayant répondu 6 ou 7, on impute par la moyenne du sous groupe 1-5
#          niG_niD=case_when(q27a==6 ~ 1,
#                            TRUE ~ 0),
#          NSP_GD=case_when(q27a==7 ~1,
#                           TRUE ~ 0))
# 
# #Ajout de ces nouvelles variables dans les ACP
# KMO(select(base_q10q22_q27a, q10a, q10b, q10c, q10d, q10e, q10f, q22a, q22b, q22c, q22d, q22e, q22f, q22g, G_D, niG_niD, NSP_GD))
# bartlett.test(select(base_q10q22_q27a, q10a, q10b, q10c, q10d, q10e, q10f, q22a, q22b, q22c, q22d, q22e, q22f, q22g, G_D, niG_niD, NSP_GD))
# pca_result <- PCA(select(base_q10q22_q27a, q10a, q10b, q10c, q10d, q10e, q10f, q22a, q22b, q22c, q22d, q22e, q22f, q22g, G_D, niG_niD, NSP_GD), scale.unit = TRUE, graph = FALSE)
# var_colors <- ifelse(rownames(pca_result$var$coord) %in% c("G_D", "niG_niD", "NSP_GD"), "red", "blue")
# fviz_pca_var(pca_result, col.var = var_colors)
# fviz_eig(pca_result)
# 
# 
# KMO(select(base_q26q31_q27a, q26a, q26b, q26c, q26d, q26e, q31a, q31b, q31c, q31d, q31e, q31f, G_D, niG_niD, NSP_GD))
# bartlett.test(select(base_q26q31_q27a, q26a, q26b, q26c, q26d, q26e, q31a, q31b, q31c, q31d, q31e, q31f, G_D, niG_niD, NSP_GD))
# pca_result <- PCA(select(base_q26q31_q27a, q26a, q26b, q26c, q26d, q26e, q31a, q31b, q31c, q31d, q31e, q31f, G_D, niG_niD, NSP_GD), scale.unit = TRUE, graph = FALSE)
# var_colors <- ifelse(rownames(pca_result$var$coord) %in% c("G_D", "niG_niD", "NSP_GD"), "red", "blue")
# fviz_pca_var(pca_result, col.var = var_colors)
# fviz_eig(pca_result)
