Quelques éléments sur les création de Facteurs

Ce document est à supprimer avant de finaliser le Git, mais il sert à la rédaction / traçabilité

# Facteur Q10

Il s'agit d'un facteur sur la conscience écologique / la nécessité de se mobiliser

Ici on ne garde qu'un facteur car le drop est important avec le deuxième, qui est très proche du troisième. Mais attention, on n'a qu'**1/3 de la variance**. L'axe principal semble vraiment opposer une écologie politique type gauche à une écologie subordonnée à d'autres questions, voir pas écolo. L'axe 2 est intéressant cependant, il semble sur-déterminé par la question sur le progrès technologique (50% de la contribution). Vers le bas on a peut-être des gens qui ne croient pas vraiment au changement climatique ? En tous cas, notre premier axe est très corrélé à la q11 (0,5) mais ce n'est pas le cas des deux autres facteurs

Il manque 252 observations (NA)

Rappel du code pour les contributions fviz_contrib(pca_result, choice = "var", axes = 2, top = 10)

# Facteur Q22

Il s'agit d'un facteursur le soutien déclaré à des formes de mobilisations écologiques

Ici c'est quand même un peu bullshit de traiter comme du numérique, au sens où on a bien des choses ordonnées, mais bon...

Pour les NA,on en a 443 quand même...

Sur le résultat, l'axe 1 est clairement un indicateur de volonté (déclarative) de se mobiliser (attention ! ici une valeur élevée signifie peu se mobiliser par contre). L'axe 2 se démarque aussi, donc on le garde, et là c'est clairement une question de modes d'actions (avant tout le résultat de la question sur les dégradations, secondairement sur le blocage du lycée).

# Facteur q26

Facteur sur le sentiment de compétence politique et l'intérêt.

Peu de NA ici, on exclut 145 observations. Par ailleurs, on a 2/3 de la variance sur notre axe principal, donc c'est énorme ici.

# Facteur q31

Ici il s'agit de trouver un positionnement politique --\> Peu probable d'avoir vraiment un axe...

De fait, le test KMO indique qu'on devrait plutôt renoncer, ou au moins virer la b... Mais Barlett a l'air d'accord.

Donc c'est pas mal le bordel cette histoire. EN gros, l'axe 1, avec comme principale contribution d, A notamment semble plutôt renvoyer à un positionnement économique, même si la présence de f montre que ce n'est pas exactement cela (et "c"" est encore un autre problème). L'axe 2, lui, est dominé par f, e, et b, donc plutôt des positionnements sociétaux, ou moraux. L'axe 3 j'ai du mal à comprendre, donc je ne le garde pas.

# Maintenant ?

Il reste donc à regarder un peu plus ces facteurs : pertinence, croisement avec d'autres variables, projections dans le plan de certaines variables. Pour q31 notamment il doit être possible de projeter q30, et peut-être q27, pour interpréter

# Ajout de q27a (orientation politique déclarée)

Si on rajoute l'orientation politique déclarée sur l'ACP des questions 10 et 22 (questions du module sur les convictions écologiques), on ne trouve pas (ou presque) de contribution de q27a aux axes principaux (en revanche, une forte contribution au 3ème axe). Les convictions écologiques telles que décrites par les questions 10 et 22 et l'orientation politique telle que décrite par la question 27a sont donc décorrélées, sinon orthogonales.

Pour l'ACP des question 26 et 31, l'orientation politique de 27a contribue bien aux axes, avec notamment les NSP constituant un groupe à part (logique car ce sont peut-être des élèves moins politisés, donc moins radicaux sur les autres questions, et plus irréguliers), et les élèves de droite étant d'accord pour retirer l'allocation chômage aux chômeurs n'étant pas en recherche active. Les ACP sont selon KMO et bartlett encore pertinentes, même si fatalement la variance expliquée par les axes principaux chute...
