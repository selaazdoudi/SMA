################################################################################
##### FORÊTS CAUSALES ET SORTED CAUSAL EFFECTS (CATE & GATE) ###################
################################################################################

rm(list = ls())
gc()

# Librairies
library(tidyverse)
library(arrow)
# install.packages("grf")
library(grf) 

# 1. Chargement et préparation (Horizon h = 12)
chemin_panel <- "C:/Users/Public/Documents/Salma_CEA/Extension_DARES/forets_causales/panel_pour_glm.parquet"

df_12 <- read_parquet(chemin_panel) %>% 
  filter(h == 12) %>% 
  # Retirer les éventuelles valeurs manquantes pour la forêt (ou les imputer au préalable)
  drop_na(y_salarie, groupe, SEXE, age_ouv_droit, niv_dip, qualif_classe, naf_64, log_sjr) %>%
  mutate(
    # Variable de traitement (1 = RCI / Groupe traité, 0 = Groupe de contrôle)
    W = as.numeric(groupe == "1"),
    # Variable d'intérêt (Emploi salarié au 12ème mois)
    Y = y_salarie
  )

# 2. Préparation des matrices pour le package `grf`
# grf demande une matrice numérique pour les variables explicatives (X)
covariables <- df_12 %>% 
  select(SEXE, age_ouv_droit, niv_dip, qualif_classe, naf_64, log_sjr, islr)

# Transformation des variables catégorielles en indicatrices (One-Hot Encoding)
X <- model.matrix(~ . - 1, data = covariables)
Y <- df_12$Y
W <- df_12$W

# 3. Entraînement de la Forêt Causale
# Note : Sur un très gros volume (> 500k lignes), cela peut prendre du temps. 
# grf est parallélisé par défaut (il utilise tous les cœurs disponibles).
set.seed(42)
message("Entraînement de la forêt causale en cours...")

foret_causale <- causal_forest(
  X = X,
  Y = Y,
  W = W,
  num.trees = 2000,          # Nombre d'arbres recommandés pour la stabilité du CATE
  tune.parameters = "all",   # Optimise automatiquement la profondeur, le fractionnement, etc.
  compute.oob.predictions = TRUE # Crucial pour éviter le surapprentissage
)

# 4. Estimation du CATE pour chaque individu (Out-Of-Bag)
df_12 <- df_12 %>% 
  mutate(
    CATE = foret_causale$predictions,
    # On extrait aussi l'estimation de la variance pour faire des intervalles de confiance
    CATE_se = sqrt(foret_causale$variance.estimates)
  )

# 5. Méthode des Sorted Causal Effects
# On trie les individus selon leur CATE (du plus négatif au plus positif)
# et on les découpe en déciles (10 groupes de taille égale).
df_12 <- df_12 %>%
  mutate(decile_CATE = ntile(CATE, 10))

# Calcul du GATE (Group Average Treatment Effect) par décile
# On utilise les scores doublement robustes (AIPW) fournis par grf pour une estimation non-biaisée
scores_aipw <- get_scores(foret_causale)
df_12$score_causal <- scores_aipw

gate_par_decile <- df_12 %>%
  group_by(decile_CATE) %>%
  summarise(
    effectif = n(),
    GATE = mean(score_causal),       # Effet causal moyen du groupe
    CATE_moyen = mean(CATE),         # Prédiction moyenne du modèle (très proche du GATE)
    se_GATE = sd(score_causal) / sqrt(n()), # Erreur standard du GATE
    IC_bas = GATE - 1.96 * se_GATE,
    IC_haut = GATE + 1.96 * se_GATE,
    .groups = "drop"
  )

print(gate_par_decile)

# 6. Caractérisation des profils extrêmes
# Qui sont les perdants (Décile 1) vs les grands gagnants (Décile 10) de la RCI ?
profils_extremes <- df_12 %>%
  filter(decile_CATE %in% c(1, 10)) %>%
  group_by(decile_CATE) %>%
  summarise(
    age_moyen = mean(age_ouv_droit),
    pct_femmes = mean(SEXE == "F", na.rm = TRUE) * 100,
    salaire_jr_moyen = mean(exp(log_sjr)),
    pct_cadres = mean(qualif_classe == "Cadre", na.rm = TRUE) * 100,
    # Ajouter ici vos autres variables pour analyser qui se trouve où
    .groups = "drop"
  )

print(profils_extremes)
