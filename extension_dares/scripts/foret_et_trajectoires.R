################################################################################
##### FORÊTS CAUSALES : HÉTÉROGÉNÉITÉ (CATE) ET TRAJECTOIRES PAR QUINTILE ######
################################################################################

# Nettoyage de l'environnement
rm(list = ls())
gc()

# Librairies
library(tidyverse)
library(arrow)
library(grf)
library(writexl)
library(ggplot2)

# 1. Chemins d'accès
chemin_panel <- "C:/Users/Public/Documents/Salma_CEA/Extension_DARES/forets_causales/panel_pour_glm.parquet"
export_path  <- "C:/Users/Public/Documents/Salma_CEA/Extension_DARES/resultats"
if (!dir.exists(export_path)) dir.create(export_path, recursive = TRUE)

# 2. Chargement des données
df <- read_parquet(chemin_panel) %>% 
  drop_na(h, y_salarie, groupe, SEXE, age_ouv_droit, niv_dip, qualif_classe, naf_64, log_sjr, islr) %>%
  mutate(
    W = as.numeric(rupco == "1"),
    Y = y_salarie
  )

unique(df$SEXE)
unique(df$qualif_classe)

# Liste pour stocker les résultats hétérogènes
resultats_heterogenes_list <- vector("list", 36)

# 3. Boucle sur les horizons (M+1 à M+36)
for(i in 1:36) {
  
  message(sprintf("Estimation de l'hétérogénéité - Horizon M+%d ...", i))
  
  dfi <- df %>% filter(h == i)
  if(nrow(dfi) == 0) next
  
  # A. Préparation
  covariables <- dfi %>% select(SEXE, age_ouv_droit, niv_dip, qualif_classe, naf_64, log_sjr, islr)
  X <- model.matrix(~ . - 1, data = covariables)
  Y <- dfi$Y
  W <- dfi$W
  
  # B. Entraînement de la forêt
  foret_mois <- causal_forest(
    X = X,
    Y = Y,
    W = W,
    num.trees = 500,           
    sample.fraction = 0.1,     
    tune.parameters = "none",
    compute.oob.predictions = TRUE,
    num.threads = parallel::detectCores() - 1
  )
  
  # C. Extraction du CATE individuel (l'effet propre à chaque individu)
  dfi$CATE <- foret_mois$predictions
  
  # D. Création des Quintiles d'effets (1 = Pire effet / Perdants, 5 = Meilleur effet / Gagnants)
  dfi <- dfi %>% mutate(quintile_CATE = ntile(CATE, 5))
  
  # E. Agrégation par Quintile (On se concentre sur le rupco TRAITÉ W == 1)
  # Pour comparer ce qu'ils ont vécu (observé) à ce qu'ils auraient vécu (contrefactuel)
  stats_quintiles <- dfi %>%
    filter(W == 1) %>% 
    group_by(quintile_CATE) %>%
    summarise(
      horizon = i,
      effectif_traites = n(),
      
      # 1. L'effet causal moyen du rupco (GATE)
      effet_causal_moyen = mean(CATE), 
      
      # 2. Le Taux Observé
      taux_observe = mean(Y) * 100,
      
      # 3. Le Taux Contrefactuel (Attendu sans RCI) = Observé - Effet
      taux_attendu_sans_rci = (mean(Y) - mean(CATE)) * 100,
      
      # 4. Profilage (Qui sont-ils à ce mois-ci ?)
      pct_femmes = mean(SEXE == "2", na.rm = TRUE) * 100,
      age_moyen = mean(age_ouv_droit, na.rm = TRUE),
      pct_cadres = mean(qualif_classe == "cadre", na.rm = TRUE) * 100,
      
      .groups = "drop"
    ) %>%
    mutate(M = paste0("M+", i))
  
  # F. Sauvegarde
  resultats_heterogenes_list[[i]] <- stats_quintiles
  
  rm(foret_mois, dfi, X, covariables, stats_quintiles)
  gc()
}

# 4. Consolidation des données
df_trajectoires <- bind_rows(resultats_heterogenes_list) %>%
  mutate(
    M = factor(M, levels = paste0("M+", 1:36)),
    Groupe = paste("Quintile", quintile_CATE)
  )

# 5. Export Excel
file_export <- file.path(export_path, "Trajectoires_Heterogenes_RCI.xlsx")
write_xlsx(list("Trajectoires_Quintiles" = df_trajectoires), file_export)
message("Données exportées dans : ", file_export)

# ==============================================================================
# 6. BONUS VISUEL : TRACER LA CONVERGENCE POUR LES EXTRÊMES (Q1 vs Q5)
# ==============================================================================

# On filtre sur les grands perdants (Q1) et les grands gagnants (Q5)
df_graph <- df_trajectoires %>% 
  filter(quintile_CATE %in% c(1, 5)) %>%
  mutate(Profil = ifelse(quintile_CATE == 5, "Q5 : Les grands gagnants de la RCI", 
                         "Q1 : Les pénalisés par la RCI"))

graph_convergence <- ggplot(df_graph, aes(x = horizon)) +
  # Courbe Taux Observé (Ligne pleine)
  geom_line(aes(y = taux_observe, color = "1. Taux Observé (Avec RCI)"), size = 1.2) +
  # Courbe Taux Contrefactuel (Ligne pointillée)
  geom_line(aes(y = taux_attendu_sans_rci, color = "2. Taux Attendu (Sans RCI)"), size = 1.2, linetype = "dashed") +
  
  # Faceting pour séparer visuellement le Q1 et le Q5
  facet_wrap(~ Profil, ncol = 2) +
  
  scale_color_manual(values = c("1. Taux Observé (Avec RCI)" = "#0073C2", 
                                "2. Taux Attendu (Sans RCI)" = "#EFC000")) +
  labs(
    title = "Convergence du retour à l'emploi : Observé vs Attendu",
    subtitle = "Comparaison entre les bénéficiaires les plus favorisés (Q5) et les plus pénalisés (Q1)",
    x = "Mois après l'entrée (Horizon)",
    y = "Taux de retour à l'emploi (%)",
    color = "Légende :"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )

print(graph_convergence)