################################################################################
##### MODÉLISATION GLM MOIS PAR MOIS ET TAUX CONTRÔLÉS #########################
################################################################################

# Nettoyage de l'environnement
rm(list = ls())
gc()

# Librairies
library(tidyverse)
library(arrow)
library(broom)
library(writexl)

# 1. Chemins d'accès
chemin_panel <- "C:/Users/Public/Documents/Salma_CEA/Extension_DARES/forets_causales/panel_pour_glm.parquet"
export_path  <- "C:/Users/Public/Documents/Salma_CEA/Extension_DARES/resultats"
if (!dir.exists(export_path)) dir.create(export_path, recursive = TRUE)

# 2. Chargement et préparation des données
# On prépare les facteurs et on définit la variable d'intérêt (équivalent de rupco)
df <- read_parquet(chemin_panel) %>% 
  mutate(
    # Transformation en facteurs
    across(c(SEXE, niv_dip, qualif_classe, naf_64, duree_cat), as.factor),
    h_fac = factor(h, levels = 1:36),
    
    # Création d'une variable binaire d'intérêt (à adapter selon vos vrais groupes)
    # Par exemple, si groupe "1" est votre population cible :
    est_traite = as.integer(groupe == "1") 
  )

# 3. Paramétrage des modèles
# Variables explicatives du modèle (contrôle + variable d'intérêt)
termes_exp <- c("est_traite", "SEXE", "age_ouv_droit", "niv_dip", 
                "qualif_classe", "duree_cat", "log_sjr", "islr", "naf_64")

fm_salarie <- reformulate(termes_exp, response = "y_salarie")
fm_durable <- reformulate(termes_exp, response = "y_durable")

# Initialisation des listes de résultats
coef_start_sal <- NULL
coef_start_dur <- NULL

coefs_list_sal <- vector("list", 36)
taux_all_list_sal <- vector("list", 36)
taux_traite_list_sal <- vector("list", 36)

coefs_list_dur <- vector("list", 36)
taux_all_list_dur <- vector("list", 36)
taux_traite_list_dur <- vector("list", 36)

# 4. Boucle sur les horizons (M+1 à M+36)
for(i in 1:36) {
  
  message(sprintf("Estimation Horizon M+%d ...", i))
  
  # Filtre sur l'horizon
  dfi <- df %>% 
    filter(h == i) %>%
    mutate(M = paste0("M+", i))
  
  ###### A. Modèle : Emploi Salarié (y_salarie) ######
  mod_sal <- glm(fm_salarie, 
                 data = dfi,
                 family = binomial(link = "logit"),
                 na.action = na.exclude,
                 control = list(maxit = 50),
                 start = coef_start_sal)
  
  # Récupération des coefficients
  coefs_list_sal[[i]] <- tidy(mod_sal) %>% 
    mutate(horizon = i, modele = "emploi_salarie")
  
  # Prédictions et contrefactuel (est_traite = 0)
  eta_sal <- predict(mod_sal, newdata = dfi, type = "link")
  b_t_sal <- unname(coef(mod_sal)[["est_traite"]])
  p_hat_sal_ctrl <- plogis(eta_sal - b_t_sal * as.numeric(dfi$est_traite))
  
  ###### B. Modèle : Emploi Durable (y_durable) ######
  mod_dur <- glm(fm_durable, 
                 data = dfi,
                 family = binomial(link = "logit"),
                 na.action = na.exclude,
                 control = list(maxit = 50),
                 start = coef_start_dur)
  
  coefs_list_dur[[i]] <- tidy(mod_dur) %>% 
    mutate(horizon = i, modele = "emploi_durable")
  
  # Prédictions et contrefactuel (est_traite = 0)
  eta_dur <- predict(mod_dur, newdata = dfi, type = "link")
  b_t_dur <- unname(coef(mod_dur)[["est_traite"]])
  p_hat_dur_ctrl <- plogis(eta_dur - b_t_dur * as.numeric(dfi$est_traite))
  
  ###### C. Intégration et Agrégation ######
  dfi <- dfi %>% 
    mutate(p_hat_sal_ctrl = p_hat_sal_ctrl,
           p_hat_dur_ctrl = p_hat_dur_ctrl)
  
  # Agrégation globale (Taux bruts vs Taux contrôlés)
  agg_all <- dfi %>% 
    group_by(M, coh_lbl) %>% 
    summarise(
      n = n(),
      brut_sal = mean(y_salarie, na.rm = TRUE),
      brut_dur = mean(y_durable, na.rm = TRUE),
      ctrl_sal = mean(p_hat_sal_ctrl, na.rm = TRUE),
      ctrl_dur = mean(p_hat_dur_ctrl, na.rm = TRUE),
      .groups = "drop"
    )
  
  taux_all_list_sal[[i]] <- agg_all %>% select(M, coh_lbl, n, brut = brut_sal, ctrl = ctrl_sal)
  taux_all_list_dur[[i]] <- agg_all %>% select(M, coh_lbl, n, brut = brut_dur, ctrl = ctrl_dur)
  
  # Agrégation ciblée (Uniquement pour le groupe traité)
  agg_traite <- dfi %>% 
    filter(est_traite == 1) %>% 
    group_by(M, coh_lbl) %>% 
    summarise(
      n = n(),
      brut_sal = mean(y_salarie, na.rm = TRUE),
      brut_dur = mean(y_durable, na.rm = TRUE),
      ctrl_sal = mean(p_hat_sal_ctrl, na.rm = TRUE),
      ctrl_dur = mean(p_hat_dur_ctrl, na.rm = TRUE),
      .groups = "drop"
    )
  
  taux_traite_list_sal[[i]] <- agg_traite %>% select(M, coh_lbl, n, brut = brut_sal, ctrl = ctrl_sal)
  taux_traite_list_dur[[i]] <- agg_traite %>% select(M, coh_lbl, n, brut = brut_dur, ctrl = ctrl_dur)
  
  # Mise à jour des points de départ pour le mois suivant
  coef_start_sal <- tryCatch(coef(mod_sal), error = function(e) NULL)
  coef_start_dur <- tryCatch(coef(mod_dur), error = function(e) NULL)
  
  # Libération de la mémoire
  rm(mod_sal, mod_dur, eta_sal, eta_dur, p_hat_sal_ctrl, p_hat_dur_ctrl, dfi, agg_all, agg_traite)
  gc()
} 

# 5. Consolidation des résultats

# Fonction utilitaire pour consolider les coefficients
consolider_coefs <- function(liste_coefs) {
  bind_rows(liste_coefs) %>% 
    mutate(
      significativite = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.10 ~ ".",
        TRUE ~ ""
      )
    ) %>%
    select(horizon, term, estimate, p.value, significativite)
}

tab_beta_sal <- consolider_coefs(coefs_list_sal)
tab_beta_dur <- consolider_coefs(coefs_list_dur)

# Fonction utilitaire pour consolider les courbes
consolider_courbes <- function(liste_taux) {
  bind_rows(liste_taux) %>% 
    group_by(M) %>% 
    summarise(
      taux_brut = 100 * sum(n * brut, na.rm = TRUE) / sum(n, na.rm = TRUE),
      taux_ctrl = 100 * sum(n * ctrl, na.rm = TRUE) / sum(n, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(
      M = factor(M, levels = paste0("M+", 1:36)),
      ecart = taux_brut - taux_ctrl
    ) %>% 
    arrange(M)
}

courbe_traite_sal <- consolider_courbes(taux_traite_list_sal)
courbe_traite_dur <- consolider_courbes(taux_traite_list_dur)

# 6. Exportation vers Excel
file_export <- file.path(export_path, "Resultats_Modelisation_Emploi.xlsx")

sheet_export <- list(
  "Courbe_Traite_Sal" = courbe_traite_sal,
  "Courbe_Traite_Dur" = courbe_traite_dur,
  "Coefs_Sal_Detail" = tab_beta_sal %>% filter(term == "est_traite") %>% mutate(odds_ratio = exp(estimate)),
  "Coefs_Dur_Detail" = tab_beta_dur %>% filter(term == "est_traite") %>% mutate(odds_ratio = exp(estimate)),
  "Tous_Coefs_Sal" = tab_beta_sal,
  "Tous_Coefs_Dur" = tab_beta_dur
)

write_xlsx(sheet_export, file_export)
message("Fin du traitement ! Résultats exportés dans : ", file_export)
