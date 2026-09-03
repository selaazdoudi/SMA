################################################################################
##### SCRIPT 3bis : ECHANTILLONNAGE, SEQUENCES ET TYPOLOGIE (RCI 2019) #########
################################################################################

rm(list = ls())
gc()

library(tidyverse)
library(arrow)
library(TraMineR)
library(TraMineRextras)
library(cluster)
library(WeightedCluster)
library(RColorBrewer)

dir_out <- "C:/Users/Public/Documents/Salma_CEA/Etude_typeboulle/lt/"

# Horizon commun a toutes les cohortes 2019
H_MAX <- 72

################################################################################
# 1. ECHANTILLONNAGE STRATIFIE (10 000 EPISODES RCI)
################################################################################
# la cle est id_spell, pas id_midas. 

panel_es_lazy <- open_dataset(file.path(dir_out, "panel_pour_appariement_2019.parquet"))

df_individus <- panel_es_lazy %>%
  filter(groupe == "RCI") %>%
  distinct(id_spell, id_midas, SEXE, qualif_classe, niv_dip, duree_cat,
           age_fin_ctt_redresse, log_sjr) %>%
  collect() %>%
  mutate(
    tranche_age = case_when(
      age_fin_ctt_redresse < 30 ~ "- de 30 ans",
      age_fin_ctt_redresse >= 30 & age_fin_ctt_redresse < 40 ~ "30-39 ans",
      age_fin_ctt_redresse >= 40 & age_fin_ctt_redresse < 50 ~ "40-49 ans",
      age_fin_ctt_redresse >= 50 ~ "50 ans et +",
      TRUE ~ "Inconnu"
    ),
    tranche_alloc = ntile(log_sjr, 4)
  )

N_total       <- nrow(df_individus)
n_echantillon <- 10000
f             <- n_echantillon / N_total

set.seed(2019)

ids_echantillonnes <- df_individus %>%
  group_by(SEXE, niv_dip, qualif_classe, duree_cat, tranche_age, tranche_alloc) %>%
  mutate(
    n_strate  = n(),
    rang_alea = sample(n_strate),
    n_attendu = n_strate * f,
    n_tire    = floor(n_attendu) + as.integer(runif(1) < (n_attendu - floor(n_attendu)))
  ) %>%
  ungroup() %>%
  filter(rang_alea <= n_tire) %>%
  select(id_spell)

cat("\nTaille de l'echantillon tire :", nrow(ids_echantillonnes), "\n")

panel_echantillon <- panel_es_lazy %>%
  select(id_spell, h, y_salarie, y_ns_min, y_ns_max,
         f_indemnise, f_minima, f_retraite) %>%
  filter(h >= 1, h <= H_MAX) %>%
  collect() %>%
  inner_join(ids_echantillonnes, by = "id_spell")

################################################################################
# 2. DEFINITION DES ETATS
################################################################################

# ETATS SELON LE DEGRE D'INTEGRATION SUR LE MARCHE DU TRAVAIL 



VAR_ANS <- "y_ns_min"   # ou "y_ns_max"

seq_wide <- panel_echantillon %>%
  mutate(
    y_ans = .data[[VAR_ANS]],
    etat_seq = case_when(
      y_salarie == 1 & (f_indemnise == 1 | f_minima == 1) ~ "EMP_AIDES",
      y_salarie == 1 & f_indemnise == 0 & f_minima == 0   ~ "EMP_SAL",
      y_salarie == 0 & f_indemnise == 1                   ~ "CHOM_IND",
      y_ans == 1                                          ~ "AUTRE_ANS",
      f_minima == 1                                       ~ "AUTRE_MINIMA",
      f_retraite == 1                                     ~ "AUTRE_RETRAITE",
      TRUE                                                ~ "AUTRE_INACTIF"
    )
  ) %>%
  select(id_spell, h, etat_seq) %>%
  arrange(id_spell, h) %>%
  pivot_wider(names_from = h, values_from = etat_seq,
              names_prefix = "m", names_sort = TRUE)

# Controle : aucune sequence incomplete
stopifnot(!any(is.na(seq_wide)))
cat("Sequences completes :", nrow(seq_wide), "\n")

id_info     <- seq_wide %>% select(id_spell)
matrice_seq <- seq_wide %>% select(starts_with("m"))

################################################################################
# 3. OBJET SEQUENCE TRAMINER
################################################################################

alphabet_etats <- c("EMP_SAL", "EMP_AIDES", "CHOM_IND", "AUTRE_ANS",
                    "AUTRE_MINIMA", "AUTRE_RETRAITE", "AUTRE_INACTIF")
labels_etats <- c("Emploi salarie", "Emploi avec aides", "Chomage indemnise",
                  "Activite non salariee", "Minima sociaux", "Retraite", "Inactif")
palette_couleurs <- c("#1A9850", "#A6D96A", "#4393C3", "#D73027",
                      "#984EA3", "#4D4D4D", "#E0E0E0")

seq_obj <- seqdef(
  matrice_seq,
  var      = paste0("m", 1:H_MAX),
  alphabet = alphabet_etats,
  labels   = labels_etats,
  cpal     = palette_couleurs,
  id       = id_info$id_spell,
  xtstep   = 6
)

################################################################################
# 4. DESCRIPTION GLOBALE
################################################################################

cat("\nNombre de sequences distinctes :", nrow(seqtab(seq_obj, idx = 0)), "\n")

seqdplot(seq_obj, border = NA, with.legend = "right", cex.legend = 0.8,
         main = paste0("Repartition des etats RCI (mois 1 a ", H_MAX, ")"))
seqIplot(seq_obj, sortv = "from.start", border = NA, with.legend = "right",
         main = "Tapis des trajectoires individuelles RCI")

################################################################################
# 5. MATRICE DE DISTANCE ET CAH
################################################################################
# seqdist sur 10 000 sequences produit une matrice de 100 millions de doubles,
# soit environ 800 Mo. 

couts <- seqsubm(seq_obj, method = "CONSTANT", cval = 2)
diss  <- seqdist(seq_obj, method = "OM", indel = 1, sm = couts)

arbre <- hclust(as.dist(diss), method = "ward.D2")
plot(arbre, labels = FALSE, hang = -1, main = "CAH (Ward) sur distances OM")

qualite <- as.clustrange(arbre, diss = diss, ncluster = 10)
summary(qualite, max.rank = 3)
plot(qualite, stat = c("ASW", "HC", "PBC"), norm = "zscore")

k      <- 5   # a ajuster selon les indices ci-dessus
classe <- cutree(arbre, k = k)

rm(diss); gc()

################################################################################
# 6. DESCRIPTION DES CLASSES
################################################################################

cat("\nRepartition des classes :\n")
print(round(100 * prop.table(table(classe)), 1))

seqdplot(seq_obj, group = classe, border = NA, with.legend = "right", cex.legend = 0.7)
seqIplot(seq_obj, group = classe, sortv = "from.start", border = NA)
seqmtplot(seq_obj, group = classe, with.legend = "right")

id_info$classe <- factor(classe, levels = 1:k,
                         labels = paste0("Classe ", 1:k, " (a definir)"))

################################################################################
# 7. PROFIL SOCIODEMOGRAPHIQUE DES CLASSES
################################################################################

profil_classes <- id_info %>%
  left_join(df_individus, by = "id_spell") %>%
  group_by(classe) %>%
  summarise(
    n            = n(),
    pct_effectif = 100 * n() / nrow(id_info),
    age_moyen    = mean(age_fin_ctt_redresse, na.rm = TRUE),
    pct_femmes   = 100 * mean(SEXE == "2", na.rm = TRUE),
    pct_cadres   = 100 * mean(qualif_classe == "cadre", na.rm = TRUE),
    .groups = "drop"
  )

print(profil_classes)

write_csv(profil_classes, file.path(dir_out, "resultats", "profil_classes_sequences.csv"))
write_parquet(id_info, file.path(dir_out, "classes_sequences_2019.parquet"))