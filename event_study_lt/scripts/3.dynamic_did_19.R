################################################################################
##### SCRIPT 3 : DYNAMIC DID & STATISTIQUES DESCRIPTIVES #######################
##### Version optimisee memoire : DuckDB fait les jointures et agregations #####
################################################################################

rm(list = ls())
gc()

library(tidyverse)
library(arrow)
library(duckdb)
library(dbplyr)
library(fixest)
library(broom)
library(writexl)

dir_out     <- "C:/Users/Public/Documents/Salma_CEA/Etude_typeboulle/lt/"
dir_tmp     <- "C:/Users/Public/Documents/Salma_CEA/duckdb_temp"
export_path <- file.path(dir_out, "resultats")
if (!dir.exists(export_path)) dir.create(export_path, recursive = TRUE)

# Dernier horizon ou toutes les cohortes 2019 sont observees
H_MAX <- 72

################################################################################
# 1. CONNEXION ET JOINTURE COTE DUCKDB
################################################################################

con <- dbConnect(duckdb::duckdb(), dbdir = file.path(dir_tmp, "did.duckdb"))
dbExecute(con, paste0("PRAGMA temp_directory='", dir_tmp, "';"))
dbExecute(con, "PRAGMA memory_limit='35GB';")
dbExecute(con, "PRAGMA threads=8;")
dbExecute(con, "PRAGMA preserve_insertion_order=false;")

duckdb_register_arrow(con, "panel_src",
                      open_dataset(file.path(dir_out, "panel_pour_appariement_2019.parquet")))
duckdb_register_arrow(con, "apparie_src",
                      open_dataset(file.path(dir_out, "echantillon_apparie_2019.parquet")))

# Table de travail : seulement les apparies, seulement les colonnes du modele
dbExecute(con, sprintf("
  CREATE OR REPLACE TABLE panel_es AS
  SELECT
    p.id_spell, p.id_midas, p.deb_mois, p.h,
    a.traite, a.paire, a.groupe,
    p.y_salarie, p.y_cdi, p.y_emploi_durable, p.y_formation,p.y_ns_min, p.y_ns_max,
    p.y_acces_emploi, p.y_acces_emploi_ns,
    p.y_acces_emploi_global_min, p.y_acces_emploi_global_max,
  FROM panel_src p
  JOIN apparie_src a ON a.id_spell = p.id_spell
  WHERE p.h <= %d
", H_MAX))

panel_db <- tbl(con, "panel_es")

# Controle de taille avant de rapatrier 
panel_db %>% summarise(n_lignes = n(), n_spells = n_distinct(id_spell)) %>% collect()

################################################################################
# 2. STATISTIQUES DESCRIPTIVES (agregees dans DuckDB)
################################################################################
# L'agregation se fait cote base : seul le resultat, quelques centaines de
# lignes, revient en RAM.

stats_descriptives <- panel_db %>%
  group_by(h, traite) %>%
  summarise(
    n                   = n(),
    taux_salarie        = mean(y_salarie, na.rm = TRUE),
    taux_cdi            = mean(y_cdi, na.rm = TRUE),
    taux_emploi_durable = mean(y_emploi_durable, na.rm = TRUE),
    taux_formation      = mean(y_formation, na.rm = TRUE),
    taux_ns_min         = mean(y_ns_min, na.rm = TRUE),
    taux_ns_max         = mean(y_ns_max, na.rm = TRUE),
    taux_indemnise      = mean(indemnise, na.rm = TRUE),
    taux_minima         = mean(f_minima, na.rm = TRUE),
    taux_retraite       = mean(y_retraite, na.rm = TRUE),
    taux_acces_ns       = mean(y_acces_emploi_ns, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  collect() %>%
  mutate(groupe_nom = if_else(traite == 1, "RCI", "Temoins"))

stats_desc_excel <- stats_descriptives %>%
  select(-traite) %>%
  pivot_wider(names_from = groupe_nom, values_from = c(-h, -groupe_nom),
              names_glue = "{.value}_{groupe_nom}") %>%
  arrange(h)

################################################################################
# 3. FONCTIONS ECONOMETRIQUES
################################################################################
# On ne rapatrie qu'un outcome a la fois : le data.frame passe a feols ne
# contient que les 5 colonnes necessaires, puis il est detruit.

charger_outcome <- function(outcome, h_min = -24) {
  panel_db %>%
    filter(h >= h_min) %>%
    select(all_of(c("id_spell", "deb_mois", "h", "traite", "paire", outcome))) %>%
    collect()
}

estimer_event_study <- function(outcome) {
  d <- charger_outcome(outcome, h_min = -24)
  f <- as.formula(paste0(outcome,
                         " ~ i(h, ref = -1) + i(h, traite, ref = -1) | id_spell + deb_mois"))
  m <- feols(f, data = d, cluster = ~id_spell, mem.clean = TRUE)
  rm(d); gc()
  m
}

estimer_event_study_cumule <- function(outcome) {
  d <- charger_outcome(outcome, h_min = -1)
  f <- as.formula(paste0(outcome,
                         " ~ i(h, ref = -1) + i(h, traite, ref = -1) | paire + deb_mois"))
  m <- feols(f, data = d, cluster = ~id_spell, mem.clean = TRUE)
  rm(d); gc()
  m
}

extraire_event_study <- function(modele, nom_outcome) {
  df_coefs <- tidy(modele, conf.int = TRUE) %>%
    filter(str_detect(term, "traite")) %>%
    mutate(h = as.numeric(str_extract(term, "-?\\d+")), outcome = nom_outcome) %>%
    select(outcome, h, estimate, conf.low, conf.high, p.value)
  
  bind_rows(df_coefs,
            tibble(outcome = nom_outcome, h = -1,
                   estimate = 0, conf.low = 0, conf.high = 0, p.value = NA_real_)) %>%
    arrange(h)
}

################################################################################
# 4. ESTIMATIONS
################################################################################

estimer_et_extraire <- function(outcome, libelle, cumule = FALSE) {
  message("  estimation : ", libelle)
  m <- if (cumule) estimer_event_study_cumule(outcome) else estimer_event_study(outcome)
  r <- extraire_event_study(m, libelle)
  rm(m); gc()
  r
}

res_salarie        <- estimer_et_extraire("y_salarie",        "Emploi salarie")
res_cdi            <- estimer_et_extraire("y_cdi",            "Emploi en CDI")
res_emploi_durable <- estimer_et_extraire("y_emploi_durable", "Emploi durable")
res_formation      <- estimer_et_extraire("y_formation",      "Presence en formation")

res_ns_min         <- estimer_et_extraire("y_ns_min", "Emploi non salarie (borne basse)")
res_ns_max         <- estimer_et_extraire("y_ns_max", "Emploi non salarie (borne haute)")

res_acces_salarie  <- estimer_et_extraire("y_acces_emploi",    "Acces a l'emploi salarie", cumule = TRUE)
res_acces_ns       <- estimer_et_extraire("y_acces_emploi_ns", "Acces a l'emploi non salarie", cumule = TRUE)
res_acces_glob_min <- estimer_et_extraire("y_acces_emploi_global_min", "Acces global (borne basse)", cumule = TRUE)
res_acces_glob_max <- estimer_et_extraire("y_acces_emploi_global_max", "Acces global (borne haute)", cumule = TRUE)

resultats_finaux <- bind_rows(
  res_salarie, res_cdi, res_emploi_durable, res_formation,
  res_ns_min, res_ns_max,
  res_acces_salarie, res_acces_ns, res_acces_glob_min, res_acces_glob_max)

################################################################################
# 5. EXPORT
################################################################################

write_xlsx(
  list(
    "Statistiques_Brutes"    = stats_desc_excel,
    "Tous_les_resultats_DiD" = resultats_finaux,
    "Emploi_Salarie"         = res_salarie,
    "Emploi_CDI"             = res_cdi,
    "Emploi_Durable"         = res_emploi_durable,
    "Formation"              = res_formation,
    "NonSalarie_BorneBasse"  = res_ns_min,
    "NonSalarie_BorneHaute"  = res_ns_max,
    "Acces_Salarie"          = res_acces_salarie,
    "Acces_NonSalarie"       = res_acces_ns,
    "Acces_Global_Min"       = res_acces_glob_min,
    "Acces_Global_Max"       = res_acces_glob_max
  ),
  path = file.path(export_path, "Resultat_Event_Study_RCI_2019.xlsx")
)
