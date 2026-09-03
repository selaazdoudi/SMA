################################################################################
##### CONSTRUCTION DU PANEL VARIABLE EMPLOI S  #################################
##### POUR LE GLM  #############################################################
################################################################################

rm(list = ls())
gc()
library(tidyverse); library(duckdb); library(tidyr)
library(dbplyr); library(lubridate)

dir_tmp <- "C:/Users/Public/Documents/Salma_CEA/duckdb_temp"
if (!dir.exists(dir_tmp)) dir.create(dir_tmp, recursive = TRUE)

con <- dbConnect(duckdb::duckdb(),
                 dbdir = file.path(dir_tmp, "panel_salarie_glm_2022.duckdb")))

# ---- reglages memoire --------------------------------------------------------.
dbExecute(con, paste0("PRAGMA temp_directory='", dir_tmp, "';"))
dbExecute(con, "PRAGMA memory_limit='35GB';")
dbExecute(con, "PRAGMA threads=4;")   
dbExecute(con, "PRAGMA preserve_insertion_order=false;")

# ---- vues sur les sources ----------------------------------------------------
dbExecute(con, "
  CREATE OR REPLACE VIEW champ_22 AS
  SELECT * FROM read_parquet('C:/Users/Public/Documents/Salma_CEA/Extension_DARES/champ_retour_emploi_fdc2022_groupes.parquet');")


# ---- parametres --------------------------------------------------------------
date_deb_obs <- as.Date("2019-01-01")
date_fin_obs <- as.Date("2025-12-31")
mois_fin_obs <- as.Date("2025-12-01")

annees_obs   <- 2019:2025

# Utilitaire : remplace les jetons @XXX@ dans une requete SQL.
injecter <- function(sql, ...) {
  remplacements <- list(...)
  for (nom in names(remplacements)) {
    sql <- gsub(paste0("@", nom, "@"), as.character(remplacements[[nom]]), sql, fixed = TRUE)
  }
  sql
}

# Utilitaire : libere la memoire entre deux grosses etapes
respirer <- function(msg) {
  dbExecute(con, "CHECKPOINT;")
  gc()
  message(">>> ", msg, " : OK  (", format(Sys.time(), "%H:%M:%S"), ")")
}

################################################################################
##### CHAMP DE BASE ############################################################
################################################################################

champ_base_lazy <- tbl(con, "champ_22") %>%
  filter(decision_dal == "01" & type_dal %in% c("01", "02", "03")) %>%
  select("id_midas", "qualif_classe", "niv_dip", "SEXE", "naf_64", "islr", "age_ouv_droit",
         "age_fin_ctt_redresse", "rupco", "periode_essai_cdi", "groupe", "motif_fin_cdt",
         "date_fin_der_CTT", "coh_lbl", "deb_mois", "duree_cat", "KROD3", "log_sjr") %>%
  group_by(id_midas, date_fin_der_CTT) %>%
  window_order(desc(KROD3), desc(log_sjr), deb_mois) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  # Identifiant d'EPISODE : un individu peut avoir plusieurs fins de contrat (rarissime en réalité)
  mutate(id_spell = paste0(id_midas, "_", as.character(date_fin_der_CTT)))

dbExecute(con, "DROP TABLE IF EXISTS champ_base;")
champ_base <- compute(champ_base_lazy, name = "champ_base", temporary = FALSE)
respirer("champ_base materialise")

champ_base %>%
  summarise(n_lignes = n(), n_spells = n_distinct(id_spell),
            n_personnes = n_distinct(id_midas)) %>%
  collect()

tableau_groupe <- champ_base %>%
  group_by(groupe, coh_lbl) %>%
  summarise(nombre_personnes = n(), .groups = "drop") %>%
  collect() %>%
  pivot_wider(names_from = groupe, values_from = nombre_personnes, values_fill = 0) %>%
  arrange(coh_lbl)

tableau_groupe

dbExecute(con, "DROP TABLE IF EXISTS cohortes_ids;")
dbExecute(con, "
  CREATE TABLE cohortes_ids AS
  SELECT DISTINCT id_midas FROM champ_base;")

################################################################################
##### SQUELETTE DU PANEL #######################################################
################################################################################

cal <- tibble(mois_obs_cal = seq(date_deb_obs, mois_fin_obs, by = "1 month")) %>%
  mutate(nb_jours_mois = as.integer(days_in_month(mois_obs_cal)))

dbWriteTable(con, "cal_s", cal, temporary = FALSE, overwrite = TRUE)
cal_s <- tbl(con, "cal_s")

dbExecute(con, "DROP TABLE IF EXISTS panel;")
panel_squelette <- champ_base %>%
  cross_join(cal_s) %>%
  mutate(h = (year(mois_obs_cal) - year(deb_mois)) * 12 + month(mois_obs_cal) - month(deb_mois)) %>%
  filter(h >= 1 & h <= 36) %>%
  mutate(h_max_obs = (year(local(mois_fin_obs)) - year(deb_mois)) * 12 +
           month(local(mois_fin_obs)) - month(deb_mois)) %>%
  compute(name = "panel", temporary = FALSE)

dbExecute(con, "DROP TABLE IF EXISTS panel_ids;")
dbExecute(con, "
  CREATE TABLE panel_ids AS
  SELECT DISTINCT id_midas, mois_obs_cal FROM panel;")

respirer("squelette du panel")

################################################################################
#####  EMPLOI SALARIE (MMO) ####################################################
################################################################################
# Dates BRUTES pour la durabilite, dates BORNEES a la fenetre pour le depliage.

sql_raw_mmo <- "
  SELECT
    m.id_midas,
    TRY_CAST(m.DebutCTT AS DATE) AS debut_brut,
    COALESCE(TRY_CAST(m.FinCTT AS DATE), DATE '@FIN@') AS fin_brut,
    GREATEST(TRY_CAST(m.DebutCTT AS DATE), DATE '@DEB@') AS debut_obs,
    LEAST(COALESCE(TRY_CAST(m.FinCTT AS DATE), DATE '@FIN@'), DATE '@FIN@') AS fin_obs,
    CASE
      WHEN m.nature IN ('01','09','50','82','91') THEN 'CDI'
      WHEN m.nature IN ('02','07','10','20','21','32','51','52','80','81','92','93') THEN 'CDD'
      WHEN m.nature IN ('03','08') THEN 'interim'
      WHEN m.nature IN ('29','53','60','70','89','90') THEN 'autre'
      ELSE 'autre'
    END AS type_contrat,
    SUBSTR(m.CodeApet, 1, 2) AS naf_88_mmo,
    LPAD(TRIM(CAST(m.MotifRupture AS VARCHAR)),3,'0') AS motif_rupture,
    TRY_CAST(m.FinCTT AS DATE)                        AS fin_reelle,
    CASE
      WHEN (CASE
              WHEN m.nature IN ('01','09','50','82','91') THEN 'CDI'
              WHEN m.nature IN ('02','07','10','20','21','32','51','52','80','81','92','93') THEN 'CDD'
              WHEN m.nature IN ('03','08') THEN 'interim'
              ELSE 'autre'
            END) = 'CDI'
        OR ((CASE
              WHEN m.nature IN ('02','07','10','20','21','32','51','52','80','81','92','93') THEN 'CDD'
              ELSE 'x'
            END) = 'CDD'
            AND (COALESCE(TRY_CAST(m.FinCTT AS DATE), DATE '@FIN@')
                 - TRY_CAST(m.DebutCTT AS DATE) + 1) >= 180)
      THEN 1 ELSE 0
    END AS est_durable
  FROM read_parquet('//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/MMO/MMO_2_@AN@_M8.parquet',
                    union_by_name = true) m
  WHERE m.id_midas IN (SELECT id_midas FROM cohortes_ids)
    AND TRY_CAST(m.DebutCTT AS DATE) IS NOT NULL
    AND TRY_CAST(m.Quali_Salaire_Base AS INTEGER) = 7
    AND TRY_CAST(m.DebutCTT AS DATE) <= DATE '@FIN@'
    AND COALESCE(TRY_CAST(m.FinCTT AS DATE), DATE '@FIN@') >= DATE '@DEB@'
    AND TRY_CAST(m.DebutCTT AS DATE)
        <= COALESCE(TRY_CAST(m.FinCTT AS DATE), DATE '@FIN@')
"

dbExecute(con, "DROP TABLE IF EXISTS raw_mmo;")
for (an in annees_obs) {
  sql_an <- injecter(sql_raw_mmo, AN = an, DEB = date_deb_obs, FIN = date_fin_obs)
  if (an == annees_obs[1]) {
    dbExecute(con, paste0("CREATE TABLE raw_mmo AS ", sql_an))
  } else {
    dbExecute(con, paste0("INSERT INTO raw_mmo ", sql_an))
  }
  message("  lecture MMO ", an, " ... ok")
}
respirer("raw_mmo")

# --- Depliage mensuel, annee par annee ------------------------------------

sql_mmo_annee <- "
WITH contrats AS (
  SELECT id_midas, type_contrat, est_durable, naf_88_mmo,
         GREATEST(debut_obs, DATE '@AN@-01-01') AS d1,
         LEAST(fin_obs,      DATE '@AN@-12-31') AS d2
  FROM raw_mmo
  WHERE debut_obs <= DATE '@AN@-12-31'
    AND fin_obs   >= DATE '@AN@-01-01'
),
mois_contrats AS (
  SELECT id_midas, type_contrat, est_durable, naf_88_mmo,
         CAST(unnest(generate_series(
         DATE_TRUNC('month',d1),
         DATE_TRUNC('month',d2),
         INTERVAL 1 MONTH
         )) AS DATE) as mois_obs
FROM contrats
  WHERE d1 <= d2
)
SELECT id_midas, 
       mois_obs, 
       1 as y_salarie,
       MAX(CASE WHEN type_contrat = 'CDI'     THEN 1 ELSE 0 END) AS y_cdi,
       MAX(est_durable) AS y_durable,
FROM mois_contrats
  GROUP BY 1, 2
 "


dbExecute(con, "DROP TABLE IF EXISTS presence_mmo;")
for (an in annees_obs) {
  sql_an <- injecter(sql_mmo_annee, AN = an)
  if (an == annees_obs[1]) {
    dbExecute(con, paste0("CREATE TABLE presence_mmo AS ", sql_an))
  } else {
    dbExecute(con, paste0("INSERT INTO presence_mmo ", sql_an))
  }
  message("  depliage MMO ", an, " ... ok")
  gc()
}
respirer("presence_mmo")

presence_mmo_s <- tbl(con, "presence_mmo")


################################################################################
#####  JOINTURE AVEC LE SQUELETTE ##############################################
################################################################################

panel_final_lazy <- tbl(con, "panel") %>% 
  left_join(tbl(con,"presence_mmo"),
            by= c("id_midas" = "id_midas", "mois_obs_cal"="mois_obs_cal")) %>% 
  mutate(
    y_salarie=coalesce(y_salarie,0),
    y_cdi=coalesce(y_cdi,0),
    y_durable=coalesce(y_durable,0)
  )


################################################################################
#####  ENREGISTREMENT ##########################################################
################################################################################

chemin_export <- "C:/Users/Public/Documents/Salma_CEA/Extension_DARES/forets_causales/panel_pour_glm.parquet"
dbExecute(con, paste0("COPY panel_pour_glm TO '", chemin_export, "'(FORMAT PARQUET);"))
  )
