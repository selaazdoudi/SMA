################################################################################
##### SCRIPT 2  : PANEL 30 MOIS, COURBES, CUMULS ET SANKEY #####################
################################################################################

library(tidyverse)
library(arrow)
library(duckdb)
library(writexl)
library(lubridate)

dir_tmp <- "C:/Users/Public/Documents/Salma_CEA/duckdb_temp"
if(!dir.exists(dir_tmp)) dir.create(dir_tmp, recursive = TRUE)

con <- dbConnect(duckdb::duckdb(), dbdir = file.path(dir_tmp, "midas_part3.duckdb"))
dbExecute(con, "PRAGMA memory_limit='50GB';") 
dbExecute(con, "PRAGMA threads=8;")

export_path <- "C:/Users/Public/Documents/Salma_CEA/extension_dares/resultats/"
if(!dir.exists(export_path)) dir.create(export_path, recursive = TRUE)

# 1. On charge le champ de base généré par le Script 1
duckdb_register_arrow(con, "champ_final", 
                      open_dataset("C:/Users/Public/Documents/Salma_CEA/extension_dares/champ_retour_emploi_fdc2022_groupes_enrichi.parquet/part-0.parquet"))
champ_final <- tbl(con, "champ_final")


apercu_champ <- dbGetQuery(con,"
   SELECT *
   FROM champ_final
   LIMIT 100
   ")

################################################################################
##### ÉTAPE 1 : LE SOCLE DU PANEL (INDIVIDUS x 30 MOIS) + GROUPE ENSEMBLE
################################################################################

dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE cohortes AS
  SELECT DISTINCT id_midas, deb_mois, groupe FROM champ_final WHERE groupe IN ('RCI', 'CDI_hors_RC')
  UNION ALL
  SELECT DISTINCT id_midas, deb_mois, 'Ensemble' AS groupe FROM champ_final
")
cohortes <- tbl(con, "cohortes")

# Création d'une table avec les identifiants uniques 
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE cohortes_ids AS
  SELECT DISTINCT id_midas FROM cohortes
")

cal <- tibble(mois_obs_cal = seq(as.Date("2022-01-01"), as.Date("2025-12-31"), by = "1 month"))
dbWriteTable(con, "cal_s", cal, temporary = TRUE)
cal_s <- tbl(con, "cal_s")

panel <- cohortes %>% 
  cross_join(cal_s) %>% 
  mutate(h = (year(mois_obs_cal) - year(deb_mois)) * 12 + month(mois_obs_cal) - month(deb_mois)) %>% 
  filter(h >= 1 & h <= 30) %>% 
  compute(name = "panel", temporary = TRUE)

################################################################################
##### ÉTAPE 2 : RÉCUPÉRATION DES VARIABLES D'ACTIVITÉ ##########################
################################################################################

# --- SOURCE 1 : EMPLOI SALARIÉ & EMPLOI DURABLE (MMO) ---
# ÉTAPE A : Extraction avec Semi-Join 
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE raw_mmo AS
  SELECT 
    m.id_midas, 
    TRY_CAST(m.DebutCTT AS DATE) AS DebutCTT,
    COALESCE(TRY_CAST(m.FinCTT AS DATE), CAST('2025-12-31' AS DATE)) AS FinCTT_clean,
    CASE
      WHEN m.nature IN ('01','09','50','82','91') THEN 'CDI'
      WHEN m.nature IN ('02','07','10','20','21','32','51','52','80','81','92','93') THEN 'CDD'
      WHEN m.nature IN ('03','08') THEN 'interim'
      WHEN m.nature IN ('29','53','60','70','89','90') THEN 'autre'
      ELSE 'autre'
    END AS type_contrat
  FROM read_parquet([
    '//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/MMO/MMO_2_2022_M8.parquet',
    '//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/MMO/MMO_2_2023_M8.parquet',
    '//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/MMO/MMO_2_2024_M8.parquet',
    '//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/MMO/MMO_2_2025_M8.parquet'
  ], union_by_name = true) m
  
  -- SEMI JOIN POUR EVITER LE DOUBLE COMPTAGE
  WHERE m.id_midas IN (SELECT id_midas FROM cohortes_ids)
    AND TRY_CAST(m.DebutCTT AS DATE) IS NOT NULL
    AND TRY_CAST(m.Quali_Salaire_Base AS INTEGER) = 7
")

# ÉTAPE B : Calculs 
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE presence_mmo AS
  WITH contrats_avec_durabilite AS (
    SELECT *,
      CASE 
        WHEN type_contrat = 'CDI' OR (type_contrat = 'CDD' AND (FinCTT_clean - DebutCTT + 1 >= 180)) 
        THEN 1 ELSE 0 
      END AS est_durable
    FROM raw_mmo
  ),
  contrats_deploys AS (
    SELECT 
      id_midas,
      CAST(DATE_TRUNC('month', jour) AS DATE) AS mois_obs,
      1 AS y_salarie,
      1 AS jour_travaille,
      CASE WHEN type_contrat = 'CDI' THEN 1 ELSE 0 END AS y_cdi,
      CASE WHEN type_contrat = 'CDD' THEN 1 ELSE 0 END AS y_cdd,
      est_durable
    FROM (
      SELECT id_midas, type_contrat, est_durable,
             unnest(generate_series(DebutCTT, FinCTT_clean, INTERVAL 1 DAY)) AS jour
      FROM contrats_avec_durabilite
      WHERE DebutCTT <= FinCTT_clean
    ) t
  )
  SELECT 
    id_midas,
    mois_obs,
    MAX(y_salarie) AS y_salarie,
    LEAST(SUM(jour_travaille), 30) AS jours_trav,
    MAX(y_cdi) AS y_cdi,
    MAX(y_cdd) AS y_cdd,
    MAX(est_durable) AS y_emploi_durable
  FROM contrats_deploys
  GROUP BY 1, 2
")

# --- SOURCE 2 : INDEMNISATION CHÔMAGE & JOURS INDEMNISÉS (PJC) ---
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE presence_pjc AS
  SELECT 
    id_midas,
    CAST(DATE_TRUNC('month', d.jour) AS DATE) AS mois_obs,
    COUNT(d.jour) AS jours_indem
  FROM (
    SELECT p.id_midas, 
           unnest(generate_series(TRY_CAST(p.KDDPJ AS DATE), TRY_CAST(p.KDFPJ AS DATE), INTERVAL 1 DAY)) AS jour
    FROM read_parquet('//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/FNA/pjc.parquet') p
    
    -- SEMI JOIN POUR EVITER LE DOUBLE COMPTAGE
    WHERE p.id_midas IN (SELECT id_midas FROM cohortes_ids)
      AND p.KCPJC = '1' 
      AND TRY_CAST(p.KDDPJ AS DATE) IS NOT NULL 
      AND TRY_CAST(p.KDFPJ AS DATE) IS NOT NULL
      AND TRY_CAST(p.KDDPJ AS DATE) <= TRY_CAST(p.KDFPJ AS DATE)
  ) d
  GROUP BY 1, 2
")

# --- SOURCE 3 : NON-SALARIAT (PAF) ---
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE presence_paf AS
  WITH paf_filtre AS (
    SELECT p.id_midas, p.KCFPL, 
           TRY_CAST(p.KDDPE AS DATE) AS KDDPE, 
           COALESCE(TRY_CAST(p.KDFPE AS DATE), CAST('2025-12-31' AS DATE)) AS KDFPE
    FROM read_parquet('//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/FNA/paf.parquet') p
    WHERE p.id_midas IN (SELECT id_midas FROM cohortes_ids)
  )
  SELECT c.id_midas, c.mois_obs_cal AS mois_obs,
         MAX(CASE WHEN p.KDDPE <= c.deb_mois THEN 1 ELSE 0 END) AS ns_anterieur,
         MAX(CASE WHEN p.KDDPE >  c.deb_mois THEN 1 ELSE 0 END) AS ns_apres,
         1 AS y_ns
  -- DISTINCT pour ne joindre qu'une seule fois par individu/mois
  FROM (SELECT DISTINCT id_midas, mois_obs_cal, deb_mois FROM panel) c
  JOIN paf_filtre p
    ON c.id_midas = p.id_midas
   AND p.KCFPL = '03'
   AND p.KDDPE IS NOT NULL
   AND p.KDDPE <= last_day(c.mois_obs_cal)
   AND p.KDFPE >= c.mois_obs_cal
  GROUP BY 1, 2
")

# --- SOURCE 4 : MINIMA SOCIAUX (CNAF) ---
base_cnaf <- "//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2025T3/CNAF Minima sociaux"
files_old <- unlist(lapply(2022:2024, function(y) sprintf("%s/%d/cnaf_indiv_%02d%02d.parquet", base_cnaf, y, 1:12, y %% 100)))

base_2025 <- "//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/CNAF Minima sociaux"
files_2025 <- sprintf("%s/2025/cnaf_indiv_%02d25.parquet", base_2025, 1:7)

all_cnaf_files <- paste0("'", c(files_old, files_2025), "'", collapse = ", ")

dbExecute(con, paste0("
  CREATE OR REPLACE TEMP TABLE presence_cnaf AS
  WITH raw_cnaf AS (
    SELECT m.id_midas, m.RSAVERS, m.MTRSAVER, m.PPAVERS, m.AAHVERS, m.MTPPAVER, m.MTAAHVER,
    CASE 
      WHEN CAST(substr(CAST(m.DTREFFRE AS VARCHAR),1,4) AS INTEGER) <= 2022
        THEN strptime(CAST(m.DTREFFRE AS VARCHAR), '%Y-%d-%m')::DATE
      ELSE strptime(CAST(m.DTREFFRE AS VARCHAR), '%Y-%m-%d')::DATE
    END AS DTREFFRE_clean
    FROM read_parquet([", all_cnaf_files, "], union_by_name = true) m
    WHERE m.id_midas IN (SELECT id_midas FROM cohortes_ids)
  )
  SELECT c.id_midas, c.mois_obs_cal AS mois_obs,
         MAX(CASE WHEN m.RSAVERS IN ('C','L','J','E') AND TRY_CAST(m.MTRSAVER AS DOUBLE) > 0 THEN 1 ELSE 0 END) AS y_rsa,
         MAX(CASE WHEN m.PPAVERS != '0' AND TRY_CAST(m.MTPPAVER AS DOUBLE) > 0 THEN 1 ELSE 0 END) AS y_ppa,
         MAX(CASE WHEN m.AAHVERS != '0' AND TRY_CAST(m.MTAAHVER AS DOUBLE) > 0 THEN 1 ELSE 0 END) AS y_aah
  FROM (SELECT DISTINCT id_midas, mois_obs_cal FROM panel) c
  JOIN raw_cnaf m
    ON c.id_midas = m.id_midas
   AND date_trunc('month', m.DTREFFRE_clean) = c.mois_obs_cal
  GROUP BY 1, 2
"))

# --- SOURCE 5 : RETRAITE (FHS) ---
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE presence_retraite AS
  WITH date_retraite AS (
    SELECT d.id_midas, MIN(TRY_CAST(d.DATANN AS DATE)) AS date_ret
    FROM read_parquet('//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/FHS/de.parquet') d
    WHERE d.id_midas IN (SELECT id_midas FROM cohortes_ids)
      AND d.MOTANN = '05'
    GROUP BY d.id_midas
  )
  SELECT c.id_midas, c.mois_obs_cal AS mois_obs,
         MAX(CASE WHEN date_trunc('month', d.date_ret) <= c.mois_obs_cal THEN 1 ELSE 0 END) AS y_retraite
  FROM (SELECT DISTINCT id_midas, mois_obs_cal FROM panel) c
  JOIN date_retraite d
    ON c.id_midas = d.id_midas
  GROUP BY 1, 2
")

################################################################################
##### ÉTAPE 3 : ASSEMBLAGE DU PANEL ET CUMULS  #################################
################################################################################

dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE panel_complet AS
  WITH jointure_brute AS (
    SELECT 
      c.id_midas, c.groupe, c.deb_mois, c.h, c.mois_obs_cal,
      COALESCE(mmo.y_salarie, 0) AS y_salarie,
      COALESCE(mmo.jours_trav, 0) AS jours_trav_mois,
      COALESCE(mmo.y_cdi, 0) AS y_cdi,
      COALESCE(mmo.y_cdd, 0) AS y_cdd,
      COALESCE(mmo.y_emploi_durable, 0) AS y_emploi_durable,
      COALESCE(pjc.jours_indem, 0) AS jours_indem_mois,
      COALESCE(paf.y_ns, 0) AS y_ns,
      COALESCE(cnaf.y_rsa, 0) AS y_rsa,
      COALESCE(cnaf.y_ppa, 0) AS y_ppa,
      COALESCE(cnaf.y_aah, 0) AS y_aah,
      COALESCE(ret.y_retraite, 0) AS y_retraite
    FROM panel c
    LEFT JOIN presence_mmo mmo ON c.id_midas = mmo.id_midas AND c.mois_obs_cal = mmo.mois_obs
    LEFT JOIN presence_pjc pjc ON c.id_midas = pjc.id_midas AND c.mois_obs_cal = pjc.mois_obs
    LEFT JOIN presence_paf paf ON c.id_midas = paf.id_midas AND c.mois_obs_cal = paf.mois_obs
    LEFT JOIN presence_cnaf cnaf ON c.id_midas = cnaf.id_midas AND c.mois_obs_cal = cnaf.mois_obs
    LEFT JOIN presence_retraite ret ON c.id_midas = ret.id_midas AND c.mois_obs_cal = ret.mois_obs
  ),
  cumuls_individuels AS (
    SELECT *,
      IF(y_salarie = 1 OR y_ns = 1, 1, 0) AS y_emploi_global,
      SUM(jours_trav_mois) OVER (PARTITION BY id_midas, groupe ORDER BY h ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS cumul_jours_trav,
      SUM(jours_indem_mois) OVER (PARTITION BY id_midas, groupe ORDER BY h ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS cumul_jours_indem
    FROM jointure_brute
  )
  SELECT * FROM cumuls_individuels
")
panel_complet <- tbl(con, "panel_complet")

dbExecute(con, sprintf("
  COPY panel_complet TO '%s' (FORMAT PARQUET)
", file.path(export_path, "panel_complet_fdc2022.parquet")))

################################################################################
##### ÉTAPE 4 : PREPARATION DES RÉSULTATS (COURBES ET TAUX) ####################
################################################################################

resultats_courbes <- panel_complet %>% 
  group_by(h, groupe) %>%
  summarise(
    effectif = n(), 
    taux_salarie = mean(y_salarie, na.rm = TRUE) * 100, 
    taux_cdi = mean(y_cdi, na.rm = TRUE) * 100, 
    taux_cdd = mean(y_cdd, na.rm = TRUE) * 100, 
    taux_emploi_durable = mean(y_emploi_durable, na.rm = TRUE) * 100, 
    taux_ns = mean(y_ns, na.rm = TRUE) * 100, 
    taux_emploi_global = mean(y_emploi_global, na.rm = TRUE) * 100, 
    taux_rsa = mean(y_rsa, na.rm = TRUE) * 100, 
    taux_ppa = mean(y_ppa, na.rm = TRUE) * 100, 
    taux_retraite = mean(y_retraite, na.rm = TRUE) * 100,
    moyenne_cumul_travail = mean(cumul_jours_trav, na.rm = TRUE),
    moyenne_cumul_indemnise = mean(cumul_jours_indem, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  collect() %>%
  filter(groupe %in% c("Ensemble", "RCI", "CDI_hors_RC")) %>%
  mutate(
    h = as.integer(h),
    groupe_lbl = factor(case_when(
      groupe == "Ensemble" ~ "Ensemble des allocataires (ARE)", 
      groupe == "RCI" ~ "Fin de CDI : Rupture Conventionnelle", 
      groupe == "CDI_hors_RC" ~ "Fin de CDI : Autres motifs"
    ), levels = c("Ensemble des allocataires (ARE)", "Fin de CDI : Rupture Conventionnelle", "Fin de CDI : Autres motifs"))
  ) %>% arrange(groupe_lbl, h)

################################################################################
##### ÉTAPE 5 : TRANCHES ET QUALIFICATION DU PREMIER RETOUR À L'EMPLOI ##########
################################################################################

dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE tranches_premier_emploi AS
  WITH emploi_individus AS (
    SELECT DISTINCT id_midas, groupe, h, y_emploi_durable
    FROM panel_complet
    WHERE y_salarie = 1
  ),
  premier_emploi AS (
    SELECT id_midas, groupe, MIN(h) AS h_premier
    FROM emploi_individus
    GROUP BY id_midas, groupe
  ),
  qualif_premier_emploi AS (
    SELECT pe.id_midas, pe.groupe, pe.h_premier,
           MAX(e.y_emploi_durable) AS premier_est_durable
    FROM premier_emploi pe
    JOIN emploi_individus e 
      ON pe.id_midas = e.id_midas AND pe.groupe = e.groupe AND pe.h_premier = e.h
    GROUP BY 1, 2, 3
  ),
  population_avec_premier AS (
    SELECT c.id_midas, c.groupe, qpe.h_premier,
           COALESCE(qpe.premier_est_durable, 0) AS premier_est_durable
    FROM (SELECT DISTINCT id_midas, groupe FROM panel_complet) c
    LEFT JOIN qualif_premier_emploi qpe ON c.id_midas = qpe.id_midas AND c.groupe = qpe.groupe
  ),
  tranches_categorisees AS (
    SELECT id_midas, groupe,
      CASE 
        WHEN h_premier BETWEEN 1 AND 3 THEN '1. M+1 à M+3 (Rebond immédiat)'
        WHEN h_premier BETWEEN 4 AND 6 THEN '2. M+4 à M+6'
        WHEN h_premier BETWEEN 7 AND 12 THEN '3. M+7 à M+12'
        WHEN h_premier BETWEEN 13 AND 24 THEN '4. M+13 à M+24'
        WHEN h_premier > 24 AND h_premier <= 30 THEN '5. M+25 à M+30'
        ELSE '6. Non retrouvés (sur la période)'
      END AS tranche_delai,
      CASE 
        WHEN h_premier IS NULL THEN '3. Non retrouvé'
        WHEN premier_est_durable = 1 THEN '1. Premier emploi durable (CDI / CDD long)'
        ELSE '2. Premier emploi non durable (CDD court / Intérim)'
      END AS type_premier_emploi
    FROM population_avec_premier
  )
  SELECT groupe, tranche_delai, type_premier_emploi, COUNT(DISTINCT id_midas) AS effectif,
         CAST(COUNT(DISTINCT id_midas) AS DOUBLE) / SUM(COUNT(DISTINCT id_midas)) OVER (PARTITION BY groupe) AS part
  FROM tranches_categorisees
  GROUP BY groupe, tranche_delai, type_premier_emploi
")

resultats_tranches <- tbl(con, "tranches_premier_emploi") %>%
  collect() %>%
  filter(groupe %in% c("Ensemble", "RCI", "CDI_hors_RC")) %>%
  mutate(
    groupe_lbl = factor(case_when(
      groupe == "Ensemble" ~ "Ensemble des allocataires (ARE)", 
      groupe == "RCI" ~ "Fin de CDI : Rupture Conventionnelle", 
      groupe == "CDI_hors_RC" ~ "Fin de CDI : Autres motifs"
    ), levels = c("Ensemble des allocataires (ARE)", "Fin de CDI : Rupture Conventionnelle", "Fin de CDI : Autres motifs")),
    part_pct = part * 100
  )

################################################################################
##### ÉTAPE 6 : DIAGRAMME DE SANKEY (ÉTATS EXCLUSIFS PRIORITISÉS) ###############
################################################################################

resultats_sankey <- panel_complet %>%
  filter(h %in% c(6, 12, 18, 24)) %>% 
  mutate(
    etat_unique = case_when(
      # 1. Sortie du marché
      y_retraite == 1                        ~ "1. Retraite",
      
      # 2. Situations de cumul (Activité salariée soutenue par un filet de sécurité)
      y_salaire == 1 & jours_indem_mois > 0 ~ "2. Cumul Emploi Salarié + Chômage",
      y_salarie == 1 &  (y_rsa == 1 | y_ppa == 1 | y_aah == 1) ~ "3. Cumul Emploi Salarié + Minima sociaux "                     ~ "2. Emploi non durable (CDD court / Intérim)",
      
      # 3. Emploi "pur" (L'individu travaille sans percevoir d'ARE ni de minima)
      y_salarie == 1                                    ~ "4. Emploi salarié",
      
      # 4. Création d'entreprise
      y_ns == 1                                       ~ "5. Création d'entreprise",
      
      # 5. Filets de sécurité "purs"
      jours_indem_mois > 0                            ~ "6. Chômage indemnisé",
      (y_rsa == 1 | y_ppa == 1 | y_aah == 1)          ~ "7. Minima sociaux",
      
      TRUE                                            ~ "8. Autre"
    )
  ) %>%
  group_by(groupe, h, etat_unique) %>%
  summarise(effectif = n(), .groups = "drop") %>%
  collect() %>%
  filter(groupe %in% c("RCI", "CDI_hors_RC")) %>% 
  arrange(groupe, h, etat_unique)

################################################################################
##### EXPORT FINAL ET NETTOYAGE ################################################
################################################################################

write_xlsx(
  list(
    "Donnees_Courbes_Et_Cumuls" = resultats_courbes,
    "Tranches_Premier_Emploi"   = resultats_tranches,
    "Donnees_Sankey"            = resultats_sankey
  ), 
  path = file.path(export_path, "Resultats_Complets_Trajectoires.xlsx")
)


################################################################################
##### LIBEREZ DUCKDB ###########################################################
################################################################################
dbDisconnect(con, shutdown = TRUE)
file.remove(file.path(dir_tmp, "midas_part3.duckdb"))
if(file.exists(file.path(dir_tmp, "midas_part3.duckdb.wal"))) file.remove(file.path(dir_tmp, "midas_part3.duckdb.wal"))


