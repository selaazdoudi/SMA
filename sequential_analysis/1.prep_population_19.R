################################################################################
##### SCRIPT 1 : PREPARATION DE LA BASE PANEL (COHORTES 2019) ##################
################################################################################
################################################################################

rm(list = ls())
gc()
library(tidyverse); library(duckdb); library(tidyr)
library(dbplyr); library(lubridate)

dir_tmp <- "C:/Users/Public/Documents/Salma_CEA/duckdb_temp"
if (!dir.exists(dir_tmp)) dir.create(dir_tmp, recursive = TRUE)

con <- dbConnect(duckdb::duckdb(),
                 dbdir = file.path(dir_tmp, "construction_panel_2019.duckdb"))

# ---- reglages memoire --------------------------------------------------------.
dbExecute(con, paste0("PRAGMA temp_directory='", dir_tmp, "';"))
dbExecute(con, "PRAGMA memory_limit='35GB';")
dbExecute(con, "PRAGMA threads=4;")   
dbExecute(con, "PRAGMA preserve_insertion_order=false;")

# ---- vues sur les sources (streaming, pas de materialisation R) --------------
dbExecute(con, "
  CREATE OR REPLACE VIEW champ_base_src AS
  SELECT * FROM read_parquet('C:/Users/Public/Documents/Salma_CEA/Extension_DARES/champ_retour_emploi_fdc2019_groupes.parquet');")

dbExecute(con, "
  CREATE OR REPLACE VIEW PJC_s AS
  SELECT * FROM read_parquet('//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/FNA/pjc.parquet');")

# ---- parametres --------------------------------------------------------------
date_deb_obs <- as.Date("2017-01-01")
date_fin_obs <- as.Date("2025-12-31")
mois_fin_obs <- as.Date("2025-12-01")
chemin_paf   <- "//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/FNA/paf.parquet"

annees_obs   <- 2017:2025

# Utilitaire : remplace les jetons @XXX@ dans une requete SQL.
# Plus sur que sprintf, qui casse des qu'un '%' apparait dans le SQL (strptime).
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

champ_base_lazy <- tbl(con, "champ_base_src") %>%
  filter(decision_dal == "01" & type_dal %in% c("01", "02", "03")) %>%
  filter(groupe %in% c("RCI", "CDI_hors_RC")) %>%
  select("id_midas", "qualif_classe", "niv_dip", "SEXE", "naf_64", "islr", "age_ouv_droit",
         "age_fin_ctt_redresse", "rupco", "periode_essai_cdi", "groupe", "motif_fin_cdt",
         "date_fin_der_CTT", "coh_lbl", "deb_mois", "duree_cat", "KROD3", "log_sjr") %>%
  group_by(id_midas, date_fin_der_CTT) %>%
  window_order(desc(KROD3), desc(log_sjr), deb_mois) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  # Identifiant d'EPISODE : un individu peut avoir plusieurs fins de contrat
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
# Deplace AVANT les sources : panel_ids sert de filtre aux blocs CNAF/retraite.

cal <- tibble(mois_obs_cal = seq(date_deb_obs, mois_fin_obs, by = "1 month")) %>%
  mutate(nb_jours_mois = as.integer(days_in_month(mois_obs_cal)))

dbWriteTable(con, "cal_s", cal, temporary = FALSE, overwrite = TRUE)
cal_s <- tbl(con, "cal_s")

dbExecute(con, "DROP TABLE IF EXISTS panel;")
panel_squelette <- champ_base %>%
  cross_join(cal_s) %>%
  mutate(h = (year(mois_obs_cal) - year(deb_mois)) * 12 + month(mois_obs_cal) - month(deb_mois)) %>%
  filter(h >= -24 & h <= 72) %>%
  mutate(h_max_obs = (year(local(mois_fin_obs)) - year(deb_mois)) * 12 +
           month(local(mois_fin_obs)) - month(deb_mois)) %>%
  compute(name = "panel", temporary = FALSE)

# Materialise UNE fois le DISTINCT (id_midas, mois) : il etait recalcule dans
# le bloc CNAF puis dans le bloc retraite.
dbExecute(con, "DROP TABLE IF EXISTS panel_ids;")
dbExecute(con, "
  CREATE TABLE panel_ids AS
  SELECT DISTINCT id_midas, mois_obs_cal FROM panel;")

respirer("squelette du panel")

################################################################################
##### SOURCE 1 : EMPLOI SALARIE (MMO) ##########################################
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

# --- Depliage journalier, annee par annee ------------------------------------

sql_mmo_annee <- "
WITH contrats AS (
  SELECT id_midas, type_contrat, est_durable, naf_88_mmo,
         GREATEST(debut_obs, DATE '@AN@-01-01') AS d1,
         LEAST(fin_obs,      DATE '@AN@-12-31') AS d2
  FROM raw_mmo
  WHERE debut_obs <= DATE '@AN@-12-31'
    AND fin_obs   >= DATE '@AN@-01-01'
),
jours AS (
  SELECT id_midas, type_contrat, est_durable, naf_88_mmo,
         CAST(unnest(generate_series(d1, d2, INTERVAL 1 DAY)) AS DATE) AS jour
  FROM contrats
  WHERE d1 <= d2
),
jours_uniq AS (
  SELECT id_midas, jour,
         MAX(est_durable) AS est_durable,
         MAX(naf_88_mmo)  AS naf_88_mmo,
         MAX(CASE WHEN type_contrat = 'CDI'     THEN 1 ELSE 0 END) AS c_cdi,
         MAX(CASE WHEN type_contrat = 'CDD'     THEN 1 ELSE 0 END) AS c_cdd,
         MAX(CASE WHEN type_contrat = 'interim' THEN 1 ELSE 0 END) AS c_int,
         MAX(CASE WHEN type_contrat = 'autre'   THEN 1 ELSE 0 END) AS c_aut
  FROM jours
  GROUP BY 1, 2
)
SELECT
  id_midas,
  CAST(DATE_TRUNC('month', jour) AS DATE) AS mois_obs,
  1                AS y_salarie,
  MAX(naf_88_mmo)  AS naf_88_mmo,
  COUNT(*)         AS jours_trav,
  MAX(c_cdi)       AS y_cdi,
  MAX(c_cdd)       AS y_cdd,
  MAX(est_durable) AS y_emploi_durable,
  SUM(c_cdi)       AS jours_cdi,
  SUM(c_cdd)       AS jours_cdd_ventiles,
  SUM(c_int)       AS jours_interim,
  SUM(c_aut)       AS jours_autre
FROM jours_uniq
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
##### SOURCE 2 : FORMATION (PJC, codes AREF) ###################################
################################################################################

codes_aref <- c("33","34","35","48","49","55","65","83",
                "AD","BC","BN","CK","DQ","EL","EX","FO","GZ","HO")
codes_aref_sql <- paste0("'", codes_aref, "'", collapse = ", ")

sql_formation_annee <- "
WITH raw_formation AS (
  SELECT id_midas,
    GREATEST(TRY_CAST(KDDPJ AS DATE), DATE '@AN@-01-01', DATE '@DEB@') AS date_deb,
    LEAST(TRY_CAST(KDFPJ AS DATE), DATE '@AN@-12-31', DATE '@FIN@')    AS date_fin
  FROM PJC_s
  WHERE KCALF IN (@CODES@)
    AND id_midas IN (SELECT id_midas FROM cohortes_ids)
    AND TRY_CAST(KDDPJ AS DATE) IS NOT NULL
    AND TRY_CAST(KDFPJ AS DATE) IS NOT NULL
    AND TRY_CAST(KDDPJ AS DATE) <= DATE '@AN@-12-31'
    AND TRY_CAST(KDFPJ AS DATE) >= DATE '@AN@-01-01'
),
formation_depliee AS (
  SELECT id_midas,
         CAST(unnest(generate_series(date_deb, date_fin, INTERVAL 1 DAY)) AS DATE) AS jour
  FROM raw_formation
  WHERE date_deb <= date_fin
),
jours_uniq AS (
  SELECT DISTINCT id_midas, jour FROM formation_depliee
)
SELECT id_midas,
  CAST(DATE_TRUNC('month', jour) AS DATE) AS mois_obs,
  1        AS jour_formation,
  COUNT(*) AS jours_formation
FROM jours_uniq
GROUP BY 1, 2
"

dbExecute(con, "DROP TABLE IF EXISTS presence_formation;")
for (an in annees_obs) {
  sql_an <- injecter(sql_formation_annee, AN = an, DEB = date_deb_obs,
                     FIN = date_fin_obs, CODES = codes_aref_sql)
  if (an == annees_obs[1]) {
    dbExecute(con, paste0("CREATE TABLE presence_formation AS ", sql_an))
  } else {
    dbExecute(con, paste0("INSERT INTO presence_formation ", sql_an))
  }
  message("  formation ", an, " ... ok")
}
respirer("presence_formation")

presence_formation_s <- tbl(con, "presence_formation")

################################################################################
##### SOURCE 3 : INDEMNISATION (PJC, KCPJC = '1') ##############################
################################################################################
sql_pjc_annee <- "
WITH raw_pjc AS (
  SELECT id_midas,
    GREATEST(TRY_CAST(KDDPJ AS DATE), DATE '@AN@-01-01', DATE '@DEB@') AS date_deb,
    LEAST(COALESCE(TRY_CAST(KDFPJ AS DATE), DATE '@FIN@'),
          DATE '@AN@-12-31', DATE '@FIN@')                             AS date_fin
  FROM PJC_s
  WHERE id_midas IN (SELECT id_midas FROM cohortes_ids)
    AND TRY_CAST(KDDPJ AS DATE) IS NOT NULL
    AND KCPJC = '1'
    AND TRY_CAST(KDDPJ AS DATE) <= DATE '@AN@-12-31'
    AND COALESCE(TRY_CAST(KDFPJ AS DATE), DATE '@FIN@') >= DATE '@AN@-01-01'
),
pjc_depliee AS (
  SELECT id_midas,
         CAST(unnest(generate_series(date_deb, date_fin, INTERVAL 1 DAY)) AS DATE) AS jour
  FROM raw_pjc
  WHERE date_deb <= date_fin
),
jours_uniq AS (
  SELECT DISTINCT id_midas, jour FROM pjc_depliee
)
SELECT id_midas,
  CAST(DATE_TRUNC('month', jour) AS DATE) AS mois_obs,
  1        AS indemnise,
  COUNT(*) AS jours_indemnises
FROM jours_uniq
GROUP BY 1, 2
"

dbExecute(con, "DROP TABLE IF EXISTS presence_pjc;")
for (an in annees_obs) {
  sql_an <- injecter(sql_pjc_annee, AN = an, DEB = date_deb_obs, FIN = date_fin_obs)
  if (an == annees_obs[1]) {
    dbExecute(con, paste0("CREATE TABLE presence_pjc AS ", sql_an))
  } else {
    dbExecute(con, paste0("INSERT INTO presence_pjc ", sql_an))
  }
  message("  indemnisation ", an, " ... ok")
  gc()
}
respirer("presence_pjc")

presence_pjc_s <- tbl(con, "presence_pjc")

################################################################################
##### SOURCE 4 : MINIMA SOCIAUX (CNAF) #########################################
################################################################################

base_cnaf  <- "//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2025T3/CNAF Minima sociaux"
base_2025  <- "//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/CNAF Minima sociaux"

fichiers_cnaf_annee <- function(an) {
  if (an <= 2024) {
    sprintf("%s/%d/cnaf_indiv_%02d%02d.parquet", base_cnaf, an, 1:12, an %% 100)
  } else {
    sprintf("%s/2025/cnaf_indiv_%02d25.parquet", base_2025, 1:7)
  }
}

dbExecute(con, "DROP TABLE IF EXISTS presence_cnaf;")
for (an in annees_obs) {
  
  fichiers <- paste0("'", fichiers_cnaf_annee(an), "'", collapse = ", ")
  
  sql_an <- paste0("
    WITH raw_cnaf AS (
      SELECT m.id_midas, m.RSAVERS, m.MTRSAVER, m.PPAVERS, m.AAHVERS, m.MTPPAVER, m.MTAAHVER,
      CASE
        WHEN CAST(substr(CAST(m.DTREFFRE AS VARCHAR),1,4) AS INTEGER) <= 2022
          THEN strptime(CAST(m.DTREFFRE AS VARCHAR), '%Y-%d-%m')::DATE
        ELSE strptime(CAST(m.DTREFFRE AS VARCHAR), '%Y-%m-%d')::DATE
      END AS DTREFFRE_clean
      FROM read_parquet([", fichiers, "], union_by_name = true) m
      WHERE m.id_midas IN (SELECT id_midas FROM cohortes_ids)
    ),
    cible AS (
      SELECT id_midas, mois_obs_cal FROM panel_ids
      WHERE year(mois_obs_cal) = ", an, "
    )
    SELECT c.id_midas, c.mois_obs_cal AS mois_obs,
           MAX(CASE WHEN m.RSAVERS IN ('C','L','J','E') AND TRY_CAST(m.MTRSAVER AS DOUBLE) > 0 THEN 1 ELSE 0 END) AS y_rsa,
           MAX(CASE WHEN m.PPAVERS != '0' AND TRY_CAST(m.MTPPAVER AS DOUBLE) > 0 THEN 1 ELSE 0 END) AS y_ppa,
           MAX(CASE WHEN m.AAHVERS != '0' AND TRY_CAST(m.MTAAHVER AS DOUBLE) > 0 THEN 1 ELSE 0 END) AS y_aah
    FROM cible c
    JOIN raw_cnaf m
      ON c.id_midas = m.id_midas
     AND date_trunc('month', m.DTREFFRE_clean) = c.mois_obs_cal
    GROUP BY 1, 2
  ")
  
  if (an == annees_obs[1]) {
    dbExecute(con, paste0("CREATE TABLE presence_cnaf AS ", sql_an))
  } else {
    dbExecute(con, paste0("INSERT INTO presence_cnaf ", sql_an))
  }
  message("  CNAF ", an, " ... ok")
  gc()
}
respirer("presence_cnaf")

presence_cnaf_s <- tbl(con, "presence_cnaf")

################################################################################
##### SOURCE 5 : RETRAITE (FHS + MMO) ##########################################
################################################################################

dbExecute(con,"DROP TABLE IF EXISTS retraite_mmo_brut;")
dbExecute(con, injecter("
  CREATE TABLE retraite_mmo_brut AS
  SELECT id_midas, motif_rupture, fin_reelle AS date_ret_mmo
  FROM raw_mmo
  WHERE motif_rupture IN ('038','039')
    AND fin_reelle IS NOT NULL
    AND fin_reelle BETWEEN DATE '@DEB@' AND DATE '@FIN@'
", DEB = date_deb_obs, FIN = date_fin_obs))

respirer("retraite_mmo_brut")

dbExecute(con, "DROP TABLE IF EXISTS date_retraite_ind;")
dbExecute(con, "
  CREATE TABLE date_retraite_ind AS
  WITH ret_ft AS (
    SELECT d.id_midas, MIN(TRY_CAST(d.DATANN AS DATE)) AS date_ret
    FROM read_parquet('//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/FHS/de.parquet') d
    WHERE d.id_midas IN (SELECT id_midas FROM cohortes_ids)
      AND d.MOTANN = '05'
    GROUP BY 1
  ),
  ret_mmo AS(
    SELECT id_midas, MIN(date_ret_mmo) AS date_ret
    FROM retraite_mmo_brut
    GROUP BY 1
  ),
  sources AS (
    SELECT id_midas, date_ret, 'ft' AS src FROM ret_ft
    UNION ALL
    SELECT id_midas, date_ret, 'mmo' AS src FROM ret_mmo
  )
  SELECT 
    id_midas,
    MIN(date_ret)                          AS date_ret,
    MIN(CASE WHEN src = 'ft' THEN date_ret END)  AS date_ret_ft,
    MIN(CASE WHEN src = 'mmo' THEN date_ret END) AS date_ret_mmo
  FROM sources
  GROUP BY 1
")

dbExecute(con, "DROP TABLE IF EXISTS presence_retraite;")
dbExecute(con, "
  CREATE TABLE presence_retraite AS
  SELECT 
  c.id_midas,
  c.mois_obs_cal AS mois_obs,
  MAX(CASE WHEN date_trunc('month', p.date_ret) <= c.mois_obs_cal THEN 1 ELSE 0 END) AS y_retraite,
  MAX(CASE WHEN date_trunc('month', p.date_ret_ft) <= c.mois_obs_cal THEN 1 ELSE 0 END) AS y_retraite_ft,
  MAX(CASE WHEN date_trunc('month', p.date_ret_mmo) <= c.mois_obs_cal THEN 1 ELSE 0 END) AS y_retraite_mmo,
  MAX(CASE WHEN date_trunc('month', p.date_ret) <= c.mois_obs_cal 
              AND COALESCE(m.y_salarie,0) = 1 THEN 1 ELSE 0 END) AS y_cumul_emploi_retraite
  FROM panel_ids c
  JOIN date_retraite_ind p
    ON c.id_midas = c.id_midas
  LEFT JOIN presence_mmo m
    ON m.id_midas = c.id_midas
   AND m.mois_obs = c.mois_obs_cal
  GROUP BY 1,2
")
respirer("presence_retraite")

presence_retraite_s <- tbl(con, "presence_retraite")


################################################################################
##### SOURCE 6 : ANS, PRESENCE SOUS LES DEUX BORNES ############################
################################################################################
#   indemnise = 1 -> on observe      -> min = 1, max = 1
#   indemnise = 0 -> on ne sait plus -> min = 0 (on ferme toutes les ANS)
#                                       max = 1 (elles sont toutes en cours)

dbExecute(con, "DROP TABLE IF EXISTS raw_paf;")
dbExecute(con, injecter("
  CREATE TABLE raw_paf AS
  SELECT id_midas,
    CAST(DATE_TRUNC('month', TRY_CAST(KDDPE AS DATE)) AS DATE) AS mois_deb,
    CAST(DATE_TRUNC('month', TRY_CAST(KDFPE AS DATE)) AS DATE) AS mois_fin_declaree,
    (TRY_CAST(KDFPE AS DATE) IS NULL) AS ans_sans_fin
  FROM read_parquet('@PAF@')
  WHERE KCFPL = '03'
    AND id_midas IN (SELECT id_midas FROM cohortes_ids)
    AND TRY_CAST(KDDPE AS DATE) IS NOT NULL
", PAF = chemin_paf))

dbExecute(con, "DROP TABLE IF EXISTS presence_paf;")
dbExecute(con, injecter("
  CREATE TABLE presence_paf AS
  WITH paf_mensuel AS (
    SELECT p.id_midas, c.mois_obs_cal AS mois_obs,
      MAX(CASE WHEN p.ans_sans_fin THEN 1 ELSE 0 END) AS ans_ouverte
    FROM raw_paf p
    JOIN cal_s c
      ON c.mois_obs_cal >= p.mois_deb
     AND c.mois_obs_cal <= COALESCE(p.mois_fin_declaree, DATE '@MFIN@')
    GROUP BY p.id_midas, c.mois_obs_cal
  )
  SELECT
    a.id_midas,
    a.mois_obs,
    a.ans_ouverte,
    CASE WHEN COALESCE(j.indemnise, 0) = 1 THEN 1 ELSE 0 END AS y_ns_min,
    1 AS y_ns_max
  FROM paf_mensuel a
  LEFT JOIN presence_pjc j
    ON j.id_midas = a.id_midas AND j.mois_obs = a.mois_obs
", MFIN = mois_fin_obs))
respirer("presence_paf")

presence_paf_ns <- tbl(con, "presence_paf")

presence_paf_ns %>%
  summarise(n = n(),
            part_ouverte = mean(as.numeric(ans_ouverte)),
            tx_min = mean(as.numeric(y_ns_min)),
            tx_max = mean(as.numeric(y_ns_max))) %>%
  collect()

################################################################################
##### SOURCE 7 : DEBUTS D'ANS (flag d'acces) ###################################
################################################################################

dbExecute(con, "DROP TABLE IF EXISTS debut_ans;")
dbExecute(con, "
  CREATE TABLE debut_ans AS
  SELECT id_midas, mois_deb AS mois_obs, 1 AS y_debut_ns
  FROM raw_paf
  GROUP BY 1, 2
")

debut_ans_s <- tbl(con, "debut_ans")

################################################################################
##### JOINTURE #################################################################
################################################################################

panel_joint_lazy <- tbl(con, "panel") %>%
  left_join(presence_mmo_s,       by = c("id_midas" = "id_midas", "mois_obs_cal" = "mois_obs")) %>%
  left_join(presence_formation_s, by = c("id_midas" = "id_midas", "mois_obs_cal" = "mois_obs")) %>%
  left_join(presence_pjc_s,       by = c("id_midas" = "id_midas", "mois_obs_cal" = "mois_obs")) %>%
  left_join(presence_paf_ns,      by = c("id_midas" = "id_midas", "mois_obs_cal" = "mois_obs")) %>%
  left_join(debut_ans_s,          by = c("id_midas" = "id_midas", "mois_obs_cal" = "mois_obs")) %>%
  left_join(presence_cnaf_s,      by = c("id_midas" = "id_midas", "mois_obs_cal" = "mois_obs")) %>%
  left_join(presence_retraite_s,  by = c("id_midas" = "id_midas", "mois_obs_cal" = "mois_obs")) %>%
  mutate(
    y_salarie          = coalesce(y_salarie, 0),
    jours_trav         = coalesce(jours_trav, 0),
    y_cdi              = coalesce(y_cdi, 0),
    y_cdd              = coalesce(y_cdd, 0),
    y_emploi_durable   = coalesce(y_emploi_durable, 0),
    y_formation        = coalesce(jour_formation, 0),
    jours_formation    = coalesce(jours_formation, 0),
    jours_cdi          = coalesce(jours_cdi, 0),
    jours_cdd_ventiles = coalesce(jours_cdd_ventiles, 0),
    jours_interim      = coalesce(jours_interim, 0),
    jours_autre        = coalesce(jours_autre, 0),
    
    indemnise          = coalesce(indemnise, 0),
    jours_indemnises   = coalesce(jours_indemnises, 0),
    
    y_ns_min           = coalesce(y_ns_min, 0),
    y_ns_max           = coalesce(y_ns_max, 0),
    y_debut_ns         = coalesce(y_debut_ns, 0),
    ans_ouverte        = coalesce(ans_ouverte, 0),
    
    y_rsa              = coalesce(y_rsa, 0),
    y_ppa              = coalesce(y_ppa, 0),
    y_aah              = coalesce(y_aah, 0),
    y_retraite         = coalesce(y_retraite, 0),
    y_retraite_ft      = coalesce(y_retraite_ft, 0),
    y_retraite_mmo      = coalesce(y_retraite_mmo, 0),
    y_cumul_emploi_retraite      = coalesce(y_cumul_emploi_retraite, 0),
    
    
    
    jours_ns_min       = nb_jours_mois * y_ns_min,
    jours_ns_max       = nb_jours_mois * y_ns_max,
    
    part_mois_trav      = jours_trav       / nb_jours_mois,
    part_mois_indemnise = jours_indemnises / nb_jours_mois,
    
    y_emploi_global_min = if_else(y_salarie == 1 | y_ns_min == 1, 1, 0),
    y_emploi_global_max = if_else(y_salarie == 1 | y_ns_max == 1, 1, 0),
    
    # Alias attendus par le script de sequences
    f_indemnise = indemnise,
    f_minima    = if_else(y_rsa == 1 | y_ppa == 1 | y_aah == 1, 1, 0),
    f_retraite  = y_retraite
  )

dbExecute(con, "DROP TABLE IF EXISTS panel_joint;")
panel_joint <- compute(panel_joint_lazy, name = "panel_joint", temporary = FALSE)
respirer("panel_joint materialise")

# Les tables sources ne servent plus : on libere l'espace du fichier .duckdb
for (t in c("presence_mmo", "presence_formation", "presence_pjc", "presence_paf",
            "debut_ans", "presence_cnaf", "presence_retraite", "retraite_mmo_brut",
            "date_retraite_ind",
            "raw_mmo", "raw_paf", "panel", "panel_ids")) {
  dbExecute(con, paste0("DROP TABLE IF EXISTS ", t, ";"))
}
respirer("nettoyage des tables intermediaires")

panel_final_lazy <- panel_joint %>%
  mutate(
    top_salarie_post_rupture    = if_else(h >= 1, y_salarie, 0),
    top_debut_ns_post_rupture   = if_else(h >= 1, y_debut_ns, 0),
    top_global_min_post_rupture = if_else(h >= 1, y_emploi_global_min, 0),
    top_global_max_post_rupture = if_else(h >= 1, y_emploi_global_max, 0)
  ) %>%
  group_by(id_spell) %>%
  window_order(h) %>%
  mutate(
    y_acces_emploi            = if_else(cumsum(top_salarie_post_rupture)    > 0, 1, 0),
    y_acces_emploi_ns         = if_else(cumsum(top_debut_ns_post_rupture)   > 0, 1, 0),
    y_acces_emploi_global_min = if_else(cumsum(top_global_min_post_rupture) > 0, 1, 0),
    y_acces_emploi_global_max = if_else(cumsum(top_global_max_post_rupture) > 0, 1, 0)
  ) %>%
  ungroup()

dbExecute(con, "DROP TABLE IF EXISTS panel_final;")
panel_final <- compute(panel_final_lazy, name = "panel_final", temporary = FALSE)
respirer("panel_final materialise")

################################################################################
##### CONTROLES ################################################################
################################################################################

# Unicite de la cle : doit renvoyer 0
panel_final %>% count(id_spell, h) %>% filter(n > 1) %>%
  summarise(nb_doublons = n()) %>% collect()

# Coherence des compteurs de jours
panel_final %>%
  summarise(depass_trav  = sum(if_else(jours_trav       > nb_jours_mois, 1, 0), na.rm = TRUE),
            depass_indem = sum(if_else(jours_indemnises > nb_jours_mois, 1, 0), na.rm = TRUE)) %>%
  collect()

# Bornes ANS et couverture par horizon
panel_final %>%
  group_by(h, groupe) %>%
  summarise(tx_ns_min = mean(y_ns_min), tx_ns_max = mean(y_ns_max),
            tx_indemnise = mean(indemnise), tx_minima = mean(f_minima),
            n = n(), .groups = "drop") %>%
  collect() %>% arrange(h, groupe) %>% print(n = 40)

################################################################################
##### EXPORT ###################################################################
################################################################################
# COPY depuis la table deja materialisee 

chemin_export <- "C:/Users/Public/Documents/Salma_CEA/Etude_typeboulle/lt/panel_pour_appariement_2019.parquet"

dbExecute(con, paste0(
  "COPY (SELECT * FROM panel_final) TO '", chemin_export,
  "' (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 122880);"))

message("Export termine : ", chemin_export)

dbDisconnect(con, shutdown = TRUE)
