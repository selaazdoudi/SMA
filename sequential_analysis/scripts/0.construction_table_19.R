################################################################################
##### TABLE RETOUR A L'EMPLOI FINS DE CONTRAT 2019, OD INITIALES ARE ###########
################################################################################

##################
### CHARGEMENT ###
##################
library(tidyverse)
library(arrow)
library(duckdb)
library(lubridate)
library(dbplyr)

dir_out <- "C:/Users/Public/Documents/Salma_CEA/Extension_DARES/"
dir_tmp <- "C:/Users/Public/Documents/Salma_CEA/duckdb_temp"
if(!dir.exists(dir_tmp)) dir.create(dir_tmp, recursive = TRUE)
if(!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# 1. Configuration de la base DuckDB 
con <- dbConnect(duckdb::duckdb(), dbdir = file.path(dir_tmp, "midas_temp.duckdb"))
dbExecute(con, "PRAGMA memory_limit='50GB';") 
dbExecute(con, "PRAGMA threads=8;")           

# 2. Lecture  
duckdb_register_arrow(con, "DE_s", open_dataset("//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/FHS/de.parquet"))
duckdb_register_arrow(con, "PJC_s", open_dataset("//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/FNA/pjc.parquet"))
duckdb_register_arrow(con, "ODD_s", open_dataset("//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/FNA/odd.parquet"))
duckdb_register_arrow(con, "CDT_s", open_dataset("//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/FNA/cdt.parquet"))
duckdb_register_arrow(con, "DAL_s", open_dataset("//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/FNA/dal.parquet"))
duckdb_register_arrow(con, "mmo_22", open_dataset("//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/MMO/MMO_2_2022_M8.parquet"))
duckdb_register_arrow(con, "mmo_23", open_dataset("//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/MMO/MMO_2_2023_M8.parquet"))
duckdb_register_arrow(con, "mmo_24", open_dataset("//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/MMO/MMO_2_2024_M8.parquet"))
duckdb_register_arrow(con, "mmo_25", open_dataset("//casd.fr/casdfs/Projets/UNEDIC0/Data/MIDAS_MIDAS_2026T1/MMO/MMO_2_2025_M8.parquet"))

# Creation des pointeurs
DE_s  <- tbl(con, "DE_s")
PJC_s <- tbl(con, "PJC_s")
ODD_s <- tbl(con, "ODD_s")
CDT_s <- tbl(con, "CDT_s")
DAL_s <- tbl(con, "DAL_s")
mmo_22 <- tbl(con, "mmo_22")
mmo_23 <- tbl(con, "mmo_23")
mmo_24 <- tbl(con, "mmo_24")
mmo_25 <- tbl(con, "mmo_25")

#################
### PARAMETRES ##
#################
fdc_deb <- as.Date("2019-01-01")   # Fenetre des FINS DE CONTRAT retenues (Annee 2019)
fdc_fin <- as.Date("2019-12-31")
fin_obs <- as.Date("2025-12-31")   # Fin d'observation MMO
date_cc <- as.Date("2023-02-01")

are_codes <- c("01","21","22","27","40","43","47","54","64","67","82",
               "AC","BB","BK","CJ","DM","EF","EW","FL","GV","HF","HI")
rci_codes <- c("88","93","161","162","163","164","165","166","167",
               "168","169","170","171","172","173","174","175")

################################################################################
################### SOCLE : DROITS + DATE D'OUVERTURE (PJC) #####################
################################################################################
date_ouv_droit <- PJC_s %>%
  group_by(id_midas, KROD3) %>%
  summarise(deb_droit = min(KDDPJ, na.rm = TRUE), .groups = "drop") %>%
  compute(name = "date_ouv_droit", temporary = TRUE)

recup_od <- ODD_s %>%
  rename(KROD3 = KROD1) %>%
  right_join(date_ouv_droit, by = c("id_midas", "KROD3")) %>%
  rename(duree_max = KPJDXP, annexe = KCRD, SJR_ouv = KQCSJP, islr = KQISLP) %>%
  group_by(id_midas, KROD3) %>% filter(duree_max == max(duree_max, na.rm=TRUE)) %>% ungroup() %>%
  group_by(id_midas, KROD3) %>% filter(SJR_ouv == max(SJR_ouv, na.rm=TRUE))  %>% ungroup() %>%
  group_by(id_midas, KROD3) %>% window_order(id_midas, KROD3) %>%
  mutate(ordre = row_number()) %>% ungroup() %>% filter(ordre == 1) %>%
  filter(KCAAJ %in% are_codes & !annexe %in% c("28","29") & duree_max > 0) %>%
  select(id_midas, KROD3, deb_droit, KCCT1, duree_max, annexe, SJR_ouv,islr, KQBIAP) %>%
  compute(name = "recup_od", temporary = TRUE)

recup_dal <- DAL_s %>%
  rename(KROD3 = KROD1) %>%
  select(id_midas, KROD3, KCNDDA, KCDAL) %>%
  rename(decision_dal = KCNDDA, type_dal = KCDAL) %>%
  inner_join(recup_od, by = c("id_midas", "KROD3")) %>%
  mutate(ordre = case_when(decision_dal == "01" ~ 1,
                           decision_dal == "02" ~ 2,
                           decision_dal == "10" ~ 3,
                           decision_dal %in% c("12","13") ~ 4,
                           decision_dal == "11" ~ 5,
                           decision_dal == "08" ~ 6,
                           decision_dal %in% c("03","09") ~ 7,
                           decision_dal %in% c("04","05","06","07") ~ 8,
                           is.na(decision_dal) ~ 9,
                           TRUE ~ 10)) %>%
  group_by(id_midas, KROD3) %>% filter(ordre == min(ordre, na.rm=TRUE)) %>% ungroup() %>%
  group_by(id_midas, KROD3) %>% window_order(id_midas, KROD3) %>%
  filter(row_number() == 1) %>% ungroup() %>%
  filter(decision_dal %in% c("01","10","12","13")) %>%
  compute(name = "recup_dal", temporary = TRUE)

################################################################################
################## CONTRAT GENERATEUR : DOUBLE APPARIEMENT CDT ##################
################################################################################
CDT_par_ODD <- CDT_s %>%
  rename(KCCT1 = KCCT) %>%
  select(id_midas, KCCT1, KDFPE, KCMCA, KDDPE, KCMET, KCEP2) %>%
  right_join(recup_dal, by = c("id_midas", "KCCT1")) %>%
  group_by(id_midas, KROD3) %>%
  mutate(cdt_na = ifelse(is.na(KDFPE), 1, 0), nb_cdt_na = sum(cdt_na, na.rm=TRUE), nb_obs = n()) %>% ungroup() %>%
  mutate(retenu = case_when(nb_cdt_na == nb_obs ~ 1,
                            nb_cdt_na <  nb_obs & cdt_na == 0 ~ 1, TRUE ~ 0)) %>%
  filter(retenu == 1) %>%
  group_by(id_midas, KROD3) %>% window_order(id_midas, KROD3) %>%
  mutate(ordre = row_number()) %>% ungroup() %>% filter(ordre == 1) %>% select(-ordre)

par_dal <- DAL_s %>%
  rename(KROD3 = KROD1) %>%
  select(id_midas, KROD3, KCDA) %>%
  right_join(recup_dal, by = c("id_midas", "KROD3"))

CDT_par_dal <- CDT_s %>%
  rename(KCDA = KCDA1) %>%
  select(id_midas, KCDA, KDFPE, KCMCA, KDDPE, KCMET, KCEP2) %>%
  right_join(par_dal, by = c("id_midas", "KCDA")) %>%
  group_by(id_midas, KROD3) %>%
  mutate(cdt_na = ifelse(is.na(KDFPE), 1, 0), nb_cdt_na = sum(cdt_na, na.rm=TRUE), nb_obs = n()) %>% ungroup() %>%
  mutate(retenu = case_when(nb_cdt_na == nb_obs ~ 1,
                            nb_cdt_na <  nb_obs & cdt_na == 0 ~ 1, TRUE ~ 0)) %>%
  filter(retenu == 1) %>%
  group_by(id_midas, KROD3) %>% window_order(id_midas, KROD3) %>%
  mutate(ordre = row_number()) %>% ungroup() %>% filter(ordre == 1) %>% select(-ordre, -retenu)

recup_CDT_tot <- union_all(CDT_par_dal, CDT_par_ODD) %>% 
  group_by(id_midas, KROD3) %>%
  mutate(cdt_na = ifelse(is.na(KDFPE), 1, 0), nb_cdt_na = sum(cdt_na, na.rm=TRUE), nb_obs = n()) %>% ungroup() %>%
  mutate(retenu = case_when(nb_cdt_na == nb_obs ~ 1,
                            nb_cdt_na <  nb_obs & cdt_na == 0 ~ 1, TRUE ~ 0)) %>%
  filter(retenu == 1) %>%
  group_by(id_midas, KROD3) %>% window_order(id_midas, KROD3) %>%
  mutate(ordre = row_number()) %>% ungroup() %>% filter(ordre == 1) %>% select(-ordre, -retenu) %>%
  rename(date_fin_der_CTT = KDFPE, motif_rupture_der_CTT = KCMCA,
         date_deb_der_CTT = KDDPE, metier_der_CTT = KCMET, code_naf = KCEP2) %>%
  compute(name = "recup_CDT_tot", temporary = TRUE)

# Nettoyage manuel pour menager l'espace disque 
dbExecute(con, "DROP TABLE recup_od")

################################################################################
############### CARACTERISTIQUES (DE) + REDRESSEMENT DUREEE/AGE ##################
################################################################################
recup_inscription <- DE_s %>%
  select(id_midas, DATINS, DATANN, NIVFOR, QUALIF, SEXE, datnais, DIPLOME) %>%
  right_join(recup_CDT_tot, by = c("id_midas")) %>%
  mutate(inscrit_sur_od = case_when(DATINS <= deb_droit & (DATANN >= deb_droit | is.na(DATANN)) ~ 1, TRUE ~ 0),
         ecart_temp = abs(as.numeric(deb_droit - DATINS))) %>%
  group_by(id_midas, KROD3) %>%
  mutate(keep = case_when(inscrit_sur_od == 1 ~ 2,
                          inscrit_sur_od == 0 & ecart_temp == min(ecart_temp, na.rm=TRUE) ~ 1, TRUE ~ 0)) %>% ungroup() %>%
  group_by(id_midas, KROD3) %>% filter(keep == max(keep, na.rm=TRUE)) %>% ungroup() %>%
  group_by(id_midas, KROD3) %>% window_order(id_midas) %>% filter(row_number() == 1) %>% ungroup() %>%
  mutate(niv_dip = case_when(NIVFOR %in% c("AFS","C12","C3A","CFG","CP4","NV5") | (NIVFOR == "NV4" & DIPLOME == "N") ~ "inferieur_bac",
                             (NIVFOR == "NV4" & DIPLOME == "D") | (NIVFOR == "NV3" & DIPLOME == "N") ~ "Bac",
                             NIVFOR %in% c("NV2","NV1") | (NIVFOR == "NV3" & DIPLOME == "D") ~ "superieur",
                             TRUE ~ "manquant"),
         qualif_classe = case_when(QUALIF %in% c("1","2","5") ~ "ouvrier_employe_non_qualifie",
                                   QUALIF %in% c("3","4","6") ~ "ouvrier_employe_qualifie",
                                   QUALIF %in% c("7","8") ~ "agent_maitrise",
                                   QUALIF == "9" ~ "cadre", TRUE ~ "manquant"))

duree_max_redressee <- recup_inscription %>%
  mutate(age_ouv_droit = as.numeric(deb_droit - datnais)/365.25,
         age_fin_ctt   = as.numeric(date_fin_der_CTT - datnais)/365.25,
         age_fin_ctt_redresse = ifelse(is.na(age_fin_ctt), age_ouv_droit, age_fin_ctt)) %>%
  mutate(duree_max = case_when(
    age_fin_ctt_redresse < 53 & (date_fin_der_CTT < date_cc | (is.na(date_fin_der_CTT) & deb_droit < date_cc)) ~ if_else(duree_max > 730, 730, duree_max),
    age_fin_ctt_redresse >= 53 & age_fin_ctt_redresse < 55 & (date_fin_der_CTT < date_cc | (is.na(date_fin_der_CTT) & deb_droit < date_cc)) ~ if_else(duree_max > 913, 913, duree_max),
    age_fin_ctt_redresse >= 55 & (date_fin_der_CTT < date_cc | (is.na(date_fin_der_CTT) & deb_droit < date_cc)) ~ if_else(duree_max > 1095, 1095, duree_max),
    age_fin_ctt_redresse < 53 & (date_fin_der_CTT >= date_cc | (is.na(date_fin_der_CTT) & deb_droit >= date_cc)) ~ if_else(duree_max > 548, 548, duree_max),
    age_fin_ctt_redresse >= 53 & age_fin_ctt_redresse < 55 & (date_fin_der_CTT >= date_cc | (is.na(date_fin_der_CTT) & deb_droit >= date_cc)) ~ if_else(duree_max > 685, 685, duree_max),
    age_fin_ctt_redresse >= 55 & (date_fin_der_CTT >= date_cc | (is.na(date_fin_der_CTT) & deb_droit >= date_cc)) ~ if_else(duree_max > 822, 822, duree_max)))

################################################################################
############### MOTIF DE RUPTURE + RCI + GROUPE D'ANALYSE #######################
################################################################################
regroupement_motifs_fin_ctt <- duree_max_redressee %>%
  mutate(duree_cdt = as.numeric(date_fin_der_CTT - date_deb_der_CTT) + 1,
         rupco = if_else(motif_rupture_der_CTT %in% rci_codes, 1, 0),
         motif_fin_cdt = case_when(
           motif_rupture_der_CTT %in% c("10","11","12","13","14","15","16","17","22","23","24","25","26","27","30","31","32","33","34","39","80","85") ~ "Licenciement economique",
           motif_rupture_der_CTT %in% c("18","19","20","21","28","29","35","36","37","42","44","46","49","50","51","57","82","83","86","91","95","116") ~ "Autre licenciement",
           motif_rupture_der_CTT %in% rci_codes ~ "RCI",
           motif_rupture_der_CTT %in% c("92","94") ~ "RCC",
           motif_rupture_der_CTT %in% c("43","45","52","60","62","63","64","65","66","67","68","69","70","71","72","77","78","79","87","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155") ~ "Depart volontaire",
           motif_rupture_der_CTT %in% c("35","40","54","86","87") ~ "Fin de CDD",
           motif_rupture_der_CTT == "41" ~ "Fin d'une mission d'interim",
           motif_rupture_der_CTT %in% c("54","81") ~ "Fin de contrat d'apprentissage",
           motif_rupture_der_CTT %in% c("00","48","90","99","999","XXX","38","84") ~ "Autres ou manquant",
           TRUE ~ "NA"),
         periode_essai_cdi = if_else(motif_rupture_der_CTT == "42" & duree_cdt > 31, 1, 0)) %>%
  filter(motif_fin_cdt != "NA") %>%
  mutate(fin_cdi = if_else(motif_fin_cdt %in% c("Licenciement economique","Autre licenciement",
                                                "RCI","RCC","Depart volontaire") |
                             periode_essai_cdi == 1, 1, 0),
         groupe = case_when(rupco == 1 ~ "RCI",
                            fin_cdi == 1 & !motif_fin_cdt %in% c("RCI","RCC") ~ "CDI_hors_RC",
                            TRUE ~ "Autre")) %>%
  compute(name = "regroupement_motifs_fin_ctt", temporary = TRUE)

dbExecute(con, "DROP TABLE recup_CDT_tot")

# DEFINITION DES COHORTES
reperages_cohortes <- regroupement_motifs_fin_ctt %>%
   filter(!is.na(date_fin_der_CTT)) %>% 
   
   # CORRECTION 1 : filter() au lieu de mutate()
   filter(date_fin_der_CTT >= fdc_deb & date_fin_der_CTT <= fdc_fin) %>%
   
   mutate(coh      = strftime(date_fin_der_CTT, "%m"),
          coh_lbl = strftime(date_fin_der_CTT, "%Y_%m"),
          deb_mois = date_trunc("month", date_fin_der_CTT)) %>%
   distinct(id_midas, KROD3, date_fin_der_CTT, .keep_all = TRUE) %>%
   mutate(
     pcs = case_when(
     QUALIF %in% c("1","2","5") ~ "Employes/ouvriers non qualifies",
     QUALIF %in% c("3","4","6","7") ~ "Employes/ouvriers qualifies, techniciens",
     QUALIF %in% c("8","9") ~ "Cadres et professions intermediaires",
     TRUE ~ "NA"
   ),
   log_sjr  = log(1+SJR_ouv),
   duree_cat = if_else(duree_max > 0 & duree_max < 182, "Moins de 6 mois",
               if_else(duree_max >= 182 & duree_max < 364, "6-12 mois",
               if_else(duree_max >= 364 & duree_max < 548,"12-18 mois",
               if_else(duree_max >= 548 & duree_max < 730, "18-24 mois",
               if_else(duree_max >= 730, "24 mois ou plus", "NA"))))),
   
   naf_88 = substr(code_naf,1,2),
   naf_64 = case_when(
     naf_88 == "01" ~ "AZ1", naf_88 == "02" ~ "AZ2", naf_88 == "03" ~ "AZ3",
     naf_88 %in% c("05", "06", "07", "08", "09") ~ "BZ0",
     naf_88 %in% c("10", "11", "12") ~ "CA0", naf_88 %in% c("13", "14", "15") ~ "CB0",
     naf_88 == "16" ~ "CC1", naf_88 == "17" ~ "CC2", naf_88 == "18" ~ "CC3",
     naf_88 == "19" ~ "CD0", naf_88 == "20" ~ "CE0", naf_88 == "21" ~ "CF0",
     naf_88 == "22" ~ "CG1", naf_88 == "23" ~ "CG2", naf_88 == "24" ~ "CH1",
     naf_88 == "25" ~ "CH2", naf_88 == "26" ~ "CI0", naf_88 == "27" ~ "CJ0",
     naf_88 == "28" ~ "CK0", naf_88 == "29" ~ "CL1", naf_88 == "30" ~ "CL2",
     naf_88 %in% c("31", "32") ~ "CM1", naf_88 == "33" ~ "CM2", naf_88 == "35" ~ "DZ0",
     naf_88 == "36" ~ "EZ1", naf_88 %in% c("37", "38", "39") ~ "EZ2",
     naf_88 %in% c("41", "42", "43") ~ "FZ0", naf_88 == "45" ~ "GZ1",
     naf_88 == "46" ~ "GZ2", naf_88 == "47" ~ "GZ3", naf_88 == "49" ~ "HZ1",
     naf_88 == "50" ~ "HZ2", naf_88 == "51" ~ "HZ3", naf_88 == "52" ~ "HZ4",
     naf_88 == "53" ~ "HZ5", naf_88 %in% c("55", "56") ~ "IZ0", naf_88 == "58" ~ "JA1",
     naf_88 %in% c("59", "60") ~ "JA2", naf_88 == "61" ~ "JB0", naf_88 %in% c("62", "63") ~ "JC0",
     naf_88 == "64" ~ "KZ1", naf_88 == "65" ~ "KZ2", naf_88== "66" ~ "KZ3",
     naf_88 == "68" ~ "LZ0", naf_88 %in% c("69", "70") ~ "MA1", naf_88 == "71" ~ "MA2",
     naf_88 == "72" ~ "MB0", naf_88 == "73" ~ "MC1",   naf_88 %in% c("74", "75") ~ "MC2",
     naf_88 == "77" ~ "NZ1", naf_88 == "78" ~ "NZ2", naf_88 == "79" ~ "NZ3",
     naf_88 %in% c("80", "81", "82") ~ "NZ4", naf_88 == "84" ~ "OZ0", naf_88 == "85" ~ "PZ0",
     naf_88 == "86" ~ "QA0", naf_88 %in% c("87", "88") ~ "QQB", naf_88 %in% c("90", "91", "92") ~ "RZ1",
     naf_88 == "93" ~ "RZ2", naf_88 == "94" ~ "SZ1", naf_88 == "95" ~ "SZ2",
     naf_88 == "96" ~ "SZ3", naf_88 %in% c("97", "98") ~ "TZ0", naf_88 == "99" ~ "UZ0",
     TRUE ~ "Inconnu"
   )
 ) %>% 
 compute(name="reperage_cohortes", temporary=TRUE)


# -----------------------------------------------------------------------------
# ECRITURE FINALE EN PARQUET
# -----------------------------------------------------------------------------
# CORRECTION 2 : Export via collect() puis write_parquet() pour un fichier propre
reperages_cohortes %>%
  collect() %>%
  write_parquet(file.path(dir_out, "champ_retour_emploi_fdc2019_groupes.parquet"))


# FIN DU SCRIPT : Nettoyage pour recuperer l'espace CASD
dbDisconnect(con, shutdown = TRUE)
file.remove(file.path(dir_tmp, "midas_temp.duckdb"))
if(file.exists(file.path(dir_tmp, "midas_temp.duckdb.wal"))) {
  file.remove(file.path(dir_tmp, "midas_temp.duckdb.wal"))
}


