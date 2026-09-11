################################################################################
##### SCRIPT FINAL : ANALYSE DE SEQUENCES ######################################
################################################################################
# 1. TIRAGES PAR PAIRES 
# 2. CONSTRUCTION ET FORMATAGE DES SEQUENCES 
# 3. TYPOLOGIE COMMUNE AVEC CONSOLIDATION PAM
# 4. DESCRIPTION DE LA TYPOLOGIE GLOBALE
# 5. COMPARAISON : REPARTITION DANS LES CLASSES ET INDICATEURS DE COMPLEXITE
################################################################################

rm(list = ls())
gc()

library(tidyverse)
library(arrow)
library(TraMineR)
library(TraMineRextras)
library(WeightedCluster)
library(nnet)

dir_out  <- "C:/Users/Public/Documents/Salma_CEA/Etude_typeboulle/lt/"
dir_res  <- file.path(dir_out, "resultats_communs")
if (!dir.exists(dir_res)) dir.create(dir_res, recursive = TRUE)


# --- FONCTION POUR EXPORT GRAPHIQUE ---
exporter_graphe <- function(nom_fichier, expression_graphique){
  # CORRECTION: height au lieu de hieght
  png(file.path(dir_res, nom_fichier), width=2400, height=1600, res=180)
  eval(substitute(expression_graphique))
  dev.off()
}


################################################################################
##### PARAMETRES ###############################################################
################################################################################

H_MIN_SEQ   <- -6    
H_MAX       <- 72    
H_DEB_CLUST <- 1     

N_PAIRES    <- 5000  
GRAINE      <- 2019

VAR_ANS <- "y_ns_max"
UTILISER_RETRAITE <- TRUE
SEUIL_SECRET <- 11

################################################################################
##### 1 & 2. CHARGEMENT ET TIRAGE PAR PAIRE ####################################
################################################################################

apparie <- read_parquet(file.path(dir_out, "echantillon_apparie_2019.parquet")) %>%
  select(id_spell, id_midas, paire, traite, groupe, deb_mois,
         SEXE, qualif_classe, niv_dip, duree_cat,
         age_fin_ctt_redresse, log_sjr)

verif_paires <- apparie %>%
  group_by(paire) %>%
  summarise(n = n(), n_traites = sum(traite), .groups = "drop")

stopifnot(all(verif_paires$n == 2 & verif_paires$n_traites == 1))

set.seed(GRAINE)
paires_tirees <- verif_paires %>%
  slice_sample(n = min(N_PAIRES, nrow(verif_paires))) %>%
  pull(paire)

ech <- apparie %>% filter(paire %in% paires_tirees)

cat("Paires tirées :", length(paires_tirees),
    "| séquences :", nrow(ech),
    "| dont traités :", sum(ech$traite), "\n")

################################################################################
##### 3. CONSTRUCTION DES ETATS ################################################
################################################################################

panel_lazy <- open_dataset(file.path(dir_out, "panel_pour_appariement_2019.parquet"))

panel_ech <- panel_lazy %>%
  select(id_spell, h, y_salarie, y_ns_min, y_ns_max,
         f_indemnise, f_minima, f_retraite) %>%
  filter(h >= H_MIN_SEQ, h <= H_MAX) %>%
  collect() %>%
  inner_join(ech %>% select(id_spell, paire, traite), by = "id_spell")

# CORRECTION: On utilise panel_ech (les données R) et pas panel_lazy
panel_etats <- panel_ech %>% 
    mutate(
      y_ans = .data[[VAR_ANS]],
      etat_seq = case_when(
        y_ans == 1 & y_salarie == 1                        ~ "ANS_SAL",
        y_ans == 1 & f_indemnise == 1                      ~ "ANS_INDEM",
        y_ans == 1                                         ~ "ANS_SEULE",
        y_salarie == 1 & (f_indemnise == 1 | f_minima == 1) ~ "EMP_AIDES",
        y_salarie == 1                                     ~ "EMP_SAL",
        f_indemnise == 1                                   ~ "CHOM_IND",
        f_minima == 1                                      ~ "AUTRE_MINIMA",
        UTILISER_RETRAITE & f_retraite == 1                ~ "AUTRE_RETRAITE",
        TRUE                                               ~ "NON_OBSERVE"
      )
    )

################################################################################
##### 4. OBJETS SEQUENCE #######################################################
################################################################################

alphabet_etats <- c("EMP_SAL", "EMP_AIDES", "ANS_SAL", "ANS_INDEM", "ANS_SEULE", "CHOM_IND", "AUTRE_MINIMA", "AUTRE_RETRAITE", "NON_OBSERVE")
labels_etats   <- c("Emploi salarié", "Emploi salarié avec aides", "Non salarié + salarié", "Non salarié indemnisé", "Non salarié seul", "Chômage indemnisé", "Minima sociaux", "Retraite", "Non observé")
palette_couleurs <- c("#1A9850", "#A6D96A", "#8C510A", "#D73027", "#F46D43", "#4393C3", "#984EA3", "#4D4D4D", "#E0E0E0")

etats_presents   <- intersect(alphabet_etats, unique(panel_etats$etat_seq))
idx              <- match(etats_presents, alphabet_etats)
labels_presents  <- labels_etats[idx]
couleurs_presentes <- palette_couleurs[idx]

wide_post <- panel_etats %>%
    filter(h >= H_DEB_CLUST, h <= H_MAX) %>%
    select(id_spell, h, etat_seq) %>%
    arrange(id_spell, h) %>%
    pivot_wider(
      names_from = "h", 
      values_from = "etat_seq", 
      names_prefix = "m", 
      names_sort = TRUE, 
      values_fill = "NON_OBSERVE" # SECURITE POUR TRAMINER
    )
  
cols <- paste0("m", H_DEB_CLUST:H_MAX)

seq_post <- seqdef(
  wide_post, var = cols, alphabet = etats_presents, labels = labels_presents, cpal = couleurs_presentes, id = wide_post$id_spell, xtstep = 6)

info_post <- tibble(id_spell = wide_post$id_spell) %>% left_join(ech, by = "id_spell")

# INDICATEURS DE COMPLEXITE
info_post$complexite <- as.numeric(seqici(seq_post))
info_post$nb_episodes <- as.numeric(seqtransn(seq_post)) + 1


################################################################################
##### 5. DISTANCES ET TYPOLOGIES  ##############################################
################################################################################
METHODE_COUTS <- "CONSTANT"
couts_commun <- seqsubm(seq_post, method = METHODE_COUTS, cval = 2)
diss_commun  <- seqdist(seq_post, method = "OM", indel = 1.5, sm = couts_commun)

# CHOIX DE K
K_choisi <- 3

# CAH + CONSOLIDATION PAM
arbre <- hclust(as.dist(diss_commun), method="ward.D2")
part_cah <- cutree(arbre, k=K_choisi)

part_pam <- wcKMedoids(diss_commun, k=K_choisi, initialclust=part_cah, cluster.only=TRUE)

info_post$classe <- factor(as.numeric(as.factor(part_pam)), levels = 1:K_choisi, labels = paste0("Classe ", 1:K_choisi))


################################################################################
##### 6. DESCRIPTION VISUELLE ##################################################
################################################################################

exporter_graphe("desc_globale_1_chronogramme.png",
                seqdplot(seq_post, group = info_post$classe, border=NA, with.legend = "right", cex.legend = 0.7, main = "Répartition des états par classe"))
  
exporter_graphe("desc_globale_2_temps_moyen.png",
                seqmtplot(seq_post, group = info_post$classe, border=NA, with.legend = "right", cex.legend = 0.7, main = "Temps moyen par état"))

exporter_graphe("desc_globale_3_seq_representatives.png",
                seqrplot(seq_post, group = info_post$classe, dist.matrix=diss_commun, border = NA, coverage=0.3, with.legend = "right", main = "Séquences représentatives"))

################################################################################
##### 7. COMPARAISON RCI VS TEMOINS ############################################
################################################################################

repartition <- info_post %>% 
  mutate(groupe_nom = if_else(traite==1, "RCI", "Témoins")) %>% 
  count(groupe_nom, classe) %>% 
  group_by(groupe_nom) %>% 
  mutate(pct= 100 * n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from=groupe_nom, values_from=pct) %>% 
  mutate(Diff_RCI_vs_Temoin_pp = RCI - Témoins)

print(repartition)

stats_complexite <- info_post %>% 
  mutate(groupe_nom = if_else(traite==1, "RCI", "Témoins")) %>% 
  group_by(groupe_nom) %>% 
  summarise(
    complexite_moyenne = mean(complexite, na.rm=TRUE),
    nb_episodes_moyen = mean(nb_episodes, na.rm=TRUE)
  )

print(stats_complexite)
  
################################################################################
##### 8. EXPORT ################################################################
################################################################################

# CORRECTION : l'objet s'appelle info_post (info_post_final n'existe plus)
write_parquet(
  info_post %>% select(id_spell, paire, traite, groupe, classe, complexite, nb_episodes),
  file.path(dir_out, "classes_sequences_communes_propre.parquet")
)

cat("\nTerminé. Sorties dans :", dir_res, "\n")
