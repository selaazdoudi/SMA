################################################################################
##### APPARIEMENT ##############################################################
################################################################################

library(tidyverse)
library(arrow)
library(duckdb)
library(dbplyr)
library(MatchIt)

dir_out <- "C:/Users/Public/Documents/Salma_CEA/Etude_typeboulle/lt/"
dir_tmp <- "C:/Users/Public/Documents/Salma_CEA/duckdb_temp"
if(!dir.exists(dir_tmp)) dir.create(dir_tmp, recursive = TRUE)

con <- dbConnect(duckdb::duckdb(), dbdir = file.path(dir_tmp, "appariement.duckdb"))
dbExecute(con, "PRAGMA memory_limit='35GB';")
dbExecute(con, "PRAGMA threads=8;")

duckdb_register_arrow(con, "panel_final",
  open_dataset(file.path(dir_out, "panel_pour_appariement_2019.parquet")))

panel_final <- tbl(con, "panel_final")
colnames(panel_final)


################################################################################
##### HISTORIQUE DU PASSE ######################################################
################################################################################
# >>> MODIF : jours_non_salarie -> jours_ns_min (mois reellement observes).

hist_large <- panel_final %>%
  filter(h >= -24 & h <= -1) %>%
  select(id_midas, id_spell, deb_mois, h,
         jours_cdi, jours_cdd_ventiles, jours_interim, jours_autre,
         jours_ns_min, jours_indemnises) %>%
  collect() %>%
  mutate(mois_pre = abs(h)) %>%
  pivot_wider(
    id_cols    = id_spell,
    names_from = mois_pre,
    values_from = c(jours_cdi, jours_cdd_ventiles, jours_interim, jours_autre,
                    jours_ns_min, jours_indemnises),
    names_glue = "{.value}_m{mois_pre}",
    values_fill=0
  )

exemple <- hist_large %>% head(10) %>% print()

# indicatrices de parcours anterieur, a ajouter au score
hist_flags <- diag_pre %>%
  select(id_midas, deb_mois, a_ete_indemnise, a_eu_ans_min)

df_pop <- panel_final %>%
  filter(h == 0) %>%
  collect() %>%
  left_join(hist_large, by = "id_spell") %>%
  mutate(traite = if_else(groupe == "RCI", 1, 0))

colonnes_jours <- grep("^jours_(cdi|cdd_ventiles|interim|autre|ns_min|indemnises)_m",
                       names(df_pop), value = TRUE)

df_pop <- df_pop %>%
  mutate(across(all_of(colonnes_jours), ~ coalesce(.x, 0)))

################################################################################
##### PREP VAR POUR MATCHING  ##################################################
################################################################################

vars_continues <- c(
  "age_fin_ctt_redresse", colonnes_jours
)
vars_facteurs <- c("niv_dip", "qualif_classe", "naf_64")

vars_continues <- intersect(vars_continues, names(df_pop))
vars_facteurs  <- intersect(vars_facteurs,  names(df_pop))

df_pop <- df_pop %>%
  mutate(across(all_of(vars_facteurs),
                ~ as.factor(replace_na(as.character(.x), "manquant")))) %>%
  drop_na(all_of(vars_continues), traite, SEXE, deb_mois)

vars_constantes <- vars_continues[map_lgl(vars_continues,
                                          ~ n_distinct(df_pop[[.x]]) <= 1)]
if (length(vars_constantes) > 0) {
  message("Variables constantes retirees : ", paste(vars_constantes, collapse = ", "))
  vars_continues <- setdiff(vars_continues, vars_constantes)
}

formule_ps <- as.formula(paste("traite ~",
                               paste(c(vars_continues, vars_facteurs), collapse = " + ")))
print(formule_ps)

################################################################################
##### BOUCLE APPARIEMENT PAR COHORTE MENSUELLE  ################################
################################################################################
CALIPER <- 0.05

cohortes <- sort(unique(df_pop$deb_mois))

resultats <- map(cohortes, function(m) {
  df_m <- df_pop %>% filter(deb_mois == m)

  # Une variable constante dans la cohorte fait echouer le glm : on la retire
  fac_utiles <- vars_facteurs[map_lgl(vars_facteurs,  ~ n_distinct(df_m[[.x]]) > 1)]

    con_utiles <- vars_continues[map_lgl(vars_continues, ~ n_distinct(df_m[[.x]]) > 1)]

  f_m <- as.formula(paste("traite ~",
                          paste(c(con_utiles, fac_utiles), collapse = " + ")))

  tryCatch({
    mo <- matchit(f_m, data = df_m, method = "nearest", distance = "glm",
                  caliper = CALIPER, ratio = 1, exact = ~SEXE, replace = FALSE)
    list(cohorte = m, matchit = mo, data = match.data(mo))
  }, error = function(e) { message("Cohorte ", m, " : ", e$message); NULL })
})

resultats  <- compact(resultats)
df_apparie <- map_dfr(resultats, function(r) {
  r$data %>% mutate(paire = paste0(as.character(r$cohorte), "_", subclass))
})

################################################################################
##### INVESTIGATION PERTES DU MATCHING  ########################################
################################################################################


bilan_pertes_global <- tibble(
  Statut = c("1.RCI cibles (Avant appariement)",
             "2.RCI appariés (Après appariement)",
             "3.RCI perdus"),
  Effectif=c(
    sum(df_pop$traite==1),
    sum(df_apparie$traite==1),
    sum(df_pop$traite==1) - sum(df_apparie$traite==1)
  )
) %>% 
  mutate(Pourcentage = round(Effectif/Effectif[1]*100,1))
    
bilan_pertes_cohorte <- df_pop %>% 
  filter(traite==1) %>% 
  group_by(deb_mois) %>% 
  summarise(n_avant=n(), .groups="drop") %>% 
  left_join(
    df_apparie %>% 
      filter(traite == 1) %>% 
      group_by(deb_mois) %>% 
      summarise(n_apres=n(), .groups="drop"),
    by="deb_mois"
  ) %>% 
  mutate(
    n_apres = coalesce(n_apres,0L),
    n_perdus =  n_avant - n_apres,
    taux_perte_pct = round((n_perdus /n_avant) * 100,1)
  ) %>% 
  arrange(desc(taux_perte_pct))



################################################################################
##### 3. TABLE D'EQUILIBRE #####################################################
################################################################################

calculer_equilibre <- function(df, libelle) {
  X <- model.matrix(~ . - 1,
                    data = df[, c(vars_continues, vars_facteurs)] %>% droplevels())
  t <- df$traite == 1
  tibble(
    variable   = colnames(X),
    moy_traite = colMeans(X[t, , drop = FALSE]),
    moy_temoin = colMeans(X[!t, , drop = FALSE]),
    sd_traite  = apply(X[t, , drop = FALSE], 2, sd),
    var_ratio  = apply(X[t, , drop = FALSE], 2, var) /
      apply(X[!t, , drop = FALSE], 2, var)
  ) %>%
    mutate(smd = (moy_traite - moy_temoin) / if_else(sd_traite == 0, NA_real_, sd_traite),
           echantillon = libelle)
}

equilibre <- bind_rows(
  calculer_equilibre(df_pop,     "Avant appariement"),
  calculer_equilibre(df_apparie, "Apres appariement")
)

cat("\nDesequilibres residuels les plus eleves apres appariement :\n")
equilibre %>% filter(echantillon == "Apres appariement") %>%
  arrange(desc(abs(smd))) %>% head(10) %>% print()

################################################################################
##### 4. FIGURE LOVE ###########################################################
################################################################################

ordre_vars <- equilibre %>% filter(echantillon == "Avant appariement") %>%
  arrange(abs(smd)) %>% pull(variable)

love_plot <- equilibre %>%
  mutate(variable    = factor(variable, levels = ordre_vars),
         echantillon = factor(echantillon,
                              levels = c("Avant appariement", "Apres appariement"))) %>%
  ggplot(aes(x = abs(smd), y = variable, color = echantillon)) +
  geom_vline(xintercept = 0,    color = "grey20") +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "grey40") +
  geom_point(size = 2, alpha = 0.85) +
  labs(title = "Figure Love de l'appariement par score de propension",
       x = "Difference des moyennes standardisees (valeur absolue)",
       y = NULL, color = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(love_plot)
ggsave(file.path(dir_out, "love_plot_psm_2019.png"),
       love_plot, width = 8, height = 8, dpi = 300)

################################################################################
##### 5. SUPPORT COMMUN ########################################################
################################################################################

df_ps <- bind_rows(
  map_dfr(resultats, ~ tibble(ps = .x$matchit$distance, traite = .x$matchit$treat)) %>%
    mutate(echantillon = "Avant appariement"),
  df_apparie %>% transmute(ps = distance, traite, echantillon = "Apres appariement")
) %>%
  mutate(groupe = if_else(traite == 1, "RCI", "Fin de CDI hors RC"),
         echantillon = factor(echantillon,
                              levels = c("Avant appariement", "Apres appariement")))

p_support <- ggplot(df_ps, aes(x = ps, fill = groupe)) +
  geom_density(alpha = 0.5, color = NA) +
  facet_wrap(~ echantillon) +
  labs(title = "Distribution du score de propension",
       x = "Score de propension", y = "Densite", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p_support)
ggsave(file.path(dir_out, "support_commun_2019.png"),
       p_support, width = 9, height = 5, dpi = 300)

################################################################################
##### 6. EXPORT ################################################################
################################################################################


write_parquet(df_apparie, file.path(dir_out, "echantillon_apparie_2019.parquet"))
write_csv(equilibre, file.path(dir_out, "table_equilibre_2019.csv"))

dbDisconnect(con, shutdown = TRUE)
