################################################################################
##### CUMUL JOURS TRAVAILLES VS CUMUL JOURS INDEMNISES #########################
################################################################################

library(tidyverse)
library(arrow)
library(writexl)

dir_out <- "C:/Users/Public/Documents/Salma_CEA/Etude_typeboulle/lt/"
export_path <- file.path(dir_out,"resultats")
if(!dir.exists(export_path)) dir.create(export_path,recursive=TRUE)

df_apparie <- read_parquet(file.path(dir_out, "echantillon_apparie_2019.parquet")) %>% 
  select(id_spell,deb_mois, groupe,traite,paire) %>% 
  
panel <- open_dataset(file.path(dir_out, "panel_pour_appariement_2019.parquet"))

panel_post <- panel %>% 
  select(id_spell, h, jours_trav, jours_indemnises, nb_jours_mois) %>% 
  filter(h>=1 , h<=72) %>% 
  collect () %>% 
  inner_join(df_apparie, by = "id_spell") %>% 
  arrange(id_spell,h) 


cumuls_ind <- panel_post %>% 
  group_by(id_spell) %>%
  arrange(h, .by_group=TRUE) %>% 
  mutate(
    cum_jours_trav=cumsum(jours_trav),
    cum_jours_indem=cumsum(jours_indemnises),
    cum_jours_calend = cumsum(nb_jours_mois),
    part_trav= cum_jours_trav / cum_jours_calend,
    part_indem= cum_jours_indem / cum_jours_calend
  ) %>% 
  ungroup()


cumuls_groupe <- cumul_indem %>% 
  group_by(h,traite) %>% 
  summarise(
    n=n(),
    moy_cum_trav = mean (cum_jours_trav),
    moy_cum_indem = mean (cum_jours_indem),
    moy_part_trav = mean (part_trav),
    moy_part_indem = mean (part_indem),
    med_cum_trav = median (cum_jours_trav),
    med_cum_indem = median (cum_jours_indem),
    med_part_trav = median (part_trav),
    med_part_indem = median (part_indem),
    .groups="drop"
  ) %>% 
  mutate(groupe_nom=if_else(traite==1, "RCI", "Temoins"))

cumuls_excel <- cumuls_groupe %>% 
  select(-traite) %>% 
  pivot_wider(
    names_from=groupe_nom,
    values_from=c(n, moy_cum_trav, moy_cum_indem, moy_part_trav, moy_part_indem, med_cum_trav,
                  med_cum_indem, med_part_trav, med_part_indem),
    names_glue = "{.value}_{groupe_nom}"
  ) 

nom_fichier <- file.path(export_path, "Cumuls_jours_RCI_2019.xlsx")

write_xlsx(
  list(
    "Cumuls_par_groupe" = cumuls_excel,
    "Format_long" = cumul_groupe
  ),
  path=nom_fichier
) 



  )
)
  