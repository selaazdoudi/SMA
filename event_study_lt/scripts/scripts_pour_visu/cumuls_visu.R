# =============================================================================
# Cumuls de jours - RCI vs temoins (sorties de CDI hors RCI), cohorte 2019
# Suivi sur 72 mois, n = 254 418 par groupe (apparies 1:1)
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
# install.packages("patchwork") si besoin
library(patchwork)

chemin <- "Cumuls_jours_RCI_2019.xlsx"   # <- adapter

# -----------------------------------------------------------------------------
# 1. Import
# -----------------------------------------------------------------------------

# Format long : une ligne par (horizon, groupe) -> ideal pour ggplot
long <- read_excel(chemin, sheet = "Format_long") %>%
  mutate(
    groupe = factor(groupe_nom, levels = c("Temoins", "RCI")),
    annee  = h / 12
  )

# Format large : une ligne par horizon -> ideal pour calculer les ecarts
large <- read_excel(chemin, sheet = "Cumuls_par_groupe")

# Palette
pal <- c("Temoins" = "#5B6C7F", "RCI" = "#C0392B")

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey35"),
      plot.caption = element_text(colour = "grey45", hjust = 0),
      legend.position = "top",
      legend.title = element_blank()
    )
)

# Reperes annuels sur l'axe des horizons
brk_h <- seq(1, 72, by = 12)


# -----------------------------------------------------------------------------
# 3. LE graphique central : ecart RCI - temoins
#    (le renversement du differentiel d'indemnisation)
# -----------------------------------------------------------------------------

ecarts <- large %>%
  transmute(
    h,
    `Jours indemnises` = moy_cum_indem_RCI - moy_cum_indem_Temoins,
    `Jours travailles` = moy_cum_trav_RCI  - moy_cum_trav_Temoins
  ) %>%
  pivot_longer(-h, names_to = "indicateur", values_to = "ecart")

# Mois ou l'ecart d'indemnisation repasse sous zero, calcule et non code en dur
bascule <- ecarts %>%
  filter(indicateur == "Jours indemnises", h > 3, ecart < 0) %>%
  slice_min(h) %>% pull(h)

g1 <- ggplot(ecarts, aes(h, ecart, colour = indicateur)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_vline(xintercept = bascule, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 1) +
  annotate("text", x = bascule + 1, y = 4, hjust = 0, size = 3.4,
           colour = "grey35",
           label = paste0("bascule au mois ", bascule)) +
  scale_colour_manual(values = c("Jours indemnises" = "#C0392B",
                                 "Jours travailles" = "#5B6C7F")) +
  scale_x_continuous(breaks = brk_h) +
  labs(
    title = "Ecart RCI - temoins en jours cumules",
    subtitle = "Au-dessus de zero : les RCI en consomment davantage",
    x = "Mois depuis la sortie", y = "Jours (RCI - temoins)",
    caption = "Lecture : les RCI consomment plus vite leur droit, puis passent durablement en dessous."
  )
g1


dat <- read_excel(chemin, sheet = "Cumuls_par_groupe") %>%
  arrange(h) %>%
  mutate(ecart = moy_cum_indem_RCI - moy_cum_indem_Temoins)

# Mois ou le cumul repasse sous zero, calcule et non code en dur
bascule <- dat %>% filter(h > 3, ecart < 0) %>% slice_min(h) %>% pull(h)
g2 <- ggplot(dat, aes(x = h, y = ecart)) +
  geom_col(fill = "#2F6FE0", width = 0.65) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = 0.4) +
  geom_vline(xintercept = bascule - 0.5, linetype = "dashed", colour = "grey60") +
  annotate("text", x = bascule + 1.5, y = 4.5, hjust = 0, size = 3.4,
           colour = "grey35", label = paste0("bascule au mois ", bascule)) +
  scale_x_continuous(breaks = c(1, 13, 25, 37, 49, 61, 72),
                     expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_number(accuracy = 1, decimal.mark = ",")) +
  labs(
    title    = "Ecart cumule RCI - temoins en jours indemnises",
    subtitle = "Au-dessus de zero : depuis leur sortie de CDI, les RCI ont ete indemnises davantage",
    x = "Mois depuis la sortie", y = "Jours (RCI - temoins)",
    caption  = "Lecture : au mois 24, un RCI a cumule 5,8 jours indemnises de plus que le temoin auquel il est apparie."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    axis.ticks         = element_blank(),
    plot.title.position = "plot",
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey40"),
    plot.caption  = element_text(colour = "grey45", hjust = 0)
  )
g2



