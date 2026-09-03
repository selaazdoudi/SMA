# ============================================================
# Scraper forum Juritravail - rupture conventionnelle
# Le forum tourne sous Discourse : j'ajoute ".json" aux URLs
# pour obtenir directement les donnees, sans parser du HTML.
# ============================================================

library(httr)
library(jsonlite)

BASE <- "https://forum.juritravail.com"
TAG  <- "rupture-conventionnelle"
DELAY <- 1.5   # pause entre requetes (secondes)

# --- helper : GET + JSON ---
# Je télécharge les pages et je transforme les objets json en objets R
get_json <- function(url) {
  r <- GET(url, add_headers(`User-Agent` = "Mozilla/5.0"), timeout(15))
  Sys.sleep(DELAY)
  if (status_code(r) != 200) { message("Echec: ", url); return(NULL) }
  fromJSON(content(r, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

# --- nettoyage HTML -> texte ---
clean <- function(x) {
  if (is.null(x)) return("")
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&amp;", "&", x); x <- gsub("&#39;|&apos;", "'", x)
  x <- gsub("&quot;", '"', x); x <- gsub("&nbsp;", " ", x)
  x <- gsub("&lt;", "<", x);  x <- gsub("&gt;", ">", x)
  trimws(gsub("\\s+", " ", x))
}

# --- 1. recuperer la liste des sujets (pagination du tag) ---
message("Recuperation de la liste des sujets...")
sujets <- list()
page <- 0
repeat {
  url <- if (page == 0) paste0(BASE, "/tag/", TAG, ".json")
  else paste0(BASE, "/tag/", TAG, ".json?page=", page)
  d <- get_json(url)
  tl <- d$topic_list$topics
  if (is.null(tl) || length(tl) == 0) break
  for (t in tl) sujets[[as.character(t$id)]] <- t
  message("  page ", page, " : ", length(tl), " sujets (total ", length(sujets), ")")
  page <- page + 1
}
message(length(sujets), " sujets trouves.\n")

# --- 2. recuperer tous les messages de chaque sujet ---
message("Recuperation des messages...")
lignes <- list()
i <- 0
for (t in sujets) {
  i <- i + 1
  message("[", i, "/", length(sujets), "] ", t$title)
  d <- get_json(paste0(BASE, "/t/", t$slug, "/", t$id, ".json"))
  posts <- d$post_stream$posts
  if (is.null(posts)) next
  for (p in posts) {
    lignes[[length(lignes) + 1]] <- data.frame(
      sujet_id = t$id,
      sujet    = t$title,
      auteur   = ifelse(is.null(p$username), NA, p$username),
      date     = ifelse(is.null(p$created_at), NA, p$created_at),
      message  = clean(p$cooked),
      url      = paste0(BASE, "/t/", t$slug, "/", t$id),
      stringsAsFactors = FALSE
    )
  }
}

df <- do.call(rbind, lignes)

# --- 3. sauvegarde dans le dossier de travail ---
fichier <- file.path(getwd(), "rupture_conventionnelle.csv")
write.csv(df, fichier, row.names = FALSE, fileEncoding = "UTF-8")
message("\nTermine : ", nrow(df), " messages enregistres dans :\n", fichier)
