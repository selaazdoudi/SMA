# ============================================================
# Scraper forum-juridique.net -> Rupture conventionnelle
# ============================================================

pkgs <- c("httr", "rvest", "stringr")
m <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(m) > 0) install.packages(m, repos = "https://cloud.r-project.org")
library(httr); library(rvest); library(stringr)

BASE  <- "https://www.forum-juridique.net"
CAT   <- "/travail/salaries/rupture-conventionnelle/"
UA    <- "Mozilla/5.0 (recherche-personnelle)"
DELAY <- 2                # pause entre requetes (secondes)
MAX_LIST_PAGES <- 3       # nb de pages de listing a parcourir (augmente si besoin)

get_page <- function(url) {
  r <- tryCatch(GET(url, add_headers(`User-Agent` = UA), timeout(20)),
                error = function(e) { message("  reseau: ", conditionMessage(e)); NULL })
  Sys.sleep(DELAY)
  if (is.null(r) || status_code(r) != 200) return(NULL)
  read_html(content(r, as = "text", encoding = "UTF-8"))
}
abs_url <- function(h) ifelse(str_starts(h, "http"), h, paste0(BASE, h))

# --- 1. liste des sujets (avec pagination) ---
message("1/2 Liste des sujets...")
a_visiter <- CAT; vus <- character(0); topics <- character(0); n <- 0
while (length(a_visiter) > 0 && n < MAX_LIST_PAGES) {
  url <- abs_url(a_visiter[1]); a_visiter <- a_visiter[-1]
  if (url %in% vus) next
  vus <- c(vus, url); n <- n + 1
  message("  page ", n, ": ", url)
  pg <- get_page(url); if (is.null(pg)) next
  h <- pg |> html_elements("a") |> html_attr("href"); h <- h[!is.na(h)]
  suj <- h[str_detect(h, "-t\\d+\\.html$")]
  topics <- unique(c(topics, abs_url(suj)))
  pagin <- unique(h[str_detect(h, "rupture-conventionnelle") & !str_detect(h, "-t\\d+\\.html$")])
  a_visiter <- unique(c(a_visiter, pagin))
  message("    ", length(suj), " sujets (total ", length(topics), ")")
}
message("-> ", length(topics), " sujets.\n")

# --- 2. messages de chaque sujet ---
message("2/2 Messages...")
lignes <- list()
for (i in seq_along(topics)) {
  message("[", i, "/", length(topics), "] ", topics[i])
  pg <- get_page(topics[i]); if (is.null(pg)) next
  
  titre <- pg |> html_element("h1") |> html_text2()
  if (is.na(titre) || titre == "") titre <- pg |> html_element("title") |> html_text2()
  
  full <- pg |> html_text2()
  deb <- str_locate(full, "Posté le")[1, 1]
  fin <- str_locate(full, "Posez votre question juridique|PAGE\\s*:")[1, 1]
  zone <- if (!is.na(deb)) substr(full, deb, ifelse(is.na(fin), nchar(full), fin - 1)) else full
  zone <- str_squish(zone)
  
  aut <- pg |> html_elements("a") |> html_attr("href")
  aut <- aut[!is.na(aut) & str_detect(aut, "txt_pseudo=")]
  aut <- utils::URLdecode(str_match(aut, "txt_pseudo=([^&]+)")[, 2])
  
  dm  <- str_match_all(zone, "Le\\s+(\\d{2}/\\d{2}/\\d{4})\\s+à\\s+(\\d{2}:\\d{2})")[[1]]
  nm  <- nrow(dm)
  if (nm == 0) {
    lignes[[length(lignes)+1]] <- data.frame(sujet=titre, auteur=NA, date=NA, heure=NA,
                                             message=NA, fil_complet=zone, url=topics[i],
                                             stringsAsFactors=FALSE); next
  }
  pos <- str_locate_all(zone, "Le\\s+\\d{2}/\\d{2}/\\d{4}\\s+à\\s+\\d{2}:\\d{2}")[[1]]
  corps <- character(nm)
  for (j in seq_len(nm)) {
    s <- pos[j, 2] + 1; e <- if (j < nm) pos[j+1, 1] - 1 else nchar(zone)
    txt <- substr(zone, s, e)
    if (j < nm && !is.na(aut[j+1]))
      txt <- str_remove(txt, str_c(str_replace_all(aut[j+1], "([\\W])", "\\\\\\1"), "\\s*$"))
    corps[j] <- str_squish(txt)
  }
  auteur_col <- if (length(aut) >= nm) aut[seq_len(nm)] else c(aut, rep(NA, nm - length(aut)))
  lignes[[length(lignes)+1]] <- data.frame(sujet=titre, auteur=auteur_col, date=dm[,2],
                                           heure=dm[,3], message=corps, fil_complet=zone,
                                           url=topics[i], stringsAsFactors=FALSE)
}

df <- do.call(rbind, lignes)
fichier <- file.path(getwd(), "rupture_conventionnelle_fj.csv")
write.csv(df, fichier, row.names = FALSE, fileEncoding = "UTF-8")
message("\nTermine : ", nrow(df), " lignes -> ", fichier)