# ==============================================================================
# Analyse de saisonnalité des prix en circuits courts
# - Extraction des séries mensuelles pondérées par catégorie de produit
# - Décomposition STL (Trend + Seasonal + Remainder)
# - Modélisation SARIMA automatique
# - Comparaison avec les prix conventionnels INSEE
# - Export des résultats et graphiques
#
# Usage : Rscript scripts/analyse_saisonnalite.R
# ==============================================================================

library(RMariaDB)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forecast)  # pour stl, auto.arima, etc.
library(lubridate)

cat("=== ANALYSE DE SAISONNALITÉ ET SÉRIES TEMPORELLES DE PRIX ===\n")
cat(sprintf("Date : %s\n\n", Sys.time()))

# =====================================================================
# 1. CONNEXION ET EXTRACTION DES SÉRIES MENSUELLES
# =====================================================================
cat("--- Étape 1 : Extraction des séries mensuelles ---\n")

con <- tryCatch({
  dbConnect(MariaDB(), dbname = Sys.getenv("DB_NAME", "market_prices"), host = "127.0.0.1", 
            port = 3306, user = "root", password = "", charset = "utf8mb4")
}, error = function(e) stop("Impossible de se connecter à MariaDB."))

dbExecute(con, "SET NAMES utf8mb4")

# Requête directe des données propres depuis la table prix_trie
query <- "
  SELECT 
    transaction_hash,
    plateforme,
    categorie_standard,
    prix_final,
    organic,
    date,
    code_postal,
    dept,
    poids_kg
  FROM prix_trie
  WHERE is_outlier = 0 
    AND date IS NOT NULL
"

all_data <- tryCatch(dbGetQuery(con, query), error = function(e) {
  stop("Impossible de requêter la table prix_trie : ", e$message)
})

# Traitement R des données propres
all_data <- all_data %>%
  mutate(
    date = as.Date(date),
    annee_mois = format(date, "%Y-%m"),
    annee = as.integer(format(date, "%Y")),
    mois = as.integer(format(date, "%m")),
    total_ligne = prix_final * poids_kg
  ) %>%
  filter(annee >= 2020, !is.na(poids_kg), poids_kg > 0, is.finite(poids_kg))

cat(sprintf("\n   TOTAL : %d transactions propres extraites directement de 'prix_trie'\n\n", nrow(all_data)))

# =====================================================================
# 2. AGRÉGATION MENSUELLE PONDÉRÉE PAR CATÉGORIE
# =====================================================================
cat("--- Étape 2 : Agrégation mensuelle pondérée ---\n")

# Série mensuelle toutes plateformes confondues (moyenne pondérée par le poids)
series_mensuelles <- all_data %>%
  group_by(categorie_standard, annee_mois) %>%
  summarise(
    prix_moyen_pondere = sum(total_ligne, na.rm = TRUE) / sum(poids_kg, na.rm = TRUE),
    prix_median = median(prix_final, na.rm = TRUE),
    ecart_type = sd(prix_final, na.rm = TRUE),
    nb_transactions = n(),
    volume_kg = sum(poids_kg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(paste0(annee_mois, "-01"))) %>%
  arrange(categorie_standard, date)

# Série mensuelle par plateforme
series_par_plateforme <- all_data %>%
  group_by(categorie_standard, plateforme, annee_mois) %>%
  summarise(
    prix_moyen_pondere = sum(total_ligne, na.rm = TRUE) / sum(poids_kg, na.rm = TRUE),
    nb_transactions = n(),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(paste0(annee_mois, "-01"))) %>%
  arrange(categorie_standard, plateforme, date)

cat(sprintf("   %d catégories avec séries temporelles\n", 
            length(unique(series_mensuelles$categorie_standard))))
cat(sprintf("   Période couverte : %s à %s\n\n", 
            min(series_mensuelles$date), max(series_mensuelles$date)))

# =====================================================================
# 3. CHARGEMENT DES PRIX DE RÉFÉRENCE INSEE
# =====================================================================
cat("--- Étape 3 : Chargement des prix conventionnels INSEE ---\n")

df_insee <- NULL
if (dbExistsTable(con, "prix_reference_hors_cc")) {
  df_insee <- dbReadTable(con, "prix_reference_hors_cc") %>%
    mutate(date = as.Date(paste0(date_mois, "-01"))) %>%
    filter(date >= as.Date("2021-01-01")) %>%
    rename(prix_insee = prix_moyen_kg) %>%
    select(categorie_standard, date, prix_insee)
  
  cat(sprintf("   %d observations INSEE chargées (%d catégories)\n\n",
              nrow(df_insee), length(unique(df_insee$categorie_standard))))
} else {
  cat("   [WARN] Table 'prix_reference_hors_cc' absente, pas de comparaison INSEE\n\n")
}

# =====================================================================
# 4. DÉCOMPOSITION STL
# =====================================================================
cat("--- Étape 4 : Décomposition STL par catégorie ---\n")

# Sélection des catégories avec au moins 18 mois de données
categories_eligibles <- series_mensuelles %>%
  group_by(categorie_standard) %>%
  summarise(n_mois = n(), .groups = "drop") %>%
  filter(n_mois >= 18) %>%
  pull(categorie_standard)

cat(sprintf("   %d catégories éligibles (>= 18 mois de données)\n", 
            length(categories_eligibles)))

# Dossier de sortie pour les graphiques
dir.create("cartes_produits/saisonnalite", showWarnings = FALSE, recursive = TRUE)

# Stocker les résultats STL
resultats_stl <- list()
resultats_sarima <- list()

for (cat in categories_eligibles) {
  
  cat_data <- series_mensuelles %>% 
    filter(categorie_standard == cat) %>%
    arrange(date)
  
  # Compléter les mois manquants par interpolation linéaire
  date_range <- seq.Date(min(cat_data$date), max(cat_data$date), by = "month")
  cat_complete <- data.frame(date = date_range) %>%
    left_join(cat_data, by = "date") %>%
    mutate(
      categorie_standard = cat,
      # Interpolation linéaire pour les mois manquants
      prix_moyen_pondere = approx(
        x = which(!is.na(prix_moyen_pondere)),
        y = prix_moyen_pondere[!is.na(prix_moyen_pondere)],
        xout = 1:n(),
        rule = 2
      )$y
    )
  
  n_obs <- nrow(cat_complete)
  
  # Construire l'objet ts (série temporelle)
  start_year <- year(min(cat_complete$date))
  start_month <- month(min(cat_complete$date))
  
  ts_prix <- ts(cat_complete$prix_moyen_pondere, 
                start = c(start_year, start_month), 
                frequency = 12)
  
  # ---- STL ----
  stl_result <- tryCatch({
    stl(ts_prix, s.window = "periodic", robust = TRUE)
  }, error = function(e) {
    cat(sprintf("   [WARN] STL échoué pour '%s': %s\n", cat, e$message))
    return(NULL)
  })
  
  if (is.null(stl_result)) next
  
  # Extraire les composantes
  decomp <- as.data.frame(stl_result$time.series)
  decomp$date <- cat_complete$date
  decomp$original <- cat_complete$prix_moyen_pondere
  decomp$categorie_standard <- cat
  
  # Stocker
  resultats_stl[[cat]] <- decomp
  
  # ---- SARIMA automatique ----
  sarima_model <- tryCatch({
    auto.arima(ts_prix, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
  }, error = function(e) {
    cat(sprintf("   [WARN] SARIMA échoué pour '%s'\n", cat))
    return(NULL)
  })
  
  if (!is.null(sarima_model)) {
    # Prévision sur 6 mois
    forecast_result <- forecast(sarima_model, h = 6)
    
    resultats_sarima[[cat]] <- list(
      model = sarima_model,
      forecast = forecast_result,
      order = arimaorder(sarima_model),
      seasonal = sarima_model$arma[c(3, 4, 7)]
    )
  }
  
  # ---- GRAPHIQUE STL ----
  
  # Joindre les prix INSEE si disponibles
  cat_insee <- NULL
  if (!is.null(df_insee)) {
    cat_insee <- df_insee %>% filter(categorie_standard == cat)
  }
  
  # Construire le dataframe de tracé
  df_plot <- decomp %>%
    pivot_longer(cols = c("original", "trend", "seasonal", "remainder"),
                 names_to = "composante", values_to = "valeur") %>%
    mutate(composante = factor(composante, 
                               levels = c("original", "trend", "seasonal", "remainder"),
                               labels = c("Prix observé (€/kg)", "Tendance", 
                                          "Saisonnalité", "Résidu")))
  
  # Graphique 1 : Décomposition STL
  p_stl <- ggplot(df_plot, aes(x = date, y = valeur)) +
    geom_line(color = "steelblue", linewidth = 0.7) +
    facet_wrap(~ composante, ncol = 1, scales = "free_y") +
    theme_minimal(base_size = 12) +
    labs(
      title = paste("Décomposition STL :", cat),
      subtitle = "Circuits courts — Toutes plateformes confondues (moyenne pondérée par le poids)",
      x = NULL, y = NULL
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
      strip.text = element_text(face = "bold", size = 11)
    )
  
  ggsave(filename = sprintf("cartes_produits/saisonnalite/STL_%s.png", 
                             str_replace_all(cat, "[^a-zA-Z0-9]", "_")),
         plot = p_stl, width = 10, height = 8, dpi = 200, bg = "white")
  
  # Graphique 2 : Comparaison CC vs INSEE
  if (!is.null(cat_insee) && nrow(cat_insee) > 0) {
    df_compare <- decomp %>%
      select(date, `Circuits Courts` = original) %>%
      left_join(cat_insee %>% select(date, `INSEE (conventionnel)` = prix_insee), by = "date") %>%
      pivot_longer(cols = -date, names_to = "Source", values_to = "prix_kg") %>%
      filter(!is.na(prix_kg))
    
    if (nrow(df_compare) > 0) {
      p_compare <- ggplot(df_compare, aes(x = date, y = prix_kg, color = Source)) +
        geom_line(linewidth = 0.8) +
        scale_color_manual(values = c("Circuits Courts" = "#2980B9", 
                                      "INSEE (conventionnel)" = "#E74C3C")) +
        theme_minimal(base_size = 12) +
        labs(
          title = paste("Comparaison des prix :", cat),
          subtitle = "Circuits courts (pondéré) vs. Prix conventionnels (INSEE)",
          x = NULL, y = "Prix moyen (€/kg)", color = NULL
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
          legend.position = "bottom"
        )
      
      ggsave(filename = sprintf("cartes_produits/saisonnalite/Compare_%s.png",
                                str_replace_all(cat, "[^a-zA-Z0-9]", "_")),
             plot = p_compare, width = 10, height = 5, dpi = 200, bg = "white")
    }
  }
  
  # Graphique 3 : Prévision SARIMA
  if (!is.null(resultats_sarima[[cat]])) {
    fc <- resultats_sarima[[cat]]$forecast
    
    # Construire un dataframe pour le tracé de la prévision
    n_hist <- length(ts_prix)
    dates_hist <- cat_complete$date
    dates_fc <- seq.Date(max(dates_hist) + months(1), by = "month", length.out = 6)
    
    df_fc <- data.frame(
      date = c(dates_hist, dates_fc),
      prix = c(as.numeric(ts_prix), as.numeric(fc$mean)),
      type = c(rep("Historique", n_hist), rep("Prévision", 6)),
      lo80 = c(rep(NA, n_hist), as.numeric(fc$lower[,1])),
      hi80 = c(rep(NA, n_hist), as.numeric(fc$upper[,1])),
      lo95 = c(rep(NA, n_hist), as.numeric(fc$lower[,2])),
      hi95 = c(rep(NA, n_hist), as.numeric(fc$upper[,2]))
    )
    
    ord <- resultats_sarima[[cat]]$order
    model_label <- sprintf("ARIMA(%d,%d,%d)×(%d,%d,%d)[12]", 
                           ord[1], ord[2], ord[3],
                           resultats_sarima[[cat]]$seasonal[1],
                           resultats_sarima[[cat]]$seasonal[2],
                           resultats_sarima[[cat]]$seasonal[3])
    
    p_fc <- ggplot(df_fc, aes(x = date, y = prix)) +
      geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "#AED6F1", alpha = 0.4, na.rm = TRUE) +
      geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "#5DADE2", alpha = 0.4, na.rm = TRUE) +
      geom_line(aes(color = type), linewidth = 0.8) +
      scale_color_manual(values = c("Historique" = "#2C3E50", "Prévision" = "#E74C3C")) +
      theme_minimal(base_size = 12) +
      labs(
        title = paste("Prévision SARIMA :", cat),
        subtitle = paste("Modèle :", model_label, "— IC 80% et 95%"),
        x = NULL, y = "Prix moyen (€/kg)", color = NULL
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
        legend.position = "bottom"
      )
    
    ggsave(filename = sprintf("cartes_produits/saisonnalite/SARIMA_%s.png",
                              str_replace_all(cat, "[^a-zA-Z0-9]", "_")),
           plot = p_fc, width = 10, height = 5, dpi = 200, bg = "white")
  }
}

cat(sprintf("\n   STL terminé pour %d catégories\n", length(resultats_stl)))
cat(sprintf("   SARIMA ajusté pour %d catégories\n\n", length(resultats_sarima)))

# =====================================================================
# 5. TABLEAU SYNTHÉTIQUE DE SAISONNALITÉ
# =====================================================================
cat("--- Étape 5 : Tableau synthétique ---\n")

# Calculer l'amplitude saisonnière et les mois pic/creux pour chaque produit
synthese_saison <- data.frame(
  categorie = character(),
  amplitude_saisonniere = numeric(),
  amplitude_relative_pct = numeric(),
  prix_moyen_produit = numeric(),
  mois_pic = integer(),
  mois_creux = integer(),
  tendance_pente = numeric(),
  sarima_order = character(),
  aic = numeric(),
  stringsAsFactors = FALSE
)

for (cat in names(resultats_stl)) {
  decomp <- resultats_stl[[cat]]
  
  # Profil saisonnier moyen (les 12 premiers mois si la série est assez longue)
  mois <- month(decomp$date)
  profil_saisonnier <- tapply(decomp$seasonal, mois, mean)
  
  # Amplitude = max - min du profil saisonnier
  amplitude <- max(profil_saisonnier) - min(profil_saisonnier)
  mois_pic <- as.integer(names(which.max(profil_saisonnier)))
  mois_creux <- as.integer(names(which.min(profil_saisonnier)))
  
  # Calcul du prix moyen et de l'amplitude relative (%)
  prix_moyen_cat <- mean(decomp$original, na.rm = TRUE)
  amplitude_relative <- (amplitude / prix_moyen_cat) * 100
  
  # Pente de la tendance (régression linéaire sur la tendance)
  trend_lm <- lm(decomp$trend ~ seq_len(nrow(decomp)))
  pente_mensuelle <- coef(trend_lm)[2]  # variation mensuelle en €/kg
  
  # Info SARIMA
  sarima_str <- NA
  aic_val <- NA
  if (cat %in% names(resultats_sarima)) {
    ord <- resultats_sarima[[cat]]$order
    seas <- resultats_sarima[[cat]]$seasonal
    sarima_str <- sprintf("(%d,%d,%d)×(%d,%d,%d)", ord[1], ord[2], ord[3], seas[1], seas[2], seas[3])
    aic_val <- resultats_sarima[[cat]]$model$aic
  }
  
  synthese_saison <- rbind(synthese_saison, data.frame(
    categorie = cat,
    amplitude_saisonniere = round(amplitude, 3),
    amplitude_relative_pct = round(amplitude_relative, 2),
    prix_moyen_produit = round(prix_moyen_cat, 3),
    mois_pic = mois_pic,
    mois_creux = mois_creux,
    tendance_pente = round(pente_mensuelle, 4),
    sarima_order = sarima_str,
    aic = round(aic_val, 1),
    stringsAsFactors = FALSE
  ))
}

# Trier par amplitude relative (%) décroissante
synthese_saison <- synthese_saison %>% arrange(desc(amplitude_relative_pct))

# Correspondance numéro de mois -> nom
noms_mois <- c("Jan", "Fév", "Mar", "Avr", "Mai", "Jun", 
               "Jul", "Aoû", "Sep", "Oct", "Nov", "Déc")
synthese_saison$mois_pic_nom <- noms_mois[synthese_saison$mois_pic]
synthese_saison$mois_creux_nom <- noms_mois[synthese_saison$mois_creux]

cat("\n   === TABLEAU SYNTHÉTIQUE DE SAISONNALITÉ ===\n\n")
cat(sprintf("   %-20s  %8s  %8s  %8s  %8s  %10s  %s\n", 
            "Catégorie", "Ampl(€)", "Ampl(%)", "Pic", "Creux", "Tendance", "SARIMA"))
cat(paste0("   ", paste(rep("-", 90), collapse = ""), "\n"))

for (i in 1:nrow(synthese_saison)) {
  row <- synthese_saison[i,]
  cat(sprintf("   %-20s  %8.3f  %7.2f%%  %8s  %8s  %+10.4f  %s\n",
              row$categorie, row$amplitude_saisonniere, row$amplitude_relative_pct,
              row$mois_pic_nom, row$mois_creux_nom,
              row$tendance_pente, 
              ifelse(is.na(row$sarima_order), "(échec)", row$sarima_order)))
}

# =====================================================================
# 6. GRAPHIQUE SYNTHÉTIQUE : PROFIL SAISONNIER COMPARÉ
# =====================================================================
cat("\n--- Étape 6 : Graphique synthétique des profils saisonniers ---\n")

# Top 12 catégories par amplitude saisonnière
top_cats <- head(synthese_saison$categorie, 12)

# Calculer le profil saisonnier moyen normalisé (en % de la moyenne)
profils_list <- list()
for (cat in top_cats) {
  decomp <- resultats_stl[[cat]]
  mois <- month(decomp$date)
  prix_moyen_global <- mean(decomp$original, na.rm = TRUE)
  profil <- tapply(decomp$seasonal, mois, mean)
  
  profils_list[[cat]] <- data.frame(
    categorie = cat,
    mois = 1:12,
    effet_saisonnier_pct = as.numeric(profil / prix_moyen_global * 100)
  )
}

df_profils <- bind_rows(profils_list) %>%
  mutate(mois_label = factor(noms_mois[mois], levels = noms_mois))

p_profils <- ggplot(df_profils, aes(x = mois_label, y = effet_saisonnier_pct, 
                                    color = categorie, group = categorie)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Profils saisonniers comparés des prix en circuits courts",
    subtitle = "Top 12 catégories par amplitude saisonnière (en % du prix moyen)",
    x = NULL, y = "Effet saisonnier (% du prix moyen)", color = "Produit"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("cartes_produits/saisonnalite/00_Profils_Saisonniers_Compares.png",
       plot = p_profils, width = 12, height = 7, dpi = 200, bg = "white")

# =====================================================================
# 7. SAUVEGARDE EN BASE ET EXPORT RDS
# =====================================================================
cat("--- Étape 7 : Sauvegarde ---\n")

# Sauvegarder la table synthèse en BDD
tryCatch({
  dbWriteTable(con, "synthese_saisonnalite", synthese_saison, 
               overwrite = TRUE, row.names = FALSE)
  cat("   Table 'synthese_saisonnalite' écrite en base\n")
}, error = function(e) {
  cat(sprintf("   [WARN] Écriture en base échouée : %s\n", e$message))
})

# Sauvegarder les séries mensuelles en BDD
tryCatch({
  dbWriteTable(con, "series_mensuelles_cc", series_mensuelles,
               overwrite = TRUE, row.names = FALSE)
  cat("   Table 'series_mensuelles_cc' écrite en base\n")
}, error = function(e) {
  cat(sprintf("   [WARN] Écriture en base échouée : %s\n", e$message))
})

# Export RDS pour app Shiny
export_dir <- "app_visu/data"
dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(series_mensuelles, file.path(export_dir, "series_mensuelles_cc.rds"))
saveRDS(series_par_plateforme, file.path(export_dir, "series_par_plateforme.rds"))
saveRDS(synthese_saison, file.path(export_dir, "synthese_saisonnalite.rds"))
cat("   Fichiers RDS exportés pour Shiny\n")

# Stocker les composantes STL dans un unique dataframe pour export
all_stl <- bind_rows(resultats_stl)
saveRDS(all_stl, file.path(export_dir, "decomposition_stl.rds"))
cat("   Composantes STL exportées\n")

dbDisconnect(con)

cat(sprintf("\n=== ANALYSE DE SAISONNALITÉ TERMINÉE (%s) ===\n", Sys.time()))
cat(sprintf("   %d graphiques STL générés dans cartes_produits/saisonnalite/\n", 
            length(resultats_stl)))
cat(sprintf("   %d modèles SARIMA ajustés\n", length(resultats_sarima)))
cat(sprintf("   Amplitude saisonnière max : %s (%.2f €/kg)\n", 
            synthese_saison$categorie[1], synthese_saison$amplitude_saisonniere[1]))
