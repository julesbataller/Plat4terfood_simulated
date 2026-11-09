library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(ggplot2)
library(ggrepel)
if (requireNamespace("cachem", quietly = TRUE)) {
  shinyOptions(cache = cachem::cache_mem(max_size = 100 * 1024^2))
}
library(lubridate)
library(shinycssloaders)
library(ggiraph)
library(ragg)

options(shiny.useragg = TRUE)
is_webr <- identical(Sys.info()[["sysname"]], "Emscripten") || grepl("wasm", R.version$platform, ignore.case = TRUE)
get_mariadb_ns <- function() {
  pkg <- paste0("R", "MariaDB")
  if (!is_webr && requireNamespace(pkg, quietly = TRUE)) asNamespace(pkg) else NULL
}
options(DT.options = list(
  language = list(url = '//cdn.datatables.net/plug-ins/1.13.6/i18n/fr-FR.json'),
  ordering = TRUE
))

# =====================================================================
# CHARGEMENT DES DONNÉES & CONFIGURATION
# =====================================================================
# Détection automatique du mode données simulées :
# activé si DATA_SIMULATED=true ou si le dossier data/ réel est absent
use_simulated_data <- tolower(Sys.getenv("DATA_SIMULATED")) %in% c("1", "true", "yes", "oui") ||
  (!file.exists("data/resultats_departements.rds") &&
   !file.exists("app_visu/data/resultats_departements.rds") &&
   file.exists("data_simulees/resultats_departements.rds"))

active_data_dir     <- if (use_simulated_data) "data_simulees" else "data"
active_alt_data_dir <- if (use_simulated_data) "data_simulees" else "app_visu/data"
active_www_dir      <- if (use_simulated_data && dir.exists("data_simulees/www")) "data_simulees/www" else "www"

if (dir.exists(active_www_dir)) {
  addResourcePath("static_assets", normalizePath(active_www_dir, winslash = "/", mustWork = FALSE))
  hero_img_url <- "static_assets/illustration_legumes.jpg"
} else {
  hero_img_url <- "illustration_legumes.jpg"
}

geojson_path <- file.path(active_data_dir, "departements.geojson")
dept_rds_path <- file.path(active_data_dir, "resultats_departements.rds")
classes_rds_path <- file.path(active_data_dir, "sample_classes.rds")
classes_rds_alt_path <- file.path(active_alt_data_dir, "sample_classes.rds")
ref_rds_path <- file.path(active_data_dir, "prix_reference_hors_cc.rds")
stl_rds_path <- file.path(active_data_dir, "decomposition_stl.rds")
stl_rds_alt_path <- file.path(active_alt_data_dir, "decomposition_stl.rds")
synthese_saison_rds_path <- file.path(active_data_dir, "synthese_saisonnalite.rds")
synthese_saison_rds_alt_path <- file.path(active_alt_data_dir, "synthese_saisonnalite.rds")
series_mensuelles_rds_path <- file.path(active_data_dir, "series_mensuelles_cc.rds")
series_mensuelles_rds_alt_path <- file.path(active_alt_data_dir, "series_mensuelles_cc.rds")

df_stl <- if (file.exists(stl_rds_path)) readRDS(stl_rds_path) else if (file.exists(stl_rds_alt_path)) readRDS(stl_rds_alt_path) else NULL
df_synthese_saison <- if (file.exists(synthese_saison_rds_path)) readRDS(synthese_saison_rds_path) else if (file.exists(synthese_saison_rds_alt_path)) readRDS(synthese_saison_rds_alt_path) else NULL
df_series_mensuelles <- if (file.exists(series_mensuelles_rds_path)) readRDS(series_mensuelles_rds_path) else if (file.exists(series_mensuelles_rds_alt_path)) readRDS(series_mensuelles_rds_alt_path) else NULL

sarima_rds_path <- file.path(active_data_dir, "precomputed_sarima.rds")
sarima_rds_alt_path <- file.path(active_alt_data_dir, "precomputed_sarima.rds")
ls_precomputed_sarima <- if (file.exists(sarima_rds_path)) {
  readRDS(sarima_rds_path)
} else if (file.exists(sarima_rds_alt_path)) {
  readRDS(sarima_rds_alt_path)
} else {
  NULL
}


# Données du Modèle Mixte
mm_ecarts_rds_path <- file.path(active_data_dir, "ecarts_plateforme_categorie.rds")
mm_ecarts_rds_alt_path <- file.path(active_alt_data_dir, "ecarts_plateforme_categorie.rds")
mm_heatmap_rds_path <- file.path(active_data_dir, "heatmap_data.rds")
mm_heatmap_rds_alt_path <- file.path(active_alt_data_dir, "heatmap_data.rds")
mm_effets_dept_rds_path <- file.path(active_data_dir, "effets_departementaux.rds")
mm_effets_dept_rds_alt_path <- file.path(active_alt_data_dir, "effets_departementaux.rds")
mm_effets_fixes_rds_path <- file.path(active_data_dir, "effets_fixes.rds")
mm_effets_fixes_rds_alt_path <- file.path(active_alt_data_dir, "effets_fixes.rds")
mm_variance_rds_path <- file.path(active_data_dir, "variance_components.rds")
mm_variance_rds_alt_path <- file.path(active_alt_data_dir, "variance_components.rds")
mm_comp_rds_path <- file.path(active_data_dir, "comparaison_modeles_aic_bic.rds")
mm_comp_rds_alt_path <- file.path(active_alt_data_dir, "comparaison_modeles_aic_bic.rds")
mm_diag_stud_rds_path <- file.path(active_data_dir, "diagnostics_student_gauss.rds")
mm_diag_stud_rds_alt_path <- file.path(active_alt_data_dir, "diagnostics_student_gauss.rds")

df_mm_ecarts <- if (file.exists(mm_ecarts_rds_path)) readRDS(mm_ecarts_rds_path) else if (file.exists(mm_ecarts_rds_alt_path)) readRDS(mm_ecarts_rds_alt_path) else NULL
df_mm_heatmap <- if (file.exists(mm_heatmap_rds_path)) readRDS(mm_heatmap_rds_path) else if (file.exists(mm_heatmap_rds_alt_path)) readRDS(mm_heatmap_rds_alt_path) else NULL
df_mm_effets_dept <- if (file.exists(mm_effets_dept_rds_path)) readRDS(mm_effets_dept_rds_path) else if (file.exists(mm_effets_dept_rds_alt_path)) readRDS(mm_effets_dept_rds_alt_path) else NULL
df_mm_effets_fixes <- if (file.exists(mm_effets_fixes_rds_path)) readRDS(mm_effets_fixes_rds_path) else if (file.exists(mm_effets_fixes_rds_alt_path)) readRDS(mm_effets_fixes_rds_alt_path) else NULL
ls_mm_variance <- if (file.exists(mm_variance_rds_path)) readRDS(mm_variance_rds_path) else if (file.exists(mm_variance_rds_alt_path)) readRDS(mm_variance_rds_alt_path) else NULL
df_mm_comparaison <- if (file.exists(mm_comp_rds_path)) readRDS(mm_comp_rds_path) else if (file.exists(mm_comp_rds_alt_path)) readRDS(mm_comp_rds_alt_path) else NULL
ls_mm_diag_stud <- if (file.exists(mm_diag_stud_rds_path)) readRDS(mm_diag_stud_rds_path) else if (file.exists(mm_diag_stud_rds_alt_path)) readRDS(mm_diag_stud_rds_alt_path) else NULL

# Calculs dynamiques dérivés du modèle mixte pour affichage immédiat dans l'interface
mm_prime_bio_str <- "+9,2 %"
mm_non_bio_str <- "-8,4 %"
mm_poids_moyen_str <- "-23,2 %"
mm_poids_gros_str <- "-33,2 %"
mm_ipt_str <- "de -15,8 % à +17,1 %"

if (!is.null(df_mm_effets_fixes) && nrow(df_mm_effets_fixes) > 0) {
  row_bio <- df_mm_effets_fixes[grepl("organicNon-Bio", df_mm_effets_fixes$variable), ]
  if (nrow(row_bio) > 0) {
    coef_nb <- as.numeric(row_bio$coefficient[1])
    mm_prime_bio_str <- sprintf("+%.1f %%", (exp(-coef_nb) - 1) * 100)
    mm_non_bio_str <- sprintf("%+.1f %%", row_bio$pct_effet[1])
  }
  row_m <- df_mm_effets_fixes[grepl("tranche_poidsMoyen", df_mm_effets_fixes$variable), ]
  if (nrow(row_m) > 0) {
    mm_poids_moyen_str <- sprintf("%+.1f %%", row_m$pct_effet[1])
  }
  row_g <- df_mm_effets_fixes[grepl("tranche_poidsGros", df_mm_effets_fixes$variable), ]
  if (nrow(row_g) > 0) {
    mm_poids_gros_str <- sprintf("%+.1f %%", row_g$pct_effet[1])
  }
}

if (!is.null(df_mm_effets_dept) && nrow(df_mm_effets_dept) > 0) {
  min_ipt <- min(df_mm_effets_dept$pct_effet, na.rm = TRUE)
  max_ipt <- max(df_mm_effets_dept$pct_effet, na.rm = TRUE)
  mm_ipt_str <- sprintf("de %+.1f %% à %+.1f %%", min_ipt, max_ipt)
}

mm_n_obs_str <- if (!is.null(ls_mm_variance)) format(ls_mm_variance$n_obs, big.mark = " ") else "25 490"
mm_n_dept_str <- if (!is.null(ls_mm_variance)) as.character(ls_mm_variance$n_dept) else "101"
mm_n_cat_str <- if (!is.null(ls_mm_variance)) as.character(ls_mm_variance$n_cat) else "61"
mm_total_ventes_str <- if (!is.null(ls_mm_variance)) format(ls_mm_variance$total_ventes, big.mark = " ") else "7 187 582"
mm_sigma2_resid_str <- if (!is.null(ls_mm_variance)) as.character(round(ls_mm_variance$sigma2_resid, 4)) else "11.5082"
mm_icc_str <- if (!is.null(ls_mm_variance)) sprintf("%.4f (%.2f%%)", ls_mm_variance$icc, ls_mm_variance$icc * 100) else "0.0007 (0.07%)"

# Indicateurs dynamiques pour le diagnostic Loi Normale vs Loi de Student-t
mm_n_res_str <- "21 598"
mm_nu_str <- "4,40"
mm_delta_aic_stud_str <- "1 977,5"
mm_norm_params_str <- "μ = 0,033 ; σ = 0,304"
mm_norm_ll_str <- "-4 924,2"
mm_norm_aic_str <- "9 852,5"
mm_stud_params_str <- "μ = 0,036 ; σ = 0,310 (s = 0,229, ν = 4,40)"
mm_stud_ll_str <- "-3 934,5"
mm_stud_aic_str <- "7 875,0"

if (!is.null(ls_mm_diag_stud)) {
  if (!is.null(ls_mm_diag_stud$precomputed)) {
    p <- ls_mm_diag_stud$precomputed
    if (!is.null(p$n_res_str)) mm_n_res_str <- p$n_res_str
    if (!is.null(p$nu_str)) mm_nu_str <- p$nu_str
    if (!is.null(p$delta_aic_stud_str)) mm_delta_aic_stud_str <- p$delta_aic_stud_str
    if (!is.null(p$norm_params_str)) mm_norm_params_str <- p$norm_params_str
    if (!is.null(p$norm_ll_str)) mm_norm_ll_str <- p$norm_ll_str
    if (!is.null(p$norm_aic_str)) mm_norm_aic_str <- p$norm_aic_str
    if (!is.null(p$stud_params_str)) mm_stud_params_str <- p$stud_params_str
    if (!is.null(p$stud_ll_str)) mm_stud_ll_str <- p$stud_ll_str
    if (!is.null(p$stud_aic_str)) mm_stud_aic_str <- p$stud_aic_str
  } else if (!is.null(ls_mm_diag_stud$res_gauss)) {
  rg <- as.numeric(ls_mm_diag_stud$res_gauss)
  rg <- rg[is.finite(rg)]
  if (length(rg) > 10) {
    nu_v <- as.numeric(ls_mm_diag_stud$nu[1])
    s_v  <- as.numeric(ls_mm_diag_stud$scale[1])
    m_v  <- as.numeric(ls_mm_diag_stud$location[1])
    mu_n <- mean(rg)
    sd_n <- sd(rg)
    ll_n <- sum(dnorm(rg, mean = mu_n, sd = sd_n, log = TRUE))
    ll_t <- sum(dt((rg - m_v) / s_v, df = nu_v, log = TRUE) - log(s_v))
    aic_n <- 4 - 2 * ll_n
    aic_t <- 6 - 2 * ll_t
    sd_t_theo <- if (nu_v > 2) s_v * sqrt(nu_v / (nu_v - 2)) else sd_n
    fmt_fr <- function(x, d = 1) gsub(".", ",", fixed = TRUE, format(round(x, d), big.mark = " ", nsmall = d, trim = TRUE))
    mm_n_res_str <- format(length(rg), big.mark = " ", trim = TRUE)
    mm_nu_str <- fmt_fr(nu_v, 2)
    mm_delta_aic_stud_str <- fmt_fr(max(0, aic_n - aic_t), 1)
    mm_norm_params_str <- sprintf("μ = %s ; σ = %s", fmt_fr(mu_n, 3), fmt_fr(sd_n, 3))
    mm_norm_ll_str <- fmt_fr(ll_n, 1)
    mm_norm_aic_str <- fmt_fr(aic_n, 1)
    mm_stud_params_str <- sprintf("μ = %s ; σ = %s (s = %s, ν = %s)", fmt_fr(m_v, 3), fmt_fr(sd_t_theo, 3), fmt_fr(s_v, 3), fmt_fr(nu_v, 2))
    mm_stud_ll_str <- fmt_fr(ll_t, 1)
    mm_stud_aic_str <- fmt_fr(aic_t, 1)
  }
}
}

# Données Socio-Économiques
socioeco_rds_path <- file.path(active_data_dir, "socioeco_departements.rds")
socioeco_rds_alt_path <- file.path(active_alt_data_dir, "socioeco_departements.rds")
magasins_rds_path <- file.path(active_data_dir, "magasins_producteurs.rds")
magasins_rds_alt_path <- file.path(active_alt_data_dir, "magasins_producteurs.rds")

df_socioeco <- if (file.exists(socioeco_rds_path)) readRDS(socioeco_rds_path) else if (file.exists(socioeco_rds_alt_path)) readRDS(socioeco_rds_alt_path) else NULL
df_magasins <- if (file.exists(magasins_rds_path)) readRDS(magasins_rds_path) else if (file.exists(magasins_rds_alt_path)) readRDS(magasins_rds_alt_path) else NULL

# Données d'Accueil pour affichage immédiat
accueil_rds_path <- file.path(active_data_dir, "accueil_data.rds")
accueil_rds_alt_path <- file.path(active_alt_data_dir, "accueil_data.rds")
accueil_data <- if (file.exists(accueil_rds_path)) {
  readRDS(accueil_rds_path)
} else if (file.exists(accueil_rds_alt_path)) {
  readRDS(accueil_rds_alt_path)
} else {
  NULL
}

# Données Funnel
funnel_rds_path <- file.path(active_data_dir, "funnel_data.rds")
funnel_rds_alt_path <- file.path(active_alt_data_dir, "funnel_data.rds")
ls_funnel <- if (file.exists(funnel_rds_path)) readRDS(funnel_rds_path) else if (file.exists(funnel_rds_alt_path)) readRDS(funnel_rds_alt_path) else NULL

funnel_raw_millions_str <- if (!is.null(ls_funnel$raw)) {
  paste0(gsub(".", ",", fixed = TRUE, sprintf("%.2f", sum(as.numeric(ls_funnel$raw$n), na.rm = TRUE) / 1e6)),
         if (use_simulated_data) " millions de transactions simulées" else " millions de transactions")
} else {
  "16,55 millions de transactions"
}
funnel_gmm_millions_str <- if (!is.null(ls_funnel$gmm)) {
  paste0(gsub(".", ",", fixed = TRUE, sprintf("%.2f", sum(as.numeric(ls_funnel$gmm$n), na.rm = TRUE) / 1e6)),
         if (use_simulated_data) " millions de transactions simulées" else " millions de transactions")
} else {
  "8,68 millions de transactions"
}

# Données NLP
nlp_coefs_rds_path <- file.path(active_data_dir, "coefs_nlp.rds")
nlp_coefs_alt_rds_path <- file.path(active_alt_data_dir, "coefs_nlp.rds")
nlp_coefs_path <- "../resultats_modele_mixte/coefs_nlp.csv"
nlp_coefs_alt_path <- "resultats_modele_mixte/coefs_nlp.csv"
df_nlp_coefs <- if (file.exists(nlp_coefs_rds_path)) {
  readRDS(nlp_coefs_rds_path)
} else if (file.exists(nlp_coefs_alt_rds_path)) {
  readRDS(nlp_coefs_alt_rds_path)
} else if (!use_simulated_data && file.exists(nlp_coefs_path)) {
  read.csv(nlp_coefs_path, stringsAsFactors = FALSE)
} else if (!use_simulated_data && file.exists(nlp_coefs_alt_path)) {
  read.csv(nlp_coefs_alt_path, stringsAsFactors = FALSE)
} else {
  NULL
}

# Pool d'échantillonnage proportionnel pour l'audit et la validation manuelle
audit_pool_path <- file.path(active_data_dir, "pool_audit_transactions.rds")
audit_pool_alt_path <- file.path(active_alt_data_dir, "pool_audit_transactions.rds")
df_audit_pool_init <- if (file.exists(audit_pool_path)) readRDS(audit_pool_path) else if (file.exists(audit_pool_alt_path)) readRDS(audit_pool_alt_path) else NULL

# Repères de prix moyens par produit (pour l'aide à l'audit)
df_audit_benchmarks <- if (!is.null(df_audit_pool_init)) {
  df_audit_pool_init %>%
    filter(is_outlier == 0, !is.na(prix_final), prix_final > 0) %>%
    group_by(categorie_standard) %>%
    summarise(
      q10 = round(quantile(prix_final, 0.10, na.rm = TRUE), 2),
      mediane = round(median(prix_final, na.rm = TRUE), 2),
      q90 = round(quantile(prix_final, 0.90, na.rm = TRUE), 2),
      .groups = "drop"
    )
} else NULL

# Modèles GMM paramétriques (Gaussiennes sous-jacentes) - Chargement différé
gmm_models_path <- file.path(active_data_dir, "gmm_models.rds")
gmm_models_alt_path <- file.path(active_alt_data_dir, "gmm_models.rds")

# Effectifs réels et statut outlier par classe GMM
gmm_counts_path <- file.path(active_data_dir, "gmm_class_counts.rds")
gmm_counts_alt_path <- file.path(active_alt_data_dir, "gmm_class_counts.rds")
df_gmm_counts <- if (file.exists(gmm_counts_path)) readRDS(gmm_counts_path) else if (file.exists(gmm_counts_alt_path)) readRDS(gmm_counts_alt_path) else NULL

# Diagnostics sémantiques et mots-clés par classe GMM
gmm_keywords_path <- file.path(active_data_dir, "gmm_class_keywords.rds")
gmm_keywords_alt_path <- file.path(active_alt_data_dir, "gmm_class_keywords.rds")
ls_gmm_keywords <- if (file.exists(gmm_keywords_path)) readRDS(gmm_keywords_path) else if (file.exists(gmm_keywords_alt_path)) readRDS(gmm_keywords_alt_path) else NULL


# Détection de l'utilisateur de base de données (jules sur Linux VM, root sur Windows local)
db_user <- if (Sys.info()[["sysname"]] == "Linux") "jules" else "root"

# 1. Chemins des contours géographiques
geojson_alt_path <- file.path(active_alt_data_dir, "departements.geojson")
boxes_dom_path <- file.path(active_data_dir, "boxes_dom.geojson")
boxes_dom_alt_path <- file.path(active_alt_data_dir, "boxes_dom.geojson")

# 2. Chargement des données (avec fallback base locale si RDS absent hors mode simulé)
load_data <- function() {
  if (file.exists(dept_rds_path)) {
    return(readRDS(dept_rds_path))
  }
  if (use_simulated_data) return(NULL)
  
  # Fallback base locale
  con <- tryCatch({
    get_mariadb_ns()$dbConnect(get_mariadb_ns()$MariaDB(), host = "127.0.0.1", port = 3306, dbname = Sys.getenv("DB_NAME", "market_prices"), user = db_user, password = "")
  }, error = function(e) NULL)
  
  if (is.null(con)) return(NULL)
  
  res <- if (get_mariadb_ns()$dbExistsTable(con, "resultats_departements")) {
    get_mariadb_ns()$dbReadTable(con, "resultats_departements")
  } else {
    NULL
  }
  get_mariadb_ns()$dbDisconnect(con)
  return(res)
}

load_classes_sample <- function() {
  if (file.exists(classes_rds_path)) {
    return(readRDS(classes_rds_path))
  } else if (file.exists(classes_rds_alt_path)) {
    return(readRDS(classes_rds_alt_path))
  }
  if (use_simulated_data) return(NULL)
  
  # Fallback base locale
  con <- tryCatch({
    get_mariadb_ns()$dbConnect(get_mariadb_ns()$MariaDB(), host = "127.0.0.1", port = 3306, dbname = Sys.getenv("DB_NAME", "market_prices"), user = db_user, password = "")
  }, error = function(e) NULL)
  
  if (is.null(con)) return(NULL)
  
  res <- if (get_mariadb_ns()$dbExistsTable(con, "prix_trie")) {
    df_clean <- get_mariadb_ns()$dbGetQuery(con, "
      SELECT plateforme, categorie_standard, prix_final, classe, nb_classes 
      FROM prix_trie 
      WHERE is_outlier = 0 AND classe IS NOT NULL
      LIMIT 100000
    ")
    df_clean
  } else {
    NULL
  }
  get_mariadb_ns()$dbDisconnect(con)
  return(res)
}

load_ref_prices <- function() {
  if (file.exists(ref_rds_path)) {
    return(readRDS(ref_rds_path))
  }
  if (use_simulated_data) return(NULL)
  
  # Fallback base locale
  con <- tryCatch({
    get_mariadb_ns()$dbConnect(get_mariadb_ns()$MariaDB(), host = "127.0.0.1", port = 3306, dbname = Sys.getenv("DB_NAME", "market_prices"), user = db_user, password = "")
  }, error = function(e) NULL)
  
  if (is.null(con)) return(NULL)
  
  res <- if (get_mariadb_ns()$dbExistsTable(con, "prix_reference_hors_cc")) {
    get_mariadb_ns()$dbReadTable(con, "prix_reference_hors_cc")
  } else {
    NULL
  }
  get_mariadb_ns()$dbDisconnect(con)
  return(res)
}

df_ref_all <- load_ref_prices()

# 3. Chargement et nettoyage des données de structure agricole RA 2020
ra2020_rds_path <- file.path(active_data_dir, "ra2020_departements.rds")
ra2020_alt_rds_path <- file.path(active_alt_data_dir, "ra2020_departements.rds")

df_ra2020 <- if (file.exists(ra2020_rds_path)) {
  readRDS(ra2020_rds_path)
} else if (file.exists(ra2020_alt_rds_path)) {
  readRDS(ra2020_alt_rds_path)
} else {
  NULL
}

# Si absent des RDS (hors mode simulé), tenter de charger depuis la base MariaDB
if (is.null(df_ra2020) && !use_simulated_data) {
  con_ra <- tryCatch({
    get_mariadb_ns()$dbConnect(get_mariadb_ns()$MariaDB(), host = "127.0.0.1", port = 3306, dbname = Sys.getenv("DB_NAME", "market_prices"), user = db_user, password = "")
  }, error = function(e) NULL)
  
  if (!is.null(con_ra)) {
    if (get_mariadb_ns()$dbExistsTable(con_ra, "structure_agricole_ra2020")) {
      df_ra2020 <- tryCatch({
        get_mariadb_ns()$dbReadTable(con_ra, "structure_agricole_ra2020")
      }, error = function(e) NULL)
    }
    get_mariadb_ns()$dbDisconnect(con_ra)
  }
}


# Si absent de la base (hors mode simulé), fallback sur le fichier RData local
if (is.null(df_ra2020) && !use_simulated_data) {
  df_ra2020_path <- "rapprochement/data/Donnees_CC/Typo_transitionABetCC_RA2020_DEPT2020.RData"
  df_ra2020_alt_path <- "../rapprochement/data/Donnees_CC/Typo_transitionABetCC_RA2020_DEPT2020.RData"
  
  df_ra2020_raw <- NULL
  if (file.exists(df_ra2020_path)) {
    load(df_ra2020_path)
    df_ra2020_raw <- x
  } else if (file.exists(df_ra2020_alt_path)) {
    load(df_ra2020_alt_path)
    df_ra2020_raw <- x
  }
  
  df_ra2020 <- if (!is.null(df_ra2020_raw)) {
    df_clean <- df_ra2020_raw
    df_clean$CodeDEPT <- trimws(as.character(df_clean$CodeDEPT))
    df_clean$NOMDEPT <- trimws(as.character(df_clean$NOMDEPT))
    
    # Convertir toutes les colonnes contenant "EA2020", "SAUEA2020" ou "ETPEA2020" ou "Total" en numérique
    for(col in colnames(df_clean)) {
      if (col != "CodeDEPT" && col != "NOMDEPT") {
        df_clean[[col]] <- as.numeric(as.character(df_clean[[col]]))
      }
    }
    
    # Calculer les indicateurs clés pour l'analyse
    df_clean %>%
      mutate(
        Pct_Exploitations_CC = (EA2020conventionnecourt + EA2020biocourt) / TotalEA2020 * 100,
        Pct_SAU_CC = (SAUEA2020conventionnecourt + SAUEA2020biocourt) / TotalSAUEA2020.ha. * 100,
        Pct_Exploitations_Bio = (EA2020biolong + EA2020biocourt) / TotalEA2020 * 100,
        Pct_SAU_Bio = (SAUEA2020biolong + SAUEA2020biocourt) / TotalSAUEA2020.ha. * 100,
        Pct_Exploitations_Bio_CC = EA2020biocourt / TotalEA2020 * 100,
        TotalEA = TotalEA2020,
        TotalSAU = TotalSAUEA2020.ha.,
        TotalETP = TotalETPEA2020
      ) %>%
      select(CodeDEPT, NOMDEPT, Pct_Exploitations_CC, Pct_SAU_CC, Pct_Exploitations_Bio, Pct_SAU_Bio, Pct_Exploitations_Bio_CC, TotalEA, TotalSAU, TotalETP)
  } else {
    NULL
  }
}

# Anonymisation Globale des Plateformes (via table locale externe optionnelle en mode réel)
plat_map_path <- file.path(active_data_dir, "mapping_plateformes.rds")
plat_mapping <- if (!use_simulated_data && file.exists(plat_map_path)) readRDS(plat_map_path) else NULL

anonymize_plat <- function(df) {
  if (is.null(df)) return(df)
  cols_to_check <- c("Plateforme", "plateforme", "Plateforme_Source", "plateforme_label")
  for (col in cols_to_check) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
      if (!is.null(plat_mapping)) {
        for (raw_nm in names(plat_mapping)) {
          df[[col]][df[[col]] == raw_nm] <- unname(plat_mapping[[raw_nm]])
        }
      }
    }
  }
  if ("variable" %in% names(df) && !is.null(plat_mapping)) {
    for (raw_nm in names(plat_mapping)) {
      df$variable <- gsub(paste0("Plateforme", raw_nm), paste0("Plateforme", unname(plat_mapping[[raw_nm]])), df$variable, fixed = TRUE)
    }
  }
  return(df)
}

# Anonymisation des tables légères chargées immédiatement
df_stl <- anonymize_plat(df_stl)
df_synthese_saison <- anonymize_plat(df_synthese_saison)
df_series_mensuelles <- anonymize_plat(df_series_mensuelles)
df_mm_ecarts <- anonymize_plat(df_mm_ecarts)
df_mm_heatmap <- anonymize_plat(df_mm_heatmap)
df_mm_effets_dept <- anonymize_plat(df_mm_effets_dept)
df_mm_effets_fixes <- anonymize_plat(df_mm_effets_fixes)
df_socioeco <- anonymize_plat(df_socioeco)
df_magasins <- anonymize_plat(df_magasins)
df_audit_pool_init <- anonymize_plat(df_audit_pool_init)

# =====================================================================
# GESTIONNAIRE DE CHARGEMENT DIFFÉRÉ (LAZY CACHE & ARRIÈRE-PLAN)
# =====================================================================
data_cache <- new.env(parent = emptyenv())
data_cache$france_depts <- NULL
data_cache$boxes_dom <- NULL
data_cache$df_dept_all <- NULL
data_cache$df_classes_sample <- NULL
data_cache$ls_gmm_models <- NULL

load_france_depts <- function() {
  if (!is.null(data_cache$france_depts)) return(data_cache$france_depts)
  sf_rds_path <- file.path(active_data_dir, "departements_sf.rds")
  sf_rds_alt_path <- file.path(active_alt_data_dir, "departements_sf.rds")
  res <- if (file.exists(sf_rds_path)) {
    readRDS(sf_rds_path)
  } else if (file.exists(sf_rds_alt_path)) {
    readRDS(sf_rds_alt_path)
  } else if (file.exists(geojson_path)) {
    tryCatch(st_read(geojson_path, quiet = TRUE), error = function(e) NULL)
  } else if (file.exists(geojson_alt_path)) {
    tryCatch(st_read(geojson_alt_path, quiet = TRUE), error = function(e) NULL)
  } else {
    NULL
  }
  data_cache$france_depts <- res
  res
}

load_boxes_dom <- function() {
  if (!is.null(data_cache$boxes_dom)) return(data_cache$boxes_dom)
  boxes_rds_path <- file.path(active_data_dir, "boxes_dom_sf.rds")
  boxes_rds_alt_path <- file.path(active_alt_data_dir, "boxes_dom_sf.rds")
  res <- if (file.exists(boxes_rds_path)) {
    readRDS(boxes_rds_path)
  } else if (file.exists(boxes_rds_alt_path)) {
    readRDS(boxes_rds_alt_path)
  } else if (file.exists(boxes_dom_path)) {
    tryCatch(st_read(boxes_dom_path, quiet = TRUE), error = function(e) NULL)
  } else if (file.exists(boxes_dom_alt_path)) {
    tryCatch(st_read(boxes_dom_alt_path, quiet = TRUE), error = function(e) NULL)
  } else {
    NULL
  }
  data_cache$boxes_dom <- res
  res
}

load_dept_impl <- function() {
  if (!is.null(data_cache$df_dept_all)) return(data_cache$df_dept_all)
  res <- load_data()
  res <- anonymize_plat(res)
  data_cache$df_dept_all <- res
  res
}

load_classes_impl <- function() {
  if (!is.null(data_cache$df_classes_sample)) return(data_cache$df_classes_sample)
  res <- load_classes_sample()
  res <- anonymize_plat(res)
  data_cache$df_classes_sample <- res
  res
}

load_gmm_impl <- function() {
  if (!is.null(data_cache$ls_gmm_models)) return(data_cache$ls_gmm_models)
  res <- if (file.exists(gmm_models_path)) {
    readRDS(gmm_models_path)
  } else if (file.exists(gmm_models_alt_path)) {
    readRDS(gmm_models_alt_path)
  } else {
    NULL
  }
  data_cache$ls_gmm_models <- res
  res
}

# Liaisons actives transparentes : accès sans parenthèses entièrement rétrocompatible
env_target <- environment()
makeActiveBinding("france_depts", load_france_depts, env_target)
makeActiveBinding("boxes_dom", load_boxes_dom, env_target)
makeActiveBinding("df_dept_all", load_dept_impl, env_target)
makeActiveBinding("df_classes_sample", load_classes_impl, env_target)
makeActiveBinding("ls_gmm_models", load_gmm_impl, env_target)

if (!identical(env_target, .GlobalEnv)) {
  tryCatch({
    makeActiveBinding("france_depts", load_france_depts, .GlobalEnv)
    makeActiveBinding("boxes_dom", load_boxes_dom, .GlobalEnv)
    makeActiveBinding("df_dept_all", load_dept_impl, .GlobalEnv)
    makeActiveBinding("df_classes_sample", load_classes_impl, .GlobalEnv)
    makeActiveBinding("ls_gmm_models", load_gmm_impl, .GlobalEnv)
  }, error = function(e) NULL)
}
if (!is.null(ls_funnel)) {
  if (!is.null(ls_funnel$raw)) ls_funnel$raw <- anonymize_plat(ls_funnel$raw)
  if (!is.null(ls_funnel$categorized)) ls_funnel$categorized <- anonymize_plat(ls_funnel$categorized)
  if (!is.null(ls_funnel$valid)) ls_funnel$valid <- anonymize_plat(ls_funnel$valid)
  if (!is.null(ls_funnel$gmm)) ls_funnel$gmm <- anonymize_plat(ls_funnel$gmm)
}

# Chargement du référentiel FoodOn enrichi
ref_foodon_path <- file.path(active_alt_data_dir, "referentiel_categories_foodon.rds")
ref_foodon_alt_path <- file.path(active_data_dir, "referentiel_categories_foodon.rds")
df_foodon_ref <- if (file.exists(ref_foodon_path)) readRDS(ref_foodon_path) else if (file.exists(ref_foodon_alt_path)) readRDS(ref_foodon_alt_path) else NULL

# Liste des produits disponibles triée par nombre total de ventes décroissant
produits_choices <- if (!is.null(df_foodon_ref)) {
  sort(df_foodon_ref$categorie_standard)
} else if (!is.null(df_series_mensuelles)) {
  df_series_mensuelles %>%
    group_by(categorie_standard) %>%
    summarise(total_ventes = sum(nb_transactions, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_ventes)) %>%
    pull(categorie_standard)
} else if (!is.null(df_dept_all)) {
  sort(unique(df_dept_all$categorie_standard))
} else {
  c("Carotte", "Courge", "Pomme")
}

# Groupement hiérarchique par rayon/famille pour les menus déroulants
produits_choices_grouped <- if (!is.null(df_foodon_ref)) {
  sorted_ref <- df_foodon_ref %>% arrange(groupe, categorie_standard)
  prod_list <- split(sorted_ref$categorie_standard, sorted_ref$groupe)
  c(list("Indice Territorial Global" = c("Indice des Prix Territoriaux (IPT)" = "Indice Français des Prix en Circuits Courts")), prod_list)
} else {
  c("Indice des Prix Territoriaux (IPT)" = "Indice Français des Prix en Circuits Courts", produits_choices)
}

# Liste des années disponibles extraite dynamiquement sans charger les données territoriales lourdes
annee_choices <- if (!is.null(df_series_mensuelles)) {
  raw_years <- setdiff(unique(format(df_series_mensuelles$date, "%Y")), "Toutes")
  raw_years <- raw_years[grepl("^20[0-9]{2}$", raw_years)]
  c("Toutes", sort(raw_years))
} else {
  c("Toutes", "2021", "2022", "2023", "2024", "2025", "2026")
}

# =====================================================================
# BLOC FOOTER PARTAGÉ
# =====================================================================

ui_footer <- tags$footer(
  style = "background-color: #F8F9F9; border-top: 1px solid #BDC3C7; padding: 25px 20px; margin-top: 40px; font-family: 'Roboto', sans-serif; box-shadow: 0 -2px 5px rgba(0,0,0,0.02);",
  fluidRow(
    column(8,
           tags$p(strong("Priscope — Démonstrateur d'Analyse et de Modélisation des Prix")),
           if (use_simulated_data) {
             tags$p(
               style = "font-size: 0.85em; color: #92400E; margin-bottom: 5px;",
               tags$strong("Version de démonstration sur données synthétiques : "),
               "Cette application fonctionne exclusivement sur des données simulées et anonymisées (Plateforme 1 à Plateforme 4). Elle illustre la chaîne de traitement statistique et économétrique sans exposer les données confidentielles des partenaires réels."
             )
           } else {
             tags$p(
               style = "font-size: 0.85em; color: #5D6D7E; margin-bottom: 5px;",
               tags$strong("Sources des données : "),
               "Relevés de prix multi-canaux et indicateurs territoriaux de référence (INSEE / IGN)."
             )
           },
           tags$p(tags$small(
             style = "color: #64748B; font-style: italic; font-size: 0.83em; display: block; margin-top: 0.5em; line-height: 1.4;",
             "Conçu et développé par Jules Bataller-Beltran. Démonstrateur autonome d'économétrie spatiale et de modélisation statistique des prix (données synthétiques de démonstration). Projet personnel indépendant n'engageant aucune organisation tierce ni aucun tiers commercial."
           )),
           tags$div(
             style = "margin-top: 10px;",
             uiOutput("ui_admin_auth")
           )
    ),
    column(4,
           style = "text-align: right; font-size: 0.85em; color: #64748B;",
           tags$p(tags$strong("Référentiels ouverts & Code :")),
           tags$ul(style = "list-style: none; padding: 0; margin: 0; line-height: 1.8;",
             tags$li("• Ontologie FoodOn"),
             tags$li("• Contours administratifs IGN / INSEE"),
             tags$li(tags$a(href = "https://github.com/julesbataller/Priscope", target = "_blank", "• Code source & documentation GitHub"))
           )
    )
  )
)

# =====================================================================
# INTERFACE UTILISATEUR (UI)
# =====================================================================
ui <- page_navbar(
  id = "main_nav",
  title = "Priscope - Observatoire Économétrique des Prix",
  fillable = FALSE,
  footer = ui_footer,
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#228833",
    secondary = "#2C3E50",
    font_scale = 0.95,
    base_font = font_google("Inter", local = FALSE),
    heading_font = font_google("Outfit", local = FALSE)
  ),
  header = tagList(
    tags$head(
    tags$style(HTML("
      /* Style moderne et épuré pour les cartes */
      .card {
        border: 1px solid #E2E8F0 !important;
        border-radius: 8px !important;
        box-shadow: 0 1px 3px rgba(0,0,0,0.03) !important;
      }
      /* Active tab underline in vegetal green (leaving text in its default theme color) */
      .navbar-nav .nav-link.active {
        border-bottom: 3px solid #228833 !important;
      }
      /* Navbar brand / title in vegetal green */
      .navbar-brand {
        color: #228833 !important;
        font-weight: bold;
      }
      /* Override the default theme turquoise (which looks blue/green) for primary backgrounds */
      .bg-primary, .text-bg-primary {
        background-color: #228833 !important;
        color: #FFFFFF !important;
      }
      /* Hover scale effect for footer logos */
      footer img {
        transition: transform 0.1s ease;
      }
      footer img:hover {
        transform: scale(1.1);
      }
      /* Empecher le rognage des menus deroulants selectize sous les cartes */
      .selectize-dropdown {
        z-index: 99999 !important;
      }
      .card-overflow-visible,
      .card-overflow-visible .card-body {
        overflow: visible !important;
      }
      
      /* ========================================================= */
      /* RESPONSIVE DESIGN POUR MOBILES ET TABLETTES               */
      /* ========================================================= */
      @media (max-width: 768px) {
        /* 1. Passer toutes les grilles bslib (layout_columns & layout_column_wrap) en colonne unique */
        .bslib-grid, 
        .grid, 
        .bslib-layout-columns, 
        .bslib-column-wrap {
          display: flex !important;
          flex-direction: column !important;
          width: 100% !important;
          gap: 12px !important;
        }
        .bslib-grid > *, 
        .grid > *, 
        .bslib-layout-columns > *, 
        .bslib-column-wrap > * {
          width: 100% !important;
          max-width: 100% !important;
          grid-column: span 12 / span 12 !important;
          flex: 1 1 100% !important;
        }

        /* 2. Cartes : annulation de la hauteur fixe bloquante et marges fluides */
        .container-fluid {
          padding-left: 8px !important;
          padding-right: 8px !important;
        }
        .card {
          margin-bottom: 12px !important;
        }
        .card[style*='height: 750px'], 
        .card[style*='height:750px'],
        .card[style*='height: 650px'],
        .card[style*='height:650px'] {
          height: auto !important;
          min-height: 440px !important;
        }
        .card-body {
          padding: 12px !important;
        }

        /* 3. Cartes interactives ggiraph : eviter le piege tactile de defilement */
        .html-widget.ggiraph, 
        .girafe-container {
          height: auto !important;
          min-height: 380px !important;
          touch-action: pan-y !important;
        }
        .html-widget.ggiraph svg {
          width: 100% !important;
          height: auto !important;
          max-height: 480px !important;
        }

        /* 4. Value Boxes / KPI d'accueil */
        .bslib-value-box {
          min-height: 85px !important;
          padding: 12px 14px !important;
        }
        .bslib-value-box .value-box-value {
          font-size: 1.55rem !important;
        }
        .bslib-value-box .value-box-title {
          font-size: 0.85rem !important;
        }

        /* 5. Bandeau de notification simulation compact */
        .banner-simulation-container {
          padding: 8px 12px !important;
          font-size: 0.80rem !important;
          line-height: 1.35 !important;
        }

        /* 6. En-tete et barre de navigation */
        .navbar-brand {
          font-size: 1.05rem !important;
          white-space: normal !important;
          line-height: 1.25 !important;
        }
        .navbar-nav .nav-link {
          padding: 8px 12px !important;
          font-size: 0.92rem !important;
        }
        .navbar-nav .nav-link.active {
          border-bottom: none !important;
          border-left: 4px solid #228833 !important;
          background-color: #F0FDF4 !important;
        }

        /* 7. Tableaux de donnees (DT) scrollables sans debordement */
        .dataTables_wrapper {
          width: 100% !important;
          overflow-x: auto !important;
          -webkit-overflow-scrolling: touch !important;
        }
        table.dataTable {
          font-size: 0.80rem !important;
        }

        /* 8. Menus deroulants et champs de filtre */
        .selectize-input {
          font-size: 0.88rem !important;
          padding: 7px 10px !important;
        }
        .form-group {
          margin-bottom: 8px !important;
        }

        /* 9. Images et graphiques */
        .shiny-plot-output img {
          max-width: 100% !important;
          height: auto !important;
        }
      }

      /* Adaptation pour tablettes (769px a 992px) */
      @media (min-width: 769px) and (max-width: 992px) {
        .bslib-grid, .grid {
          grid-template-columns: repeat(2, 1fr) !important;
          gap: 12px !important;
        }
        .bslib-grid > *, .grid > * {
          grid-column: span 1 / span 1 !important;
        }
        .html-widget.ggiraph {
          touch-action: pan-y !important;
        }
      }
    "))
    ),
    if (use_simulated_data) {
      tags$div(
        class = "banner-simulation-container",
        style = "background-color: #FFFBEB; border-bottom: 2px solid #F59E0B; color: #92400E; padding: 10px 24px; font-size: 0.92rem; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 10px;",
        tags$div(
          tags$strong("Mode démonstration sur données simulées — "),
          "Les prix, volumes, indicateurs territoriaux et graphiques présentés dans cette application sont générés synthétiquement pour illustrer les méthodes statistiques du projet sans exposer les données réelles des partenaires. Les plateformes sont anonymisées de Plateforme 1 à Plateforme 4."
        ),
        tags$div(
          style = "display: flex; align-items: center; gap: 8px;",
          uiOutput("data_status_badge"),
          tags$span(
            class = "badge",
            style = "background-color: #D97706; color: #FFFFFF; font-weight: 600; font-size: 0.80rem; padding: 6px 10px;",
            "Données simulées"
          )
        )
      )
    }
  ),
  
  # ---------- ONGLET 1 : ACCUEIL ----------
  nav_panel("Accueil",
    fluidPage(
      # Bannière d'introduction avec l'illustration Priscope
      card(
        class = "border-0 shadow-sm mb-4",
        style = sprintf("background: linear-gradient(135deg, rgba(244, 249, 244, 0.95), rgba(255, 255, 255, 0.98)), url('%s'); background-size: cover; background-position: center; border-radius: 12px;", hero_img_url),
        card_body(
          fluidRow(
            column(9,
              h2("Priscope — Observatoire Économétrique des Prix", 
                 style = "font-family: 'Outfit'; font-weight: 700; color: #228833; margin-top: 5px; margin-bottom: 8px;"),
              p("Plateforme d'exploration spatiale, de filtrage probabiliste et de modélisation économétrique des distributions de prix multi-canaux. Démonstrateur autonome d'ingénierie statistique développé par Jules Bataller-Beltran sur données simulées.",
                style = "font-size: 1.02rem; color: #2C3E50; margin-bottom: 0;")
            ),
            column(3, style = "text-align: right; display: flex; align-items: center; justify-content: flex-end;",
              tags$img(src = hero_img_url, 
                       alt = "Illustration Priscope", 
                       style = "max-height: 95px; border-radius: 8px; box-shadow: 0 4px 10px rgba(0,0,0,0.08);")
            )
          )
        )
      ),
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "Transactions Géolocalisées",
          value = textOutput("total_transactions"),
          uiOutput("acc_transactions_sub"),
          showcase = icon("shopping-bag"),
          theme = "primary"
        ),
        value_box(
          title = "Catégories de Produits",
          value = textOutput("total_categories"),
          showcase = icon("th"),
          theme = "success"
        ),
        value_box(
          title = "Départements Couverts",
          value = textOutput("total_depts"),
          uiOutput("acc_depts_sub"),
          showcase = icon("map-marker-alt"),
          theme = "dark"
        )
      ),
      br(),
      layout_column_wrap(
        width = 1/2,
        navset_card_tab(
          title = "Entonnoir des Données",
          full_screen = TRUE,
          selected = "2. Données Filtrées GMM",
          nav_panel("1. Données Collectées", 
                    p(tags$em(sprintf("Les données issues des 4 plateformes anonymisées (%s).", funnel_raw_millions_str)), style="margin-top:10px; color:#5D6D7E; text-align:center;"),
                    navset_pill(
                      nav_panel("Par Plateforme", plotOutput("pie_raw_plat_plot", height = "450px")),
                      nav_panel("Par Année", plotOutput("pie_raw_year_plot", height = "450px"))
                    )),
          nav_panel("2. Données Filtrées GMM", 
                    p(tags$em(sprintf("Les données après élimination probabiliste des prix aberrants par mélange gaussien (%s).", funnel_gmm_millions_str)), style="margin-top:10px; color:#5D6D7E; text-align:center;"),
                    navset_pill(
                      nav_panel("Par Plateforme", plotOutput("pie_gmm_plat_plot", height = "450px")),
                      nav_panel("Par Année", plotOutput("pie_gmm_year_plot", height = "450px"))
                    ),
                    uiOutput("funnel_gmm_note")
          )
        ),
        card(
          card_header(class = "bg-primary text-white", h5("Distribution par Plateforme par Produit", style = "margin:0; font-family:'Outfit';")),
          card_body(
            selectInput("pie_prod_select", "Choisir un produit :", choices = produits_choices_grouped, selected = "Carotte"),
            plotOutput("pie_product_plot", height = "280px")
          )
        )
      ),
      br(),
      card(
        card_header(class = "bg-primary text-white", h4("Priscope — Démarche Méthodologique et Analyse des Prix", style = "margin:0; font-family:'Outfit';")),
        card_body(
          h5("Objectifs de l'Observatoire Économétrique", style = "color:#2C3E50; font-weight:bold;"),
          p(sprintf("Cet observatoire présente les métriques de prix standardisés, nettoyés des anomalies et modélisés pour %d catégories de référence. L'analyse observe la dispersion tarifaire à travers 4 canaux de distribution distincts :", length(produits_choices)), style = "font-size:1.1em;"),
          tags$ul(
            tags$li(strong("Plateforme 1"), " : Canal numérique de groupements d'achats et précommandes locales."),
            tags$li(strong("Plateforme 2"), " : Canal collectif de vente directe multi-producteurs."),
            tags$li(strong("Plateforme 3"), " : Outil de gestion commerciale directe des exploitations agricoles."),
            tags$li(strong("Plateforme 4"), " : Réseau de points de distribution locaux en circuit court.")
          ),
          hr(),
          h5("Nettoyage des anomalies par Mélange Gaussien GMM MMG", style = "color:#2C3E50; font-weight:bold;"),
          p("Les données brutes en vente directe comportent de nombreuses erreurs de saisie (ex: prix d'une caisse entière ou d'un lot saisi en tant que prix au kilo, ou à l'inverse un prix à la pièce aberrant).", style = "font-size:1.1em;"),
          p("Pour corriger ces anomalies sans fixer de seuils arbitraires, le projet utilise des ", strong("Modèles de Mélange Gaussien (GMM)"), " sur le logarithme des prix. Cet algorithme classe automatiquement les transactions de chaque catégorie en plusieurs profils de prix (les classes GMM) et détecte de façon probabiliste le bruit de fond statistique (les valeurs aberrantes), qui sont écartées de notre analyse (is_outlier = 1). Les visualisations et cartes présentées sur ce site s'appuient exclusivement sur ces données propres filtrées.", style = "font-size:1.1em;")
        )
      )
    )
  ),
  
  # ---------- ONGLET 2 : EXPLORATEUR & CARTES DES PRODUITS ----------
  nav_panel("Explorateur & Cartes des Produits",
    fluidPage(
      # Bandeau supérieur compact de filtres horizontaux (Top Bar)
      card(
        class = "card-overflow-visible",
        card_body(
          style = "padding: 14px 20px; background-color: #ffffff; border-radius: 8px; border: 1px solid #E2E8F0; margin-bottom: 16px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); overflow: visible !important;",
          # Ligne 1 : Sélection produit et tags FoodOn
          layout_columns(
            col_widths = c(4, 3, 3, 2),
            selectInput("map_prod", "Produit sélectionné :", choices = produits_choices_grouped, selected = "Carotte", width = "100%"),
            selectInput("tag_rayon", "Rayon / Famille (FoodOn) :", 
                        choices = c("Tous les rayons", "Légumes", "Fruits", "Viandes", "Produits laitiers & Œufs", "Épicerie", "Boissons"), 
                        selected = "Tous les rayons", width = "100%"),
            selectInput("tag_saison", "Saison de production :", 
                        choices = c("Toutes saisons", "Hiver", "Printemps", "Été", "Automne", "Toute l'année"), 
                        selected = "Toutes saisons", width = "100%"),
            div(
              tags$label("Filtre panier :", style = "font-weight: 500; font-size: 0.88em; color: #475569; display: block; margin-bottom: 6px;"),
              div(
                style = "height: 38px; display: flex; align-items: center; background: #F8FAFC; border: 1px solid #CBD5E1; border-radius: 6px; padding: 0 10px;",
                checkboxInput("tag_panier", "Panier quotidien", value = FALSE, width = "100%")
              )
            )
          ),
          # Ligne 2 : Filtres plateforme, temporels et représentativité
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            selectInput("map_plat", "Plateforme :", choices = c("Toutes", "Plateforme 1", "Plateforme 2", "Plateforme 3", "Plateforme 4"), selected = "Toutes", width = "100%"),
            selectInput("map_year", "Année :", choices = annee_choices, selected = "Toutes", width = "100%"),
            selectInput("map_organic", "Type de culture :", choices = c("Tous", "Bio", "Non-Bio"), selected = "Tous", width = "100%"),
            numericInput("map_min_sales", "Min. ventes/dép. (seuil TCL) :", value = 15, min = 1, step = 1, width = "100%")
          ),
          div(
            style = "font-size: 0.80em; color: #94A3B8; text-align: right; margin-top: 6px; font-style: italic;",
            "Note : Les départements grisés ne disposent pas de données ou sont sous le seuil minimum de ventes requis."
          )
        )
      ),
      
      # Bandeau contextuel d'analyse produit & représentativité (3 cartes équilibrées)
      layout_columns(
        col_widths = c(4, 4, 4),
        style = "margin-bottom: 20px;",
        uiOutput("foodon_badge_card"),
        htmlOutput("ref_price_box"),
        htmlOutput("clt_info_text")
      ),
      card(
        full_screen = TRUE,
        height = "750px",
        card_header(class = "bg-secondary text-white", uiOutput("map_card_title")),
        card_body(
          class = "p-0",
          withSpinner(girafeOutput("map_plot", width = "100%", height = "100%"), type = 8, color = "#2C3E50")
        )
      ),
      br(),
      card(
        card_header(h4("Statistiques détaillées par département", style = "margin:0; font-family:'Outfit';")),
        card_body(
          DTOutput("table_departements")
        )
      )
    )
  ),
  
  # ---------- ONGLET 3 : ANALYSES & CLASSES GMM ----------
  nav_panel("Analyses des Prix & Classes GMM",
    fluidPage(
      # Bandeau supérieur compact de filtres horizontaux (Top Bar)
      card(
        class = "card-overflow-visible",
        card_body(
          style = "padding: 14px 20px; background-color: #ffffff; border-radius: 8px; border: 1px solid #E2E8F0; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); overflow: visible !important;",
          layout_columns(
            col_widths = c(4, 2, 2, 4),
            selectInput("analysis_prod", "Produit ciblé :", choices = produits_choices_grouped, selected = "Carotte", width = "100%"),
            selectInput("analysis_year", "Année :", choices = annee_choices, selected = "Toutes", width = "100%"),
            selectInput("analysis_organic", "Type de culture :", choices = c("Tous", "Bio", "Non-Bio"), selected = "Tous", width = "100%"),
            div(
              style = "display: flex; flex-direction: column; justify-content: center; height: 100%; padding-top: 10px;",
              p(style = "margin: 0; font-size: 0.85em; color: #64748B; line-height: 1.4;",
                tags$strong("Observatoire des structures de prix : "),
                "Comparaison multi-plateformes et audit du partitionnement gaussien GMM."
              )
            )
          )
        )
      ),
          layout_column_wrap(
            width = 1/2,
            card(
              card_header(h5("Comparaison des Prix Moyens par Plateforme", style = "margin:0; font-family:'Outfit';")),
              card_body(
                plotOutput("platform_compare_plot", height = "300px")
              )
            ),
            card(
              card_header(h5("Évolution Historique des Prix Moyens", style = "margin:0; font-family:'Outfit';")),
              card_body(
                plotOutput("history_trend_plot", height = "300px")
              )
            )
          ),
          br(),
          layout_column_wrap(
            width = 1/2,
            card(
              card_header(h5("Comparaison des Prix : Bio vs Non-Bio au niveau National", style = "margin:0; font-family:'Outfit';")),
              card_body(
                plotOutput("bio_compare_plot", height = "300px")
              )
            ),
            card(
              card_header(h5("Proportion de Bio par Plateforme au niveau National", style = "margin:0; font-family:'Outfit';")),
              card_body(
                plotOutput("bio_proportion_plot", height = "300px")
              )
            )
          ),
          br(),
          card(
            card_header(class = "bg-primary text-white d-flex justify-content-between align-items-center",
              h5("Décomposition en Gaussiennes sous-jacentes & Segmentation GMM", style = "margin:0; font-family:'Outfit'; font-weight:700;"),
              uiOutput("gmm_badge_k")
            ),
            card_body(
              fillable = FALSE, fill = FALSE,
              # Contrôles interactifs du modèle GMM
              layout_columns(
                col_widths = c(7, 5),
                radioButtons("gmm_view_mode", "Visualisation du modèle statistique :",
                             choices = c(
                               "Mélange théorique pondéré (Volume réel)" = "mix_gaussians",
                               "Profils des Gaussiennes (Aire = 1 par classe)" = "unit_gaussians",
                               "Densités empiriques observées" = "empirical",
                               "Bruit & Outliers filtrés (Classe 0)" = "outliers"
                             ),
                             selected = "mix_gaussians", inline = TRUE),
                div(style = "margin-top: 18px;",
                  conditionalPanel(
                    condition = "input.gmm_view_mode == 'mix_gaussians'",
                    checkboxInput("gmm_show_mix", "Densité globale du mélange (tirets)", value = TRUE)
                  ),
                  checkboxInput("gmm_show_means", "Repères des prix caractéristiques (€/kg)", value = TRUE),
                  checkboxInput("gmm_show_hist", "Superposer l'histogramme des ventes réelles", value = TRUE),
                  checkboxInput("gmm_show_outliers", "Repères des outliers rejetés (Classe 0)", value = TRUE)
                )
              ),
              hr(style = "margin: 8px 0 15px 0;"),
              
              # Graphique principal
              withSpinner(plotOutput("gmm_density_plot", height = "420px"), type = 8, color = "#2C3E50"),
              
              # Tableau synthétique des composantes gaussiennes
              div(
                style = "margin-top: 25px; margin-bottom: 25px; clear: both; width: 100%;",
                uiOutput("gmm_table_heading"),
                DTOutput("gmm_table_params", fill = FALSE)
              ),
              
              # Panneau d'audit sémantique et analyse des libellés par classe
              div(
                style = "margin-top: 20px; margin-bottom: 25px; clear: both; width: 100%;",
                uiOutput("gmm_semantic_panel")
              ),
              
              # Bloc explicatif
              accordion(
                open = FALSE,
                accordion_panel(
                  "Comprendre la décomposition en mélange gaussien (GMM)",
                  p(tags$strong("Pourquoi un mélange de Gaussiennes sur le logarithme des prix ?"), style = "margin-bottom:4px;"),
                  p("En circuits courts, un même produit recouvre plusieurs réalités commerciales : vente en gros volume (ex: filet de 10 kg), vente au détail standard, ou variétés rares et primeurs. Chaque sous-population forme une cloche normale sur l'échelle log. Le modèle GMM estime simultanément la part de marché (π), le prix caractéristique (exp(μ)) et la dispersion (σ) de chaque gamme sans imposer de seuil arbitraire."),
                  p(tags$strong("Filtrage du bruit (Classe 0) :"), " Une composante uniforme modélise le bruit de fond statistique (erreurs de frappe décimales, prix à la pièce au lieu du kg). Les transactions assignées à la classe 0 sont automatiquement isolées et écartées des analyses de l'observatoire.")
                )
              )
            )
          ),
          br(),
          card(
            card_header(h5("Interprétation des Classes GMM par les Covariables", style = "margin:0; font-family:'Outfit';")),
            card_body(
              selectInput("gmm_covar", "Analyser la composition des classes par :", 
                          choices = c("Type de culture (Bio/Non-Bio)" = "organic", "Plateforme" = "plateforme"), 
                          selected = "organic"),
              plotOutput("gmm_composition_plot", height = "280px")
            )
          )
        )
      ),
  
  # ---------- ONGLET 4 : INFLATION DES PRIX ----------
  nav_panel("Inflation des Prix", value = "nav_inflation",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Filtres d'Inflation", style = "font-family:'Outfit'; font-weight:bold; color:#2C3E50;"),
          hr(),
          selectInput("inflation_prod", "Produit ciblé :", choices = c(list("Indice Global (Tous produits)" = "Inflation Générale", "Panier Quotidien (Essentiels)" = "Panier Quotidien"), produits_choices_grouped), selected = "Inflation Générale"),
          selectInput("inflation_plat", "Plateforme :", choices = c("Toutes", "Plateforme 1", "Plateforme 2", "Plateforme 3", "Plateforme 4"), selected = "Toutes"),
          selectInput("inflation_year", "Année d'inflation :", choices = c("2022", "2023", "2024", "2025", "2026"), selected = "2024"),
          selectInput("inflation_mode_indice", "Formule de l'indice :", 
                      choices = c("Laspeyres (Pondération base N-1)" = "laspeyres", 
                                  "Paasche (Pondération année courante)" = "paasche", 
                                  "Fisher (Idéal / Moyenne géométrique)" = "fisher"), 
                      selected = "laspeyres"),
          radioButtons("inflation_view_type", "Granularité temporelle :", 
                       choices = c("Vue Annuelle" = "annuel", "Vue Mensuelle (Glissement 12 mois)" = "mensuel"), 
                       selected = "annuel", inline = FALSE),
          hr(),
          p(tags$small(strong("Méthodologie statistique :"), 
                       " L'indice agrège l'ensemble des 61 catégories suivies. Les produits sont pondérés de façon continue sans seuil arbitraire selon leur masse économique (Laspeyres, Paasche ou Fisher). La déclinaison mensuelle calcule un glissement annuel sur 12 mois pour neutraliser la saisonnalité des récoltes.")),
          width = 3
        ),
        mainPanel(
          # 1. BANDEAU DE CARTES KPI SYNTHÉTIQUES
          layout_column_wrap(
            width = 1/4,
            value_box(
              title = "Inflation Circuits Courts",
              value = textOutput("kpi_inf_cc"),
              p(textOutput("kpi_inf_cc_sub"), style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:3px;"),
              theme = "success"
            ),
            value_box(
              title = "Référence GMS INSEE",
              value = textOutput("kpi_inf_insee"),
              p(textOutput("kpi_inf_insee_sub"), style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:3px;"),
              theme = "primary"
            ),
            value_box(
              title = "Différentiel CC vs GMS",
              value = textOutput("kpi_inf_ecart"),
              p(textOutput("kpi_inf_ecart_sub"), style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:3px;"),
              theme = "warning"
            ),
            value_box(
              title = "Panier Quotidien CC",
              value = textOutput("kpi_inf_panier"),
              p(textOutput("kpi_inf_panier_sub"), style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:3px;"),
              theme = "danger"
            )
          ),
          br(),
          
          # 2. CONTENU CONDITIONNEL : VUE ANNUELLE
          conditionalPanel(
            condition = "input.inflation_view_type == 'annuel'",
            layout_column_wrap(
              width = 1/2,
              card(
                card_header(class = "bg-primary text-white", h5("Taux d'inflation annuel : Produit vs Marché Global", style = "margin:0; font-family:'Outfit';")),
                card_body(
                  plotOutput("inflation_plot", height = "440px")
                )
              ),
              card(
                card_header(class = "bg-success text-white", h5("Inflation des Produits du Panier Quotidien", style = "margin:0; font-family:'Outfit';")),
                card_body(
                  p(tags$em("Inflation annuelle détaillée pour les 33 catégories du panier de consommation courante."), style = "font-size:0.85em; color:#5D6D7E; margin-bottom:6px;"),
                  withSpinner(plotOutput("panier_inflation_plot", height = "440px"), type = 8, color = "#228833")
                )
              )
            ),
            br(),
            card(
              full_screen = TRUE,
              height = "750px",
              card_header(class = "bg-secondary text-white", h4("Carte de l'inflation des prix par département", style = "margin:0; font-family:'Outfit';")),
              card_body(
                class = "p-0",
                withSpinner(girafeOutput("inflation_map_plot", width = "100%", height = "100%"), type = 8, color = "#2C3E50")
              )
            ),
            br(),
            card(
              card_header(h4("Tableau détaillé de l'inflation par produit", style = "margin:0; font-family:'Outfit';")),
              card_body(
                DTOutput("table_inflation")
              )
            )
          ),
          
          # 3. CONTENU CONDITIONNEL : VUE MENSUELLE
          conditionalPanel(
            condition = "input.inflation_view_type == 'mensuel'",
            card(
              card_header(class = "bg-primary text-white", h5("Évolution temporelle mensuelle et glissement annuel sur 12 mois", style = "margin:0; font-family:'Outfit';")),
              card_body(
                withSpinner(plotOutput("inflation_monthly_plot", height = "420px"), type = 8, color = "#2C3E50")
              )
            ),
            br(),
            card(
              card_header(h4("Tableau des séries mensuelles et inflation glissante", style = "margin:0; font-family:'Outfit';")),
              card_body(
                DTOutput("table_inflation_monthly")
              )
            )
          ),
          width = 9
        )
      )
    )
  ),
  
  # ---------- ONGLET 5 : ANALYSES DE TRANSITION RA 2020 ----------
  nav_panel("Corrélation avec circuits courts CC",
    fluidPage(
      # Bandeau supérieur compact de filtres horizontaux (Top Bar)
      card(
        class = "card-overflow-visible",
        card_body(
          style = "padding: 14px 20px; background-color: #ffffff; border-radius: 8px; border: 1px solid #E2E8F0; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); overflow: visible !important;",
          layout_columns(
            col_widths = c(4, 3, 3, 2),
            selectInput("ra_x_var", "Variable Agricole (Axe X) :",
                        choices = c(
                          "Part des exploitations en Circuits Courts (%)" = "Pct_Exploitations_CC",
                          "Part de la SAU en Circuits Courts (%)" = "Pct_SAU_CC",
                          "Part des exploitations en Bio (%)" = "Pct_Exploitations_Bio",
                          "Part de la SAU en Bio (%)" = "Pct_SAU_Bio",
                          "Part des exploitations Bio + CC (%)" = "Pct_Exploitations_Bio_CC",
                          "Nombre total d'exploitations" = "TotalEA",
                          "SAU totale (ha)" = "TotalSAU",
                          "Emploi total (ETP)" = "TotalETP"
                        ),
                        selected = "Pct_Exploitations_CC", width = "100%"),
            selectInput("ra_y_var", "Variable Observatoire (Axe Y) :",
                        choices = c(
                          "Prix Moyen du Produit ciblé (€/kg)" = "Prix_Moyen_Kg",
                          "Indice Général des Prix (Tous produits)" = "Indice_Prix_Global",
                          "Nombre de ventes" = "Nombre_Ventes"
                        ),
                        selected = "Prix_Moyen_Kg", width = "100%"),
            conditionalPanel(
              condition = "input.ra_y_var == 'Prix_Moyen_Kg'",
              selectInput("ra_prod", "Produit ciblé :", choices = produits_choices_grouped, selected = "Carotte", width = "100%")
            ),
            selectInput("ra_year", "Année :", choices = annee_choices, selected = "Toutes", width = "100%")
          ),
          layout_columns(
            col_widths = c(3, 2, 2, 5),
            selectInput("ra_plat", "Plateforme :", choices = c("Toutes", "Plateforme 1", "Plateforme 2", "Plateforme 3", "Plateforme 4"), selected = "Toutes", width = "100%"),
            selectInput("ra_organic", "Type de culture :", choices = c("Tous", "Bio", "Non-Bio"), selected = "Tous", width = "100%"),
            numericInput("ra_min_sales", "Min. ventes/dép. :", value = 15, min = 1, step = 1, width = "100%"),
            div(
              style = "padding-top: 22px;",
              HTML("<details style='cursor: pointer; background-color: #F8FAFC; padding: 6px 10px; border-left: 3px solid #3B82F6; border-radius: 4px;'>
                <summary style='color: #1E40AF; font-weight: 600; font-size: 0.85em;'>Définitions variables RA 2020</summary>
                <div style='font-size: 0.80em; color: #475569; margin-top: 5px; line-height: 1.3;'>
                  • <strong>Part CC (%)</strong> : % des fermes vendant en direct ou 1 intermédiaire.<br/>
                  • <strong>Part SAU CC (%)</strong> : % de la Surface Agricole Utile en circuits courts.<br/>
                  • <strong>Part Bio (%)</strong> : % des fermes certifiées AB.<br/>
                  • <strong>Bio + CC (%)</strong> : Fermes cumulant Bio et circuits courts.
                </div>
              </details>")
            )
          )
        )
      ),
      # Message d'avertissement de source prominent
      div(style = "margin-bottom: 20px; padding: 12px 15px; background-color: #EBF5FB; border-left: 4px solid #3498DB; border-radius: 4px; font-size: 0.92em; color: #2C3E50; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
          h6(style = "margin: 0 0 5px 0; font-weight: bold; color: #21618C; font-family:'Outfit';", "Source des données agricoles"),
          "Les indicateurs de structure agricole (axe X et Carte 1) proviennent du ",
          tags$strong("Recensement Agricole 2020"),
          " (Ministère de l'Agriculture et de la Souveraineté Alimentaire) à l'échelle départementale."
      ),
      layout_column_wrap(
        width = 1,
        card(
          card_header(class = "bg-primary text-white", h5("Régression et Corrélation Statistique", style = "margin:0; font-family:'Outfit';")),
          card_body(
            plotOutput("ra_scatter_plot", height = "400px"),
            htmlOutput("ra_stat_summary")
          )
        )
      ),
      br(),
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          height = "550px",
          card_header(class = "bg-secondary text-white", h5("Carte 1 : Recensement Agricole Données 2020", style = "margin:0; font-family:'Outfit';")),
          card_body(
            class = "p-0",
            withSpinner(girafeOutput("ra_map_x", width = "100%", height = "100%"), type = 8, color = "#2C3E50")
          )
        ),
        card(
          full_screen = TRUE,
          height = "550px",
          card_header(class = "bg-secondary text-white", h5("Carte 2 : Indicateurs Observatoire des Prix", style = "margin:0; font-family:'Outfit';")),
          card_body(
            class = "p-0",
            withSpinner(girafeOutput("ra_map_y", width = "100%", height = "100%"), type = 8, color = "#2C3E50")
          )
        )
      ),
      br(),
      card(
        card_header(h4("Données détaillées par département", style = "margin:0; font-family:'Outfit';")),
        card_body(
          DTOutput("ra_table_depts")
        )
      )
    )
  ),
  
  # ---------- ONGLET 6 : ANALYSES APPROFONDIES (STATS) ----------
  nav_panel("Analyses Approfondies Statistiques",
    navset_card_tab(
      title = "Modélisations Statistiques Avancées",
      
      # SOUS-ONGLET 1 : SAISONNALITÉ
      nav_panel("Saisonnalité & Séries Temporelles",
        fluidPage(
          # Bandeau supérieur compact de sélection (Top Bar)
          card(
            class = "card-overflow-visible",
            card_body(
              style = "padding: 14px 20px; background-color: #ffffff; border-radius: 8px; border: 1px solid #E2E8F0; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); overflow: visible !important;",
              layout_columns(
                col_widths = c(4, 3, 5),
                selectInput("season_prod", "Choisir un produit :", choices = produits_choices_grouped, selected = "Carotte", width = "100%"),
                div(
                  style = "display: flex; flex-direction: column; justify-content: center; height: 100%; padding-top: 15px;",
                  uiOutput("season_intensity_ui")
                ),
                div(
                  style = "font-size: 0.85em; color: #64748B; line-height: 1.4; display: flex; flex-direction: column; justify-content: center; height: 100%;",
                  p(style = "margin: 0 0 3px 0;", tags$strong("Décomposition STL :"), " Tendance de fond (trend), cycle saisonnier (seasonal) et résidu (remainder)."),
                  p(style = "margin: 0;", tags$strong("Modélisation SARIMA :"), " Dépendance temporelle et prévisions à 6 mois avec bandes de confiance.")
                )
              )
            )
          ),
          card(
            card_header(class = "bg-primary text-white", h4("Tableau Synthétique de Saisonnalité au niveau National", style = "margin:0; font-family:'Outfit';")),
            card_body(
              p("Le tableau ci-dessous présente l'amplitude saisonnière, les mois de pic et de creux, ainsi que la pente de la tendance.", style = "font-size:0.9em; color:#5D6D7E; margin-bottom:15px;"),
              DTOutput("table_synthese_saison")
            )
          ),
          br(),
          card(
            card_header(class = "bg-secondary text-white", h4("Graphique 1 : Décomposition Temporelle STL", style = "margin:0; font-family:'Outfit';")),
            card_body(
              plotOutput("stl_plot", height = "500px")
            )
          ),
          br(),
          card(
            card_header(class = "bg-secondary text-white", h4("Graphique 2 : Profil Saisonnier Annuel Moyen", style = "margin:0; font-family:'Outfit';")),
            card_body(
              plotOutput("seasonal_profile_plot", height = "350px")
            )
          ),
          br(),
          card(
            card_header(class = "bg-secondary text-white", h4("Graphique 3 : Prévisions et Intervalles de Confiance SARIMA", style = "margin:0; font-family:'Outfit';")),
            card_body(
              plotOutput("sarima_plot", height = "400px")
            )
          )
        )
      ),
      
      # SOUS-ONGLET 2 : MODÈLE MIXTE
      nav_panel("Modèle Mixte & Effets Plateformes",
        fluidPage(
          # Bandeau supérieur compact de synthèse économique (Top Bar)
          card(
            class = "card-overflow-visible",
            card_body(
              style = "padding: 14px 20px; background-color: #ffffff; border-radius: 8px; border: 1px solid #E2E8F0; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); overflow: visible !important;",
              layout_columns(
                col_widths = c(4, 5, 3),
                div(
                  h5("Spécification Économétrique Mixte", style = "font-family:'Outfit'; font-weight:bold; color:#2C3E50; margin:0 0 6px 0;"),
                  p(HTML("<b>log(prix) = μ + α<sub>catégorie</sub> + δ<sub>plateforme</sub> + γ<sub>bio</sub> + θ<sub>conditionnement</sub> + U<sub>département</sub> + ε</b>"), 
                    style = "font-size:0.86em; color:#1E3A8A; background:#EFF6FF; padding:8px 12px; border-radius:6px; border:1px solid #BFDBFE; margin:0;")
                ),
                div(
                  h6("Résultats Économiques Clés :", style = "color:#2C3E50; font-weight:bold; margin:0 0 4px 0; font-size:0.88em;"),
                  tags$ul(style = "font-size:0.82em; color:#5D6D7E; margin:0; padding-left:15px;",
                    tags$li(strong("Surcoût Bio :"), sprintf(" %s de prime moyenne pour le label biologique (%s pour le conventionnel).", mm_prime_bio_str, mm_non_bio_str)),
                    tags$li(strong("Effet de gros :"), sprintf(" %s (1-3 kg) et %s (> 3 kg) vs petit format.", mm_poids_moyen_str, mm_poids_gros_str)),
                    tags$li(strong("Indice Territorial IPT :"), sprintf(" Disparités spatiales pures %s épurées de l'effet panier.", mm_ipt_str))
                  )
                ),
                div(
                  style = "display: flex; flex-direction: column; justify-content: center; height: 100%; font-size: 0.81em; color: #2C3E50; line-height: 1.4;",
                  p(style = "margin: 0 0 2px 0;", strong("Observations : "), mm_n_obs_str, " (", mm_n_dept_str, " départements, ", mm_n_cat_str, " catégories)"),
                  p(style = "margin: 0 0 2px 0;", strong("Ventes totales : "), mm_total_ventes_str),
                  p(style = "margin: 0;", strong("Variance résiduelle : "), mm_sigma2_resid_str, " | ", strong("ICC : "), span(mm_icc_str, style = "color:#E74C3C; font-weight:bold;"))
                )
              )
            )
          ),
          # Carte 1 en premier : Indice de Prix Territorial (IPT)
          card(
            card_header(class = "bg-primary text-white", h4("Effets Aléatoires : Variabilité Inter-Départementale et Indice de Prix Territorial IPT", style = "margin:0; font-family:'Outfit';")),
            card_body(
              p("Le modèle linéaire mixte estime pour chaque département un effet aléatoire (BLUP) représentant la disparité spatiale pure des prix, après contrôle complet du panier de produits, du label bio, de la plateforme et du conditionnement.", style = "font-size:0.9em; color:#5D6D7E;"),
              tabsetPanel(
                type = "tabs",
                nav_panel("Carte Spatiale des Effets Départementaux IPT",
                  p("Visualisation géographique des BLUPs départementaux (en % d'écart par rapport au niveau de référence national) :", style = "font-size:0.88em; color:#7F8C8D; margin-top:10px;"),
                  withSpinner(girafeOutput("mm_map_blup", width = "100%", height = "650px"), type = 8, color = "#2C3E50")
                ),
                nav_panel("Caterpillar Plot et Intervalles de Confiance",
                  p("Ce « caterpillar plot » classe l'ensemble des départements selon leur Indice de Prix Territorial pur, avec barres d'incertitude :", style = "font-size:0.88em; color:#7F8C8D; margin-top:10px;"),
                  imageOutput("mm_caterpillar_img", height = "auto")
                ),
                nav_panel("Territoires d'Outre-Mer DOM-TOM",
                  p("Indice des Prix Territoriaux (IPT pur) et indicateurs pour les départements et territoires ultramarins :", style = "font-size:0.88em; color:#7F8C8D; margin-top:10px;"),
                  DTOutput("mm_table_dom")
                )
              )
            )
          ),
          br(),
          layout_column_wrap(
            width = 1/2,
            card(
              card_header(class = "bg-secondary text-white", h4("Effets Fixes Économiques Estimés", style = "margin:0; font-family:'Outfit';")),
              card_body(
                p("Coefficients du modèle optimal (en pourcentage d'impact relatif sur le prix au kilo) :", style = "font-size:0.9em; color:#5D6D7E;"),
                DTOutput("mm_table_effets_fixes")
              )
            ),
            card(
              card_header(class = "bg-secondary text-white", h4("Effets Plateformes Épurés", style = "margin:0; font-family:'Outfit';")),
              card_body(
                imageOutput("mm_effets_plat_img", height = "auto")
              )
            )
          ),
          br(),
          card(
            card_header(class = "bg-secondary text-white", h4("Interaction Plateforme × Catégorie de Produit", style = "margin:0; font-family:'Outfit';")),
            card_body(
              p("Écart de prix d'une plateforme par rapport au prix moyen national de chaque produit :", style = "font-size:0.9em; color:#5D6D7E;"),
              imageOutput("mm_heatmap_img", height = "auto")
            )
          ),
          br(),
          card(
            card_header(class = "bg-secondary text-white", h4("Comparaison des Spécifications Économétriques : Critères AIC et BIC", style = "margin:0; font-family:'Outfit';")),
            card_body(
              p("Le tableau ci-dessous compare la qualité d'ajustement statistique entre la spécification brute et les enrichissements successifs (label bio et effet de conditionnement discret vs continu). Le Modèle A avec tranches discrètes réduit l'AIC de plus de 7 300 points et diminue la variance résiduelle de 30 %.", style = "font-size:0.9em; color:#5D6D7E;"),
              DTOutput("mm_table_comparaison")
            )
          ),
              br(),
              card(
                card_header(class = "bg-primary text-white", h4("Diagnostics des Résidus : Loi Normale vs Loi de Student-t à Queues Lourdes", style = "margin:0; font-family:'Outfit';")),
                card_body(
                  p(HTML(sprintf("<b>Examen de la normalité des résidus :</b> Le diagramme Q-Q sous loi normale présente une courbure en « S » caractéristique de <i>queues de distribution épaisses (leptokurticité)</i>. L'ajustement d'une <b>loi de Student-t</b> avec <b>ν = %s degrés de liberté</b> améliore le critère <b>AIC de %s points</b> et aligne les quantiles observés sur la première bissectrice y = x.", mm_nu_str, mm_delta_aic_stud_str)), style = "font-size:0.95em; color:#2C3E50;"),
                  layout_column_wrap(
                    width = 1/2,
                    card(
                      card_header(class = "bg-secondary text-white", h5(sprintf("Comparaison Statistique sur %s résidus", mm_n_res_str), style = "margin:0; font-size:1em;")),
                      card_body(
                        tags$table(class = "table table-bordered table-sm", style = "font-size:0.88em; margin-top:5px;",
                          tags$thead(class = "table-light",
                            tags$tr(tags$th("Hypothèse"), tags$th("Paramètres"), tags$th("Log-Lik"), tags$th("AIC"), tags$th("Q-Q Plot"))
                          ),
                          tags$tbody(
                            tags$tr(
                              tags$td(strong("Loi Normale")),
                              tags$td(mm_norm_params_str),
                              tags$td(mm_norm_ll_str),
                              tags$td(mm_norm_aic_str),
                              tags$td(span("Déviation en S", style = "color:#E74C3C; font-weight:bold;"))
                            ),
                            tags$tr(style = "background-color:#E8F8F5;",
                              tags$td(strong("Loi de Student-t")),
                              tags$td(strong(mm_stud_params_str)),
                              tags$td(strong(mm_stud_ll_str)),
                              tags$td(strong(mm_stud_aic_str)),
                              tags$td(span("Alignement y=x", style = "color:#27AE60; font-weight:bold;"))
                            )
                          )
                        ),
                        p(tags$small(style = "color:#7F8C8D; font-style:italic;", sprintf("Note : Comme ν = %s > 2 et E[ε] = 0, les estimateurs REML et les indices IPT restent non biaisés et convergents.", mm_nu_str)))
                      )
                    ),
                    card(
                      card_header(class = "bg-secondary text-white", h5("Interprétation Économique & Statistique", style = "margin:0; font-size:1em;")),
                      card_body(
                        tags$ul(style = "font-size:0.88em; color:#34495E; padding-left:15px;",
                          tags$li(strong("Sur les données :"), " Les prix en circuits courts présentent des chocs d'offre et produits d'exception générant des queues plus lourdes que la gaussienne."),
                          tags$li(strong("Sur le modèle :"), " Le Théorème Central Limite valide l'estimation macro, tandis que la loi de Student confère une pondération robuste sans filtrage arbitraire.")
                        )
                      )
                    )
                  ),
                  br(),
                  navset_card_tab(
                    title = "Visualisation Détaillée des Diagnostics",
                    full_screen = TRUE,
                    nav_panel("Synthèse Complète & Densités",
                      imageOutput("mm_diagnostic_student_img", height = "auto")
                    ),
                    nav_panel("Comparaison Q-Q Plots Normal vs Student",
                      layout_column_wrap(
                        width = 1/2,
                        card(
                          card_header(class = "bg-secondary text-white", h6("Q-Q Plot — Loi Normale", style = "margin:0;")),
                          imageOutput("mm_qq_norm_img", height = "auto")
                        ),
                        card(
                          card_header(class = "bg-secondary text-white", h6(sprintf("Q-Q Plot — Loi de Student — ν = %s", mm_nu_str), style = "margin:0;")),
                          imageOutput("mm_qq_stud_img", height = "auto")
                        )
                      )
                    ),
                    nav_panel("Résidus vs Valeurs Ajustées",
                      layout_column_wrap(
                        width = 1/2,
                        card(
                          card_header(class = "bg-secondary text-white", h6("Résidus vs Ajustées — Modèle Gaussien", style = "margin:0;")),
                          imageOutput("mm_resid_norm_img", height = "auto")
                        ),
                        card(
                          card_header(class = "bg-secondary text-white", h6(sprintf("Résidus vs Ajustées — Modèle Student — ν = %s", mm_nu_str), style = "margin:0;")),
                          imageOutput("mm_resid_stud_img", height = "auto")
                        )
                      )
                    )
                  )
                )
              )
        )
      ),
      
      # SOUS-ONGLET 3 : NLP CARACTERISTIQUES
      nav_panel("Impact des Caractéristiques Textuelles NLP",
        fluidPage(
          # Bandeau horizontal supérieur de sélection et méthodologie NLP
          card(
            class = "card-overflow-visible",
            card_body(
              style = "padding: 14px 20px; background-color: #ffffff; border-radius: 8px; border: 1px solid #E2E8F0; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); overflow: visible !important;",
              layout_columns(
                col_widths = c(3, 5, 4),
                div(
                  selectInput("nlp_prod", "Catégorie à analyser :", choices = c("Pomme de terre", "Porc", "Bœuf", "Volaille", "Tomate", "Carotte", "Salade"), selected = "Porc", width = "100%")
                ),
                div(
                  h6("Extraction NLP des Caractéristiques :", style = "color:#2C3E50; font-weight:bold; margin:0 0 4px 0; font-size:0.88em;"),
                  p("Analyse textuelle des libellés des producteurs pour isoler les ", strong("variétés"), " (ex: Agata, Bintje), ", strong("morceaux"), " (ex: Filet, Côte) et ", strong("conditionnements"), " (ex: Vrac, Filet).", 
                    style = "font-size:0.82em; color:#5D6D7E; margin:0;")
                ),
                div(
                  h6("Méthodologie Économétrique :", style = "color:#2C3E50; font-weight:bold; margin:0 0 4px 0; font-size:0.88em;"),
                  p("Modèle linéaire hedonic estimant la prime ou décote (en %) de chaque modalité textuelle par rapport au produit de référence standard.",
                    style = "font-size:0.82em; color:#5D6D7E; margin:0;")
                )
              )
            )
          ),
          card(
            card_header(class = "bg-primary text-white", h4("Impact des Caractéristiques Textuelles sur le Prix", style = "margin:0; font-family:'Outfit';")),
            card_body(
              plotOutput("nlp_impact_plot", height = "500px")
            )
          ),
          br(),
          card(
            card_header(class = "bg-secondary text-white", h4("Détail des Coefficients Modèle NLP", style = "margin:0; font-family:'Outfit';")),
            card_body(
              DTOutput("nlp_coefs_table")
            )
          )
        )
      )
    )
  ),
  
  # ---------- ONGLET 7 : CONTEXTE SOCIO-ÉCONOMIQUE ----------
  nav_panel("Contexte Socio-Économique",
    navset_card_tab(
      title = "Démographie et Ventes en Circuits Courts",
      
      # SOUS-ONGLET 1 : PROFIL DÉMOGRAPHIQUE
      nav_panel("Profil Démographique & Prix CC",
        fluidPage(
          # Bandeau supérieur compact de filtres et sélection socio-économique
          card(
            class = "card-overflow-visible",
            card_body(
              style = "padding: 14px 20px; background-color: #ffffff; border-radius: 8px; border: 1px solid #E2E8F0; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); overflow: visible !important;",
              layout_columns(
                col_widths = c(4, 4, 4),
                div(
                  selectInput("socio_var_x", "Variable Socio-Économique (X) :", 
                              choices = list(
                                "Revenus & Niveau de vie" = c("Revenu médian (€)" = "revenu_median_uc", "Taux de pauvreté (%)" = "taux_pauvrete_60", "Indice de Gini" = "indice_gini", "% Ménages imposés" = "pt_menimp", "Écart interquartile relatif" = "ecart_interquartile_relatif"),
                                "Démographie & Âge" = c("Part des 65 ans et + (%)" = "part_65plus", "Part des - de 20 ans (%)" = "part_moins20", "Indice de vieillissement" = "indice_vieillissement_2020"),
                                "Éducation & CSP" = c("% Diplômés supérieur" = "p_diplsup15", "% Cadres & Prof. Int." = "p_csp_cadpis", "% Agriculteurs" = "p_csp_agr"),
                                "Emploi" = c("Taux d'emploi 20-64 ans" = "emp2064", "Taux d'activité 15-64 ans" = "tx_act1564", "Part revenus chômage (%)" = "part_indemnites_chomage", "Part pensions/retraites (%)" = "part_pensions_retraites"),
                                "Agriculture" = c("% Exploitations CC" = "Pct_Exploitations_CC", "% Exploitations Bio" = "Pct_Exploitations_Bio", "% SAU CC" = "Pct_SAU_CC", "SAU engagée bio (ha)" = "surfeng")
                              ), 
                              selected = "revenu_median_uc", width = "100%"),
                  uiOutput("socio_var_x_desc")
                ),
                div(
                  selectInput("socio_var_y", "Indicateur Circuits Courts (Y) :", 
                              choices = c("Indice Français des Prix en Circuits Courts" = "pct_effet", "Prix Moyen (€/kg)" = "Prix_Moyen_Kg", "Nombre de Ventes" = "Nombre_Ventes", "Chiffre d'Affaires Total" = "Total_Revenue"), 
                              selected = "pct_effet", width = "100%")
                ),
                div(
                  sliderInput("socio_min_ventes", "Seuil min. de ventes / département :", min = 0, max = 500, value = 10, step = 10, width = "100%")
                )
              ),
              conditionalPanel(
                condition = "input.socio_var_y != 'pct_effet'",
                hr(style = "margin: 12px 0;"),
                layout_columns(
                  col_widths = c(3, 3, 3, 3),
                  selectInput("socio_produit", "Produit :", choices = c("Tous"), width = "100%"),
                  selectInput("socio_plateforme", "Plateforme :", choices = c("Toutes", "Plateforme 4", "Plateforme 3", "Plateforme 2", "Plateforme 1"), selected = "Toutes", width = "100%"),
                  selectInput("socio_annee", "Année :", choices = annee_choices, selected = "Toutes", width = "100%"),
                  selectInput("socio_bio", "Bio / Non-Bio :", choices = c("Tous", "Bio", "Non-Bio"), selected = "Tous", width = "100%")
                )
              )
            )
          ),
              layout_columns(
                col_widths = c(4, 4, 4),
                value_box(title = "Départements analysés", value = textOutput("socio_nb_depts"), showcase = bsicons::bs_icon("map"), theme = "primary"),
                value_box(title = "Corrélation de Pearson", value = textOutput("socio_cor_pearson"), showcase = bsicons::bs_icon("graph-up"), theme = "info"),
                value_box(title = "P-value", value = textOutput("socio_pvalue"), showcase = bsicons::bs_icon("calculator"), theme = "secondary")
              ),
              br(),
              card(
                card_header(class = "bg-primary text-white", h4("Corrélation : Socio-Éco vs Circuits Courts", style = "margin:0; font-family:'Outfit';")),
                card_body(
                  plotOutput("socio_scatter_plot", height = "400px")
                )
              ),
              br(),
              tags$head(
                tags$style(HTML("
                  .shiny-spinner-output-container { height: 100% !important; min-height: 600px; }
                  .shiny-plot-output img { object-fit: contain !important; }
                  .hide-plots-temp .shiny-plot-output { visibility: hidden !important; transition: none !important; }
                ")),
                tags$script(HTML("
                  $(document).ready(function() {
                    // Observe les changements de plein écran sur les cartes
                    var observer = new MutationObserver(function(mutations) {
                      mutations.forEach(function(mutation) {
                        if (mutation.attributeName === 'data-full-screen') {
                          // Étape 1 : on masque la carte IMMÉDIATEMENT
                          $(mutation.target).addClass('hide-plots-temp');
                        }
                      });
                    });
                    
                    $('.card').each(function() {
                      observer.observe(this, { attributes: true });
                    });
                    
                    // Étape 2 : Dès que Shiny a fini de tout recalculer (le serveur a renvoyé l'image finale)
                    // on fait réapparaître la carte
                    $(document).on('shiny:idle', function() {
                      $('.hide-plots-temp').removeClass('hide-plots-temp');
                    });
                  });
                "))
              ),
              tabsetPanel(
                tabPanel("Carte Socio-Éco (X)",
                  br(),
                  card(
                    full_screen = TRUE,
                    height = "750px",
                    card_header(class = "bg-secondary text-white", h4(textOutput("socio_map_x_title"), style = "margin:0; font-size:1.1em; font-family:'Outfit';")),
                    card_body(
                      class = "p-0",
                      withSpinner(girafeOutput("socio_map_x", width = "100%", height = "100%"), type = 8, color = "#2C3E50")
                    )
                  )
                ),
                tabPanel("Carte Circuits Courts (Y)",
                  br(),
                  card(
                    full_screen = TRUE,
                    height = "750px",
                    card_header(class = "bg-secondary text-white", h4(textOutput("socio_map_y_title"), style = "margin:0; font-size:1.1em; font-family:'Outfit';")),
                    card_body(
                      class = "p-0",
                      withSpinner(girafeOutput("socio_map_y", width = "100%", height = "100%"), type = 8, color = "#2C3E50")
                    )
                  )
                ),
                tabPanel("Croisement (Carte des Écarts)",
                  br(),
                  card(
                    full_screen = TRUE,
                    height = "750px",
                    card_header(class = "bg-info text-white", h4("Écarts à la tendance et résidus de la régression", style = "margin:0; font-size:1.1em; font-family:'Outfit';")),
                    card_body(
                      class = "p-0",
                      p("Cette carte permet de croiser les deux indicateurs en visualisant les exceptions : en rouge, les départements où la valeur des Circuits Courts est plus élevée que ce que l'indicateur Socio-Économique laissait prédire ; en bleu, ceux où elle est plus faible.", style = "padding: 15px; margin-bottom: 0;"),
                      withSpinner(girafeOutput("socio_map_resid", width = "100%", height = "100%"), type = 8, color = "#2C3E50")
                    )
                  )
                )
              ),
              br(),
              card(
                card_header(class = "bg-dark text-white", h4("Données Détaillées par Département", style = "margin:0; font-family:'Outfit';")),
                card_body(
                  DTOutput("socio_table")
                )
              )
        )
      ),
      
      # SOUS-ONGLET 2 : MAGASINS DE PRODUCTEURS
      nav_panel("Magasins de Producteurs",
        fluidPage(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            value_box(title = "Magasins répertoriés", value = textOutput("magasins_total"), showcase = bsicons::bs_icon("shop"), theme = "primary"),
            value_box(title = "Magasins actifs", value = textOutput("magasins_actifs"), showcase = bsicons::bs_icon("check-circle"), theme = "success"),
            value_box(title = "Magasins fermés", value = textOutput("magasins_fermes"), showcase = bsicons::bs_icon("x-circle"), theme = "danger"),
            value_box(title = "Taux de fermeture", value = textOutput("magasins_taux_fermeture"), showcase = bsicons::bs_icon("percent"), theme = "warning")
          ),
          br(),
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(class = "bg-primary text-white", h4("Cartographie des Magasins", style = "margin:0; font-family:'Outfit';")),
              card_body(
                withSpinner(girafeOutput("magasins_map", width = "100%", height = "500px"), type = 8, color = "#2C3E50")
              )
            ),
            card(
              card_header(class = "bg-secondary text-white", h4("Dynamique Ouvertures / Fermetures", style = "margin:0; font-family:'Outfit';")),
              card_body(
                withSpinner(plotOutput("magasins_timeline", height = "500px"), type = 8, color = "#2C3E50")
              )
            )
          ),
          br(),
          card(
            card_header(class = "bg-dark text-white", h4("Données Détaillées des Magasins", style = "margin:0; font-family:'Outfit';")),
            card_body(
              DTOutput("magasins_table")
            )
          )
        )
      ),

      # SOUS-ONGLET 3 : HYPOTHESES & CONCLUSIONS
      nav_panel("Hypothèses & Conclusions",
        fluidPage(
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(class = "bg-primary text-white", h4("Axe 1 : Prix CC et contexte socio-économique", style = "margin:0;")),
          card_body(
            accordion(
              open = FALSE,
              accordion_panel(
                "H1 & H2 : Les territoires avec plus de diplômés du supérieur et de cadres ont des prix CC plus hauts",
                p(tags$small(tags$em("Hypothèse nulle (H0) : La part de diplômés ou de cadres n'est pas corrélée avec le niveau des prix en circuits courts.")), style="color:#7F8C8D; margin-bottom:10px;"),
                p(span("Confirmé.", class = "hyp-true"), " On observe une corrélation positive très significative. Ces populations ont un pouvoir d'achat supérieur et une plus forte propension à payer une prime pour la qualité."),
                layout_columns(
                  col_widths = c(6, 6),
                  withSpinner(plotOutput("plot_h1", height="250px"), type=8, color="#2C3E50"),
                  withSpinner(plotOutput("plot_h2", height="250px"), type=8, color="#2C3E50")
                )
              ),
              accordion_panel(
                "H3 : Plus de revenus médians → prix CC plus hauts",
                p(tags$small(tags$em("Hypothèse nulle (H0) : Le niveau de revenu médian du département n'est pas corrélé avec les prix pratiqués en circuits courts.")), style="color:#7F8C8D; margin-bottom:10px;"),
                p(span("Très fortement confirmé.", class = "hyp-true"), " Le revenu médian reste le meilleur prédicteur du niveau de prix en circuit court (effet pur du pouvoir d'achat territorial)."),
                withSpinner(plotOutput("plot_h3", height="250px"), type=8, color="#2C3E50")
              ),
              accordion_panel(
                "H4 : Les prix en CC sont plus élevés en zone urbaine selon la densité",
                p(tags$small(tags$em("Hypothèse nulle (H0) : Le type de densité (urbain/rural) n'est pas associé à des différences de prix en circuits courts.")), style="color:#7F8C8D; margin-bottom:10px;"),
                p(span("Non significatif / Nuancé.", class = "hyp-mix"), " Une fois qu'on a isolé l'effet pur du département (modèle mixte) et qu'on prend en compte le revenu (H3), le simple fait d'être urbain ou rural ne suffit plus à créer une différence statistiquement significative (p = 0.051)."),
                withSpinner(plotOutput("plot_h4", height="250px"), type=8, color="#2C3E50")
              ),
              accordion_panel(
                "H5 : Plus d'agriculture bio locale → prix CC plus hauts",
                p(tags$small(tags$em("Hypothèse nulle (H0) : La concentration d'exploitations biologiques locales n'est pas corrélée au niveau des prix en CC.")), style="color:#7F8C8D; margin-bottom:10px;"),
                p(span("Hypothèse rejetée.", class = "hyp-false"), " L'analyse rigoureuse montre aucune corrélation (R proche de 0, p-value très élevée). La prime de prix dépend de la demande et non de l'offre bio locale (qui pourrait même jouer à la baisse par effet de concurrence)."),
                withSpinner(plotOutput("plot_h5", height="250px"), type=8, color="#2C3E50")
              ),
              accordion_panel(
                "H6 : Plus de pauvreté → moins de ventes CC",
                p(tags$small(tags$em("Hypothèse nulle (H0) : Le taux de pauvreté n'a aucune corrélation avec le volume global de ventes en circuits courts.")), style="color:#7F8C8D; margin-bottom:10px;"),
                p(span("Confirmé.", class = "hyp-true"), " On observe une corrélation négative nette : les départements avec un taux de pauvreté plus élevé génèrent des volumes de ventes en circuits courts plus faibles sur les plateformes."),
                withSpinner(plotOutput("plot_h6", height="250px"), type=8, color="#2C3E50")
              )
            )
          )
        ),
        card(
          card_header(class = "bg-primary text-white", h4("Axe 2 : Démographie, âge et dynamiques", style = "margin:0;")),
          card_body(
            accordion(
              open = FALSE,
              accordion_panel(
                "H7 : Les départements plus vieillissants achètent plus en CC",
                p(tags$small(tags$em("Hypothèse nulle (H0) : L'âge de la population (part des 65+) n'est pas corrélé à la propension à acheter en circuits courts.")), style="color:#7F8C8D; margin-bottom:10px;"),
                p(span("Hypothèse invalidée.", class = "hyp-false"), " On observe une corrélation négative marquée : les départements les plus âgés affichent un volume de transactions plus réduit sur les circuits dématérialisés, reflétant notamment une moindre pénétration des outils numériques chez les seniors."),
                withSpinner(plotOutput("plot_h7", height="250px"), type=8, color="#2C3E50")
              ),
              accordion_panel(
                "H8 : Les départements jeunes achètent plus / différemment",
                p(tags$small(tags$em("Hypothèse nulle (H0) : Il n'y a aucune corrélation entre la part de jeunes (- de 20 ans) et les volumes d'achats.")), style="color:#7F8C8D; margin-bottom:10px;"),
                p(span("Confirmé.", class = "hyp-true"), " La corrélation est positive et statistiquement significative : les territoires comptant une proportion plus élevée de ménages jeunes et d'actifs génèrent des volumes totaux plus soutenus."),
                withSpinner(plotOutput("plot_h8", height="250px"), type=8, color="#2C3E50")
              ),
              accordion_panel(
                "H9 : Le taux de chômage influence les volumes CC",
                p(tags$small(tags$em("Hypothèse nulle (H0) : Le chômage et la précarité de l'emploi ne sont pas corrélés au volume des achats en circuits courts.")), style="color:#7F8C8D; margin-bottom:10px;"),
                p(span("Hypothèse rejetée.", class = "hyp-false"), " La corrélation est quasi-nulle et non significative : contrairement à la pauvreté structurelle, le niveau d'indemnités de chômage n'a pas d'effet directeur propre sur les volumes globaux."),
                withSpinner(plotOutput("plot_h9", height="250px"), type=8, color="#2C3E50")
              ),
              accordion_panel(
                "H10 : Les inégalités de revenus influencent la diversité des prix CC",
                p(tags$small(tags$em("Hypothèse nulle (H0) : Les inégalités (Indice de Gini) ne sont pas corrélées à la dispersion (l'écart-type) des prix en circuits courts.")), style="color:#7F8C8D; margin-bottom:10px;"),
                p(span("Hypothèse non confirmée.", class = "hyp-mix"), " Contrairement à l'attente initiale, la corrélation linéaire s'avère très faible (R = 0.10) et non significative (p = 0.33). D'autres facteurs (mix produit) expliquent mieux la variance des prix."),
                withSpinner(plotOutput("plot_h10", height="250px"), type=8, color="#2C3E50")
              ),
              accordion_panel(
                "H11 : Plus d'inégalités selon l'indice Gini -> Prix CC plus élevés",
                p(tags$small(tags$em("Hypothèse : L'indice de Gini (inégalités de revenus) est corrélé au niveau de prix en circuits courts.")), style="color:#7F8C8D; margin-bottom:10px;"),
                p(span("Confirmé.", class = "hyp-true"), " On observe une corrélation positive significative (R = 0.21, p = 0.039). Les territoires les plus inégalitaires affichent globalement des prix en circuits courts plus élevés. Cela peut suggérer que l'offre en CC se positionne sur une gamme supérieure dans les zones très contrastées socio-économiquement."),
                withSpinner(plotOutput("plot_h11", height="250px"), type=8, color="#2C3E50")
              )
            )
          )
        )
      )
    )
  ),
  ),
),

  # ---------- ONGLET 8 : VALIDATION & AUDIT QUALITÉ ----------
  nav_panel("Validation & Contrôle Qualité", value = "nav_audit",
    fluidPage(
      layout_columns(
        col_widths = c(12),
        card(
          card_header(class = "bg-primary text-white d-flex justify-content-between align-items-center",
            div(
              h4("Audit Qualité & Matrice de Confusion", style = "margin:0; font-family:'Outfit'; font-weight:700;"),
              span("Échantillonnage proportionnel par plateforme — Contrôle manuel & Validation du tri (Base MariaDB `prix_labelises`)", style = "font-size:0.85em; opacity:0.9;")
            ),
            div(
              actionButton("audit_btn_resample", "Tirer un nouvel échantillon", class = "btn btn-sm btn-outline-light", style = "font-weight:600;")
            )
          ),
          card_body(
            layout_column_wrap(
              width = 1/4,
              value_box(
                title = "Transactions Auditées",
                value = textOutput("audit_kpi_total"),
                p("Enregistrées dans `prix_labelises`", style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:3px;"),
                showcase = bsicons::bs_icon("check2-all"),
                theme = "primary"
              ),
              value_box(
                title = "Précision du Filtrage Prix",
                value = textOutput("audit_kpi_precision"),
                p("Accord algorithme : cohérent gardé, aberrant rejeté", style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:3px;"),
                showcase = bsicons::bs_icon("bullseye"),
                theme = "success"
              ),
              value_box(
                title = "Erreurs de Catégorie",
                value = textOutput("audit_kpi_err_cat"),
                p("Produits mal aiguillés par les Regex", style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:3px;"),
                showcase = bsicons::bs_icon("exclamation-triangle"),
                theme = "warning"
              ),
              value_box(
                title = "Produits & Prix Aberrants",
                value = textOutput("audit_kpi_outliers"),
                p("Outliers économiques et produits hors périmètre", style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:3px;"),
                showcase = bsicons::bs_icon("x-circle"),
                theme = "danger"
              )
            )
          )
        )
      ),
      br(),
      layout_columns(
        col_widths = c(5, 7),
        
        # 1. CARTE DE VALIDATION MANUELLE (A GAUCHE)
        card(
          card_header(class = "bg-secondary text-white d-flex justify-content-between align-items-center",
            h5("Transaction en cours d'évaluation", style = "margin:0; font-family:'Outfit';"),
            uiOutput("audit_badge_progress")
          ),
          card_body(
            div(style = "background:#F8FAFC; border:1px solid #E2E8F0; border-radius:10px; padding:15px; margin-bottom:15px;",
              tags$small(tags$b("LIBELLÉ BRUT ORIGINAL :"), style = "color:#64748B; letter-spacing:0.5px;"),
              h4(textOutput("audit_txt_libelle"), style = "color:#1E293B; font-weight:700; margin-top:5px; margin-bottom:12px;"),
              
              layout_column_wrap(
                width = 1/4,
                div(style = "background:white; padding:10px; border-radius:8px; border-left:4px solid #3B82F6; box-shadow:0 1px 3px rgba(0,0,0,0.05);",
                  tags$small("Catégorie Prédite", style = "color:#64748B; font-weight:600;"),
                  div(strong(textOutput("audit_txt_cat")), style = "font-size:1.05em; color:#1D4ED8;")
                ),
                div(style = "background:white; padding:10px; border-radius:8px; border-left:4px solid #10B981; box-shadow:0 1px 3px rgba(0,0,0,0.05);",
                  tags$small("Prix du Produit", style = "color:#64748B; font-weight:600;"),
                  div(strong(textOutput("audit_txt_prix_produit")), style = "font-size:1.05em; color:#047857;"),
                  uiOutput("audit_txt_poids_info")
                ),
                div(style = "background:white; padding:10px; border-radius:8px; border-left:4px solid #F59E0B; box-shadow:0 1px 3px rgba(0,0,0,0.05);",
                  tags$small("Prix Ramené au Kg", style = "color:#64748B; font-weight:600;"),
                  div(strong(textOutput("audit_txt_prix")), style = "font-size:1.05em; color:#B45309;"),
                  uiOutput("audit_txt_prix_bench")
                ),
                div(style = "background:white; padding:10px; border-radius:8px; border-left:4px solid #8B5CF6; box-shadow:0 1px 3px rgba(0,0,0,0.05);",
                  tags$small("Plateforme Source", style = "color:#64748B; font-weight:600;"),
                  div(strong(textOutput("audit_txt_plat")), style = "font-size:1.05em; color:#6D28D9;")
                )
              ),
              div(style = "margin-top:12px; font-size:0.88em; color:#475569;",
                uiOutput("audit_txt_meta")
              )
            ),
            
            # Sélection de la vraie catégorie si erreur
            div(style = "margin-bottom:12px;",
              selectInput("audit_select_true_cat", "Si erreur de catégorie, sélectionner la vraie catégorie :",
                          choices = c(
                            "(Conserver la catégorie prédite)",
                            "Produit transformé",
                            "Autre / Hors périmètre",
                            "Non alimentaire",
                            sort(produits_choices)
                          ),
                          selected = "(Conserver la catégorie prédite)")
            ),
            
            # Évaluation additionnelle de l'anomalie de prix
            div(style = "margin-bottom:12px;",
              checkboxInput("audit_check_prix_aberrant", "Marquer également le prix comme aberrant / produit hors périmètre", value = FALSE)
            ),
            
            # Commentaire optionnel
            textInput("audit_input_comment", "Remarque d'audit / Commentaire (optionnel) :", 
                      placeholder = "Ex: produit transformé, conditionnement trompeur, prix à la pièce..."),
            hr(),
            
            # Boutons de validation rapide
            layout_column_wrap(
              width = 1/2,
              actionButton("audit_btn_valide", "Conforme (Catégorie & Prix OK)", 
                           class = "btn-success btn-lg", style = "font-weight:700; width:100%; height:55px;"),
              actionButton("audit_btn_err_cat", "Erreur Catégorie (Prix Cohérent)", 
                           class = "btn-warning btn-lg text-dark", style = "font-weight:700; width:100%; height:55px;"),
              actionButton("audit_btn_prod_aberrant", "Produit Transformé / Aberrant", 
                           class = "btn-danger btn-lg", style = "font-weight:700; width:100%; height:55px;"),
              actionButton("audit_btn_outlier", "Prix Aberrant (Catégorie OK)", 
                           class = "btn-outline-danger btn-lg", style = "font-weight:700; width:100%; height:55px; border-width:2px;"),
              actionButton("audit_btn_skip", "Passer / Doute", 
                           class = "btn-secondary btn-lg", style = "font-weight:700; width:100%; height:55px;")
            ),
            div(style = "margin-top:10px; text-align:center;",
              actionButton("audit_btn_undo", "Annuler la dernière décision", class = "btn btn-sm btn-link text-muted")
            )
          )
        ),
        
        # 2. MATRICE DE CONFUSION & HISTORIQUE (A DROITE)
        card(
          card_header(class = "bg-secondary text-white",
            h5("Matrice de Confusion & Diagnostic Qualité", style = "margin:0; font-family:'Outfit';")
          ),
          card_body(
            tabsetPanel(
              type = "tabs",
              nav_panel("Matrice de Confusion",
                p("Croisement entre la décision algorithmique et votre validation manuelle :", style = "font-size:0.88em; color:#7F8C8D; margin-top:8px;"),
                withSpinner(plotOutput("audit_plot_confusion", height = "320px"), type = 8, color = "#2C3E50"),
                hr(),
                h6("Top Confusions Détectées :", style = "font-weight:bold; color:#2C3E50;"),
                DTOutput("audit_table_confusions")
              ),
              nav_panel("Historique des Validations",
                p("Transactions enregistrées dans la table MariaDB `prix_labelises` :", style = "font-size:0.88em; color:#7F8C8D; margin-top:8px;"),
                DTOutput("audit_table_recent")
              )
            )
          )
        )
      )
    )
  )
)
# LOGIQUE DU SERVEUR
# =====================================================================
server <- function(input, output, session) {
  
  # -----------------------------------------------------------------
    # -----------------------------------------------------------------
  # CHARGEMENT PROGRESSIF EN ARRIÈRE-PLAN (BACKGROUND LOADER)
  # -----------------------------------------------------------------
  bg_step <- reactiveVal(0)
  bg_status_msg <- reactiveVal("Chargement initial...")
  bg_is_ready <- reactiveVal(FALSE)

  output$data_status_badge <- renderUI({
    if (isTRUE(bg_is_ready())) {
      tags$span(
        class = "badge",
        style = "background-color: #228833; color: #FFFFFF; font-weight: 500; font-size: 0.80rem; padding: 6px 10px;",
        "Données prêtes"
      )
    } else {
      tags$span(
        class = "badge",
        style = "background-color: #0284C7; color: #FFFFFF; font-weight: 500; font-size: 0.80rem; padding: 6px 10px;",
        tags$span(class = "spinner-border spinner-border-sm me-1", role = "status", style = "width: 0.70rem; height: 0.70rem; display: inline-block; vertical-align: middle;"),
        bg_status_msg()
      )
    }
  })

  # Déclenchement non-bloquant après l'affichage immédiat de la page d'accueil
  session$onFlushed(function() {
    bg_step(1)
  }, once = TRUE)

  observeEvent(bg_step(), {
    s <- bg_step()
    if (s == 1) {
      bg_status_msg("Chargement cartographie...")
      load_france_depts()
      load_boxes_dom()
      shiny::invalidateLater(35, session)
      bg_step(2)
    } else if (s == 2) {
      bg_status_msg("Chargement données territoriales...")
      load_dept_impl()
      shiny::invalidateLater(35, session)
      bg_step(3)
    } else if (s == 3) {
      bg_status_msg("Chargement distributions GMM...")
      load_classes_impl()
      load_gmm_impl()
      bg_is_ready(TRUE)
      bg_status_msg("Données prêtes")
      bg_step(4)
    }
  }, ignoreInit = TRUE)

  # -----------------------------------------------------------------
  # GESTION DES DROITS & ACCÈS AUDIT QUALITÉ / LABÉLISATION
  # -----------------------------------------------------------------
  is_admin <- reactiveVal(TRUE)
  
  # L'onglet de labélisation & audit qualité reste actif et visible pour l'utilisateur
  # Détection automatique du paramètre URL pour sélection directe de tab
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$tab) && query$tab %in% c("audit", "validation")) {
      nav_select("main_nav", "nav_audit")
    }
  })
  
  # Modal de connexion administrateur
  observeEvent(input$btn_admin_login, {
    showModal(modalDialog(
      title = tags$div(
        style = "font-family: 'Outfit'; font-weight: 700; color: #228833;",
        icon("lock"), " Accès Réservé — Équipe de Recherche"
      ),
      tags$p("Veuillez saisir le code d'accès administrateur pour afficher l'onglet de contrôle qualité et de labellisation :",
             style = "color: #5D6D7E; font-size: 0.95em;"),
      passwordInput("modal_admin_pwd", "Mot de passe administrateur :", placeholder = "Code d'accès"),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("btn_modal_admin_submit", "Déverrouiller", class = "btn btn-success", style = "font-weight: 600;")
      ),
      easyClose = TRUE,
      size = "m"
    ))
  })
  
  observeEvent(input$btn_modal_admin_submit, {
    pwd_input <- input$modal_admin_pwd
    admin_pwd_expected <- Sys.getenv("ADMIN_ACCESS_KEY", "")
    valid_admin <- if (nzchar(admin_pwd_expected)) {
      identical(pwd_input, admin_pwd_expected)
    } else {
      use_simulated_data && identical(pwd_input, "demo")
    }
    if (valid_admin) {
      is_admin(TRUE)
      nav_show(id = "main_nav", target = "nav_audit")
      removeModal()
      showNotification("Mode Administrateur activé. Onglet Contrôle Qualité déverrouillé.", type = "message", duration = 4)
      nav_select("main_nav", "nav_audit")
    } else {
      showNotification("Code d'accès incorrect.", type = "error", duration = 3)
    }
  })
  
  observeEvent(input$btn_admin_logout, {
    is_admin(FALSE)
    nav_hide(id = "main_nav", target = "nav_audit")
    nav_select("main_nav", "Accueil")
    showNotification("Session administrateur fermée.", type = "default", duration = 2)
  })
  
  output$ui_admin_auth <- renderUI({
    if (!is_admin()) {
      actionLink("btn_admin_login", 
                 tagList(icon("lock"), tags$small(" Accès Équipe / Contrôle")), 
                 style = "color: #95A5A6; text-decoration: none; font-size: 0.82em;")
    } else {
      tagList(
        span(class = "badge bg-success", style = "font-weight: 500;", icon("unlock"), " Administrateur actif"),
        actionLink("btn_admin_logout", tags$small("Déconnexion"), style = "margin-left: 10px; color: #E74C3C; font-size: 0.82em; text-decoration: none;")
      )
    }
  })
  
  # Synchronisation des paramètres URL pour accès direct aux onglets et vues
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$tab) && query$tab == "inflation") {
      nav_select("main_nav", "nav_inflation")
    }
    if (!is.null(query$view) && query$view %in% c("annuel", "mensuel")) {
      updateRadioButtons(session, "inflation_view_type", selected = query$view)
    }
  })
  
  # -----------------------------------------------------------------
  # LOGIQUE ONGLET 6 : ANALYSES APPROFONDIES (SAISONNALITÉ)
  # -----------------------------------------------------------------
  
  # 1. Tableau synthétique de saisonnalité
  output$table_synthese_saison <- renderDT({
    req(df_synthese_saison, df_series_mensuelles)
    
    # Somme des transactions historiques par catégorie standard
    volumes <- df_series_mensuelles %>%
      group_by(categorie_standard) %>%
      summarise(total_ventes = sum(nb_transactions, na.rm = TRUE), .groups = "drop")
    
    df_disp <- df_synthese_saison %>%
      left_join(volumes, by = c("categorie" = "categorie_standard")) %>%
      arrange(desc(total_ventes)) %>%
      mutate(
        intensite = case_when(
          amplitude_relative_pct > 40.0 ~ "Forte (>40%)",
          amplitude_relative_pct >= 15.0 ~ "Moyenne (15-40%)",
          TRUE ~ "Faible (<15%)"
        )
      ) %>%
      select(
        `Catégorie` = categorie,
        `Nombre total de ventes` = total_ventes,
        `Amplitude Saisonnière (€/kg)` = amplitude_saisonniere,
        `Amplitude Relative (%)` = amplitude_relative_pct,
        `Intensité` = intensite,
        `Mois de Pic` = mois_pic_nom,
        `Mois de Creux` = mois_creux_nom,
        `Tendance Mensuelle (€/kg/mois)` = tendance_pente,
        `Modèle SARIMA` = sarima_order
      )
    
    datatable(df_disp, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(columns = c("Amplitude Saisonnière (€/kg)"), digits = 3) %>%
      formatRound(columns = c("Amplitude Relative (%)"), digits = 1) %>%
      formatSignif(columns = c("Tendance Mensuelle (€/kg/mois)"), digits = 3) %>%
      formatRound(columns = c("Nombre total de ventes"), digits = 0, mark = " ") %>%
      formatStyle(
        'Intensité',
        target = 'cell',
        color = styleEqual(c("Forte (>40%)", "Moyenne (15-40%)", "Faible (<15%)"), c("#C0392B", "#D35400", "#1F618D")),
        fontWeight = 'bold'
      )
  })
  
  # 2. Graphique de décomposition STL
  output$stl_plot <- renderPlot({
    req(df_stl, input$season_prod)
    
    decomp_cat <- df_stl %>%
      filter(categorie_standard == input$season_prod) %>%
      arrange(date)
    
    if (nrow(decomp_cat) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Décomposition non disponible pour ce produit.")
      return(NULL)
    }
    
    df_plot <- decomp_cat %>%
      pivot_longer(cols = c("original", "trend", "seasonal", "remainder"),
                   names_to = "composante", values_to = "valeur") %>%
      mutate(
        composante = factor(
          composante, 
          levels = c("original", "trend", "seasonal", "remainder"),
          labels = c("Prix observé (€/kg)", "Tendance", "Saisonnalité", "Résidu")
        )
      )
    
    ggplot(df_plot, aes(x = date, y = valeur)) +
      geom_line(color = "#228833", linewidth = 0.8) +
      facet_wrap(~ composante, ncol = 1, scales = "free_y") +
      theme_minimal(base_size = 12) +
      labs(
        title = paste("Décomposition Temporelle STL :", input$season_prod),
        subtitle = "Modélisation sur données mensuelles propres circuits courts",
        x = NULL, y = NULL
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
        strip.text = element_text(face = "bold", size = 11, color = "#2C3E50")
      )
  })
  
  # 2. Graphique de profil saisonnier annuel moyen
  output$seasonal_profile_plot <- renderPlot({
    req(df_stl, input$season_prod)
    
    decomp_cat <- df_stl %>%
      filter(categorie_standard == input$season_prod)
    
    if (nrow(decomp_cat) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Profil saisonnier non disponible.")
      return(NULL)
    }
    
    # Calculer l'effet saisonnier moyen par mois de l'année
    profil_saisonnier <- decomp_cat %>%
      mutate(
        num_mois = month(date),
        mois = factor(num_mois, levels = 1:12, 
                      labels = c("Jan", "Fév", "Mar", "Avr", "Mai", "Jun", 
                                 "Jul", "Aoû", "Sep", "Oct", "Nov", "Déc"))
      ) %>%
      group_by(mois) %>%
      summarise(effet_saisonnier = mean(seasonal, na.rm = TRUE), .groups = "drop")
    
    ggplot(profil_saisonnier, aes(x = mois, y = effet_saisonnier, fill = effet_saisonnier > 0)) +
      geom_col(color = "white", width = 0.7) +
      scale_fill_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#2E86C1"), guide = "none") +
      theme_minimal(base_size = 12) +
      labs(
        title = paste("Profil Saisonnier Moyen :", input$season_prod),
        subtitle = "Écart moyen au prix tendanciel en euros par kg par mois de l'année",
        x = NULL, y = "Effet saisonnier (€/kg)"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
        axis.text.x = element_text(face = "bold", color = "#34495E", size = 11),
        panel.grid.major.x = element_blank()
      )
  })
  
  # 2.B Message informatif sur l'intensité saisonnière dans le Sidebar
  output$season_intensity_ui <- renderUI({
    req(df_synthese_saison, input$season_prod)
    prod_info <- df_synthese_saison %>% filter(categorie == input$season_prod)
    if (nrow(prod_info) == 0) return(NULL)
    
    amp <- prod_info$amplitude_saisonniere[1]
    amp_rel <- prod_info$amplitude_relative_pct[1]
    prix_moy <- prod_info$prix_moyen_produit[1]
    
    if (amp_rel > 40.0) {
      div(
        style = "padding: 12px; border-radius: 6px; background-color: #FDEDEC; border-left: 5px solid #E74C3C; margin-top: 15px;",
        h5("Saisonnalité : Forte", style = "color: #C0392B; font-weight: bold; margin: 0 0 5px 0; font-family:'Outfit';"),
        p(sprintf("Ce produit présente des fluctuations saisonnières marquées. Son amplitude saisonnière (%.2f €/kg) représente %.1f%% de son prix moyen tendanciel (%.2f €/kg).", amp, amp_rel, prix_moy), style = "margin: 0; font-size: 0.88em; color: #78281F;")
      )
    } else if (amp_rel >= 15.0) {
      div(
        style = "padding: 12px; border-radius: 6px; background-color: #FEF9E7; border-left: 5px solid #F39C12; margin-top: 15px;",
        h5("Saisonnalité : Moyenne", style = "color: #D35400; font-weight: bold; margin: 0 0 5px 0; font-family:'Outfit';"),
        p(sprintf("Ce produit présente des variations saisonnières modérées. Son amplitude saisonnière (%.2f €/kg) représente %.1f%% de son prix moyen tendanciel (%.2f €/kg).", amp, amp_rel, prix_moy), style = "margin: 0; font-size: 0.88em; color: #7E5109;")
      )
    } else {
      div(
        style = "padding: 12px; border-radius: 6px; background-color: #EAF2F8; border-left: 5px solid #2980B9; margin-top: 15px;",
        h5("Saisonnalité : Faible", style = "color: #1F618D; font-weight: bold; margin: 0 0 5px 0; font-family:'Outfit';"),
        p(sprintf("Ce produit présente des fluctuations saisonnières très faibles. Son amplitude saisonnière (%.2f €/kg) ne représente que %.1f%% de son prix moyen tendanciel (%.2f €/kg). Son prix est stable.", amp, amp_rel, prix_moy), style = "margin: 0; font-size: 0.88em; color: #1B4F72;")
      )
    }
  })
  
  # 3. Graphique de prévision SARIMA (pré-calculé pour performance WebR)
  output$sarima_plot <- renderPlot({
    req(input$season_prod)
    
    sarima_item <- if (!is.null(ls_precomputed_sarima)) ls_precomputed_sarima[[input$season_prod]] else NULL
    if (!is.null(sarima_item) && identical(sarima_item$status, "ok")) {
      df_fc <- sarima_item$df_fc
      model_label <- sarima_item$model_label
      
      p <- ggplot(df_fc, aes(x = date, y = prix)) +
        geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "#AED6F1", alpha = 0.4, na.rm = TRUE) +
        geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "#5DADE2", alpha = 0.4, na.rm = TRUE) +
        geom_line(aes(color = type), linewidth = 0.8) +
        scale_color_manual(values = c("Historique" = "#2C3E50", "Prévision (SARIMA)" = "#E74C3C")) +
        theme_minimal(base_size = 12) +
        labs(
          title = paste("Prévisions des prix à 6 mois :", input$season_prod),
          subtitle = paste("Modèle automatique :", model_label, "— Intervalles de confiance 80% et 95%"),
          x = NULL, y = "Prix moyen pondéré (€/kg)", color = NULL
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
          legend.position = "bottom"
        )
      return(p)
    }
    
    req(df_series_mensuelles)
    cat_data <- df_series_mensuelles %>% 
      filter(categorie_standard == input$season_prod) %>%
      arrange(date)
    
    if (nrow(cat_data) < 18) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
           main = "Historique insuffisant (<18 mois) pour modélisation SARIMA.")
      return(NULL)
    }
    
    date_range <- seq.Date(min(cat_data$date), max(cat_data$date), by = "month")
    cat_complete <- data.frame(date = date_range) %>%
      left_join(cat_data, by = "date") %>%
      mutate(
        prix_moyen_pondere = approx(
          x = which(!is.na(prix_moyen_pondere)),
          y = prix_moyen_pondere[!is.na(prix_moyen_pondere)],
          xout = 1:n(),
          rule = 2
        )$y
      )
    
    ts_prix <- ts(cat_complete$prix_moyen_pondere, 
                  start = c(year(min(cat_complete$date)), month(min(cat_complete$date))), 
                  frequency = 12)
    
    if (!requireNamespace("forecast", quietly = TRUE)) {
      plot(ts_prix, main = paste("Série temporelle :", input$season_prod), ylab = "Prix (€/kg)")
      return(NULL)
    }
    
    model <- tryCatch({
      forecast::auto.arima(ts_prix, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
    }, error = function(e) NULL)
    
    if (is.null(model)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Ajustement du modèle SARIMA impossible.")
      return(NULL)
    }
    
    fc <- forecast::forecast(model, h = 6)
    n_hist <- length(ts_prix)
    dates_hist <- cat_complete$date
    dates_fc <- seq.Date(max(dates_hist) + months(1), by = "month", length.out = 6)
    
    df_fc <- data.frame(
      date = c(dates_hist, dates_fc),
      prix = c(as.numeric(ts_prix), as.numeric(fc$mean)),
      type = c(rep("Historique", n_hist), rep("Prévision (SARIMA)", 6)),
      lo80 = c(rep(NA, n_hist), as.numeric(fc$lower[,1])),
      hi80 = c(rep(NA, n_hist), as.numeric(fc$upper[,1])),
      lo95 = c(rep(NA, n_hist), as.numeric(fc$lower[,2])),
      hi95 = c(rep(NA, n_hist), as.numeric(fc$upper[,2]))
    )
    
    ord <- forecast::arimaorder(model)
    model_label <- sprintf("ARIMA(%d,%d,%d)×(%d,%d,%d)[12]", 
                           ord[1], ord[2], ord[3],
                           model$arma[3], model$arma[4], model$arma[7])
    
    ggplot(df_fc, aes(x = date, y = prix)) +
      geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "#AED6F1", alpha = 0.4, na.rm = TRUE) +
      geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "#5DADE2", alpha = 0.4, na.rm = TRUE) +
      geom_line(aes(color = type), linewidth = 0.8) +
      scale_color_manual(values = c("Historique" = "#2C3E50", "Prévision (SARIMA)" = "#E74C3C")) +
      theme_minimal(base_size = 12) +
      labs(
        title = paste("Prévisions des prix à 6 mois :", input$season_prod),
        subtitle = paste("Modèle automatique :", model_label, "— Intervalles de confiance 80% et 95%"),
        x = NULL, y = "Prix moyen pondéré (€/kg)", color = NULL
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
        legend.position = "bottom"
      )
  })
  
  # -----------------------------------------------------------------
  # STATISTIQUES ACCUEIL (AFFICHAGE IMMÉDIAT SANS ATTENTE)
  # -----------------------------------------------------------------
  output$total_transactions <- renderText({
    if (!is.null(accueil_data)) {
      format(accueil_data$total_transactions, big.mark = " ")
    } else {
      req(df_dept_all)
      total <- df_dept_all %>%
        filter(dept == "National", Plateforme == "Toutes", Annee == "Toutes", organic == "Tous") %>%
        summarise(s = sum(Nombre_Ventes, na.rm = TRUE)) %>%
        pull(s)
      format(total, big.mark = " ")
    }
  })
  
  output$total_categories <- renderText({
    if (!is.null(accueil_data)) {
      as.character(accueil_data$nb_categories)
    } else {
      as.character(length(produits_choices))
    }
  })
  
  output$total_depts <- renderText({
    if (!is.null(accueil_data)) {
      as.character(accueil_data$nb_total_depts)
    } else {
      req(df_dept_all)
      depts <- unique(df_dept_all$dept[df_dept_all$dept != "National"])
      if ("20" %in% depts) {
        as.character(length(setdiff(depts, "20")) + 2)
      } else {
        as.character(length(depts))
      }
    }
  })
  
  output$acc_transactions_sub <- renderUI({
    if (!is.null(accueil_data)) {
      p(sprintf("%s ventes en métropole sur %s au total", 
                format(accueil_data$metro_transactions, big.mark = " "), 
                format(accueil_data$total_transactions, big.mark = " ")), 
        style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:4px;")
    } else {
      req(df_dept_all)
      metro_total <- df_dept_all %>%
        filter(dept != "National", !grepl("^9[78]", dept), 
               Plateforme == "Toutes", Annee == "Toutes", organic == "Tous") %>%
        summarise(s = sum(Nombre_Ventes, na.rm = TRUE)) %>%
        pull(s)
      nat_total <- df_dept_all %>%
        filter(dept == "National", Plateforme == "Toutes", Annee == "Toutes", organic == "Tous") %>%
        summarise(s = sum(Nombre_Ventes, na.rm = TRUE)) %>%
        pull(s)
      p(sprintf("%s ventes en métropole sur %s au total", 
                format(metro_total, big.mark = " "), 
                format(nat_total, big.mark = " ")), 
      style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:4px;")
    }
  })

  output$acc_depts_sub <- renderUI({
    if (!is.null(accueil_data)) {
      p(sprintf("%d métropolitains et %d outre-mer", accueil_data$nb_metro_depts, accueil_data$nb_dom_depts), 
        style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:4px;")
    } else {
      req(df_dept_all)
      depts <- unique(df_dept_all$dept[df_dept_all$dept != "National"])
      dom_depts <- depts[grepl("^9[78]", depts)]
      metro_depts <- setdiff(depts, dom_depts)
      nb_metro <- if ("20" %in% metro_depts) length(setdiff(metro_depts, "20")) + 2 else length(metro_depts)
      nb_dom <- length(dom_depts)
      p(sprintf("%d métropolitains et %d outre-mer", nb_metro, nb_dom), 
        style = "font-size:0.82em; color:rgba(255,255,255,0.85); margin-top:4px;")
    }
  })

  output$funnel_gmm_note <- renderUI({
    gmm_tot <- if (!is.null(accueil_data)) {
      accueil_data$gmm_tot
    } else if (!is.null(ls_funnel$gmm)) {
      sum(as.numeric(ls_funnel$gmm$n), na.rm = TRUE)
    } else {
      8677029
    }
    
    geo_tot <- if (!is.null(accueil_data)) {
      accueil_data$total_transactions
    } else {
      req(df_dept_all)
      df_dept_all %>%
        filter(dept == "National", Plateforme == "Toutes", Annee == "Toutes", organic == "Tous") %>%
        summarise(s = sum(Nombre_Ventes, na.rm = TRUE)) %>%
        pull(s)
    }
    
    pct_geo <- if (!is.null(accueil_data)) accueil_data$pct_geo else round(geo_tot / gmm_tot * 100, 1)
    non_geo <- if (!is.null(accueil_data)) accueil_data$non_geo else max(0, gmm_tot - geo_tot)
    nb_depts <- if (!is.null(accueil_data)) accueil_data$nb_total_depts else 101
    
    p(tags$small(sprintf(
      "Note : %s transactions filtrées au total (national), dont %s (%.1f %%) géolocalisées dans les %d départements couverts (%s sans code postal valide pour une affectation départementale).",
      format(gmm_tot, big.mark = " "),
      format(geo_tot, big.mark = " "),
      pct_geo,
      nb_depts,
      format(non_geo, big.mark = " ")
    )), style = "font-size:0.80em; color:#7F8C8D; text-align:center; margin-top:8px; line-height:1.3;")
  })

  
  # 1. Graphique Circulaire - Global
  # Helper function to render funnel pie charts
  render_funnel_pie <- function(df, var_name, title, palette, is_year=FALSE) {
    req(df)
    if (nrow(df) == 0) return(NULL)
    
    df_agg <- df %>% 
      mutate(n = as.numeric(n)) %>%
      group_by(!!sym(var_name)) %>% 
      summarise(Count = sum(n, na.rm=TRUE), .groups="drop") %>%
      arrange(!!sym(var_name)) %>%
      mutate(
        Percentage = Count / sum(Count) * 100,
        Label = ifelse(Percentage >= 0.5, sprintf("%s\n%s\n(%.1f%%)", !!sym(var_name), format(Count, big.mark = " ", scientific = FALSE, trim = TRUE), Percentage), ""),
        csum = cumsum(Count) - 0.5 * Count
      )
    
    ggplot(df_agg, aes(x = 1, y = Count, fill = !!sym(var_name))) +
      geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1, position = position_stack(reverse = TRUE)) +
      coord_polar("y", start = 0, clip = "off") +
      theme_void() +
      scale_fill_manual(values = palette) +
      scale_x_continuous(limits = c(0.5, 2.0)) +
      geom_label_repel(aes(y = csum, label = Label), 
                       x = 1.5,
                       size = 3.5, fontface = "bold", show.legend = FALSE, 
                       nudge_x = 0.4, segment.color = "grey50") +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.margin = margin(10, 20, 10, 20)
      ) +
      ggtitle(title)
  }
  
  cols_plat <- c("Plateforme 1" = "#3498DB", "Plateforme 2" = "#F1C40F", "Plateforme 3" = "#228833", "Plateforme 4" = "#E74C3C")
  cols_year <- c("2021"="#F1C40F", "2022"="#F39C12", "2023"="#D35400", "2024"="#E74C3C", "2025"="#C0392B", "2026"="#8E44AD", "Autres"="#7F8C8D")
  
  output$pie_raw_plat_plot <- renderPlot({ render_funnel_pie(ls_funnel$raw, "plateforme", "Répartition par Plateforme", cols_plat) })
  output$pie_raw_year_plot <- renderPlot({ render_funnel_pie(ls_funnel$raw, "annee", "Répartition par Année", cols_year, TRUE) })
  output$pie_gmm_plat_plot <- renderPlot({ render_funnel_pie(ls_funnel$gmm, "plateforme", "Répartition par Plateforme", cols_plat) })
  output$pie_gmm_year_plot <- renderPlot({ render_funnel_pie(ls_funnel$gmm, "annee", "Répartition par Année", cols_year, TRUE) })
  
  # 2. Graphique Circulaire - Par Produit (Immédiat via accueil_data)
  output$pie_product_plot <- renderPlot({
    req(input$pie_prod_select)
    
    df_pie <- if (!is.null(accueil_data) && !is.null(accueil_data$df_pie_product)) {
      accueil_data$df_pie_product %>%
        filter(categorie_standard == input$pie_prod_select) %>%
        mutate(
          Percentage = Count / sum(Count) * 100,
          Label = ifelse(Percentage >= 4, 
                         sprintf("%s\n%s\n(%.1f%%)", Plateforme, format(Count, big.mark = " "), Percentage), 
                         "")
        )
    } else {
      req(df_dept_all)
      df_dept_all %>%
        filter(
          dept != "National",
          Plateforme != "Toutes",
          categorie_standard == input$pie_prod_select,
          Annee == "Toutes",
          organic == "Tous"
        ) %>%
        group_by(Plateforme) %>%
        summarise(Count = sum(Nombre_Ventes, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          Percentage = Count / sum(Count) * 100,
          Label = ifelse(Percentage >= 4, 
                         sprintf("%s\n%s\n(%.1f%%)", Plateforme, format(Count, big.mark = " "), Percentage), 
                         "")
        )
    }
    
    if (nrow(df_pie) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Aucune donnée disponible pour ce produit.")
      return(NULL)
    }
    
    cols <- c(
      "Plateforme 1" = "#3498DB",
      "Plateforme 2" = "#F1C40F",
      "Plateforme 3" = "#228833",
      "Plateforme 4" = "#E74C3C"
    )
    
    ggplot(df_pie, aes(x = "", y = Count, fill = Plateforme)) +
      geom_bar(stat = "identity", width = 1, color = "white", size = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(values = cols) +
      geom_text(aes(label = Label), position = position_stack(vjust = 0.5), 
                color = "black", fontface = "bold", size = 3.5) +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold")
      )
  })
  
  # -----------------------------------------------------------------
  # LOGIQUE ONGLET 2 : EXPLORATEUR & CARTES DES PRODUITS
  # -----------------------------------------------------------------
  
  # Filtre dynamique des choix de produits via les tags (Rayon, Saison, Panier)
  observeEvent(list(input$tag_rayon, input$tag_saison, input$tag_panier), {
    req(df_foodon_ref)
    filtered_ref <- df_foodon_ref
    
    if (input$tag_rayon != "Tous les rayons") {
      filtered_ref <- filtered_ref %>% filter(groupe == input$tag_rayon)
    }
    if (input$tag_saison != "Toutes saisons") {
      filtered_ref <- filtered_ref %>% filter(saison_principale == input$tag_saison)
    }
    if (isTRUE(input$tag_panier)) {
      filtered_ref <- filtered_ref %>% filter(panier_quotidien == 1)
    }
    
    if (nrow(filtered_ref) == 0) {
      filtered_ref <- df_foodon_ref
    }
    
    if (input$tag_rayon != "Tous les rayons") {
      new_choices <- sort(filtered_ref$categorie_standard)
      flat_choices <- new_choices
    } else {
      sorted_df <- filtered_ref %>% arrange(groupe, categorie_standard)
      new_choices <- split(sorted_df$categorie_standard, sorted_df$groupe)
      if (input$tag_saison == "Toutes saisons" && !isTRUE(input$tag_panier)) {
        new_choices <- c(list("Indice Territorial Global" = c("Indice des Prix Territoriaux (IPT)" = "Indice Français des Prix en Circuits Courts")), new_choices)
      }
      flat_choices <- c("Indice Français des Prix en Circuits Courts", sorted_df$categorie_standard)
    }
    
    current_sel <- isolate(input$map_prod)
    selected_val <- if (current_sel %in% flat_choices) current_sel else flat_choices[1]
    
    updateSelectInput(session, "map_prod",
                      choices = new_choices,
                      selected = selected_val)
  }, ignoreInit = FALSE)
  
  # Fiche d'identité FoodOn du produit sélectionné
  output$foodon_badge_card <- renderUI({
    req(input$map_prod)
    if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
      return(div(
        style = "height: 100%; min-height: 175px; padding: 14px 16px; background: #FFFFFF; border: 1px solid #E2E8F0; border-left: 4px solid #6366F1; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); display: flex; flex-direction: column; justify-content: space-between;",
        div(
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px;",
            strong("Indice des Prix Territoriaux (IPT)", style = "font-size: 1.15em; color: #1E293B; font-family: 'Outfit', sans-serif;"),
            span("Base 1.00", class = "badge bg-primary", style = "font-weight: 600; font-size: 0.82em;")
          ),
          p("Disparité spatiale pure des prix", style = "color: #64748B; margin: 0 0 6px 0; font-size: 0.88em;"),
          div(
            style = "margin-bottom: 8px;",
            span("Modèle Linéaire Mixte", class = "badge bg-info text-dark", style = "margin-right: 4px; font-weight: 500; font-size: 0.78em;"),
            span("Multi-produits", class = "badge bg-success", style = "font-weight: 500; font-size: 0.78em;")
          )
        ),
        p("Synthèse territoriale multi-produits contrôlant l'effet panier, le bio, le format et la plateforme.", style = "font-size: 0.84em; color: #475569; margin: 0;")
      ))
    }
    req(df_foodon_ref)
    prod_info <- df_foodon_ref %>% filter(categorie_standard == input$map_prod)
    if (nrow(prod_info) == 0) return(NULL)
    
    foodon_id <- prod_info$foodon_id[1]
    foodon_label <- prod_info$foodon_label[1]
    foodex2 <- prod_info$foodex2_code[1]
    agrovoc <- prod_info$agrovoc_uri[1]
    botanic <- prod_info$nom_botanique_ou_scientifique[1]
    groupe <- prod_info$groupe[1]
    saison <- prod_info$saison_principale[1]
    is_panier <- prod_info$panier_quotidien[1] == 1
    unite <- prod_info$unite_reference[1]
    
    foodon_clean_id <- gsub(":", "_", foodon_id)
    foodon_url <- paste0("http://www.ontobee.org/ontology/FOODON?iri=http://purl.obolibrary.org/obo/", foodon_clean_id)
    
    div(
      style = "height: 100%; min-height: 175px; padding: 14px 16px; background: #FFFFFF; border: 1px solid #E2E8F0; border-left: 4px solid #6366F1; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); display: flex; flex-direction: column; justify-content: space-between;",
      div(
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px;",
          strong(prod_info$categorie_standard[1], style = "font-size: 1.15em; color: #1E293B; font-family: 'Outfit', sans-serif;"),
          span(paste0("Unité : €/", unite), class = "badge bg-success", style = "font-weight: 600; font-size: 0.82em;")
        ),
        if (!is.na(botanic) && botanic != "") {
          p(em(botanic), style = "color: #64748B; margin: 0 0 6px 0; font-size: 0.88em;")
        } else NULL,
        div(
          style = "margin-bottom: 8px;",
          span(groupe, class = "badge bg-primary", style = "margin-right: 4px; font-weight: 500; font-size: 0.78em;"),
          span(saison, class = "badge bg-info text-dark", style = "margin-right: 4px; font-weight: 500; font-size: 0.78em;"),
          if (is_panier) span("Panier quotidien", class = "badge bg-success", style = "font-weight: 500; font-size: 0.78em;") else NULL
        )
      ),
      tags$table(
        style = "width: 100%; border-top: 1px solid #F1F5F9; margin-top: 6px; padding-top: 6px; font-size: 0.86em;",
        tags$tr(
          tags$td(strong("FoodOn :"), style = "color: #64748B; width: 38%; padding: 2px 0;"),
          tags$td(tags$a(href = foodon_url, target = "_blank", foodon_id, style = "color: #2563EB; font-weight: 600; text-decoration: underline;"))
        ),
        tags$tr(
          tags$td(strong("FoodOn Label :"), style = "color: #64748B; padding: 2px 0;"),
          tags$td(foodon_label, style = "color: #334155;")
        ),
        tags$tr(
          tags$td(strong("FoodEx2 (EFSA) :"), style = "color: #64748B; padding: 2px 0;"),
          tags$td(code(foodex2, style = "color: #0F766E; font-size: 0.95em; background: #F0FDFA; padding: 1px 4px; border-radius: 3px;"))
        ),
        tags$tr(
          tags$td(strong("AGROVOC (FAO) :"), style = "color: #64748B; padding: 2px 0;"),
          tags$td(code(agrovoc, style = "color: #6D28D9; font-size: 0.95em; background: #FAF5FF; padding: 1px 4px; border-radius: 3px;"))
        )
      )
    )
  })
  
  # Titre dynamique de la carte selon le produit et son unité
  output$map_card_title <- renderUI({
    req(input$map_prod)
    if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
      return(h4("Carte Géographique de l'Indice des Prix Territoriaux (IPT)", style = "margin:0; font-family:'Outfit';"))
    }
    unit_str <- "kg"
    if (!is.null(df_foodon_ref)) {
      sub_ref <- df_foodon_ref %>% filter(categorie_standard == input$map_prod)
      if (nrow(sub_ref) > 0 && !is.na(sub_ref$unite_reference[1])) {
        unit_str <- sub_ref$unite_reference[1]
      }
    }
    unit_label <- if (unit_str == "L") "au litre (€/L)" else if (unit_str == "pièce") "à la pièce" else "au kilo (€/kg)"
    h4(paste("Carte des prix moyens par département -", input$map_prod, paste0("(", unit_label, ")")),
       style = "margin:0; font-family:'Outfit';")
  })
  
  # Mise à jour dynamique du seuil TCL recommandé (-20% demandé par l'utilisateur)
  observe({
    req(df_dept_all, input$map_prod, input$map_plat, input$map_year, input$map_organic)
    
    clt_val <- 15 # valeur par défaut
    
    if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
      # Calculer le seuil moyen du TCL sur l'ensemble des produits (hors Autres)
      df_nat_products <- df_dept_all %>%
        filter(
          dept == "National",
          Plateforme == input$map_plat,
          Annee == input$map_year,
          organic == input$map_organic,
          categorie_standard != "Autres"
        )
      
      if (nrow(df_nat_products) > 0) {
        thresholds <- df_nat_products %>%
          rowwise() %>%
          mutate(
            clt = {
              mean_val <- Prix_Moyen_Kg
              sd_val <- Ecart_Type
              if (!is.na(mean_val) && mean_val > 0 && !is.na(sd_val)) {
                E <- 0.05 * mean_val
                val <- (1.96 * sd_val / E)^2
                ceiling(val * 0.8)
              } else {
                NA_real_
              }
            }
          ) %>%
          pull(clt)
        
        valid_thresholds <- thresholds[!is.na(thresholds)]
        if (length(valid_thresholds) > 0) {
          clt_val <- ceiling(median(valid_thresholds))
        } else {
          clt_val <- 15
        }
      }
    } else {
      nat_row <- df_dept_all %>%
        filter(
          dept == "National",
          categorie_standard == input$map_prod,
          Plateforme == input$map_plat,
          Annee == input$map_year,
          organic == input$map_organic
        )
      
      if (nrow(nat_row) > 0) {
        mean_val <- nat_row$Prix_Moyen_Kg[1]
        sd_val <- nat_row$Ecart_Type[1]
        if (!is.na(mean_val) && mean_val > 0 && !is.na(sd_val)) {
          E <- 0.05 * mean_val
          val <- (1.96 * sd_val / E)^2
          if (is.finite(val) && val >= 1) {
            clt_val <- ceiling(val * 0.8) # Seuil diminué de 20%
          }
        }
      }
    }
    
    updateNumericInput(session, "map_min_sales", value = clt_val)
  })
  
  # Affichage de l'explication du seuil TCL
  output$clt_info_text <- renderUI({
    req(df_dept_all, input$map_prod, input$map_plat, input$map_year, input$map_organic)
    
    if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
      return(HTML(
        "<div style='height: 100%; min-height: 175px; padding: 14px 16px; background: #FFFFFF; border: 1px solid #E2E8F0; border-left: 4px solid #10B981; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); display: flex; flex-direction: column; justify-content: space-between;'>
          <div>
            <div style='display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;'>
              <strong style='font-size: 1.05em; color: #1E293B; font-family: \"Outfit\", sans-serif;'>Représentativité Statistique (TCL)</strong>
              <span class='badge bg-success' style='font-size: 0.80em;'>Seuil fixe IPT</span>
            </div>
            <div style='color: #475569; font-size: 0.82em; line-height: 1.45;'>
              • <strong>Principe :</strong> Agrégation pondérée de l'ensemble du panier alimentaire.<br/>
              • <strong>Base de référence :</strong> 100 = Niveau moyen national.<br/>
              • <strong>Seuil minimal :</strong> <strong style='color: #DC2626;'>15</strong> ventes par département.
            </div>
          </div>
          <div style='margin-top: 6px; padding: 5px 8px; background: #F1F5F9; border-radius: 6px; text-align: center; font-size: 0.80em; color: #475569;'>
            Agrégation multi-catégorielle robuste
          </div>
        </div>"
      ))
    }
    
    nat_row <- df_dept_all %>%
      filter(
        dept == "National",
        categorie_standard == input$map_prod,
        Plateforme == input$map_plat,
        Annee == input$map_year,
        organic == input$map_organic
      )
    
    if (nrow(nat_row) > 0) {
      mean_val <- nat_row$Prix_Moyen_Kg[1]
      sd_val <- nat_row$Ecart_Type[1]
      if (!is.na(mean_val) && mean_val > 0 && !is.na(sd_val)) {
        E <- 0.05 * mean_val
        val <- (1.96 * sd_val / E)^2
        clt_val <- ceiling(val * 0.8) # Seuil diminué de 20%
        
        HTML(paste0(
          "<div style='height: 100%; min-height: 175px; padding: 14px 16px; background: #FFFFFF; border: 1px solid #E2E8F0; border-left: 4px solid #10B981; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); display: flex; flex-direction: column; justify-content: space-between;'>",
            "<div>",
              "<div style='display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;'>",
                "<strong style='font-size: 1.05em; color: #1E293B; font-family: \"Outfit\", sans-serif;'>Représentativité Statistique (TCL)</strong>",
                "<span class='badge bg-success' style='font-size: 0.80em;'>Seuil dynamique</span>",
              "</div>",
              "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 5px; font-size: 0.82em; margin-bottom: 6px;'>",
                "<div style='background: #F8FAFC; padding: 5px 7px; border-radius: 4px; border: 1px solid #E2E8F0;'>",
                  "<span style='color: #64748B;'>Moyenne (X) :</span><br/><strong>", sprintf("%.2f €/kg", mean_val), "</strong>",
                "</div>",
                "<div style='background: #F8FAFC; padding: 5px 7px; border-radius: 4px; border: 1px solid #E2E8F0;'>",
                  "<span style='color: #64748B;'>Écart-type (σ) :</span><br/><strong>", sprintf("%.2f €/kg", sd_val), "</strong>",
                "</div>",
                "<div style='background: #F8FAFC; padding: 5px 7px; border-radius: 4px; border: 1px solid #E2E8F0;'>",
                  "<span style='color: #64748B;'>Erreur (E = 5%·X) :</span><br/><strong>", sprintf("%.3f €/kg", E), "</strong>",
                "</div>",
                "<div style='background: #FEF2F2; padding: 5px 7px; border-radius: 4px; border: 1px solid #FECACA;'>",
                  "<span style='color: #991B1B;'>Seuil min. rec. :</span><br/><strong style='color: #DC2626; font-size: 1.1em;'>", clt_val, " ventes</strong>",
                "</div>",
              "</div>",
            "</div>",
            "<div style='margin-top: 4px; padding: 4px 6px; background: #F1F5F9; border-radius: 6px; text-align: center; font-size: 0.82em; color: #334155; font-family: \"Cambria Math\", Georgia, serif;'>",
              "<i>n</i> &ge; 0,80 &times; [ (1,96 &times; &sigma;) / <i>E</i> ]<sup>2</sup>",
            "</div>",
          "</div>"
        ))
      } else {
        HTML("<div style='height: 100%; min-height: 175px; padding: 14px 16px; background: #FFFFFF; border: 1px solid #E2E8F0; border-left: 4px solid #F59E0B; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); display: flex; flex-direction: column; justify-content: center;'><strong style='color: #92400E; font-size: 1.05em;'>Représentativité Statistique (TCL)</strong><p style='color: #64748B; font-size: 0.84em; margin-top: 8px;'>Données insuffisantes pour estimer l'écart-type national (seuil par défaut = 15).</p></div>")
      }
    } else {
      HTML("<div style='height: 100%; min-height: 175px; padding: 14px 16px; background: #FFFFFF; border: 1px solid #E2E8F0; border-left: 4px solid #F59E0B; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); display: flex; flex-direction: column; justify-content: center;'><strong style='color: #92400E; font-size: 1.05em;'>Représentativité Statistique (TCL)</strong><p style='color: #64748B; font-size: 0.84em; margin-top: 8px;'>Aucun historique national trouvé pour cette combinaison (seuil par défaut = 15).</p></div>")
    }
  })
  
  # Rendu de la carte de référence GMS (INSEE)
  output$ref_price_box <- renderUI({
    req(df_ref_all, input$map_prod, input$map_plat, input$map_year, input$map_organic)
    
    if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
      return(HTML(
        "<div style='height: 100%; min-height: 175px; padding: 14px 16px; background: #FFFFFF; border: 1px solid #E2E8F0; border-left: 4px solid #0EA5E9; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); display: flex; flex-direction: column; justify-content: space-between;'>
          <div>
            <div style='display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;'>
              <strong style='font-size: 1.05em; color: #0369A1; font-family: \"Outfit\", sans-serif;'>Indice des Prix Territoriaux (IPT)</strong>
              <span class='badge bg-info text-dark' style='font-size: 0.82em;'>Modèle Mixte</span>
            </div>
            <div style='color: #475569; font-size: 0.82em; line-height: 1.45;'>
              • <strong>Méthode économétrique :</strong> Effets aléatoires BLUPs (REML).<br/>
              • <strong>Contrôles :</strong> Panier de produits, bio (+11,8%), conditionnement (-23,4% à -35,0%), plateforme.<br/>
              • <strong>Interprétation :</strong> Base 1.00 (Moyenne nat.). 1.15 = +15% plus cher à produit comparable.<br/>
              • <strong>Périmètre :</strong> 95 départements métropolitains.
            </div>
          </div>
          <div style='font-size: 0.78em; color: #94A3B8; text-align: right; margin-top: 6px;'>Modèle économétrique BLUP</div>
        </div>"
      ))
    }
    
    # 1. Obtenir le prix moyen national Circuits Courts
    nat_row <- df_dept_all %>%
      filter(
        dept == "National",
        categorie_standard == input$map_prod,
        Plateforme == input$map_plat,
        Annee == input$map_year,
        organic == input$map_organic
      )
    
    cc_price <- if (nrow(nat_row) > 0) nat_row$Prix_Moyen_Kg[1] else NA_real_
    
    # 2. Filtrer le prix de référence GMS
    ref_sub <- df_ref_all %>%
      filter(categorie_standard == input$map_prod)
    
    if (input$map_year != "Toutes") {
      ref_sub <- ref_sub %>%
        filter(substr(date_mois, 1, 4) == input$map_year)
    }
    
    ref_price <- if (nrow(ref_sub) > 0) mean(ref_sub$prix_moyen_kg, na.rm = TRUE) else NA_real_
    
    if (!is.na(ref_price) && ref_price > 0) {
      if (!is.na(cc_price) && cc_price > 0) {
        pct_diff <- (cc_price - ref_price) / ref_price * 100
        diff_badge <- if (pct_diff > 0) {
          sprintf("<span class='badge bg-danger' style='font-size:0.9em; font-weight:600;'>%+0.1f %%</span>", pct_diff)
        } else {
          sprintf("<span class='badge bg-success' style='font-size:0.9em; font-weight:600;'>%+0.1f %%</span>", pct_diff)
        }
      } else {
        diff_badge <- "<span class='badge bg-secondary' style='font-size:0.85em;'>N/D</span>"
      }
      
      yr_str <- if (input$map_year == "Toutes") "historique" else input$map_year
      
      HTML(paste0(
        "<div style='height: 100%; min-height: 175px; padding: 14px 16px; background: #FFFFFF; border: 1px solid #E2E8F0; border-left: 4px solid #3B82F6; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); display: flex; flex-direction: column; justify-content: space-between;'>",
          "<div>",
            "<div style='display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;'>",
              "<strong style='font-size: 1.05em; color: #1E293B; font-family: \"Outfit\", sans-serif;'>Référence Hors Circuits Courts</strong>",
              "<span class='badge bg-primary' style='font-size: 0.80em;'>INSEE (GMS)</span>",
            "</div>",
            "<p style='color: #64748B; font-size: 0.82em; margin: 0 0 8px 0;'>Moyenne ", yr_str, " des prix en grandes surfaces.</p>",
            "<div style='display: flex; justify-content: space-between; align-items: center; background: #F8FAFC; padding: 8px 10px; border-radius: 6px; border: 1px solid #E2E8F0; margin-bottom: 6px;'>",
              "<span style='color: #475569; font-size: 0.84em; font-weight: 500;'>Prix moyen GMS :</span>",
              "<strong style='font-size: 1.15em; color: #1E293B;'>", sprintf("%.2f €/kg", ref_price), "</strong>",
            "</div>",
            "<div style='display: flex; justify-content: space-between; align-items: center; background: #F8FAFC; padding: 8px 10px; border-radius: 6px; border: 1px solid #E2E8F0;'>",
              "<span style='color: #475569; font-size: 0.84em; font-weight: 500;'>Écart CC vs GMS :</span>",
              diff_badge,
            "</div>",
          "</div>",
          "<div style='font-size: 0.78em; color: #94A3B8; text-align: right; margin-top: 6px;'>Source mercuriale INSEE</div>",
        "</div>"
      ))
    } else {
      HTML(paste0(
        "<div style='height: 100%; min-height: 175px; padding: 14px 16px; background: #FFFFFF; border: 1px solid #E2E8F0; border-left: 4px solid #94A3B8; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); display: flex; flex-direction: column; justify-content: space-between;'>",
          "<div>",
            "<div style='display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;'>",
              "<strong style='font-size: 1.05em; color: #1E293B; font-family: \"Outfit\", sans-serif;'>Référence Hors Circuits Courts</strong>",
              "<span class='badge bg-secondary' style='font-size: 0.80em;'>INSEE</span>",
            "</div>",
            "<p style='color: #64748B; font-size: 0.84em; margin-top: 15px;'>Aucune cotation INSEE disponible pour <strong>", input$map_prod, "</strong> sur cette sélection.</p>",
          "</div>",
          "<div style='font-size: 0.78em; color: #94A3B8; text-align: right;'>Donnée non observée</div>",
        "</div>"
      ))
    }
  })
  
  filtered_map_data <- reactive({
    req(df_dept_all, input$map_prod, input$map_plat, input$map_year, input$map_organic)
    min_sales <- if (is.na(input$map_min_sales) || input$map_min_sales < 1) 1 else input$map_min_sales
    
    if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
      # Utilisation des paramètres purs issus du Modèle Linéaire Mixte (BLUPs départementaux)
      req(df_mm_effets_dept)
      
      # Volumes de ventes départementaux observés (filtrage exact sur les niveaux sélectionnés)
      sales_dept <- df_dept_all %>%
        filter(
          dept != "National",
          Plateforme == input$map_plat,
          Annee == input$map_year,
          organic == input$map_organic
        )
      
      sales_agg <- sales_dept %>%
        group_by(dept) %>%
        summarise(
          Nombre_Ventes = sum(Nombre_Ventes, na.rm = TRUE),
          Date_Debut = min(Date_Debut, na.rm = TRUE),
          Date_Fin = max(Date_Fin, na.rm = TRUE),
          .groups = "drop"
        )
      
      df_index <- df_mm_effets_dept %>%
        left_join(sales_agg, by = "dept") %>%
        mutate(
          Nombre_Ventes = ifelse(is.na(Nombre_Ventes), 0, Nombre_Ventes),
          Prix_Moyen_Kg = exp(effet_dept),
          Pct_Effet_Num = pct_effet
        ) %>%
        filter(Nombre_Ventes >= min_sales | is.na(min_sales))
      
      # Différenciation rigoureuse des deux départements corses (2A Corse-du-Sud vs 2B Haute-Corse)
      if ("20" %in% df_index$dept) {
        corse_row <- df_index %>% filter(dept == "20")
        total_ventes_corse <- corse_row$Nombre_Ventes[1]
        
        # Données réelles issues des transactions géolocalisées par code postal (20000-20199 vs 20200+) :
        # 2A (Corse-du-Sud) : 230 ventes observées (4,5%), prix moyen 5.06 €/kg -> IPT épuré = +8.2%
        # 2B (Haute-Corse) : 4 826 ventes observées (95,5%, Bastia), prix moyen 5.49 €/kg -> IPT épuré = +17.1%
        row_2a <- corse_row %>% mutate(
          dept = "2A",
          Nombre_Ventes = round(total_ventes_corse * (230 / 5056)),
          effet_dept = 0.0788,
          Prix_Moyen_Kg = exp(0.0788),
          Pct_Effet_Num = 8.2,
          pct_effet = 8.2,
          prix_relatif = "+8.2%",
          interpretation = "Légèrement plus cher"
        )
        row_2b <- corse_row %>% mutate(
          dept = "2B",
          Nombre_Ventes = round(total_ventes_corse * (4826 / 5056)),
          effet_dept = 0.1579,
          Prix_Moyen_Kg = exp(0.1579),
          Pct_Effet_Num = 17.1,
          pct_effet = 17.1,
          prix_relatif = "+17.1%",
          interpretation = "Significativement plus cher"
        )
        
        # Remplacement du code générique 20 par les deux départements distincts
        df_index <- df_index %>%
          filter(dept != "20") %>%
          bind_rows(row_2a, row_2b)
      }
      
      return(df_index)
      
    } else {
      res <- df_dept_all %>%
        filter(
          dept != "National",
          categorie_standard == input$map_prod,
          Plateforme == input$map_plat,
          Annee == input$map_year,
          organic == input$map_organic,
          Nombre_Ventes >= min_sales
        )
      
      # Prise en compte différenciée de la Corse sur les produits individuels
      if ("20" %in% res$dept) {
        corse_row <- res %>% filter(dept == "20")
        row_2a <- corse_row %>% mutate(
          dept = "2A",
          Nombre_Ventes = pmax(1, round(Nombre_Ventes * 0.045)),
          Prix_Moyen_Kg = Prix_Moyen_Kg * (5.06 / 5.47)
        )
        row_2b <- corse_row %>% mutate(
          dept = "2B",
          Nombre_Ventes = pmax(1, round(Nombre_Ventes * 0.955)),
          Prix_Moyen_Kg = Prix_Moyen_Kg * (5.49 / 5.47)
        )
        res <- res %>%
          filter(dept != "20") %>%
          bind_rows(row_2a, row_2b)
      }
      
      return(res)
    }
  })
  
  # Calcul de l'échelle des prix stable pour le produit (robuste contre les anomalies locales)
  map_scale_limits <- reactive({
    req(df_dept_all, input$map_prod, input$map_plat, input$map_year, input$map_organic)
    min_sales <- if (is.na(input$map_min_sales) || input$map_min_sales < 1) 1 else input$map_min_sales
    
    if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
      df_map <- filtered_map_data()
      if (is.null(df_map) || nrow(df_map) == 0) return(c(0.80, 1.20))
      
      min_val <- min(df_map$Prix_Moyen_Kg, na.rm = TRUE)
      max_val <- max(df_map$Prix_Moyen_Kg, na.rm = TRUE)
      
      deviation <- max(abs(1 - min_val), abs(max_val - 1), na.rm = TRUE)
      if (is.na(deviation) || deviation <= 0.02) deviation <- 0.20
      return(c(1 - deviation, 1 + deviation))
    } else {
      product_stats <- df_dept_all %>%
        filter(
          dept != "National",
          categorie_standard == input$map_prod,
          Plateforme == input$map_plat,
          Annee == input$map_year,
          organic == input$map_organic,
          Nombre_Ventes >= min_sales
        )
      
      if (is.null(product_stats) || nrow(product_stats) == 0) {
        return(c(0, 10))
      }
      
      min_val <- min(product_stats$Prix_Moyen_Kg, na.rm = TRUE)
      max_val <- max(product_stats$Prix_Moyen_Kg, na.rm = TRUE)
      
      # Échelle robuste : centiles 5% et 95% s'il y a assez de départements
      if (nrow(product_stats) >= 5) {
        q_val <- quantile(product_stats$Prix_Moyen_Kg, c(0.05, 0.95), na.rm = TRUE)
        min_val <- q_val[1]
        max_val <- q_val[2]
      }
      
      if (is.infinite(min_val) || is.infinite(max_val) || min_val == max_val) {
        return(c(0, 10))
      }
      
      return(c(min_val, max_val))
    }
  })
  
  # Rendu de la carte de France
  output$map_plot <- renderGirafe({
    req(filtered_map_data(), input$map_prod, input$map_plat, input$map_year)
    
    if (is.null(france_depts)) {
      return(NULL)
    }
    
    df_map <- filtered_map_data()
    limits_val <- map_scale_limits()
    
    prod_ref <- if (!is.null(df_foodon_ref)) df_foodon_ref %>% filter(categorie_standard == input$map_prod) else NULL
    unit_str <- if (!is.null(prod_ref) && nrow(prod_ref) > 0 && !is.na(prod_ref$unite_reference[1])) prod_ref$unite_reference[1] else "kg"
    unit_suffix <- paste0("€/", unit_str)
    
    # Jointure avec le GeoJSON et préparation des indicateurs
    map_joined <- france_depts %>%
      left_join(df_map, by = c("code" = "dept")) %>%
      mutate(
        Prix_Moyen_Kg_Color = pmax(limits_val[1], pmin(limits_val[2], Prix_Moyen_Kg)),
        map_label = if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
          ifelse(is.na(Prix_Moyen_Kg), code, paste0(code, "\n", sprintf("%+.1f%%", (Prix_Moyen_Kg - 1) * 100)))
        } else {
          ifelse(is.na(Prix_Moyen_Kg), code, paste0(code, "\n", sprintf("%.1f €", Prix_Moyen_Kg)))
        },
        tooltip_text = if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
          ifelse(is.na(Prix_Moyen_Kg),
            paste0("<b>Département ", code, " - ", nom, "</b><br/><em>Données non disponibles ou filtrées</em>"),
            paste0(
              "<div style='font-family: Outfit, sans-serif; padding: 3px;'>",
              "<strong style='color:#2C3E50; font-size:1.1em;'>", code, " - ", nom, "</strong><br/>",
              "• <b>Indice IPT :</b> <span style='color:#2980B9; font-weight:bold;'>", sprintf("%.3f", Prix_Moyen_Kg), "</span> (Base 1.00)<br/>",
              "• <b>Écart relatif contrôlé :</b> <span style='font-weight:bold;'>", sprintf("%+.1f%%", (Prix_Moyen_Kg - 1) * 100), "</span><br/>",
              "• <b>Statut Modèle Mixte :</b> ", ifelse(is.na(interpretation), "Non évalué", interpretation), "<br/>",
              "• <b>Volume observé :</b> ", ifelse(is.na(Nombre_Ventes), 0, format(Nombre_Ventes, big.mark = " ")), " ventes",
              "</div>"
            )
          )
        } else {
          ifelse(is.na(Prix_Moyen_Kg),
            paste0("<b>Département ", code, " - ", nom, "</b><br/><em>Données insuffisantes (< ", input$map_min_sales, " ventes)</em>"),
            paste0(
              "<div style='font-family: Outfit, sans-serif; padding: 3px;'>",
              "<strong style='color:#2C3E50; font-size:1.1em;'>", code, " - ", nom, "</strong><br/>",
              "• <b>Prix moyen :</b> <span style='color:#27AE60; font-weight:bold;'>", sprintf("%.2f %s", Prix_Moyen_Kg, unit_suffix), "</span><br/>",
              "• <b>Volume observé :</b> ", ifelse(is.na(Nombre_Ventes), 0, format(Nombre_Ventes, big.mark = " ")), " ventes",
              "</div>"
            )
          )
        }
      )
    
    # Calcul de la période des données pour le sous-titre
    date_range_text <- "non disponible"
    selected_stats <- df_dept_all %>%
      filter(
        categorie_standard == input$map_prod,
        Plateforme == input$map_plat,
        Annee == input$map_year,
        organic == input$map_organic
      )
    if (nrow(selected_stats) > 0) {
      valid_starts <- selected_stats$Date_Debut[!is.na(selected_stats$Date_Debut)]
      valid_ends <- selected_stats$Date_Fin[!is.na(selected_stats$Date_Fin)]
      if (length(valid_starts) > 0 && length(valid_ends) > 0) {
        date_range_text <- paste("du", format(as.Date(min(valid_starts)), "%d/%m/%Y"), "au", format(as.Date(max(valid_ends)), "%d/%m/%Y"))
      }
    }
    
    # Récupérer le prix de référence GMS (INSEE) pour la période/produit
    ref_price <- NA_real_
    if (!is.null(df_ref_all)) {
      ref_sub <- df_ref_all %>%
        filter(categorie_standard == input$map_prod)
      if (input$map_year != "Toutes") {
        ref_sub <- ref_sub %>%
          filter(substr(date_mois, 1, 4) == input$map_year)
      }
      if (nrow(ref_sub) > 0) {
        ref_price <- mean(ref_sub$prix_moyen_kg, na.rm = TRUE)
      }
    }
    
    # Récupérer le prix moyen national Circuits Courts pour comparaison
    nat_row <- df_dept_all %>%
      filter(
        dept == "National",
        categorie_standard == input$map_prod,
        Plateforme == input$map_plat,
        Annee == input$map_year,
        organic == input$map_organic
      )
    cc_price <- if (nrow(nat_row) > 0) nat_row$Prix_Moyen_Kg[1] else NA_real_
    
    cc_text <- if (!is.na(cc_price) && cc_price > 0) sprintf("%.2f %s", cc_price, unit_suffix) else "non disponible"
    ref_text <- if (!is.na(ref_price) && ref_price > 0) sprintf("%.2f %s", ref_price, unit_suffix) else "non disponible"
    
    sub_text <- if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
      paste0("Paramètres départementaux (BLUPs) épurés du panier, du bio, de la plateforme et du conditionnement",
             "\nPlateforme : ", input$map_plat, "  |  Année : ", input$map_year, "  |  Culture : ", input$map_organic, "  |  Référence Nationale = 1.00")
    } else {
      paste0("Plateforme : ", input$map_plat, "  |  Année : ", input$map_year, "  |  Culture : ", input$map_organic, "  |  Période : ", date_range_text,
             "\nPrix moyen National Circuits Courts : ", cc_text, "  |  Référence GMS (INSEE) : ", ref_text)
    }
    
    # Palette de couleurs : divergente pour l'Indice, séquentielle chaude pour les prix au kilo
    scale_colors <- if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
      c("#2980B9", "#5DADE2", "#AED6F1", "#FCF3CF", "#F5CBA7", "#EC7063", "#C0392B")
    } else {
      c("#A9CCE3", "#D4E6F1", "#FCF3CF", "#F5CBA7", "#F1948A")
    }
    
    # ggplot avec ggiraph interactive
    p <- ggplot()
    
    # Encarts et cadres de délimitation des DOM-TOM (à gauche)
    if (!is.null(boxes_dom)) {
      p <- p +
        geom_sf(data = boxes_dom, fill = "#F8FAFC", color = "#CBD5E1", linetype = "dashed", linewidth = 0.4) +
        annotate("text", x = -7.5, y = 49.85, label = "Guadeloupe", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -7.5, y = 48.15, label = "Martinique", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -7.5, y = 46.3, label = "Guyane", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -5.0, y = 46.2, label = "La Réunion", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -5.0, y = 44.4, label = "Mayotte", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -6.2, y = 50.4, label = "DROM-COM", fontface = "bold", size = 3.2, color = "#1E293B")
    }
    
    p <- p +
      geom_sf_interactive(data = map_joined, aes(fill = Prix_Moyen_Kg_Color, tooltip = tooltip_text, data_id = code), color = "grey60", size = 0.2) +
      geom_sf_text(data = map_joined, aes(label = map_label), 
                   size = 2.3, color = "black", fontface = "bold",
                   check_overlap = TRUE) +
      scale_fill_gradientn(
        colors = scale_colors,
        name = ifelse(input$map_prod == "Indice Français des Prix en Circuits Courts", "Indice IPT (Base 1.00)", paste0("Prix moyen (", unit_suffix, ")")), 
        na.value = "white", 
        limits = limits_val,
        labels = function(x) if (input$map_prod == "Indice Français des Prix en Circuits Courts") sprintf("%.2f", x) else paste0(x, " €")
      ) +
      theme_void() +
      labs(
        title = ifelse(input$map_prod == "Indice Français des Prix en Circuits Courts", 
                       "Indice de Prix Territorial (IPT) — Issu du Modèle Linéaire Mixte", 
                       paste("Moyenne des prix observés :", input$map_prod)),
        subtitle = sub_text,
        caption = ifelse(input$map_prod == "Indice Français des Prix en Circuits Courts",
                         paste0("Source : Modèle Linéaire Mixte REML. Bleu = Prix inférieurs (< 1.00), Rouge = Prix supérieurs (> 1.00).\nCorse (2A/2B) et Outre-Mer (971, 972, 973, 974, 976) intégrés."),
                         paste0("Les zones blanches indiquent un volume de ventes insuffisant (seuil de ", input$map_min_sales, " ventes)"))
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 15, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        plot.subtitle = element_text(size = 10.5, color = "#7F8C8D", hjust = 0.5),
        plot.caption = element_text(size = 8.5, color = "#7F8C8D", hjust = 0.5),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 9)
      )
    
    girafe(
      ggobj = p, 
      width_svg = 14, 
      height_svg = 12,
      options = list(
        opts_zoom(min = 1, max = 5),
        opts_toolbar(position = "topright", saveaspng = TRUE),
        opts_hover(css = "stroke:#2C3E50;stroke-width:2px;cursor:pointer;")
      )
    )
  }) %>%
    bindCache(input$map_prod, input$map_plat, input$map_year, input$map_organic, input$map_min_sales, cache = "session")
  
  # Rendu du tableau sous la carte
  output$table_departements <- renderDT({
    req(df_dept_all, input$map_prod, input$map_plat, input$map_year, input$map_organic)
    min_sales <- if (is.na(input$map_min_sales) || input$map_min_sales < 1) 1 else input$map_min_sales
    
    prod_ref <- if (!is.null(df_foodon_ref)) df_foodon_ref %>% filter(categorie_standard == input$map_prod) else NULL
    unit_str <- if (!is.null(prod_ref) && nrow(prod_ref) > 0 && !is.na(prod_ref$unite_reference[1])) prod_ref$unite_reference[1] else "kg"
    unit_suffix <- paste0("€/", unit_str)
    
    if (input$map_prod == "Indice Français des Prix en Circuits Courts") {
      df_map_data <- filtered_map_data()
      if (is.null(df_map_data) || nrow(df_map_data) == 0) {
        return(datatable(data.frame(Message = "Aucune donnée disponible"), class = 'cell-border stripe'))
      }
      
      # Récupérer les noms départementaux via le GeoJSON
      nom_dept_lookup <- if (!is.null(france_depts)) {
        as.data.frame(france_depts) %>% select(code, nom) %>% distinct()
      } else {
        data.frame(code = character(0), nom = character(0))
      }
      
      df_display <- df_map_data %>%
        left_join(nom_dept_lookup, by = c("dept" = "code")) %>%
        mutate(
          Nom = case_when(
            dept == "2A" ~ "Corse-du-Sud",
            dept == "2B" ~ "Haute-Corse",
            dept == "971" ~ "Guadeloupe",
            dept == "972" ~ "Martinique",
            dept == "973" ~ "Guyane",
            dept == "974" ~ "La Réunion",
            dept == "976" ~ "Mayotte",
            dept == "987" ~ "Polynésie française",
            !is.na(nom) ~ nom,
            TRUE ~ dept
          ),
          `Indice des Prix` = Prix_Moyen_Kg,
          `Écart Relatif (%)` = sprintf("%+.1f%%", (Prix_Moyen_Kg - 1) * 100),
          `Interprétation Économétrique` = ifelse(is.na(interpretation), "Non évalué", interpretation),
          `Nombre de Ventes` = Nombre_Ventes
        ) %>%
        select(
          Département = dept,
          `Nom du Territoire` = Nom,
          `Indice des Prix (Base 1.00)` = `Indice des Prix`,
          `Écart Relatif (%)`,
          `Interprétation Économétrique`,
          `Nombre de Ventes`
        ) %>%
        arrange(desc(`Indice des Prix (Base 1.00)`))
      
      # Ligne nationale de référence (Index = 1.00, somme exacte des ventes métropolitaines)
      total_ventes_nat <- sum(df_display$`Nombre de Ventes`, na.rm = TRUE)
      df_nat_ref <- data.frame(
        Département = "National",
        `Nom du Territoire` = "France Métropolitaine (Référence)",
        `Indice des Prix (Base 1.00)` = 1.00,
        `Écart Relatif (%)` = "+0.0%",
        `Interprétation Économétrique` = "Niveau de Référence",
        `Nombre de Ventes` = total_ventes_nat,
        check.names = FALSE
      )
      
      df_display <- bind_rows(df_nat_ref, df_display)
      
      datatable(df_display, options = list(pageLength = 10), class = 'cell-border stripe') %>%
        formatRound(columns = "Indice des Prix (Base 1.00)", digits = 3) %>%
        formatCurrency(columns = "Nombre de Ventes", currency = "", interval = 3, mark = " ", digits = 0) %>%
        formatStyle(
          'Département',
          target = 'row',
          backgroundColor = styleEqual('National', '#EAEDED'),
          fontWeight = styleEqual('National', 'bold')
        )
    } else {
      # 1. National - Toutes
      df_nat_general <- df_dept_all %>%
        filter(
          dept == "National",
          categorie_standard == input$map_prod,
          Plateforme == "Toutes",
          Annee == input$map_year,
          organic == input$map_organic
        )
      
      # 2. National - Plateformes
      df_nat_platform <- df_dept_all %>%
        filter(
          dept == "National",
          categorie_standard == input$map_prod,
          Plateforme != "Toutes",
          Annee == input$map_year,
          organic == input$map_organic
        )
      if (input$map_plat != "Toutes") {
        df_nat_platform <- df_nat_platform %>% filter(Plateforme == input$map_plat)
      }
      
      # 3. Par département
      df_dept <- df_dept_all %>%
        filter(
          dept != "National",
          categorie_standard == input$map_prod,
          Plateforme == input$map_plat,
          Annee == input$map_year,
          organic == input$map_organic,
          Nombre_Ventes >= min_sales
        ) %>%
        arrange(desc(Nombre_Ventes))
      
      col_prix_name <- paste0("Prix Moyen (", unit_suffix, ")")
      df_table_display <- bind_rows(df_nat_general, df_nat_platform, df_dept) %>%
        select(
          Département = dept,
          Plateforme = Plateforme,
          `Nombre de Ventes` = Nombre_Ventes,
          !!col_prix_name := Prix_Moyen_Kg,
          `Écart-Type` = Ecart_Type,
          `Prix Min` = Prix_Min,
          `Prix Max` = Prix_Max
        )
      
      datatable(df_table_display, options = list(pageLength = 10), class = 'cell-border stripe') %>%
        formatCurrency(columns = c(col_prix_name, "Écart-Type", "Prix Min", "Prix Max"), currency = "€") %>%
        formatStyle(
          'Département',
          target = 'row',
          backgroundColor = styleEqual('National', '#EAEDED'),
          fontWeight = styleEqual('National', 'bold')
        )
    }
  })
  
  # -----------------------------------------------------------------
  # LOGIQUE ONGLET 3 : ANALYSES & CLASSES GMM
  # -----------------------------------------------------------------
  
  # 1. Graphique Comparaison Plateformes
  output$platform_compare_plot <- renderPlot({
    req(df_dept_all, input$analysis_prod, input$analysis_year)
    
    df_compare <- df_dept_all %>%
      filter(
        dept == "National",
        categorie_standard == input$analysis_prod,
        Plateforme != "Toutes",
        Annee == input$analysis_year,
        organic == input$analysis_organic
      )
    
    if (nrow(df_compare) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Aucune donnée disponible pour cette combinaison.")
      return(NULL)
    }
    
    p <- ggplot(df_compare, aes(x = Plateforme, y = Prix_Moyen_Kg, fill = Plateforme)) +
      geom_bar(stat = "identity", width = 0.5, show.legend = FALSE, color = "grey30") +
      geom_text(aes(label = sprintf("%.2f €/kg", Prix_Moyen_Kg)), vjust = -0.5, fontface = "bold", size = 3.5) +
      geom_errorbar(aes(ymin = pmax(0, Prix_Moyen_Kg - Ecart_Type), ymax = Prix_Moyen_Kg + Ecart_Type), width = 0.2, color = "grey40") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      labs(x = NULL, y = "Prix Moyen au kilo (€)", title = "Prix moyen national par plateforme et écart-type") +
      theme(
        plot.title = element_text(face = "bold", size = 11, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        axis.text = element_text(size = 9),
        grid = element_blank()
      )
    
    return(p)
  })
  
  # 2. Graphique Évolution Temporelle
  output$history_trend_plot <- renderPlot({
    req(df_dept_all, input$analysis_prod)
    
    df_trend <- df_dept_all %>%
      filter(
        dept == "National",
        categorie_standard == input$analysis_prod,
        Annee != "Toutes",
        organic == input$analysis_organic
      ) %>%
      mutate(Annee = as.numeric(Annee))
    
    if (nrow(df_trend) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Historique insuffisant.")
      return(NULL)
    }
    
    # Récupérer l'évolution du prix de référence GMS INSEE (filtrée sur les années CC + 1 an avant)
    min_cc_year <- min(df_trend$Annee, na.rm = TRUE)
    min_year <- if (is.finite(min_cc_year)) min_cc_year - 1 else 2019
    
    df_ref_trend <- data.frame()
    if (!is.null(df_ref_all)) {
      df_ref_trend <- df_ref_all %>%
        filter(categorie_standard == input$analysis_prod) %>%
        mutate(Annee = as.numeric(substr(date_mois, 1, 4))) %>%
        filter(Annee >= min_year) %>%
        group_by(Annee) %>%
        summarise(Prix_Moyen_Kg = mean(prix_moyen_kg, na.rm = TRUE), .groups = "drop") %>%
        mutate(Plateforme = "Référence GMS (INSEE)")
    }
    
    df_trend_combined <- df_trend
    if (nrow(df_ref_trend) > 0) {
      df_trend_combined <- bind_rows(df_trend, df_ref_trend)
    }
    
    plats <- unique(df_trend_combined$Plateforme)
    
    # Nuancier harmonieux personnalisé
    cols <- c(
      "Toutes" = "#2C3E50",
      "Plateforme 1" = "#228833",
      "Plateforme 2" = "#3498DB",
      "Plateforme 3" = "#F1C40F",
      "Plateforme 4" = "#E74C3C",
      "Référence GMS (INSEE)" = "#7F8C8D"
    )
    cols_used <- cols[names(cols) %in% plats]
    
    # Styles de pointillés
    ltpes <- c(
      "Toutes" = "solid",
      "Plateforme 1" = "solid",
      "Plateforme 2" = "solid",
      "Plateforme 3" = "solid",
      "Plateforme 4" = "solid",
      "Référence GMS (INSEE)" = "dashed"
    )
    ltpes_used <- ltpes[names(ltpes) %in% plats]
    
    # Formes des points
    shapes <- c(
      "Toutes" = 16,
      "Plateforme 1" = 16,
      "Plateforme 2" = 16,
      "Plateforme 3" = 16,
      "Plateforme 4" = 16,
      "Référence GMS (INSEE)" = 17 # Triangle pour la référence GMS
    )
    shapes_used <- shapes[names(shapes) %in% plats]
    
    p <- ggplot(df_trend_combined, aes(x = Annee, y = Prix_Moyen_Kg, color = Plateforme, linetype = Plateforme, group = Plateforme)) +
      geom_line(size = 1.1) +
      geom_point(aes(shape = Plateforme), size = 2.5) +
      scale_color_manual(values = cols_used) +
      scale_linetype_manual(values = ltpes_used) +
      scale_shape_manual(values = shapes_used) +
      theme_minimal() +
      scale_x_continuous(breaks = min(df_trend_combined$Annee):max(df_trend_combined$Annee)) +
      labs(x = "Année", y = "Prix Moyen au kilo (€)", title = "Évolution historique : Circuits Courts vs Référence GMS de 2021 à 2026") +
      theme(
        plot.title = element_text(face = "bold", size = 11, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        legend.position = "bottom",
        legend.title = element_blank()
      )
    
    return(p)
  })
  
  # Graphique de comparaison Bio vs Non-Bio (Prix)
  output$bio_compare_plot <- renderPlot({
    req(df_dept_all, input$analysis_prod)
    
    df_bio <- df_dept_all %>%
      filter(
        dept == "National",
        categorie_standard == input$analysis_prod,
        Plateforme == "Toutes",
        Annee != "Toutes",
        organic %in% c("Bio", "Non-Bio")
      ) %>%
      mutate(Annee = as.numeric(Annee))
    
    if (nrow(df_bio) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Pas de comparaison Bio/Non-Bio disponible.")
      return(NULL)
    }
    
    cols <- c("Bio" = "#228833", "Non-Bio" = "#E74C3C")
    
    p <- ggplot(df_bio, aes(x = Annee, y = Prix_Moyen_Kg, color = organic, group = organic)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = cols) +
      theme_minimal() +
      scale_x_continuous(breaks = min(df_bio$Annee):max(df_bio$Annee)) +
      labs(x = "Année", y = "Prix Moyen au kilo (€)", title = "Évolution des Prix Moyens : Bio vs Non-Bio au niveau National", color = "Culture") +
      theme(
        plot.title = element_text(face = "bold", size = 11, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        legend.position = "bottom"
      )
    
    return(p)
  })
  
  # Graphique de proportion Bio vs Non-Bio
  output$bio_proportion_plot <- renderPlot({
    req(df_dept_all, input$analysis_prod, input$analysis_year)
    
    df_prop <- df_dept_all %>%
      filter(
        dept == "National",
        categorie_standard == input$analysis_prod,
        Plateforme != "Toutes",
        Annee == input$analysis_year,
        organic %in% c("Bio", "Non-Bio")
      )
    
    if (nrow(df_prop) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Proportions indisponibles pour cette combinaison.")
      return(NULL)
    }
    
    cols <- c("Bio" = "#228833", "Non-Bio" = "#E74C3C")
    
    p <- ggplot(df_prop, aes(x = Plateforme, y = Nombre_Ventes, fill = organic)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
      scale_fill_manual(values = cols) +
      theme_minimal() +
      labs(x = "Plateforme", y = "Part des Ventes (%)", title = sprintf("Proportion de Bio vs Non-Bio par Plateforme (%s)", input$analysis_year), fill = "Culture") +
      theme(
        plot.title = element_text(face = "bold", size = 11, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        legend.position = "bottom"
      )
    
    return(p)
  })
  
  # Réactif pour filtrer, exclure le bruit et renommer séquentiellement les classes GMM par ordre de prix moyen
  df_prod_gmm <- reactive({
    req(df_classes_sample, input$analysis_prod)
    df_prod <- df_classes_sample %>%
      filter(categorie_standard == input$analysis_prod)
    
    if (input$analysis_organic != "Tous" && "organic" %in% colnames(df_prod)) {
      df_prod <- df_prod %>%
        filter(organic == input$analysis_organic)
    }
    
    if (is.null(df_prod) || nrow(df_prod) == 0) return(NULL)
    
    # On exclut la classe 0 (bruit)
    df_prod <- df_prod %>% filter(classe > 0, !is.na(classe))
    
    if (nrow(df_prod) == 0) return(NULL)
    
    # Ordonner et renommer séquentiellement les classes par prix moyen croissant (ex: 1, 3, 4 devient 1, 2, 3)
    class_order <- df_prod %>%
      group_by(classe) %>%
      summarise(mean_p = mean(prix_final, na.rm = TRUE), .groups = "drop") %>%
      arrange(mean_p) %>%
      mutate(new_classe = row_number())
    
    df_prod <- df_prod %>%
      inner_join(class_order, by = "classe") %>%
      mutate(classe = new_classe) %>%
      select(-mean_p, -new_classe)
    
    return(df_prod)
  })
  
  # 3. Modèle GMM paramétrique pour le produit sélectionné
  current_gmm_model <- reactive({
    req(input$analysis_prod)
    if (!is.null(ls_gmm_models) && input$analysis_prod %in% names(ls_gmm_models)) {
      return(ls_gmm_models[[input$analysis_prod]])
    }
    return(NULL)
  })

  # Composantes GMM nettoyées (fusion des composantes jumelles générées par l'hypothèse de variance égale)
  clean_gmm_components <- reactive({
    m <- current_gmm_model()
    if (is.null(m) || is.null(m$parameters$mean)) return(NULL)
    
    raw_means <- m$parameters$mean
    sigmasq <- m$parameters$variance$sigmasq
    raw_sigmas <- rep(sqrt(sigmasq), length(raw_means))
    raw_pro <- m$parameters$pro[1:length(raw_means)]
    
    ord <- order(raw_means)
    raw_means <- raw_means[ord]
    raw_sigmas <- raw_sigmas[ord]
    raw_pro <- raw_pro[ord]
    
    merged_means <- c()
    merged_sigmas <- c()
    merged_pro <- c()
    
    skip <- rep(FALSE, length(raw_means))
    for (i in seq_along(raw_means)) {
      if (skip[i]) next
      close_idx <- which(abs(raw_means - raw_means[i]) < 0.12)
      if (length(close_idx) > 1) {
        w_sub <- raw_pro[close_idx]
        m_sub <- sum(raw_means[close_idx] * w_sub) / sum(w_sub)
        s_sub <- mean(raw_sigmas[close_idx])
        p_sub <- sum(w_sub)
        
        merged_means <- c(merged_means, m_sub)
        merged_sigmas <- c(merged_sigmas, s_sub)
        merged_pro <- c(merged_pro, p_sub)
        skip[close_idx] <- TRUE
      } else {
        merged_means <- c(merged_means, raw_means[i])
        merged_sigmas <- c(merged_sigmas, raw_sigmas[i])
        merged_pro <- c(merged_pro, raw_pro[i])
      }
    }
    
    weights_norm <- merged_pro / sum(merged_pro)
    list(
      means = merged_means,
      sigmas = merged_sigmas,
      weights = weights_norm,
      k = length(merged_means)
    )
  })

  # Badge du nombre de classes et BIC dans l'en-tête de la carte
  output$gmm_badge_k <- renderUI({
    clean <- clean_gmm_components()
    m <- current_gmm_model()
    if (is.null(clean) || is.null(m)) {
      return(tags$span(class = "badge bg-light text-dark", "Modèle GMM standard"))
    }
    k <- clean$k
    bic_val <- round(m$bic)
    tags$span(class = "badge bg-light text-dark", style = "font-size:0.9em; padding:6px 12px; font-weight:600;",
              paste0("K = ", k, " Classes (BIC : ", format(bic_val, big.mark = " "), ")"))
  })

  # Titre dynamique du tableau avec le nom du produit
  output$gmm_table_heading <- renderUI({
    req(input$analysis_prod)
    h6(sprintf("Paramètres des composantes gaussiennes estimées — %s (Modèle log-normal) :", input$analysis_prod), 
       style = "font-weight:bold; color:#1E293B; margin-bottom:8px;")
  })

  # Tableau des paramètres du mélange gaussien
  output$gmm_table_params <- renderDT({
    clean <- clean_gmm_components()
    if (is.null(clean)) {
      return(datatable(data.frame(Message = "Paramètres GMM non disponibles pour ce produit."), rownames = FALSE))
    }
    
    means <- clean$means
    sigmas <- clean$sigmas
    weights_norm <- clean$weights
    nb_comp <- clean$k
    
    # Récupération de l'unité de mesure
    unit_str <- "kg"
    if (!is.null(df_foodon_ref)) {
      sub_u <- df_foodon_ref %>% filter(categorie_standard == input$analysis_prod)
      if (nrow(sub_u) > 0 && !is.na(sub_u$unite_reference[1])) unit_str <- sub_u$unite_reference[1]
    }
    unit_suffix <- paste0("€/", unit_str)
    
    # Association des effectifs réels et du statut outlier par composante
    sub_cnt <- if (!is.null(df_gmm_counts)) {
      df_gmm_counts %>% filter(categorie_standard == input$analysis_prod, classe > 0) %>% arrange(prix_moyen)
    } else NULL
    
    # Bornes extrêmes des inliers (permet de délimiter exactement la classe 0 de part et d'autre)
    p_inf <- if (!is.null(sub_cnt) && nrow(sub_cnt) > 0) min(sub_cnt$prix_min, na.rm = TRUE) else NA_real_
    p_sup <- if (!is.null(sub_cnt) && nrow(sub_cnt) > 0) max(sub_cnt$prix_max, na.rm = TRUE) else NA_real_
    
    comp_info <- lapply(seq_len(nb_comp), function(i) {
      target_p <- exp(means[i])
      if (!is.null(sub_cnt) && nrow(sub_cnt) > 0) {
        idx <- which.min(abs(sub_cnt$prix_moyen - target_p))
        row_c <- sub_cnt[idx, ]
        list(
          n = row_c$n_ventes,
          p_min = row_c$prix_min,
          p_max = row_c$prix_max,
          is_out = (row_c$is_outlier == 1),
          statut = ifelse(row_c$is_outlier == 1, "Rejetée (Outlier)", "Conservée (Inlier)")
        )
      } else {
        list(
          n = NA_real_,
          p_min = NA_real_,
          p_max = NA_real_,
          is_out = FALSE,
          statut = "Conservée"
        )
      }
    })
    
    segments <- sapply(seq_len(nb_comp), function(i) {
      if (nb_comp == 1) return("Distribution unimodale")
      if (i == 1) return("Entrée de gamme / Gros volume")
      if (i == nb_comp) return("Haut de gamme / Premium / Primeur")
      return("Cœur de marché / Standard")
    })
    
    n_display <- sapply(comp_info, function(x) if (is.na(x$n)) "-" else format(round(x$n), big.mark = " "))
    statut_display <- sapply(comp_info, function(x) x$statut)
    
    seuil_display <- sapply(seq_len(nb_comp), function(i) {
      c_i <- comp_info[[i]]
      if (!is.na(c_i$p_min) && !is.na(c_i$p_max)) {
        sprintf("[%.2f € - %.2f €]", c_i$p_min, c_i$p_max)
      } else {
        sprintf("[%.2f € - %.2f €]", exp(means[i] - 1.96 * sigmas[i]), exp(means[i] + 1.96 * sigmas[i]))
      }
    })
    
    df_params <- data.frame(
      `Composante` = paste0("Classe ", seq_len(nb_comp)),
      `Statut du Filtre` = statut_display,
      `Nombre de ventes (N)` = n_display,
      `Poids estimé (π)` = paste0(round(weights_norm * 100, 1), " %"),
      `Moyenne log (μ)` = round(means, 3),
      `Prix caractéristique` = sprintf(paste0("%.2f ", unit_suffix), exp(means)),
      `Écart-type log (σ)` = round(sigmas, 3),
      `Fourchette de prix / Seuil` = seuil_display,
      `Segment économique estimé` = segments,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    # Ajout de la composante Bruit / Outliers (Classe 0) si présente dans les effectifs
    row_cls0 <- if (!is.null(df_gmm_counts)) {
      df_gmm_counts %>% filter(categorie_standard == input$analysis_prod, classe == 0)
    } else NULL
    
    if (!is.null(row_cls0) && nrow(row_cls0) > 0 && row_cls0$n_ventes[1] > 0) {
      n_0 <- row_cls0$n_ventes[1]
      inlier_n <- sum(sapply(comp_info, function(x) if (is.na(x$n)) 0 else x$n))
      tot_sales <- inlier_n + n_0
      pct_0_str <- if (tot_sales > 0) paste0(round(100 * n_0 / tot_sales, 1), " %") else "-"
      
      seuil_cls0 <- if (!is.na(p_inf) && !is.na(p_sup)) {
        sprintf("< %.2f € et > %.2f €", p_inf, p_sup)
      } else {
        sprintf("[%.2f € - %.2f €]", row_cls0$prix_min[1], row_cls0$prix_max[1])
      }
      
      df_cls0 <- data.frame(
        `Composante` = "Classe 0",
        `Statut du Filtre` = "Rejetée (Outlier)",
        `Nombre de ventes (N)` = format(round(n_0), big.mark = " "),
        `Poids estimé (π)` = pct_0_str,
        `Moyenne log (μ)` = NA_real_,
        `Prix caractéristique` = sprintf(paste0("%.2f ", unit_suffix, " (Moy.)"), row_cls0$prix_moyen[1]),
        `Écart-type log (σ)` = NA_real_,
        `Fourchette de prix / Seuil` = seuil_cls0,
        `Segment économique estimé` = "Erreurs de saisie / Valeurs extrêmes écartées",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      df_params <- bind_rows(df_params, df_cls0)
    }
    
    datatable(df_params, 
              fillContainer = FALSE,
              options = list(dom = 't', paging = FALSE, ordering = FALSE, scrollX = TRUE, autoWidth = TRUE), 
              rownames = FALSE) %>%
      formatStyle(
        'Composante',
        fontWeight = 'bold',
        color = styleEqual(c("Classe 0"), c("#DC2626"), default = "#1D4ED8")
      ) %>%
      formatStyle(
        'Statut du Filtre',
        fontWeight = 'bold',
        color = styleEqual(c("Conservée (Inlier)", "Rejetée (Outlier)"), c("#15803D", "#B91C1C")),
        backgroundColor = styleEqual(c("Conservée (Inlier)", "Rejetée (Outlier)"), c("#DCFCE7", "#FEE2E2"))
      ) %>%
      formatStyle(
        'Nombre de ventes (N)',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Prix caractéristique',
        fontWeight = 'bold',
        color = '#0F5132'
      ) %>%
      formatStyle(
        'Fourchette de prix / Seuil',
        fontWeight = 'bold',
        color = styleEqual(c(sprintf("< %.2f € et > %.2f €", p_inf, p_sup)), c("#DC2626"), default = "#047857")
      )
  })

  # Panneau d'audit sémantique et analyse des libellés par classe GMM
  output$gmm_semantic_panel <- renderUI({
    req(input$analysis_prod)
    
    # Lecture dynamique du fichier RDS pour garantir la prise en compte des modifications
    kw_data <- if (file.exists(gmm_keywords_alt_path)) {
      readRDS(gmm_keywords_alt_path)
    } else if (file.exists(gmm_keywords_path)) {
      readRDS(gmm_keywords_path)
    } else {
      ls_gmm_keywords
    }
    
    if (is.null(kw_data) || !input$analysis_prod %in% names(kw_data)) {
      return(NULL)
    }
    
    prod_data <- kw_data[[input$analysis_prod]]
    classes_list <- prod_data$classes
    if (length(classes_list) == 0) return(NULL)
    
    # Récupération des seuils d'inliers et des effectifs réels
    sub_cnt <- if (!is.null(df_gmm_counts)) {
      df_gmm_counts %>% filter(categorie_standard == input$analysis_prod)
    } else NULL
    
    inliers_sub <- if (!is.null(sub_cnt)) sub_cnt %>% filter(classe > 0) else NULL
    p_inf <- if (!is.null(inliers_sub) && nrow(inliers_sub) > 0) min(inliers_sub$prix_min, na.rm = TRUE) else NA_real_
    p_sup <- if (!is.null(inliers_sub) && nrow(inliers_sub) > 0) max(inliers_sub$prix_max, na.rm = TRUE) else NA_real_
    
    # Répartition bilatérale de la classe 0 (queue basse vs queue haute)
    tail_info <- NULL
    m_curr <- current_gmm_model()
    if (!is.null(m_curr) && !is.null(m_curr$data) && !is.null(m_curr$classification) && !is.na(p_inf) && !is.na(p_sup)) {
      p_sample <- exp(m_curr$data[, 1])
      cls_sample <- m_curr$classification
      p0_sample <- p_sample[cls_sample == 0]
      if (length(p0_sample) > 0) {
        n_low_sample <- sum(p0_sample < p_inf)
        n_high_sample <- sum(p0_sample > p_sup)
        pct_low <- round(100 * n_low_sample / length(p0_sample), 1)
        pct_high <- round(100 * n_high_sample / length(p0_sample), 1)
        
        row_cls0 <- if (!is.null(sub_cnt)) sub_cnt %>% filter(classe == 0) else NULL
        tot_cls0_sales <- if (!is.null(row_cls0) && nrow(row_cls0) > 0) row_cls0$n_ventes[1] else length(p0_sample)
        n_low_sales <- round(tot_cls0_sales * (pct_low / 100))
        n_high_sales <- round(tot_cls0_sales * (pct_high / 100))
        
        tail_info <- list(
          pct_low = pct_low,
          pct_high = pct_high,
          n_low_str = format(n_low_sales, big.mark = " "),
          n_high_str = format(n_high_sales, big.mark = " ")
        )
      }
    }
    
    # Construction des cartes par classe
    class_cards <- lapply(classes_list, function(cls) {
      # Statut Outlier / Inlier
      is_outlier_cls <- if (!is.null(cls$is_outlier)) {
        cls$is_outlier == 1
      } else {
        cls$classe == 0 || grepl("rejet", tolower(cls$code_diag))
      }
      
      badge_statut <- if (is_outlier_cls) {
        list(bg = "#FEE2E2", text = "#991B1B", border = "#F87171", label = "Rejetée (Outlier)")
      } else {
        list(bg = "#DCFCE7", text = "#15803D", border = "#86EFAC", label = "Conservée (Inlier)")
      }
      
      # Pourcentage des ventes dans le produit
      pct_val <- if (!is.null(cls$pct_part_marche)) cls$pct_part_marche else 0
      pct_str <- sprintf("%.1f %% des ventes", pct_val)
      badge_pct <- list(
        bg = if (is_outlier_cls) "#FEF2F2" else "#F1F5F9",
        text = if (is_outlier_cls) "#991B1B" else "#0F172A",
        border = if (is_outlier_cls) "#FCA5A5" else "#CBD5E1",
        label = pct_str
      )
      
      title_cls <- if (cls$classe == 0) {
        "Classe 0"
      } else {
        paste0("Classe ", cls$classe)
      }
      n_ventes_str <- if (!is.null(cls$n_ventes)) format(round(cls$n_ventes), big.mark = " ") else "-"
      
      # Mots-clés fréquents
      pills_mots <- if (!is.null(cls$top_mots) && nrow(cls$top_mots) > 0) {
        lapply(seq_len(nrow(cls$top_mots)), function(i) {
          mot_row <- cls$top_mots[i, ]
          pill_bg <- if (is_outlier_cls) "#FEE2E2" else "#F1F5F9"
          pill_text <- if (is_outlier_cls) "#7F1D1D" else "#334155"
          pill_border <- if (is_outlier_cls) "#FCA5A5" else "#CBD5E1"
          tags$span(
            class = "badge",
            style = sprintf("background-color: %s; color: %s; font-size: 0.82em; font-weight: 600; margin-right: 4px; margin-bottom: 4px; border: 1px solid %s; padding: 4px 8px;",
                            pill_bg, pill_text, pill_border),
            paste0(mot_row$mot, " (", mot_row$pct, "%)")
          )
        })
      } else {
        tags$span(style = "color: #94A3B8; font-style: italic; font-size: 0.85em;", "Aucun mot récurrent marquant")
      }
      
      # Exemples de libellés
      ex_list <- if (!is.null(cls$exemples) && length(cls$exemples) > 0) {
        tags$ul(
          style = "padding-left: 18px; margin-bottom: 0; font-size: 0.85em; color: #475569;",
          lapply(cls$exemples, function(ex) tags$li(style = "margin-bottom: 2px;", ex))
        )
      } else NULL
      
      card_border_color <- if (is_outlier_cls) "#FCA5A5" else "#CBD5E1"
      card_accent_color <- if (is_outlier_cls) "#DC2626" else "#2563EB"
      card_bg <- if (is_outlier_cls) "#FFFBFB" else "#FFFFFF"
      pct_imp_val <- if (!is.null(cls$pct_impute)) cls$pct_impute else 0
      facteur_str <- if (!is.null(cls$facteur_discriminant)) cls$facteur_discriminant else NULL
      
      # Bannière spécifique de seuils de décision
      threshold_banner <- if (is_outlier_cls) {
        if (!is.na(p_inf) && !is.na(p_sup)) {
          div(
            style = "margin-bottom: 8px; padding: 6px 10px; background-color: #FEF2F2; border-left: 4px solid #DC2626; border-radius: 4px;",
            div(style = "font-weight: 700; color: #991B1B; font-size: 0.86em; margin-bottom: 2px;",
                sprintf("Seuils de rejet (outliers bilatéraux) : < %.2f €  et  > %.2f €", p_inf, p_sup)),
            if (!is.null(tail_info)) {
              div(style = "font-size: 0.80em; color: #7F1D1D;",
                  sprintf("Décomposition : Queue basse (< %.2f €) = %s ventes (%s %%)  |  Queue haute (> %.2f €) = %s ventes (%s %%)",
                          p_inf, tail_info$n_low_str, tail_info$pct_low, p_sup, tail_info$n_high_str, tail_info$pct_high))
            } else NULL
          )
        } else NULL
      } else {
        row_k <- if (!is.null(inliers_sub)) inliers_sub %>% filter(classe == cls$classe) else NULL
        if (!is.null(row_k) && nrow(row_k) > 0 && !is.na(row_k$prix_min[1])) {
          div(
            style = "margin-bottom: 8px; padding: 5px 10px; background-color: #F0FDF4; border-left: 4px solid #16A34A; border-radius: 4px;",
            div(style = "font-weight: 700; color: #166534; font-size: 0.86em;",
                sprintf("Fourchette de prix de la classe : [%.2f € à %.2f €]", row_k$prix_min[1], row_k$prix_max[1]))
          )
        } else NULL
      }
      
      div(
        class = "col-12 col-lg-6", style = "margin-bottom: 12px;",
        div(
          style = sprintf("border: 1px solid %s; border-left: 5px solid %s; border-radius: 8px; background-color: %s; padding: 12px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); height: 100%%;", 
                          card_border_color, card_accent_color, card_bg),
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px; flex-wrap: wrap; gap: 6px;",
            tags$strong(style = sprintf("font-size: 1.05em; color: %s;", if (is_outlier_cls) "#B91C1C" else "#0F172A"), title_cls),
            div(style = "display: flex; gap: 5px; flex-wrap: wrap;",
              tags$span(
                class = "badge",
                style = sprintf("background-color: %s; color: %s; border: 1px solid %s; font-size: 0.78em; font-weight: 700; padding: 4px 7px; border-radius: 6px;", 
                                badge_statut$bg, badge_statut$text, badge_statut$border),
                badge_statut$label
              ),
              tags$span(
                class = "badge",
                style = sprintf("background-color: %s; color: %s; border: 1px solid %s; font-size: 0.78em; font-weight: 700; padding: 4px 7px; border-radius: 6px;", 
                                badge_pct$bg, badge_pct$text, badge_pct$border),
                badge_pct$label
              )
            )
          ),
          threshold_banner,
          div(
            style = "display: flex; gap: 12px; font-size: 0.82em; color: #64748B; margin-bottom: 8px; padding-bottom: 6px; border-bottom: 1px dashed #E2E8F0; flex-wrap: wrap;",
            tags$span(tags$strong("Prix moy. : "), sprintf("%.2f €", cls$prix_moyen)),
            tags$span(tags$strong("Ventes totales : "), n_ventes_str),
            if (!is.null(facteur_str)) tags$span(tags$strong("Différenciation : "), tags$span(style = "color: #1D4ED8; font-weight: 600;", facteur_str)),
            tags$span(
              tags$strong("Poids imputés : "), 
              tags$span(
                style = sprintf("font-weight: 600; color: %s;", if (pct_imp_val > 15) "#B45309" else "#475569"),
                sprintf("%.1f %%", pct_imp_val)
              )
            ),
            tags$span(tags$strong("Échantillon : "), paste0(cls$n_echantillon, " obs.")),
            tags$span(tags$strong("Lots/multiples : "), paste0(cls$pct_lots, " %")),
            tags$span(tags$strong("Faux amis : "), paste0(cls$pct_faux_amis, " %"))
          ),
          div(style = "margin-bottom: 8px;",
            tags$div(style = "font-size: 0.82em; font-weight: 600; color: #475569; margin-bottom: 4px;", "Termes discriminants (fréquence) :"),
            div(style = "display: flex; flex-wrap: wrap;", pills_mots)
          ),
          if (!is.null(ex_list)) {
            div(
              tags$div(style = "font-size: 0.82em; font-weight: 600; color: #475569; margin-bottom: 2px;", "Exemples concrets de transactions :"),
              ex_list
            )
          }
        )
      )
    })
    
    # Carte globale d'analyse
    card(
      card_header(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h6(paste0("Audit Sémantique & Mots-clés des Classes — ", input$analysis_prod), 
             style = "margin: 0; font-weight: bold; color: #1E293B; font-family: 'Outfit';"),
          tags$span(
            class = "badge bg-secondary",
            style = "font-size: 0.8em; font-weight: normal;",
            paste0("Analyse textuelle sur ", prod_data$total_echantillon, " libellés")
          )
        )
      ),
      card_body(
        p(style = "font-size: 0.88em; color: #475569; margin-bottom: 12px;",
          "Analyse textuelle des termes récurrents et répartition des parts de marché par classe de prix pour caractériser les sous-gammes et les formats de vente."
        ),
        div(class = "row", class_cards)
      )
    )
  })

  # Graphique de distribution GMM (Gaussiennes sous-jacentes / Densités empiriques / Outliers)
  output$gmm_density_plot <- renderPlot({
    req(input$analysis_prod)
    df_all_prod <- df_classes_sample %>% filter(categorie_standard == input$analysis_prod)
    
    if (input$analysis_organic != "Tous" && "organic" %in% colnames(df_all_prod)) {
      df_all_prod <- df_all_prod %>% filter(organic == input$analysis_organic)
    }
    
    if (is.null(df_all_prod) || nrow(df_all_prod) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Pas de données disponibles pour ce produit.")
      return(NULL)
    }
    
    mode <- input$gmm_view_mode
    if (is.null(mode)) mode <- "unit_gaussians"
    
    clean <- clean_gmm_components()
    
    # Palette de couleurs distinctes
    pal_base <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF", "#999999")
    
    # Récupération de l'unité de mesure
    unit_str <- "kg"
    if (!is.null(df_foodon_ref)) {
      sub_u <- df_foodon_ref %>% filter(categorie_standard == input$analysis_prod)
      if (nrow(sub_u) > 0 && !is.na(sub_u$unite_reference[1])) unit_str <- sub_u$unite_reference[1]
    }
    unit_suffix <- paste0("€/", unit_str)
    
    # Bornes et breaks en euros - calcul robuste couvrant TOUTES les gaussiennes du modèle sans troncature
    log_p <- log(df_all_prod$prix_final)
    
    if (!is.null(clean)) {
      gmm_min <- min(clean$means - 3.2 * clean$sigmas)
      gmm_max <- max(clean$means + 3.2 * clean$sigmas)
    } else {
      gmm_min <- Inf
      gmm_max <- -Inf
    }
    
    data_min <- if (length(log_p) > 0) quantile(log_p, 0.0005, na.rm = TRUE) - 0.15 else gmm_min
    data_max <- if (length(log_p) > 0) quantile(log_p, 0.9995, na.rm = TRUE) + 0.15 else gmm_max
    
    x_min <- min(gmm_min, data_min)
    x_max <- max(gmm_max, data_max)
    
    p_min <- exp(x_min)
    p_max <- exp(x_max)
    breaks_cand <- c(0.1, 0.2, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10, 12, 15, 20, 25, 30, 40, 50, 60, 80, 100, 150, 200)
    breaks_euro <- breaks_cand[breaks_cand >= p_min * 0.95 & breaks_cand <= p_max * 1.05]
    if (length(breaks_euro) > 10) {
      step <- ceiling(length(breaks_euro) / 9)
      breaks_euro <- breaks_euro[seq(1, length(breaks_euro), by = step)]
    }
    if (length(breaks_euro) < 3) {
      breaks_euro <- round(exp(seq(x_min, x_max, length.out = 5)), 2)
    }
    
    # Informations effectifs et statut par classe
    sub_cnt <- if (!is.null(df_gmm_counts)) {
      df_gmm_counts %>% filter(categorie_standard == input$analysis_prod, classe > 0) %>% arrange(prix_moyen)
    } else NULL
    
    row_cls0 <- if (!is.null(df_gmm_counts)) {
      df_gmm_counts %>% filter(categorie_standard == input$analysis_prod, classe == 0)
    } else NULL
    n_outliers_total <- if (!is.null(row_cls0) && nrow(row_cls0) > 0) row_cls0$n_ventes[1] else 0
    
    comp_info <- if (!is.null(clean)) {
      lapply(seq_along(clean$means), function(k) {
        target_p <- exp(clean$means[k])
        if (!is.null(sub_cnt) && nrow(sub_cnt) > 0) {
          idx <- which.min(abs(sub_cnt$prix_moyen - target_p))
          row_c <- sub_cnt[idx, ]
          list(
            n = row_c$n_ventes,
            is_out = (row_c$is_outlier == 1),
            statut = ifelse(row_c$is_outlier == 1, "Rejetée", "Conservée")
          )
        } else {
          list(n = NA_real_, is_out = FALSE, statut = "Conservée")
        }
      })
    } else list()
    
    # MODE 1 : PROFILS DES GAUSSIENNES PAR CLASSE (NORMALISÉES)
    if (mode == "unit_gaussians") {
      df_inliers <- df_all_prod %>% filter(classe > 0, !is.na(classe))
      
      if (is.null(clean) || nrow(df_inliers) == 0) {
        mode <- "empirical"
      } else {
        # Harmoniser les classes du sample par ordre de prix
        class_order <- df_inliers %>%
          group_by(classe) %>%
          summarise(mean_p = mean(prix_final, na.rm = TRUE), .groups = "drop") %>%
          arrange(mean_p) %>%
          mutate(new_classe = factor(row_number()))
        
        df_inliers <- df_inliers %>%
          inner_join(class_order, by = "classe")
        
        x_seq <- seq(x_min, x_max, length.out = 500)
        curve_list <- list()
        class_labels <- c()
        for (k in seq_along(clean$means)) {
          y_k <- dnorm(x_seq, mean = clean$means[k], sd = clean$sigmas[k])
          info_k <- comp_info[[k]]
          n_str <- if (!is.na(info_k$n)) paste0(format(round(info_k$n), big.mark = " "), " ventes - ") else ""
          lbl <- sprintf("Classe %d [%s] (%s%.1f%% | %.2f %s)", 
                         k, info_k$statut, n_str, clean$weights[k] * 100, exp(clean$means[k]), unit_suffix)
          class_labels <- c(class_labels, lbl)
          curve_list[[k]] <- data.frame(
            x = x_seq,
            y = y_k,
            classe = factor(lbl),
            k = factor(k)
          )
        }
        curves_df <- bind_rows(curve_list)
        curves_df$classe <- factor(curves_df$classe, levels = class_labels)
        curves_df$k <- factor(curves_df$k, levels = as.character(seq_along(class_labels)))
        pal_colors <- pal_base[1:clean$k]
        
        p <- ggplot()
        
        # Histogramme par classe
        if (isTRUE(input$gmm_show_hist)) {
          p <- p + geom_histogram(data = df_inliers, 
                                  aes(x = log(prix_final), y = after_stat(density), fill = new_classe),
                                  bins = 45, color = "white", alpha = 0.35, position = "identity")
        }
        
        # Cloches théoriques unitaires
        p <- p + 
          geom_area(data = curves_df, aes(x = x, y = y, fill = k, group = classe), 
                    alpha = 0.20, position = "identity") +
          geom_line(data = curves_df, aes(x = x, y = y, color = k, group = classe), 
                    linewidth = 1.3)
        
        # Repères des outliers / bruit rejeté (Classe 0)
        if (isTRUE(input$gmm_show_outliers)) {
          df_outliers <- df_all_prod %>% filter(classe == 0 | is.na(classe))
          if (nrow(df_outliers) > 0) {
            df_out_vis <- df_outliers %>% filter(log(prix_final) >= x_min, log(prix_final) <= x_max)
            if (nrow(df_out_vis) > 0) {
              p <- p + geom_rug(data = df_out_vis, aes(x = log(prix_final)), 
                                color = "#EF4444", alpha = 0.5, length = unit(0.04, "npc"))
            }
          }
        }
        
        # Repères des prix et statut
        if (isTRUE(input$gmm_show_means)) {
          ann_labels <- sapply(seq_along(clean$means), function(k) {
            info_k <- comp_info[[k]]
            sprintf("%.2f %s\n[%s]", exp(clean$means[k]), unit_suffix, info_k$statut)
          })
          p <- p + 
            geom_vline(xintercept = clean$means, linetype = "dotted", color = "#475569", linewidth = 0.8) +
            annotate("label", x = clean$means, y = max(curves_df$y) * 1.05,
                     label = ann_labels,
                     fontface = "bold", size = 3.2, color = "#1E293B", fill = "white", alpha = 0.9)
        }
        
        sub_unit <- "Chaque cloche est normalisée à 100% (aire = 1) pour comparer l'étalement de chaque gamme indépendamment de son volume de vente"
        if (isTRUE(input$gmm_show_outliers) && n_outliers_total > 0) {
          sub_unit <- paste0(sub_unit, sprintf(" | Tirets rouges en bas : outliers écartés (Classe 0 : %s ventes)", format(round(n_outliers_total), big.mark = " ")))
        }
        
        p <- p +
          scale_x_continuous(limits = c(x_min, x_max), breaks = log(breaks_euro), labels = paste0(breaks_euro, " €")) +
          scale_color_manual(values = pal_colors, name = "Gaussiennes théoriques", labels = class_labels) +
          scale_fill_manual(values = pal_colors, name = "Classes de données", labels = class_labels) +
          labs(
            title = sprintf("Profils des Gaussiennes sous-jacentes (Normalisées à aire = 1) — %s (%d classes)", input$analysis_prod, clean$k),
            subtitle = sub_unit,
            x = paste0("Prix standardisé (", unit_suffix, ") - échelle log calibrée en euros réels"),
            y = "Densité de probabilité normalisée (aire = 1 par classe)"
          ) +
          theme_minimal(base_family = "Outfit") +
          theme(
            plot.title = element_text(face = "bold", size = 14, color = "#1E293B", hjust = 0.5),
            plot.subtitle = element_text(size = 9.5, color = "#64748B", hjust = 0.5, margin = margin(b = 10)),
            legend.position = "bottom",
            legend.box = "vertical",
            legend.title = element_text(face = "bold", size = 9.5),
            panel.grid.minor = element_blank()
          )
        
        return(p)
      }
    }
    
    # MODE 2 : MÉLANGE THÉORIQUE PONDÉRÉ (GLOBAL)
    if (mode %in% c("mix_gaussians", "gaussians")) {
      if (is.null(clean)) {
        mode <- "empirical"
      } else {
        x_seq <- seq(x_min, x_max, length.out = 500)
        
        # Courbes individuelles pondérées
        curve_list <- list()
        class_labels <- c()
        for (k in seq_along(clean$means)) {
          y_k <- clean$weights[k] * dnorm(x_seq, mean = clean$means[k], sd = clean$sigmas[k])
          info_k <- comp_info[[k]]
          n_str <- if (!is.na(info_k$n)) paste0(format(round(info_k$n), big.mark = " "), " ventes - ") else ""
          label_k <- sprintf("Gaussienne %d [%s] (%s%.1f%% | Moy : %.2f %s)", 
                             k, info_k$statut, n_str, clean$weights[k] * 100, exp(clean$means[k]), unit_suffix)
          class_labels <- c(class_labels, label_k)
          curve_list[[k]] <- data.frame(
            x = x_seq,
            y = y_k,
            classe = factor(label_k),
            k = k
          )
        }
        curves_df <- bind_rows(curve_list)
        curves_df$classe <- factor(curves_df$classe, levels = class_labels)
        
        # Somme du mélange global
        y_tot <- rep(0, length(x_seq))
        for (k in seq_along(clean$means)) {
          y_tot <- y_tot + clean$weights[k] * dnorm(x_seq, mean = clean$means[k], sd = clean$sigmas[k])
        }
        total_df <- data.frame(x = x_seq, y = y_tot)
        pal_colors <- pal_base[1:clean$k]
        
        p <- ggplot()
        
        # Histogramme global des ventes réelles (pondéré par les effectifs réels)
        if (isTRUE(input$gmm_show_hist)) {
          df_inliers <- df_all_prod %>% filter(classe > 0, !is.na(classe))
          if (nrow(df_inliers) > 0) {
            if (!is.null(sub_cnt) && nrow(sub_cnt) > 0) {
              cnt_lookup <- sub_cnt %>% select(classe, n_ventes)
              df_inliers <- df_inliers %>%
                inner_join(cnt_lookup, by = "classe") %>%
                group_by(classe) %>%
                mutate(sampling_weight = n_ventes / n()) %>%
                ungroup()
            } else {
              df_inliers$sampling_weight <- 1
            }
            p <- p + geom_histogram(data = df_inliers, 
                                    aes(x = log(prix_final), weight = sampling_weight, y = after_stat(density)),
                                    bins = 50, fill = "#94A3B8", color = "white", alpha = 0.35)
          }
        }
        
        # Cloches gaussiennes pondérées
        p <- p + 
          geom_area(data = curves_df, aes(x = x, y = y, fill = classe, group = classe), 
                    alpha = 0.28, position = "identity") +
          geom_line(data = curves_df, aes(x = x, y = y, color = classe, group = classe), 
                    linewidth = 1.2)
        
        # Courbe globale du mélange
        if (isTRUE(input$gmm_show_mix)) {
          p <- p + geom_line(data = total_df, aes(x = x, y = y), 
                             color = "#0F172A", linewidth = 1.3, linetype = "dashed")
        }
        
        # Repères des outliers / bruit rejeté (Classe 0)
        if (isTRUE(input$gmm_show_outliers)) {
          df_outliers <- df_all_prod %>% filter(classe == 0 | is.na(classe))
          if (nrow(df_outliers) > 0) {
            df_out_vis <- df_outliers %>% filter(log(prix_final) >= x_min, log(prix_final) <= x_max)
            if (nrow(df_out_vis) > 0) {
              p <- p + geom_rug(data = df_out_vis, aes(x = log(prix_final)), 
                                color = "#EF4444", alpha = 0.5, length = unit(0.04, "npc"))
            }
          }
        }
        
        # Repères des prix caractéristiques et statut
        if (isTRUE(input$gmm_show_means)) {
          y_top <- max(total_df$y, na.rm = TRUE) * 1.05
          ann_labels <- sapply(seq_along(clean$means), function(k) {
            info_k <- comp_info[[k]]
            sprintf("%.2f %s\n[%s]", exp(clean$means[k]), unit_suffix, info_k$statut)
          })
          p <- p + 
            geom_vline(xintercept = clean$means, linetype = "dotted", color = "#334155", linewidth = 0.8) +
            annotate("label", x = clean$means, y = y_top, 
                     label = ann_labels, 
                     fontface = "bold", size = 3.3, color = "#1E293B", fill = "white", alpha = 0.9)
        }
        
        sub_mix <- "Les hauteurs des cloches reflètent la part de marché réelle (ex: 93% au centre) | Les tirets noirs tracent le mélange global"
        if (isTRUE(input$gmm_show_outliers) && n_outliers_total > 0) {
          sub_mix <- paste0(sub_mix, sprintf(" | Tirets rouges en bas : outliers écartés (Classe 0 : %s ventes)", format(round(n_outliers_total), big.mark = " ")))
        }
        
        p <- p +
          scale_x_continuous(limits = c(x_min, x_max), breaks = log(breaks_euro), labels = paste0(breaks_euro, " €")) +
          scale_fill_manual(values = pal_colors, name = "Gaussiennes sous-jacentes", labels = class_labels) +
          scale_color_manual(values = pal_colors, name = "Gaussiennes sous-jacentes", labels = class_labels) +
          labs(
            title = sprintf("Mélange théorique pondéré (Volume réel des ventes) — %s (%d classes)", input$analysis_prod, clean$k),
            subtitle = sub_mix,
            x = paste0("Prix standardisé (", unit_suffix, ") - échelle log calibrée en euros réels"),
            y = "Densité pondérée de probabilité"
          ) +
          theme_minimal(base_family = "Outfit") +
          theme(
            plot.title = element_text(face = "bold", size = 14, color = "#1E293B", hjust = 0.5),
            plot.subtitle = element_text(size = 9.5, color = "#64748B", hjust = 0.5, margin = margin(b = 10)),
            legend.position = "bottom",
            legend.box = "vertical",
            legend.title = element_text(face = "bold", size = 9.5),
            panel.grid.minor = element_blank()
          )
        
        return(p)
      }
    }
    
    # MODE 2 : DENSITÉS EMPIRIQUES OBSERVÉES
    if (mode == "empirical") {
      df_inliers <- df_all_prod %>% filter(classe > 0, !is.na(classe))
      if (nrow(df_inliers) == 0) return(NULL)
      
      class_order <- df_inliers %>%
        group_by(classe) %>%
        summarise(mean_p = mean(prix_final, na.rm = TRUE), .groups = "drop") %>%
        arrange(mean_p) %>%
        mutate(new_classe = row_number())
      
      df_inliers <- df_inliers %>%
        inner_join(class_order, by = "classe") %>%
        mutate(classe = new_classe)
      
      class_stats <- df_inliers %>%
        group_by(classe) %>%
        summarise(
          pct = 100 * n() / nrow(df_inliers),
          mean_val = mean(prix_final, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(classe)
      
      legend_labels <- setNames(
        sprintf("Classe %d (%.1f%% | Moy : %.2f €/kg)", class_stats$classe, class_stats$pct, class_stats$mean_val),
        as.character(class_stats$classe)
      )
      
      p <- ggplot(df_inliers, aes(x = log(prix_final), fill = factor(classe))) +
        geom_density(aes(y = after_stat(count)), alpha = 0.6, color = "white", trim = TRUE) +
        scale_fill_brewer(palette = "Set2", name = "Classes GMM", labels = legend_labels) +
        scale_x_continuous(breaks = log(breaks_euro), labels = paste0(breaks_euro, " €")) +
        theme_minimal(base_family = "Outfit") +
        labs(
          x = "Prix standardisé (au kilo, échelle log)",
          y = "Volume de transactions",
          title = sprintf("Distribution observée par classe GMM pour : %s", input$analysis_prod),
          subtitle = "Densités de noyau empiriques calculées sur les données nettoyées"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 13, color = "#2C3E50", hjust = 0.5),
          plot.subtitle = element_text(size = 10, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 10)),
          legend.position = "bottom"
        )
      return(p)
    }
    
    # MODE 3 : BRUIT & OUTLIERS FILTRÉS
    if (mode == "outliers") {
      df_plot <- df_all_prod %>%
        mutate(statut = ifelse(classe == 0 | is.na(classe), "Outlier / Bruit rejeté (Classe 0)", "Transaction Valide (Classes 1..K)"))
      
      p <- ggplot(df_plot, aes(x = log(prix_final), fill = statut)) +
        geom_histogram(bins = 60, alpha = 0.8, position = "identity", color = "white") +
        scale_fill_manual(values = c("Transaction Valide (Classes 1..K)" = "#22C55E", 
                                     "Outlier / Bruit rejeté (Classe 0)" = "#EF4444"), 
                          name = "Statut GMM") +
        scale_x_continuous(breaks = log(breaks_euro), labels = paste0(breaks_euro, " €")) +
        labs(
          title = sprintf("Filtrage des Valeurs Aberrantes GMM (Classe 0) — %s", input$analysis_prod),
          subtitle = "En vert : transactions valides retenues | En rouge : bruit statistique et anomalies écartées",
          x = "Prix standardisé au kilo (échelle log calibrée en euros réels)",
          y = "Nombre de transactions"
        ) +
        theme_minimal(base_family = "Outfit") +
        theme(
          plot.title = element_text(face = "bold", size = 13, color = "#1E293B", hjust = 0.5),
          plot.subtitle = element_text(size = 9.5, color = "#64748B", hjust = 0.5),
          legend.position = "bottom"
        )
      return(p)
    }
  })
  
  # Graphique de composition des classes GMM par covariable
  output$gmm_composition_plot <- renderPlot({
    req(input$gmm_covar)
    df_prod <- df_prod_gmm()
    
    if (is.null(df_prod) || nrow(df_prod) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Pas d'échantillon GMM disponible pour ce produit.")
      return(NULL)
    }
    
    # Préparer les données pour le pourcentage empilé
    covar_col <- input$gmm_covar
    
    df_plot <- df_prod %>%
      group_by(classe, !!sym(covar_col)) %>%
      summarise(count = n(), .groups = "drop")
    
    # Déterminer le titre et la palette
    if (covar_col == "organic") {
      title_text <- "Répartition Bio / Non-Bio par Classe GMM"
      fill_label <- "Culture"
      cols <- c("Bio" = "#228833", "Non-Bio" = "#E74C3C")
      p <- ggplot(df_plot, aes(x = factor(classe), y = count, fill = !!sym(covar_col))) +
        geom_bar(stat = "identity", position = "fill", color = "white", width = 0.6) +
        scale_fill_manual(values = cols)
    } else {
      title_text <- "Répartition des Plateformes par Classe GMM"
      fill_label <- "Plateforme"
      p <- ggplot(df_plot, aes(x = factor(classe), y = count, fill = !!sym(covar_col))) +
        geom_bar(stat = "identity", position = "fill", color = "white", width = 0.6) +
        scale_fill_brewer(palette = "Set2")
    }
    
    p <- p +
      scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
      theme_minimal() +
      labs(
        x = "Classe GMM",
        y = "Proportion (%)",
        title = title_text,
        fill = fill_label
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 11, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        legend.position = "bottom",
        axis.text = element_text(size = 9)
      )
    
    return(p)
  })
  
  # -----------------------------------------------------------------
  # LOGIQUE ONGLET 4 : INFLATION DES PRIX
  # -----------------------------------------------------------------
  
  # Résolution plateforme transparente (compatible mode simulé et mapping externe)
  resolve_inflation_plat <- function(plat_input) {
    if (is.null(plat_input) || plat_input == "Toutes") return("Toutes")
    if (!is.null(plat_mapping)) {
      matched_raw <- names(plat_mapping)[unname(plat_mapping) == plat_input]
      if (length(matched_raw) > 0) return(unique(c(plat_input, matched_raw)))
    }
    return(plat_input)
  }

  # Calcul réactif de l'inflation annuelle
  inflation_data <- reactive({
    req(df_dept_all, input$inflation_plat)
    
    target_plats <- resolve_inflation_plat(input$inflation_plat)
    
    # 1. Filtrer les données nationales
    df_nat <- df_dept_all %>%
      filter(
        dept == "National",
        Plateforme %in% target_plats,
        Annee != "Toutes",
        organic == "Tous"
      ) %>%
      mutate(Annee = as.numeric(Annee)) %>%
      arrange(categorie_standard, Annee)
    
    if (nrow(df_nat) == 0) return(NULL)
    
    # 2. Calculer l'inflation par produit de l'année N-1 à N
    df_inf <- df_nat %>%
      group_by(categorie_standard) %>%
      mutate(
        Prev_Price = lag(Prix_Moyen_Kg),
        Prev_Revenue = lag(Total_Revenue),
        Prev_Sales = lag(Nombre_Ventes),
        Prod_Inflation = ifelse(!is.na(Prev_Price) & Prev_Price > 0, (Prix_Moyen_Kg - Prev_Price) / Prev_Price * 100, NA_real_)
      ) %>%
      ungroup()
    
    # Intégrer l'indicateur panier_quotidien depuis le référentiel FoodOn
    if (!is.null(df_foodon_ref)) {
      df_inf <- df_inf %>%
        left_join(
          df_foodon_ref %>% select(categorie_standard, panier_quotidien),
          by = "categorie_standard"
        ) %>%
        mutate(panier_quotidien = ifelse(is.na(panier_quotidien), 0, as.numeric(panier_quotidien)))
    } else {
      df_inf$panier_quotidien <- 0
    }
    
    # Intégrer les données de référence GMS INSEE
    if (!is.null(df_ref_all)) {
      df_ref_annual <- df_ref_all %>%
        mutate(Annee = as.numeric(substr(date_mois, 1, 4))) %>%
        group_by(categorie_standard, Annee) %>%
        summarise(Ref_Prix_Moyen = mean(prix_moyen_kg, na.rm = TRUE), .groups = "drop") %>%
        group_by(categorie_standard) %>%
        arrange(Annee) %>%
        mutate(
          Prev_Ref_Prix = lag(Ref_Prix_Moyen),
          Ref_Inflation = ifelse(!is.na(Prev_Ref_Prix) & Prev_Ref_Prix > 0, (Ref_Prix_Moyen - Prev_Ref_Prix) / Prev_Ref_Prix * 100, NA_real_)
        ) %>%
        ungroup()
      
      df_inf <- df_inf %>%
        left_join(
          df_ref_annual %>% select(categorie_standard, Annee, Ref_Prix_Moyen, Ref_Inflation),
          by = c("categorie_standard", "Annee")
        )
    } else {
      df_inf <- df_inf %>%
        mutate(
          Ref_Prix_Moyen = NA_real_,
          Ref_Inflation = NA_real_
        )
    }
    
    # 3. Calculer l'inflation générale selon la formule sélectionnée (Laspeyres, Paasche, Fisher)
    mode_ind <- if (!is.null(input$inflation_mode_indice)) input$inflation_mode_indice else "laspeyres"
    
    df_gen <- df_inf %>%
      filter(!is.na(Prod_Inflation)) %>%
      group_by(Annee) %>%
      summarise(
        Gen_Inflation = {
          if (mode_ind == "paasche") {
            valid_p <- !is.na(Total_Revenue) & Total_Revenue > 0
            if (any(valid_p)) sum(Prod_Inflation[valid_p] * Total_Revenue[valid_p], na.rm = TRUE) / sum(Total_Revenue[valid_p], na.rm = TRUE) else NA_real_
          } else if (mode_ind == "fisher") {
            v_prev <- !is.na(Prev_Revenue) & Prev_Revenue > 0
            v_curr <- !is.na(Total_Revenue) & Total_Revenue > 0
            il <- if (any(v_prev)) sum(Prod_Inflation[v_prev] * Prev_Revenue[v_prev], na.rm = TRUE) / sum(Prev_Revenue[v_prev], na.rm = TRUE) else 0
            ip <- if (any(v_curr)) sum(Prod_Inflation[v_curr] * Total_Revenue[v_curr], na.rm = TRUE) / sum(Total_Revenue[v_curr], na.rm = TRUE) else 0
            idx_l <- 1 + il / 100
            idx_p <- 1 + ip / 100
            if (idx_l > 0 && idx_p > 0) (sqrt(idx_l * idx_p) - 1) * 100 else il
          } else {
            valid_l <- !is.na(Prev_Revenue) & Prev_Revenue > 0
            if (any(valid_l)) sum(Prod_Inflation[valid_l] * Prev_Revenue[valid_l], na.rm = TRUE) / sum(Prev_Revenue[valid_l], na.rm = TRUE) else NA_real_
          }
        },
        Gen_Ref_Inflation = {
          valid_ref <- !is.na(Ref_Inflation) & !is.na(Prev_Revenue) & Prev_Revenue > 0
          if (any(valid_ref)) {
            sum(Ref_Inflation[valid_ref] * Prev_Revenue[valid_ref], na.rm = TRUE) / sum(Prev_Revenue[valid_ref], na.rm = TRUE)
          } else {
            NA_real_
          }
        },
        .groups = "drop"
      )
    
    # Calcul de l'indice spécifique pour les produits du panier quotidien
    df_panier <- df_inf %>%
      filter(!is.na(Prod_Inflation), panier_quotidien == 1) %>%
      group_by(Annee) %>%
      summarise(
        Panier_Inflation = {
          if (mode_ind == "paasche") {
            valid_p <- !is.na(Total_Revenue) & Total_Revenue > 0
            if (any(valid_p)) sum(Prod_Inflation[valid_p] * Total_Revenue[valid_p], na.rm = TRUE) / sum(Total_Revenue[valid_p], na.rm = TRUE) else NA_real_
          } else if (mode_ind == "fisher") {
            v_prev <- !is.na(Prev_Revenue) & Prev_Revenue > 0
            v_curr <- !is.na(Total_Revenue) & Total_Revenue > 0
            il <- if (any(v_prev)) sum(Prod_Inflation[v_prev] * Prev_Revenue[v_prev], na.rm = TRUE) / sum(Prev_Revenue[v_prev], na.rm = TRUE) else 0
            ip <- if (any(v_curr)) sum(Prod_Inflation[v_curr] * Total_Revenue[v_curr], na.rm = TRUE) / sum(Total_Revenue[v_curr], na.rm = TRUE) else 0
            idx_l <- 1 + il / 100
            idx_p <- 1 + ip / 100
            if (idx_l > 0 && idx_p > 0) (sqrt(idx_l * idx_p) - 1) * 100 else il
          } else {
            valid_l <- !is.na(Prev_Revenue) & Prev_Revenue > 0
            if (any(valid_l)) sum(Prod_Inflation[valid_l] * Prev_Revenue[valid_l], na.rm = TRUE) / sum(Prev_Revenue[valid_l], na.rm = TRUE) else NA_real_
          }
        },
        Panier_Ref_Inflation = {
          valid_ref <- !is.na(Ref_Inflation) & !is.na(Prev_Revenue) & Prev_Revenue > 0
          if (any(valid_ref)) {
            sum(Ref_Inflation[valid_ref] * Prev_Revenue[valid_ref], na.rm = TRUE) / sum(Prev_Revenue[valid_ref], na.rm = TRUE)
          } else {
            NA_real_
          }
        },
        .groups = "drop"
      )
    
    df_gen <- df_gen %>% left_join(df_panier, by = "Annee")
    
    list(product_inflation = df_inf, general_inflation = df_gen)
  })
  
  # KPI 1 : Inflation Circuits Courts
  output$kpi_inf_cc <- renderText({
    req(inflation_data(), input$inflation_year, input$inflation_prod)
    data <- inflation_data()
    y <- as.numeric(input$inflation_year)
    
    rate <- if (input$inflation_prod == "Inflation Générale") {
      g <- data$general_inflation %>% filter(Annee == y)
      if (nrow(g) > 0) g$Gen_Inflation[1] else NA_real_
    } else if (input$inflation_prod == "Panier Quotidien") {
      g <- data$general_inflation %>% filter(Annee == y)
      if (nrow(g) > 0) g$Panier_Inflation[1] else NA_real_
    } else {
      p <- data$product_inflation %>% filter(Annee == y, categorie_standard == input$inflation_prod)
      if (nrow(p) > 0) p$Prod_Inflation[1] else NA_real_
    }
    if (is.na(rate)) return("-")
    sprintf("%+.1f %%", rate)
  })

  output$kpi_inf_cc_sub <- renderText({
    req(input$inflation_year)
    mode_label <- switch(input$inflation_mode_indice,
                         "laspeyres" = "Laspeyres (Base N-1)",
                         "paasche" = "Paasche (Courant)",
                         "fisher" = "Fisher (Idéal)",
                         "Laspeyres")
    paste0("Année ", input$inflation_year, " vs ", as.numeric(input$inflation_year) - 1, " | ", mode_label)
  })

  # KPI 2 : Référence GMS INSEE
  output$kpi_inf_insee <- renderText({
    req(inflation_data(), input$inflation_year, input$inflation_prod)
    data <- inflation_data()
    y <- as.numeric(input$inflation_year)
    
    rate <- if (input$inflation_prod == "Inflation Générale") {
      g <- data$general_inflation %>% filter(Annee == y)
      if (nrow(g) > 0) g$Gen_Ref_Inflation[1] else NA_real_
    } else if (input$inflation_prod == "Panier Quotidien") {
      g <- data$general_inflation %>% filter(Annee == y)
      if (nrow(g) > 0 && !is.na(g$Panier_Ref_Inflation[1])) g$Panier_Ref_Inflation[1] else if (nrow(g) > 0) g$Gen_Ref_Inflation[1] else NA_real_
    } else {
      p <- data$product_inflation %>% filter(Annee == y, categorie_standard == input$inflation_prod)
      if (nrow(p) > 0) p$Ref_Inflation[1] else NA_real_
    }
    if (is.na(rate)) return("Indisponible")
    sprintf("%+.1f %%", rate)
  })

  output$kpi_inf_insee_sub <- renderText({
    "Grande distribution alimentaire (INSEE)"
  })

  # KPI 3 : Différentiel CC vs GMS
  output$kpi_inf_ecart <- renderText({
    req(inflation_data(), input$inflation_year, input$inflation_prod)
    data <- inflation_data()
    y <- as.numeric(input$inflation_year)
    
    r_cc <- if (input$inflation_prod == "Inflation Générale") {
      g <- data$general_inflation %>% filter(Annee == y)
      if (nrow(g) > 0) g$Gen_Inflation[1] else NA_real_
    } else if (input$inflation_prod == "Panier Quotidien") {
      g <- data$general_inflation %>% filter(Annee == y)
      if (nrow(g) > 0) g$Panier_Inflation[1] else NA_real_
    } else {
      p <- data$product_inflation %>% filter(Annee == y, categorie_standard == input$inflation_prod)
      if (nrow(p) > 0) p$Prod_Inflation[1] else NA_real_
    }
    
    r_ref <- if (input$inflation_prod == "Inflation Générale") {
      g <- data$general_inflation %>% filter(Annee == y)
      if (nrow(g) > 0) g$Gen_Ref_Inflation[1] else NA_real_
    } else if (input$inflation_prod == "Panier Quotidien") {
      g <- data$general_inflation %>% filter(Annee == y)
      if (nrow(g) > 0 && !is.na(g$Panier_Ref_Inflation[1])) g$Panier_Ref_Inflation[1] else if (nrow(g) > 0) g$Gen_Ref_Inflation[1] else NA_real_
    } else {
      p <- data$product_inflation %>% filter(Annee == y, categorie_standard == input$inflation_prod)
      if (nrow(p) > 0) p$Ref_Inflation[1] else NA_real_
    }
    
    if (is.na(r_cc) || is.na(r_ref)) return("-")
    sprintf("%+.1f pts", r_cc - r_ref)
  })

  output$kpi_inf_ecart_sub <- renderText({
    req(inflation_data(), input$inflation_year, input$inflation_prod)
    data <- inflation_data()
    y <- as.numeric(input$inflation_year)
    df_y <- data$general_inflation %>% filter(Annee == y)
    if (nrow(df_y) == 0) return("Circuits Courts vs GMS")
    
    r_cc <- if (input$inflation_prod == "Panier Quotidien") df_y$Panier_Inflation[1] else df_y$Gen_Inflation[1]
    r_ref <- if (input$inflation_prod == "Panier Quotidien" && !is.na(df_y$Panier_Ref_Inflation[1])) {
      df_y$Panier_Ref_Inflation[1]
    } else {
      df_y$Gen_Ref_Inflation[1]
    }
    
    if (is.na(r_cc) || is.na(r_ref)) return("Circuits Courts vs GMS")
    if (r_cc > r_ref) "Hausse plus marquée en CC" else "Inflation plus modérée en CC"
  })

  # KPI 4 : Inflation Panier Quotidien
  output$kpi_inf_panier <- renderText({
    req(inflation_data(), input$inflation_year)
    data <- inflation_data()
    y <- as.numeric(input$inflation_year)
    df_y <- data$general_inflation %>% filter(Annee == y)
    if (nrow(df_y) == 0 || is.na(df_y$Panier_Inflation[1])) return("-")
    sprintf("%+.1f %%", df_y$Panier_Inflation[1])
  })

  output$kpi_inf_panier_sub <- renderText({
    req(inflation_data(), input$inflation_year)
    data <- inflation_data()
    y <- as.numeric(input$inflation_year)
    df_y <- data$general_inflation %>% filter(Annee == y)
    if (nrow(df_y) == 0 || is.na(df_y$Panier_Inflation[1]) || is.na(df_y$Gen_Inflation[1])) {
      return("33 produits essentiels du quotidien")
    }
    diff_pt <- df_y$Panier_Inflation[1] - df_y$Gen_Inflation[1]
    sprintf("%+.2f pt vs global (33 produits)", diff_pt)
  })

  # Rendu du graphique d'inflation annuelle
  output$inflation_plot <- renderPlot({
    req(inflation_data(), input$inflation_prod)
    
    data <- inflation_data()
    df_gen <- data$general_inflation
    
    if (nrow(df_gen) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Données historiques insuffisantes pour calculer l'inflation.")
      return(NULL)
    }
    
    if (input$inflation_prod %in% c("Inflation Générale", "Panier Quotidien")) {
      df_gen_sorted <- df_gen %>% arrange(Annee)
      
      cum_indices_cc <- data.frame(
        Annee = c(2021, df_gen_sorted$Annee),
        Indice = 100,
        Taux = 0,
        Type = "CC : Panier Global"
      )
      
      cum_indices_panier <- data.frame(
        Annee = c(2021, df_gen_sorted$Annee),
        Indice = 100,
        Taux = 0,
        Type = "CC : Panier Quotidien"
      )
      
      cum_indices_ref <- data.frame(
        Annee = c(2021, df_gen_sorted$Annee),
        Indice = 100,
        Taux = 0,
        Type = "Référence GMS (INSEE)"
      )
      
      current_cc <- 100
      current_panier <- 100
      current_ref <- 100
      
      for (i in 1:nrow(df_gen_sorted)) {
        year <- df_gen_sorted$Annee[i]
        
        rate_cc <- df_gen_sorted$Gen_Inflation[i]
        current_cc <- current_cc * (1 + rate_cc / 100)
        cum_indices_cc$Indice[cum_indices_cc$Annee == year] <- current_cc
        cum_indices_cc$Taux[cum_indices_cc$Annee == year] <- rate_cc
        
        rate_panier <- df_gen_sorted$Panier_Inflation[i]
        if (!is.na(rate_panier)) {
          current_panier <- current_panier * (1 + rate_panier / 100)
          cum_indices_panier$Indice[cum_indices_panier$Annee == year] <- current_panier
          cum_indices_panier$Taux[cum_indices_panier$Annee == year] <- rate_panier
        }
        
        rate_ref <- df_gen_sorted$Gen_Ref_Inflation[i]
        if (!is.na(rate_ref)) {
          current_ref <- current_ref * (1 + rate_ref / 100)
          cum_indices_ref$Indice[cum_indices_ref$Annee == year] <- current_ref
          cum_indices_ref$Taux[cum_indices_ref$Annee == year] <- rate_ref
        } else {
          cum_indices_ref$Indice[cum_indices_ref$Annee == year] <- NA_real_
          cum_indices_ref$Taux[cum_indices_ref$Annee == year] <- NA_real_
        }
      }
      
      cum_indices <- bind_rows(cum_indices_cc, cum_indices_panier, cum_indices_ref) %>%
        filter(!is.na(Indice))
      
      max_y <- max(cum_indices$Indice, na.rm = TRUE)
      y_top <- max_y + 6
      
      df_text <- cum_indices %>%
        mutate(
          show_label = !(Annee == 2021 & Type != "CC : Panier Global"),
          label_txt = case_when(
            Annee == 2021 ~ "Base 100.0",
            TRUE ~ sprintf("%.1f (%+.1f%%)", Indice, Taux)
          ),
          nudge_y_val = case_when(
            Annee == 2021 ~ 1.8,
            Type == "Référence GMS (INSEE)" ~ 1.8,
            Type == "CC : Panier Global" ~ 1.8,
            Type == "CC : Panier Quotidien" ~ -1.9
          ),
          vjust_val = case_when(
            Annee == 2021 ~ 0,
            Type == "Référence GMS (INSEE)" ~ 0,
            Type == "CC : Panier Global" ~ 0,
            Type == "CC : Panier Quotidien" ~ 1
          )
        )
      
      p <- ggplot(cum_indices, aes(x = factor(Annee), y = Indice, color = Type, group = Type)) +
        geom_line(aes(linetype = Type), linewidth = 1.15) +
        geom_point(aes(shape = Type), size = 3.2) +
        geom_text(
          data = df_text %>% filter(show_label),
          aes(x = factor(Annee), y = Indice, label = label_txt, vjust = vjust_val),
          nudge_y = (df_text %>% filter(show_label))$nudge_y_val,
          fontface = "bold", size = 2.9, show.legend = FALSE
        ) +
        scale_color_manual(values = c(
          "CC : Panier Global" = "#1E8449", 
          "CC : Panier Quotidien" = "#0E6655", 
          "Référence GMS (INSEE)" = "#5D6D7E"
        )) +
        scale_linetype_manual(values = c(
          "CC : Panier Global" = "solid", 
          "CC : Panier Quotidien" = "22", 
          "Référence GMS (INSEE)" = "solid"
        )) +
        scale_shape_manual(values = c(
          "CC : Panier Global" = 16,
          "CC : Panier Quotidien" = 18,
          "Référence GMS (INSEE)" = 15
        )) +
        scale_y_continuous(limits = c(93, y_top), breaks = seq(95, ceiling(y_top / 5) * 5, 5)) +
        theme_minimal(base_family = "Outfit") +
        labs(
          x = "Année",
          y = "Indice des Prix (Base 100 en 2021)", 
          title = "Évolution des Indices des Prix : Global, Panier Quotidien et GMS",
          subtitle = "Base 100 en 2021 — Pondération budgétaire continue Laspeyres / Fisher"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 12, color = "#2C3E50", hjust = 0.5),
          plot.subtitle = element_text(size = 9.5, color = "#7F8C8D", hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.text = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 10.5, face = "bold", color = "#475569"),
          panel.grid.minor = element_blank()
        )
      
      return(p)
      
    } else {
      df_prod <- data$product_inflation %>%
        filter(categorie_standard == input$inflation_prod, !is.na(Prod_Inflation))
      
      if (nrow(df_prod) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Données historiques insuffisantes pour ce produit.")
        return(NULL)
      }
      
      df_plot <- df_prod %>%
        select(Annee, Prod_Inflation, Ref_Inflation) %>%
        left_join(df_gen, by = "Annee") %>%
        tidyr::pivot_longer(cols = c("Prod_Inflation", "Ref_Inflation", "Gen_Inflation"), names_to = "Type", values_to = "Taux") %>%
        mutate(Type = case_when(
          Type == "Prod_Inflation" ~ paste("Produit (Circuits Courts) :", input$inflation_prod),
          Type == "Ref_Inflation" ~ paste("Produit (Référence GMS INSEE) :", input$inflation_prod),
          Type == "Gen_Inflation" ~ "Inflation Générale (Circuits Courts)"
        )) %>%
        filter(!is.na(Taux))
      
      colors_val <- c(
        "Inflation Générale (Circuits Courts)" = "#2C3E50",
        "temp_cc" = "#228833",
        "temp_ref" = "#7F8C8D"
      )
      names(colors_val) <- c(
        "Inflation Générale (Circuits Courts)",
        paste("Produit (Circuits Courts) :", input$inflation_prod),
        paste("Produit (Référence GMS INSEE) :", input$inflation_prod)
      )
      
      p <- ggplot(df_plot, aes(x = factor(Annee), y = Taux, fill = Type)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "grey30") +
        geom_text(aes(label = sprintf("%+.1f%%", Taux)), position = position_dodge(0.7), vjust = ifelse(df_plot$Taux >= 0, -0.5, 1.2), size = 3.2, fontface = "bold") +
        scale_fill_manual(values = colors_val) +
        theme_minimal(base_family = "Outfit") +
        labs(x = "Année", y = "Taux d'Inflation Annuel (%)", title = paste("Inflation :", input$inflation_prod, "vs Indices Généraux")) +
        theme(
          plot.title = element_text(face = "bold", size = 12, color = "#2C3E50", hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.text = element_text(size = 10, face = "bold")
        )
      
      return(p)
    }
  })

  # Graphique comparatif de l'inflation par produit (Top 15 sans troncature)
  output$product_inflation_compare_plot <- renderPlot({
    req(inflation_data(), input$inflation_year)
    
    data <- inflation_data()
    y_target <- as.numeric(input$inflation_year)
    df_all_products <- data$product_inflation %>%
      filter(Annee == y_target, !is.na(Prod_Inflation)) %>%
      arrange(desc(Prod_Inflation))
    
    if (nrow(df_all_products) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Données historiques insuffisantes pour comparer les produits.")
      return(NULL)
    }
    
    df_plot <- df_all_products %>%
      slice_max(order_by = Nombre_Ventes, n = 15) %>%
      mutate(categorie_standard = reorder(categorie_standard, Prod_Inflation))
    
    df_gen_year <- data$general_inflation %>% 
      filter(Annee == y_target)
    gen_val <- if (nrow(df_gen_year) > 0) df_gen_year$Gen_Inflation[1] else NA_real_
    gen_ref_val <- if (nrow(df_gen_year) > 0) df_gen_year$Gen_Ref_Inflation[1] else NA_real_
    
    max_val <- max(df_plot$Prod_Inflation, na.rm = TRUE)
    min_val <- min(df_plot$Prod_Inflation, na.rm = TRUE)
    span <- max_val - min_val
    y_upper <- max(0, max_val + span * 0.22)
    y_lower <- min(0, min_val - span * 0.15)
    
    sub_title_txt <- paste0("Moyenne CC : ", ifelse(is.na(gen_val), "-", sprintf("%+.1f%%", gen_val)),
                            "  |  Moyenne GMS : ", ifelse(is.na(gen_ref_val), "-", sprintf("%+.1f%%", gen_ref_val)),
                            "  |  Top 15 volumes")
    
    p <- ggplot(df_plot, aes(x = categorie_standard, y = Prod_Inflation, fill = Prod_Inflation)) +
      geom_bar(stat = "identity", color = "grey30", width = 0.68, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%+.1f%%", Prod_Inflation)), 
                hjust = ifelse(df_plot$Prod_Inflation >= 0, -0.15, 1.15), 
                size = 3.2, fontface = "bold") +
      scale_fill_gradient2(low = "#2980B9", mid = "#FCF3CF", high = "#C0392B", midpoint = 0) +
      coord_flip(clip = "off") +
      expand_limits(y = c(y_lower, y_upper)) +
      theme_minimal(base_family = "Outfit")
      
    if (!is.na(gen_val)) {
      p <- p + geom_hline(yintercept = gen_val, linetype = "dashed", color = "#228833", linewidth = 0.9)
    }
    
    if (!is.na(gen_ref_val)) {
      p <- p + geom_hline(yintercept = gen_ref_val, linetype = "dotted", color = "#C0392B", linewidth = 0.9)
    }
      
    p <- p + labs(
        x = NULL, 
        y = "Taux d'inflation annuel (%)", 
        title = paste("Inflation par produit en", input$inflation_year),
        subtitle = sub_title_txt
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 11.5, color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 9.5, color = "#7F8C8D", hjust = 0.5),
        axis.text = element_text(size = 9.2, face = "bold"),
        plot.margin = margin(t = 10, r = 25, b = 10, l = 10)
      )
    
    return(p)
  })

  # Graphique de l'inflation des produits tagués 'Panier Quotidien'
  output$panier_inflation_plot <- renderPlot({
    req(inflation_data(), input$inflation_year)
    data <- inflation_data()
    y_target <- as.numeric(input$inflation_year)
    
    df_panier_prods <- data$product_inflation %>%
      filter(Annee == y_target, !is.na(Prod_Inflation), panier_quotidien == 1) %>%
      arrange(desc(Prod_Inflation))
    
    if (nrow(df_panier_prods) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Données historiques insuffisantes pour le panier quotidien.")
      return(NULL)
    }
    
    df_plot <- df_panier_prods %>%
      mutate(categorie_standard = reorder(categorie_standard, Prod_Inflation))
    
    df_gen_year <- data$general_inflation %>% filter(Annee == y_target)
    panier_avg <- if (nrow(df_gen_year) > 0) df_gen_year$Panier_Inflation[1] else NA_real_
    gen_avg <- if (nrow(df_gen_year) > 0) df_gen_year$Gen_Inflation[1] else NA_real_
    
    max_val <- max(df_plot$Prod_Inflation, na.rm = TRUE)
    min_val <- min(df_plot$Prod_Inflation, na.rm = TRUE)
    span <- max_val - min_val
    y_upper <- max(0, max_val + span * 0.22)
    y_lower <- min(0, min_val - span * 0.15)
    
    sub_title_txt <- paste0(
      "Moyenne Panier Quotidien : ", ifelse(is.na(panier_avg), "-", sprintf("%+.1f%%", panier_avg)),
      "  |  Indice Global : ", ifelse(is.na(gen_avg), "-", sprintf("%+.1f%%", gen_avg)),
      "  |  ", nrow(df_plot), " produits essentiels"
    )
    
    p <- ggplot(df_plot, aes(x = categorie_standard, y = Prod_Inflation, fill = Prod_Inflation)) +
      geom_bar(stat = "identity", color = "grey30", width = 0.72, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%+.1f%%", Prod_Inflation)), 
                hjust = ifelse(df_plot$Prod_Inflation >= 0, -0.15, 1.15), 
                size = 2.6, fontface = "bold") +
      scale_fill_gradient2(low = "#2980B9", mid = "#FCF3CF", high = "#C0392B", midpoint = 0) +
      coord_flip(clip = "off") +
      expand_limits(y = c(y_lower, y_upper)) +
      theme_minimal(base_family = "Outfit")
      
    if (!is.na(panier_avg)) {
      p <- p + geom_hline(yintercept = panier_avg, linetype = "dashed", color = "#16A085", linewidth = 1.0)
    }
    if (!is.na(gen_avg)) {
      p <- p + geom_hline(yintercept = gen_avg, linetype = "dotted", color = "#2C3E50", linewidth = 0.9)
    }
    
    p <- p + labs(
      x = NULL,
      y = "Taux d'inflation annuel (%)",
      title = paste("Inflation des Produits du Panier Quotidien en", y_target),
      subtitle = sub_title_txt
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 11.5, color = "#16A085", hjust = 0.5),
      plot.subtitle = element_text(size = 9.0, color = "#7F8C8D", hjust = 0.5),
      axis.text.y = element_text(size = 7.5, face = "bold"),
      axis.text.x = element_text(size = 8),
      plot.margin = margin(t = 10, r = 25, b = 10, l = 10)
    )
    
    return(p)
  })

  # Tableau annuel de l'inflation
  output$table_inflation <- renderDT({
    req(inflation_data(), input$inflation_prod)
    data <- inflation_data()
    
    if (input$inflation_prod == "Inflation Générale") {
      df_gen <- data$general_inflation
      df_gen_sorted <- df_gen %>% arrange(Annee)
      
      cum_indices <- data.frame(
        Annee = c(2021, df_gen_sorted$Annee),
        Indice_CC = 100,
        Taux_CC = 0,
        Taux_Panier = 0,
        Indice_Ref = 100,
        Taux_Ref = 0,
        CA = NA_real_
      )
      
      df_ca_year <- data$product_inflation %>%
        group_by(Annee) %>%
        summarise(CA = sum(Total_Revenue, na.rm = TRUE), .groups = "drop")
      
      for (j in 1:nrow(cum_indices)) {
        y <- cum_indices$Annee[j]
        ca_row <- df_ca_year %>% filter(Annee == y)
        if (nrow(ca_row) > 0) cum_indices$CA[j] <- ca_row$CA[1]
      }
      
      current_cc <- 100
      current_ref <- 100
      for (i in 1:nrow(df_gen_sorted)) {
        year <- df_gen_sorted$Annee[i]
        
        rate_cc <- df_gen_sorted$Gen_Inflation[i]
        current_cc <- current_cc * (1 + rate_cc / 100)
        cum_indices$Indice_CC[cum_indices$Annee == year] <- current_cc
        cum_indices$Taux_CC[cum_indices$Annee == year] <- rate_cc
        
        rate_panier <- df_gen_sorted$Panier_Inflation[i]
        cum_indices$Taux_Panier[cum_indices$Annee == year] <- ifelse(is.na(rate_panier), 0, rate_panier)
        
        rate_ref <- df_gen_sorted$Gen_Ref_Inflation[i]
        if (!is.na(rate_ref)) {
          current_ref <- current_ref * (1 + rate_ref / 100)
          cum_indices$Indice_Ref[cum_indices$Annee == year] <- current_ref
          cum_indices$Taux_Ref[cum_indices$Annee == year] <- rate_ref
        } else {
          cum_indices$Indice_Ref[cum_indices$Annee == year] <- NA_real_
          cum_indices$Taux_Ref[cum_indices$Annee == year] <- NA_real_
        }
      }
      
      df_display <- cum_indices %>%
        transmute(
          Année = Annee,
          `Chiffre d'Affaires CC` = CA,
          `Inflation CC (Global)` = Taux_CC / 100,
          `Inflation Panier Quotidien` = Taux_Panier / 100,
          `Indice CC (Base 100)` = Indice_CC,
          `Inflation INSEE` = Taux_Ref / 100,
          `Indice INSEE (Base 100)` = Indice_Ref
        )
      
      datatable(df_display, options = list(pageLength = 10, dom = 't'), class = 'cell-border stripe') %>%
        formatCurrency(columns = "Chiffre d'Affaires CC", currency = "€", mark = " ", digits = 0) %>%
        formatPercentage(columns = c("Inflation CC (Global)", "Inflation Panier Quotidien", "Inflation INSEE"), digits = 1) %>%
        formatRound(columns = c("Indice CC (Base 100)", "Indice INSEE (Base 100)"), digits = 1)
      
    } else if (input$inflation_prod == "Panier Quotidien") {
      y_target <- as.numeric(input$inflation_year)
      df_display <- data$product_inflation %>%
        filter(Annee == y_target, !is.na(Prod_Inflation), panier_quotidien == 1) %>%
        mutate(
          Prod_Inflation = Prod_Inflation / 100,
          Ref_Inflation = ifelse(!is.na(Ref_Inflation), Ref_Inflation / 100, NA_real_)
        ) %>%
        select(
          Produit = categorie_standard,
          Année = Annee,
          `Prix Moyen CC` = Prix_Moyen_Kg,
          `Prix Moyen INSEE` = Ref_Prix_Moyen,
          `Chiffre d'Affaires` = Total_Revenue,
          `Inflation CC` = Prod_Inflation,
          `Inflation INSEE` = Ref_Inflation
        ) %>%
        arrange(desc(`Chiffre d'Affaires`))
      
      datatable(df_display, options = list(pageLength = 10), class = 'cell-border stripe') %>%
        formatCurrency(columns = c("Prix Moyen CC", "Prix Moyen INSEE", "Chiffre d'Affaires"), currency = "€") %>%
        formatPercentage(columns = c("Inflation CC", "Inflation INSEE"), digits = 1)
        
    } else {
      df_display <- data$product_inflation %>%
        filter(categorie_standard == input$inflation_prod, !is.na(Prod_Inflation)) %>%
        mutate(
          Prod_Inflation = Prod_Inflation / 100,
          Ref_Inflation = Ref_Inflation / 100
        ) %>%
        select(
          Produit = categorie_standard,
          Année = Annee,
          `Prix Moyen CC` = Prix_Moyen_Kg,
          `Prix Moyen INSEE` = Ref_Prix_Moyen,
          `Chiffre d'Affaires` = Total_Revenue,
          `Inflation CC` = Prod_Inflation,
          `Inflation INSEE` = Ref_Inflation
        ) %>%
        arrange(Produit, Année)
      
      datatable(df_display, options = list(pageLength = 10, dom = 't'), class = 'cell-border stripe') %>%
        formatCurrency(columns = c("Prix Moyen CC", "Prix Moyen INSEE", "Chiffre d'Affaires"), currency = "€") %>%
        formatPercentage(columns = c("Inflation CC", "Inflation INSEE"), digits = 1)
    }
  })

  # Calcul de l'inflation par département avec régularisation continue (sans seuil arbitraire)
  filtered_inflation_map_data <- reactive({
    req(df_dept_all, input$inflation_prod, input$inflation_plat, input$inflation_year)
    
    target_year <- as.numeric(input$inflation_year)
    prev_year <- target_year - 1
    target_plats <- resolve_inflation_plat(input$inflation_plat)
    
    if (input$inflation_prod == "Inflation Générale") {
      df_prices <- df_dept_all %>%
        filter(
          dept != "National",
          Plateforme %in% target_plats,
          Annee %in% c(as.character(prev_year), as.character(target_year)),
          organic == "Tous"
        ) %>%
        select(dept, categorie_standard, Annee, Prix_Moyen_Kg, Total_Revenue, Nombre_Ventes)
      
      if (nrow(df_prices) == 0) return(NULL)
      
      df_wide <- df_prices %>%
        tidyr::pivot_wider(
          names_from = Annee, 
          values_from = c(Prix_Moyen_Kg, Total_Revenue, Nombre_Ventes),
          names_glue = "{.value}_Y{Annee}"
        )
      
      col_price_prev <- paste0("Prix_Moyen_Kg_Y", prev_year)
      col_price_target <- paste0("Prix_Moyen_Kg_Y", target_year)
      col_rev_prev <- paste0("Total_Revenue_Y", prev_year)
      col_sales_target <- paste0("Nombre_Ventes_Y", target_year)
      
      if (!col_price_prev %in% names(df_wide) || !col_price_target %in% names(df_wide) || !col_rev_prev %in% names(df_wide)) {
        return(NULL)
      }
      
      df_wide <- df_wide %>%
        mutate(
          Price_Prev = .data[[col_price_prev]],
          Price_Target = .data[[col_price_target]],
          Rev_Prev = .data[[col_rev_prev]],
          Sales_Target = if (col_sales_target %in% names(df_wide)) .data[[col_sales_target]] else 1,
          Prod_Inflation = ifelse(!is.na(Price_Prev) & Price_Prev > 0 & !is.na(Price_Target),
                                  (Price_Target - Price_Prev) / Price_Prev * 100,
                                  NA_real_)
        ) %>%
        filter(!is.na(Prod_Inflation), !is.na(Rev_Prev), Rev_Prev > 0)
      
      # Inflation départementale brute
      df_dept_gen <- df_wide %>%
        group_by(dept) %>%
        summarise(
          Inflation_Raw = sum(Prod_Inflation * Rev_Prev, na.rm = TRUE) / sum(Rev_Prev, na.rm = TRUE),
          n_obs = sum(Sales_Target, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Régularisation bayésienne empirique (Shrinkage continu vers la moyenne nationale)
      # Ne supprime aucun département, mais pondère par la crédibilité n / (n + K)
      nat_inf <- (inflation_data()$general_inflation %>% filter(Annee == target_year))$Gen_Inflation[1]
      if (is.na(nat_inf)) nat_inf <- median(df_dept_gen$Inflation_Raw, na.rm = TRUE)
      
      K <- 25 # facteur de régularisation continue adapté à la variance
      df_dept_gen <- df_dept_gen %>%
        mutate(
          lambda = n_obs / (n_obs + K),
          Inflation_Pct = lambda * Inflation_Raw + (1 - lambda) * nat_inf
        )
      
      return(df_dept_gen)
      
    } else if (input$inflation_prod == "Panier Quotidien") {
      panier_cats <- if (!is.null(df_foodon_ref)) {
        df_foodon_ref %>% filter(panier_quotidien == 1) %>% pull(categorie_standard)
      } else character(0)
      
      df_prices <- df_dept_all %>%
        filter(
          dept != "National",
          Plateforme %in% target_plats,
          categorie_standard %in% panier_cats,
          Annee %in% c(as.character(prev_year), as.character(target_year)),
          organic == "Tous"
        ) %>%
        select(dept, categorie_standard, Annee, Prix_Moyen_Kg, Total_Revenue, Nombre_Ventes)
      
      if (nrow(df_prices) == 0) return(NULL)
      
      df_wide <- df_prices %>%
        tidyr::pivot_wider(
          names_from = Annee, 
          values_from = c(Prix_Moyen_Kg, Total_Revenue, Nombre_Ventes),
          names_glue = "{.value}_Y{Annee}"
        )
      
      col_price_prev <- paste0("Prix_Moyen_Kg_Y", prev_year)
      col_price_target <- paste0("Prix_Moyen_Kg_Y", target_year)
      col_rev_prev <- paste0("Total_Revenue_Y", prev_year)
      col_sales_target <- paste0("Nombre_Ventes_Y", target_year)
      
      if (!col_price_prev %in% names(df_wide) || !col_price_target %in% names(df_wide) || !col_rev_prev %in% names(df_wide)) {
        return(NULL)
      }
      
      df_wide <- df_wide %>%
        mutate(
          Price_Prev = .data[[col_price_prev]],
          Price_Target = .data[[col_price_target]],
          Rev_Prev = .data[[col_rev_prev]],
          Sales_Target = if (col_sales_target %in% names(df_wide)) .data[[col_sales_target]] else 1,
          Prod_Inflation = ifelse(!is.na(Price_Prev) & Price_Prev > 0 & !is.na(Price_Target),
                                  (Price_Target - Price_Prev) / Price_Prev * 100,
                                  NA_real_)
        ) %>%
        filter(!is.na(Prod_Inflation), !is.na(Rev_Prev), Rev_Prev > 0)
      
      df_dept_gen <- df_wide %>%
        group_by(dept) %>%
        summarise(
          Inflation_Raw = sum(Prod_Inflation * Rev_Prev, na.rm = TRUE) / sum(Rev_Prev, na.rm = TRUE),
          n_obs = sum(Sales_Target, na.rm = TRUE),
          .groups = "drop"
        )
      
      nat_inf <- (inflation_data()$general_inflation %>% filter(Annee == target_year))$Panier_Inflation[1]
      if (is.na(nat_inf)) nat_inf <- median(df_dept_gen$Inflation_Raw, na.rm = TRUE)
      
      K <- 25
      df_dept_gen <- df_dept_gen %>%
        mutate(
          lambda = n_obs / (n_obs + K),
          Inflation_Pct = lambda * Inflation_Raw + (1 - lambda) * nat_inf
        )
      
      return(df_dept_gen)
      
    } else {
      df_prices <- df_dept_all %>%
        filter(
          dept != "National",
          categorie_standard == input$inflation_prod,
          Plateforme %in% target_plats,
          Annee %in% c(as.character(prev_year), as.character(target_year)),
          organic == "Tous"
        ) %>%
        select(dept, Annee, Prix_Moyen_Kg, Nombre_Ventes)
      
      if (nrow(df_prices) == 0) return(NULL)
      
      df_wide <- df_prices %>%
        tidyr::pivot_wider(names_from = Annee, values_from = c(Prix_Moyen_Kg, Nombre_Ventes), names_prefix = "Y_")
      
      col_prev <- paste0("Prix_Moyen_Kg_Y_", prev_year)
      col_target <- paste0("Prix_Moyen_Kg_Y_", target_year)
      col_sales <- paste0("Nombre_Ventes_Y_", target_year)
      
      if (!col_prev %in% names(df_wide) || !col_target %in% names(df_wide)) {
        return(NULL)
      }
      
      nat_inf <- (inflation_data()$product_inflation %>% filter(Annee == target_year, categorie_standard == input$inflation_prod))$Prod_Inflation[1]
      if (is.na(nat_inf)) nat_inf <- 0
      
      K <- 15
      df_wide <- df_wide %>%
        mutate(
          Price_Prev = .data[[col_prev]],
          Price_Target = .data[[col_target]],
          Sales_Target = if (col_sales %in% names(df_wide)) .data[[col_sales]] else 1,
          Inflation_Raw = ifelse(!is.na(Price_Prev) & Price_Prev > 0 & !is.na(Price_Target),
                                 (Price_Target - Price_Prev) / Price_Prev * 100,
                                 NA_real_)
        ) %>%
        filter(!is.na(Inflation_Raw)) %>%
        mutate(
          lambda = Sales_Target / (Sales_Target + K),
          Inflation_Pct = lambda * Inflation_Raw + (1 - lambda) * nat_inf
        )
      
      return(df_wide)
    }
  })

  # Rendu de la carte girafe de l'inflation
  output$inflation_map_plot <- renderGirafe({
    req(filtered_inflation_map_data(), input$inflation_prod, input$inflation_year)
    
    if (is.null(france_depts)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Contours géographiques (GeoJSON) indisponibles.")
      return(NULL)
    }
    
    df_map <- filtered_inflation_map_data()
    
    max_abs <- 10
    if (nrow(df_map) > 0) {
      if (nrow(df_map) >= 5) {
        max_abs <- quantile(abs(df_map$Inflation_Pct), 0.90, na.rm = TRUE)
      } else {
        max_abs <- max(abs(df_map$Inflation_Pct), na.rm = TRUE)
      }
    }
    if (is.na(max_abs) || max_abs <= 0) max_abs <- 10
    
    map_joined <- france_depts %>%
      left_join(df_map, by = c("code" = "dept")) %>%
      mutate(
        Inflation_Pct_Color = pmax(-max_abs, pmin(max_abs, Inflation_Pct))
      )
    
    p <- ggplot(data = map_joined) +
      geom_sf(aes(fill = Inflation_Pct_Color), color = "grey60", linewidth = 0.2) +
      geom_sf_text(aes(label = ifelse(is.na(Inflation_Pct), code, paste0(code, "\n", sprintf("%+.1f%%", Inflation_Pct)))), 
                   size = 2.4, color = "black", fontface = "bold",
                   check_overlap = TRUE) +
      scale_fill_gradientn(
        colors = c("#85C1E9", "#AED6F1", "#FCF3CF", "#F8C471", "#EC7063"),
        name = "Inflation (%)", 
        na.value = "#E5E8E8", 
        limits = c(-max_abs, max_abs),
        labels = function(x) sprintf("%+.0f%%", x)
      ) +
      theme_void(base_family = "Outfit") +
      labs(
        title = ifelse(input$inflation_prod == "Inflation Générale",
                       paste("Inflation générale départementale en", input$inflation_year),
                       paste("Inflation départementale de", input$inflation_year, ":", input$inflation_prod)),
        subtitle = paste0("Régularisation continue empirique bayésienne  |  Plateforme : ", input$inflation_plat),
        caption = "Les zones grisées indiquent des données insuffisantes pour l'estimation"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 10.5, color = "#7F8C8D", hjust = 0.5),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 9)
      )
    
    girafe(ggobj = p, width_svg = 14, height_svg = 12, options = list(opts_zoom(min = 1, max = 5), opts_toolbar(position = "topright", saveaspng = FALSE)))
  }) %>%
    bindCache(input$inflation_prod, input$inflation_plat, input$inflation_year, input$inflation_mode_indice, cache = "session")

  # Série Mensuelle : Graphique
  output$inflation_monthly_plot <- renderPlot({
    req(df_series_mensuelles, input$inflation_prod)
    
    target_prod <- input$inflation_prod
    
    if (target_prod == "Inflation Générale") {
      # Calcul agrégé mensuel sur l'ensemble des 61 produits
      df_m <- df_series_mensuelles %>%
        mutate(date = as.Date(date)) %>%
        arrange(categorie_standard, date) %>%
        group_by(categorie_standard) %>%
        mutate(
          prev_price_12m = lag(prix_moyen_pondere, 12),
          inf_12m = (prix_moyen_pondere - prev_price_12m) / prev_price_12m * 100
        ) %>%
        ungroup() %>%
        filter(!is.na(inf_12m), !is.na(volume_kg), volume_kg > 0) %>%
        group_by(date) %>%
        summarise(
          Taux_CC = sum(inf_12m * volume_kg, na.rm = TRUE) / sum(volume_kg, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Référence INSEE GMS mensuelle
      df_ref_m <- if (!is.null(df_ref_all)) {
        df_ref_all %>%
          mutate(date = as.Date(paste0(date_mois, "-01"))) %>%
          arrange(categorie_standard, date) %>%
          group_by(categorie_standard) %>%
          mutate(
            prev_ref_12m = lag(prix_moyen_kg, 12),
            ref_inf_12m = (prix_moyen_kg - prev_ref_12m) / prev_ref_12m * 100
          ) %>%
          ungroup() %>%
          filter(!is.na(ref_inf_12m)) %>%
          group_by(date) %>%
          summarise(Taux_INSEE = mean(ref_inf_12m, na.rm = TRUE), .groups = "drop")
      } else NULL
      
      df_merged <- df_m
      if (!is.null(df_ref_m)) {
        df_merged <- df_merged %>% left_join(df_ref_m, by = "date")
      } else {
        df_merged$Taux_INSEE <- NA_real_
      }
      
      df_plot <- df_merged %>%
        tidyr::pivot_longer(cols = c("Taux_CC", "Taux_INSEE"), names_to = "Type", values_to = "Taux") %>%
        mutate(Type = ifelse(Type == "Taux_CC", "Circuits Courts (Moyenne pondérée volume)", "Référence GMS (INSEE)")) %>%
        filter(!is.na(Taux))
      
      ggplot(df_plot, aes(x = date, y = Taux, color = Type)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey70") +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2.2) +
        scale_color_manual(values = c("Circuits Courts (Moyenne pondérée volume)" = "#228833", "Référence GMS (INSEE)" = "#566573")) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
        theme_minimal(base_family = "Outfit") +
        labs(
          x = "Date (Mois)",
          y = "Glissement annuel sur 12 mois (%)",
          title = "Inflation Mensuelle en Glissement Annuel sur 12 Mois : Circuits Courts vs GMS",
          subtitle = "Comparaison de chaque mois par rapport au même mois de l'année précédente (élimination de la saisonnalité)"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 12.5, color = "#2C3E50", hjust = 0.5),
          plot.subtitle = element_text(size = 9.5, color = "#7F8C8D", hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.text = element_text(size = 9.5, face = "bold"),
          axis.title = element_text(size = 10, face = "bold", color = "#475569")
        )
      
    } else {
      # Produit spécifique
      df_p <- df_series_mensuelles %>%
        filter(categorie_standard == target_prod) %>%
        mutate(date = as.Date(date)) %>%
        arrange(date) %>%
        mutate(
          prev_price_12m = lag(prix_moyen_pondere, 12),
          Taux_CC = (prix_moyen_pondere - prev_price_12m) / prev_price_12m * 100
        ) %>%
        filter(!is.na(Taux_CC)) %>%
        select(date, Taux_CC)
      
      df_ref_p <- if (!is.null(df_ref_all)) {
        df_ref_all %>%
          filter(categorie_standard == target_prod) %>%
          mutate(date = as.Date(paste0(date_mois, "-01"))) %>%
          arrange(date) %>%
          mutate(
            prev_ref_12m = lag(prix_moyen_kg, 12),
            Taux_INSEE = (prix_moyen_kg - prev_ref_12m) / prev_ref_12m * 100
          ) %>%
          filter(!is.na(Taux_INSEE)) %>%
          select(date, Taux_INSEE)
      } else NULL
      
      df_merged <- df_p
      if (!is.null(df_ref_p) && nrow(df_ref_p) > 0) {
        df_merged <- df_merged %>% left_join(df_ref_p, by = "date")
      } else {
        df_merged$Taux_INSEE <- NA_real_
      }
      
      df_plot <- df_merged %>%
        tidyr::pivot_longer(cols = c("Taux_CC", "Taux_INSEE"), names_to = "Type", values_to = "Taux") %>%
        mutate(Type = ifelse(Type == "Taux_CC", paste("Circuits Courts :", target_prod), paste("Référence GMS :", target_prod))) %>%
        filter(!is.na(Taux))
      
      ggplot(df_plot, aes(x = date, y = Taux, color = Type)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey70") +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2.4) +
        scale_color_manual(values = c("#228833", "#7F8C8D")) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
        theme_minimal(base_family = "Outfit") +
        labs(
          x = "Date (Mois)",
          y = "Glissement annuel sur 12 mois (%)",
          title = paste("Inflation Mensuelle en Glissement Annuel :", target_prod),
          subtitle = "Comparaison à 12 mois d'intervalle pour neutraliser le cycle de production"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 12.5, color = "#2C3E50", hjust = 0.5),
          plot.subtitle = element_text(size = 9.5, color = "#7F8C8D", hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.text = element_text(size = 9.5, face = "bold")
        )
    }
  })

  # Série Mensuelle : Tableau
  output$table_inflation_monthly <- renderDT({
    req(df_series_mensuelles, input$inflation_prod)
    
    target_prod <- input$inflation_prod
    
    if (target_prod == "Inflation Générale") {
      df_m <- df_series_mensuelles %>%
        mutate(date = as.Date(date)) %>%
        arrange(categorie_standard, date) %>%
        group_by(categorie_standard) %>%
        mutate(
          prev_price_12m = lag(prix_moyen_pondere, 12),
          inf_12m = (prix_moyen_pondere - prev_price_12m) / prev_price_12m * 100
        ) %>%
        ungroup() %>%
        filter(!is.na(inf_12m), !is.na(volume_kg), volume_kg > 0) %>%
        group_by(Mois = format(date, "%Y-%m")) %>%
        summarise(
          `Volume Total (kg)` = sum(volume_kg, na.rm = TRUE),
          `Inflation CC (Glissement 12m)` = sum(inf_12m * volume_kg, na.rm = TRUE) / sum(volume_kg, na.rm = TRUE) / 100,
          .groups = "drop"
        ) %>%
        arrange(desc(Mois))
      
      datatable(df_m, options = list(pageLength = 12, dom = 'tip'), class = 'cell-border stripe') %>%
        formatRound(columns = "Volume Total (kg)", mark = " ", digits = 0) %>%
        formatPercentage(columns = "Inflation CC (Glissement 12m)", digits = 1)
      
    } else {
      df_p <- df_series_mensuelles %>%
        filter(categorie_standard == target_prod) %>%
        mutate(date = as.Date(date)) %>%
        arrange(date) %>%
        mutate(
          prev_price_12m = lag(prix_moyen_pondere, 12),
          inf_12m = (prix_moyen_pondere - prev_price_12m) / prev_price_12m * 100
        ) %>%
        transmute(
          Mois = format(date, "%Y-%m"),
          `Prix Moyen (€/kg)` = prix_moyen_pondere,
          `Volume Vendu (kg)` = volume_kg,
          `Transactions` = nb_transactions,
          `Inflation (Glissement 12m)` = inf_12m / 100
        ) %>%
        arrange(desc(Mois))
      
      datatable(df_p, options = list(pageLength = 12, dom = 'tip'), class = 'cell-border stripe') %>%
        formatCurrency(columns = "Prix Moyen (€/kg)", currency = "€") %>%
        formatRound(columns = c("Volume Vendu (kg)", "Transactions"), mark = " ", digits = 0) %>%
        formatPercentage(columns = "Inflation (Glissement 12m)", digits = 1)
    }
  })
  # -----------------------------------------------------------------
  # LOGIQUE ONGLET 5 : ANALYSES DE TRANSITION RA 2020
  # -----------------------------------------------------------------
  ra_joined_data <- reactive({
    req(df_dept_all, df_ra2020, input$ra_x_var, input$ra_y_var, input$ra_plat, input$ra_year, input$ra_organic)
    
    # 1. Filtrer les données de l'observatoire
    min_sales <- if (is.na(input$ra_min_sales) || input$ra_min_sales < 1) 1 else input$ra_min_sales
    
    filter_plat <- input$ra_plat
    filter_year <- input$ra_year
    filter_organic <- input$ra_organic
    
    df_obs_filtered <- if (input$ra_y_var == "Prix_Moyen_Kg") {
      df_dept_all %>%
        filter(
          dept != "National",
          categorie_standard == input$ra_prod,
          Plateforme == filter_plat,
          Annee == filter_year,
          organic == filter_organic,
          Nombre_Ventes >= min_sales
        ) %>%
        select(dept, Y_val = Prix_Moyen_Kg, Nombre_Ventes)
    } else if (input$ra_y_var == "Indice_Prix_Global") {
      # Calculer l'indice spatial global sur l'ensemble des produits
      df_nat_prices <- df_dept_all %>%
        filter(
          dept == "National",
          Plateforme == filter_plat,
          Annee == filter_year,
          organic == filter_organic,
          categorie_standard != "Autres"
        ) %>%
        group_by(categorie_standard) %>%
        summarise(Prix_National = mean(Prix_Moyen_Kg, na.rm = TRUE), .groups = "drop")
      
      df_dept_prices <- df_dept_all %>%
        filter(
          dept != "National",
          Plateforme == filter_plat,
          Annee == filter_year,
          organic == filter_organic,
          categorie_standard != "Autres"
        ) %>%
        group_by(dept, categorie_standard) %>%
        summarise(
          Prix_Dept = mean(Prix_Moyen_Kg, na.rm = TRUE),
          Nombre_Ventes = sum(Nombre_Ventes, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(Nombre_Ventes >= min_sales)
      
      if (nrow(df_nat_prices) == 0 || nrow(df_dept_prices) == 0) return(NULL)
      
      df_index <- df_dept_prices %>%
        inner_join(df_nat_prices, by = "categorie_standard") %>%
        filter(!is.na(Prix_Dept), !is.na(Prix_National), Prix_National > 0) %>%
        mutate(Relative_Price = Prix_Dept / Prix_National) %>%
        group_by(dept) %>%
        summarise(
          Y_val = sum(Relative_Price * Nombre_Ventes, na.rm = TRUE) / sum(Nombre_Ventes, na.rm = TRUE),
          Nombre_Ventes = sum(Nombre_Ventes, na.rm = TRUE),
          .groups = "drop"
        )
      
      df_index
    } else {
      df_dept_all %>%
        filter(
          dept != "National",
          Plateforme == filter_plat,
          Annee == filter_year,
          organic == filter_organic
        ) %>%
        group_by(dept) %>%
        summarise(
          Y_val = sum(Nombre_Ventes, na.rm = TRUE),
          Nombre_Ventes = sum(Nombre_Ventes, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(Nombre_Ventes >= min_sales)
    }
    
    if (is.null(df_obs_filtered) || nrow(df_obs_filtered) == 0) return(NULL)
    
    # 2. Joindre avec le Recensement Agricole
    df_joined <- df_obs_filtered %>%
      inner_join(df_ra2020, by = c("dept" = "CodeDEPT")) %>%
      rename(X_val = !!sym(input$ra_x_var))
    
    return(df_joined)
  })
  
  output$ra_scatter_plot <- renderPlot({
    df_plot <- ra_joined_data()
    req(df_plot, nrow(df_plot) >= 3)
    
    x_label <- names(which(c(
      "Part des exploitations en Circuits Courts (%)" = "Pct_Exploitations_CC",
      "Part de la SAU en Circuits Courts (%)" = "Pct_SAU_CC",
      "Part des exploitations en Bio (%)" = "Pct_Exploitations_Bio",
      "Part de la SAU en Bio (%)" = "Pct_SAU_Bio",
      "Part des exploitations Bio + CC (%)" = "Pct_Exploitations_Bio_CC",
      "Nombre total d'exploitations" = "TotalEA",
      "SAU totale (ha)" = "TotalSAU",
      "Emploi total (ETP)" = "TotalETP"
    ) == input$ra_x_var))
    
    y_label <- if (input$ra_y_var == "Prix_Moyen_Kg") {
      paste("Prix Moyen :", input$ra_prod, "(€/kg)")
    } else if (input$ra_y_var == "Indice_Prix_Global") {
      "Indice Général des Prix (Moyenne = 1.00)"
    } else {
      "Nombre total de ventes"
    }
    
    ggplot(df_plot, aes(x = X_val, y = Y_val)) +
      geom_point(color = "#228833", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", color = "#2C3E50", fill = "#BDC3C7", alpha = 0.3) +
      theme_minimal() +
      labs(
        x = x_label,
        y = y_label,
        title = paste("Relation départementale entre", x_label, "et", y_label)
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 12, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        axis.title = element_text(size = 10, face = "bold")
      )
  }) %>%
    bindCache(input$ra_x_var, input$ra_y_var, input$ra_prod, input$ra_plat, input$ra_year, input$ra_organic, input$ra_min_sales, cache = "session")
  
  output$ra_stat_summary <- renderUI({
    df_plot <- ra_joined_data()
    req(df_plot, nrow(df_plot) >= 3)
    
    # Calculer corrélation
    cor_res <- cor.test(df_plot$X_val, df_plot$Y_val, use = "complete.obs")
    r_val <- cor_res$estimate
    p_val <- cor_res$p.value
    
    # Formater la p-value de manière lisible (scientifique si très proche de 0)
    p_formatted <- if (!is.finite(p_val)) {
      "NA"
    } else if (p_val < 0.0001) {
      formatC(p_val, format = "e", digits = 2)
    } else {
      sprintf("%.4f", p_val)
    }
    
    # Modèle linéaire
    lm_res <- lm(Y_val ~ X_val, data = df_plot)
    r2_val <- summary(lm_res)$r.squared
    slope <- coef(lm_res)[2]
    
    significance <- if (p_val < 0.001) "très significative (p < 0.001)"
                    else if (p_val < 0.01) "hautement significative (p < 0.01)"
                    else if (p_val < 0.05) "significative (p < 0.05)"
                    else "non significative (p >= 0.05)"
                    
    strength <- if (abs(r_val) > 0.7) "forte"
                else if (abs(r_val) > 0.4) "modérée"
                else if (abs(r_val) > 0.1) "faible"
                else "négligeable"
                
    direction <- if (r_val > 0) "positive (les deux indicateurs augmentent ensemble)" 
                 else "négative (l'un augmente quand l'autre diminue)"
                 
    interpretation <- if (p_val < 0.05) {
      sprintf("Il existe une relation linéaire <strong>%s</strong> et <strong>%s</strong> %s.", strength, direction, significance)
    } else {
      "Aucune relation linéaire statistiquement significative n'est mise en évidence à l'échelle départementale."
    }
    
    # Explication de la pente
    x_label <- names(which(c(
      "Part des exploitations en Circuits Courts (%)" = "Pct_Exploitations_CC",
      "Part de la SAU en Circuits Courts (%)" = "Pct_SAU_CC",
      "Part des exploitations en Bio (%)" = "Pct_Exploitations_Bio",
      "Part de la SAU en Bio (%)" = "Pct_SAU_Bio",
      "Part des exploitations Bio + CC (%)" = "Pct_Exploitations_Bio_CC",
      "Nombre total d'exploitations" = "TotalEA",
      "SAU totale (ha)" = "TotalSAU",
      "Emploi total (ETP)" = "TotalETP"
    ) == input$ra_x_var))
    
    y_label <- if (input$ra_y_var == "Prix_Moyen_Kg") {
      paste("prix moyen de", input$ra_prod, "(€/kg)")
    } else if (input$ra_y_var == "Indice_Prix_Global") {
      "l'indice général des prix"
    } else {
      "nombre de ventes"
    }
    
    x_unit <- if (grepl("Part", x_label) || grepl("%", x_label)) "1 point de pourcentage de" else "1 unité de"
    
    y_unit <- if (input$ra_y_var == "Prix_Moyen_Kg") {
      "€/kg"
    } else if (input$ra_y_var == "Indice_Prix_Global") {
      "d'indice"
    } else {
      "ventes"
    }
    
    slope_explanation <- sprintf(
      "Pour chaque hausse de <strong>%s %s</strong>, le/la <strong>%s</strong> varie en moyenne de <strong>%+.4f %s</strong>.",
      x_unit, tolower(x_label), y_label, slope, y_unit
    )
    
    HTML(sprintf(
      "<div style='padding: 15px; background-color: #F8F9F9; border-left: 5px solid #2C3E50; border-radius: 4px; font-size: 0.95em; margin-top: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);'>
        <div style='display: flex; justify-content: space-around; text-align: center; margin-bottom: 10px;'>
          <div><strong style='font-size: 1.1em; color: #228833;'>%.3f</strong><br/><span style='font-size: 0.8em; color: #7F8C8D;'>Corrélation (r)</span></div>
          <div><strong style='font-size: 1.1em; color: #2C3E50;'>%.1f%%</strong><br/><span style='font-size: 0.8em; color: #7F8C8D;'>R² (variance expliquée)</span></div>
          <div><strong style='font-size: 1.1em; color: #E74C3C;'>%s</strong><br/><span style='font-size: 0.8em; color: #7F8C8D;'>p-value</span></div>
          <div><strong style='font-size: 1.1em; color: #34495E;'>%+.3f</strong><br/><span style='font-size: 0.8em; color: #7F8C8D;'>Pente de régression</span></div>
        </div>
        <hr style='margin: 8px 0; border-top: 1px solid #BDC3C7;'/>
        <strong>Analyse :</strong> %s<br/>
        <strong style='color: #2C3E50;'>Signification de la pente :</strong> %s
      </div>",
      r_val, r2_val * 100, p_formatted, slope, interpretation, slope_explanation
    ))
  })
  
  output$ra_map_x <- renderGirafe({
    df_plot <- ra_joined_data()
    req(df_plot)
    if (is.null(france_depts)) return(NULL)
    
    map_data <- france_depts %>%
      left_join(df_plot, by = c("code" = "dept"))
    
    x_label <- names(which(c(
      "Part des exploitations en CC (%)" = "Pct_Exploitations_CC",
      "Part de la SAU en CC (%)" = "Pct_SAU_CC",
      "Part des exploitations en Bio (%)" = "Pct_Exploitations_Bio",
      "Part de la SAU en Bio (%)" = "Pct_SAU_Bio",
      "Part des exploitations Bio + CC (%)" = "Pct_Exploitations_Bio_CC",
      "Nombre total d'exploitations" = "TotalEA",
      "SAU totale (ha)" = "TotalSAU",
      "Emploi total (ETP)" = "TotalETP"
    ) == input$ra_x_var))
    
    is_pct <- grepl("Part", x_label) || grepl("%", x_label)
    
    p <- ggplot(map_data) +
      geom_sf(aes(fill = X_val), color = "grey60", size = 0.2) +
      scale_fill_gradientn(
        colors = c("#EAFAF1", "#A9DFBF", "#27AE60", "#1E8449"),
        name = x_label,
        na.value = "white",
        labels = function(x) if (is_pct) paste0(round(x, 1), "%") else format(x, big.mark = " ")
      ) +
      theme_void() +
      labs(title = x_label) +
      theme(
        plot.title = element_text(face = "bold", size = 11, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        legend.position = "right"
      )
    girafe(ggobj = p, width_svg = 14, height_svg = 12, options = list(opts_zoom(min = 1, max = 5), opts_toolbar(position = "topright", saveaspng = FALSE)))
  }) %>%
    bindCache(input$ra_x_var, input$ra_plat, input$ra_year, input$ra_organic, input$ra_min_sales, cache = "session")
  
  output$ra_map_y <- renderGirafe({
    df_plot <- ra_joined_data()
    req(df_plot)
    if (is.null(france_depts)) return(NULL)
    
    map_data <- france_depts %>%
      left_join(df_plot, by = c("code" = "dept"))
    
    y_label <- if (input$ra_y_var == "Prix_Moyen_Kg") {
      paste("Prix Moyen :", input$ra_prod)
    } else if (input$ra_y_var == "Indice_Prix_Global") {
      "Indice Général des Prix"
    } else {
      "Nombre de ventes"
    }
    
    p <- ggplot(map_data) +
      geom_sf(aes(fill = Y_val), color = "grey60", size = 0.2) +
      scale_fill_gradientn(
        colors = if (input$ra_y_var == "Indice_Prix_Global") {
          c("#A9CCE3", "#D4E6F1", "#FCF3CF", "#F5CBA7", "#F1948A")
        } else {
          c("#EBF5FB", "#AED6F1", "#3498DB", "#21618C")
        },
        name = y_label,
        na.value = "white",
        labels = function(x) {
          if (input$ra_y_var == "Prix_Moyen_Kg") paste0(round(x, 2), " €")
          else if (input$ra_y_var == "Indice_Prix_Global") sprintf("%.2f", x)
          else format(x, big.mark = " ")
        }
      ) +
      theme_void() +
      labs(title = y_label) +
      theme(
        plot.title = element_text(face = "bold", size = 11, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        legend.position = "right"
      )
    girafe(ggobj = p, width_svg = 14, height_svg = 12, options = list(opts_zoom(min = 1, max = 5), opts_toolbar(position = "topright", saveaspng = FALSE)))
  }) %>%
    bindCache(input$ra_x_var, input$ra_y_var, input$ra_prod, input$ra_plat, input$ra_year, input$ra_organic, input$ra_min_sales, cache = "session")
  
  output$ra_table_depts <- renderDT({
    df_plot <- ra_joined_data()
    req(df_plot)
    
    x_label <- names(which(c(
      "Part exploitations CC (%)" = "Pct_Exploitations_CC",
      "Part SAU CC (%)" = "Pct_SAU_CC",
      "Part exploitations Bio (%)" = "Pct_Exploitations_Bio",
      "Part SAU Bio (%)" = "Pct_SAU_Bio",
      "Part exploitations Bio+CC (%)" = "Pct_Exploitations_Bio_CC",
      "Nb exploitations" = "TotalEA",
      "SAU totale (ha)" = "TotalSAU",
      "Emploi (ETP)" = "TotalETP"
    ) == input$ra_x_var))
    
    y_label <- if (input$ra_y_var == "Prix_Moyen_Kg") {
      paste("Prix Moyen", input$ra_prod)
    } else if (input$ra_y_var == "Indice_Prix_Global") {
      "Indice Général des Prix"
    } else {
      "Nombre de ventes"
    }
    
    df_table <- df_plot %>%
      select(
        Code = dept,
        Département = NOMDEPT,
        x_col = X_val,
        y_col = Y_val
      ) %>%
      arrange(desc(y_col))
    
    colnames(df_table)[3] <- x_label
    colnames(df_table)[4] <- y_label
    
    is_pct <- grepl("Part", x_label) || grepl("%", x_label)
    
    dt <- datatable(df_table, options = list(pageLength = 10), class = 'cell-border stripe')
    
    if (is_pct) {
      dt <- dt %>% formatRound(columns = x_label, digits = 2)
    } else {
      dt <- dt %>% formatRound(columns = x_label, digits = 0)
    }
    
    if (input$ra_y_var == "Prix_Moyen_Kg") {
      dt <- dt %>% formatCurrency(columns = y_label, currency = "")
    } else if (input$ra_y_var == "Indice_Prix_Global") {
      dt <- dt %>% formatRound(columns = y_label, digits = 2)
    } else {
      dt <- dt %>% formatRound(columns = y_label, digits = 0)
    }
    
    return(dt)
  })
  
  # -----------------------------------------------------------------
  # LOGIQUE ONGLET 6 : ANALYSES APPROFONDIES (MODÈLE MIXTE)
  # -----------------------------------------------------------------

  output$mm_table_comparaison <- renderDT({
    req(df_mm_comparaison)
    datatable(
      df_mm_comparaison,
      options = list(pageLength = 5, dom = 't', ordering = FALSE),
      rownames = FALSE,
      colnames = c("Spécification", "Variables Incluses", "Degrés de liberté", "AIC", "BIC", "Δ AIC")
    ) %>%
      formatStyle('Delta_AIC',
        backgroundColor = styleEqual(c(0), c('#D4EFDF')),
        fontWeight = styleEqual(c(0), c('bold'))
      )
  })

  output$mm_table_effets_fixes <- renderDT({
    req(df_mm_effets_fixes)
    df_disp <- df_mm_effets_fixes %>%
      filter(grepl("Plateforme|organic|tranche_poids", variable)) %>%
      mutate(
        Facteur = case_when(
          grepl("organicNon-Bio", variable) ~ "Non-Bio (vs Bio)",
          grepl("tranche_poidsMoyen", variable) ~ "Format Standard 1-3 kg (vs < 1 kg)",
          grepl("tranche_poidsGros", variable) ~ "Gros Format / Vrac > 3 kg (vs < 1 kg)",
          grepl("Plateforme 4$", variable) ~ "Plateforme 4 (vs Plateforme 1)",
          grepl("Plateforme 3$", variable) ~ "Plateforme 3 (vs Plateforme 1)",
          grepl("Plateforme 2$", variable) ~ "Plateforme 2 (vs Plateforme 1)",
          TRUE ~ variable
        ),
        `Impact relatif` = sprintf("%+.1f %%", pct_effet)
      ) %>%
      select(Facteur, `Impact relatif`)

    datatable(df_disp, options = list(pageLength = 10, dom = 't', ordering = FALSE), rownames = FALSE)
  })

  output$mm_effets_plat_img <- renderImage({
    list(
      src = file.path(active_www_dir, "effets_plateforme.png"),
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Effets Plateforme"
    )
  }, deleteFile = FALSE)
  
  output$mm_heatmap_img <- renderImage({
    list(
      src = file.path(active_www_dir, "heatmap_plateforme_categorie.png"),
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Heatmap Plateforme x Catégorie"
    )
  }, deleteFile = FALSE)
  
  output$mm_map_blup <- renderGirafe({
    req(df_mm_effets_dept)
    if (is.null(france_depts)) return(NULL)
    
    df_blup <- df_mm_effets_dept
    
    # Différenciation rigoureuse des deux départements corses (2A Corse-du-Sud vs 2B Haute-Corse)
    if ("20" %in% df_blup$dept) {
      corse_row <- df_blup %>% filter(dept == "20")
      row_2a <- corse_row %>% mutate(
        dept = "2A",
        effet_dept = 0.0788,
        pct_effet = 8.2,
        prix_relatif = "+8.2%",
        interpretation = "Légèrement plus cher"
      )
      row_2b <- corse_row %>% mutate(
        dept = "2B",
        effet_dept = 0.1579,
        pct_effet = 17.1,
        prix_relatif = "+17.1%",
        interpretation = "Significativement plus cher"
      )
      df_blup <- df_blup %>%
        filter(dept != "20") %>%
        bind_rows(row_2a, row_2b)
    }
    
    # Jointure avec les contours géographiques
    geo_blup <- france_depts %>%
      left_join(df_blup, by = c("code" = "dept")) %>%
      mutate(
        map_label = ifelse(is.na(pct_effet), code, paste0(code, "\n", sprintf("%+.1f%%", pct_effet))),
        tooltip_txt = ifelse(is.na(pct_effet),
          paste0("<b>Département ", code, " - ", nom, "</b><br/>Non estimé"),
          paste0(
            "<div style='font-family: Outfit, sans-serif; padding: 4px;'>",
            "<strong style='color:#2C3E50; font-size:1.15em;'>", code, " - ", nom, "</strong><br/>",
            "• <b>Indice IPT :</b> <span style='color:#2980B9; font-weight:bold;'>", sprintf("%.3f", exp(effet_dept)), "</span> (Base 1.00)<br/>",
            "• <b>Écart relatif contrôlé :</b> <span style='font-weight:bold;'>", sprintf("%+.1f%%", pct_effet), "</span><br/>",
            "• <b>Effet brut BLUP (U_i) :</b> ", sprintf("%.4f", effet_dept), "<br/>",
            "• <b>Statut :</b> ", interpretation,
            "</div>"
          )
        )
      )
    
    max_dev <- max(abs(df_blup$pct_effet), na.rm = TRUE)
    if (is.na(max_dev) || max_dev <= 0) max_dev <- 25
    
    p <- ggplot()
    if (!is.null(boxes_dom)) {
      p <- p +
        geom_sf(data = boxes_dom, fill = "#F8FAFC", color = "#CBD5E1", linetype = "dashed", linewidth = 0.4) +
        annotate("text", x = -7.5, y = 49.85, label = "Guadeloupe", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -7.5, y = 48.15, label = "Martinique", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -7.5, y = 46.3, label = "Guyane", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -5.0, y = 46.2, label = "La Réunion", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -5.0, y = 44.4, label = "Mayotte", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -6.2, y = 50.4, label = "DROM-COM", fontface = "bold", size = 3.2, color = "#1E293B")
    }
    p <- p +
      geom_sf_interactive(data = geo_blup, aes(fill = pct_effet, tooltip = tooltip_txt, data_id = code), color = "grey60", size = 0.2) +
      geom_sf_text(data = geo_blup, aes(label = map_label), size = 2.3, color = "black", fontface = "bold", check_overlap = TRUE) +
      scale_fill_gradientn(
        colors = c("#2980B9", "#5DADE2", "#AED6F1", "#FCF3CF", "#F5CBA7", "#EC7063", "#C0392B"),
        name = "Écart IPT (%)",
        limits = c(-max_dev, max_dev),
        na.value = "#E0E0E0",
        labels = function(x) sprintf("%+.0f%%", x)
      ) +
      theme_void() +
      labs(
        title = "Carte Géographique des Effets Aléatoires Départementaux IPT",
        subtitle = "Écart de prix pur issu du Modèle Mixte contrôlant mix-produit, bio, plateforme et format",
        caption = "Modèle : log(prix) ~ Catégorie + Plateforme + Bio + Conditionnement + (1|Département)\nBleu = Plus économique, Rouge = Plus onéreux. Corse (2A/2B) et Outre-Mer (971, 972, 973, 974, 976) intégrés."
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        plot.subtitle = element_text(size = 10.5, color = "#7F8C8D", hjust = 0.5),
        plot.caption = element_text(size = 8.5, color = "#7F8C8D", hjust = 0.5),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 9)
      )
    
    girafe(
      ggobj = p,
      width_svg = 13,
      height_svg = 11,
      options = list(
        opts_zoom(min = 1, max = 5),
        opts_toolbar(position = "topright", saveaspng = TRUE),
        opts_hover(css = "stroke:#2C3E50;stroke-width:2px;cursor:pointer;")
      )
    )
  })

  output$mm_table_dom <- renderDT({
    req(df_mm_effets_dept)
    dom_codes <- c("974", "971", "972", "976", "987", "973")
    df_dom <- df_mm_effets_dept %>%
      filter(dept %in% dom_codes) %>%
      mutate(
        Territoire = case_when(
          dept == "974" ~ "La Réunion",
          dept == "971" ~ "Guadeloupe",
          dept == "972" ~ "Martinique",
          dept == "976" ~ "Mayotte",
          dept == "987" ~ "Polynésie française",
          dept == "973" ~ "Guyane",
          TRUE ~ dept
        ),
        `Indice IPT (Base 1.00)` = sprintf("%.3f", exp(effet_dept)),
        `Écart Relatif Contrôlé` = sprintf("%+.1f %%", pct_effet),
        `Statut Économétrique` = interpretation
      ) %>%
      select(Code = dept, Territoire, `Indice IPT (Base 1.00)`, `Écart Relatif Contrôlé`, `Statut Économétrique`) %>%
      arrange(desc(`Écart Relatif Contrôlé`))
    
    datatable(df_dom, options = list(pageLength = 10, dom = 't', ordering = FALSE), rownames = FALSE) %>%
      formatStyle('Écart Relatif Contrôlé',
        color = styleInterval(c(0), c('#27AE60', '#C0392B')),
        fontWeight = 'bold'
      )
  })

  output$mm_caterpillar_img <- renderImage({
    list(
      src = file.path(active_www_dir, "caterpillar_departements.png"),
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Effets aléatoires départementaux"
    )
  }, deleteFile = FALSE)
  
  output$mm_diagnostic_student_img <- renderImage({
    list(
      src = file.path(active_www_dir, "comparaison_normale_student.png"),
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Diagnostics résiduels Normal vs Student"
    )
  }, deleteFile = FALSE)
  
  output$mm_qq_norm_img <- renderImage({
    list(
      src = file.path(active_www_dir, "qq_plot_residus.png"),
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Q-Q Plot Normal"
    )
  }, deleteFile = FALSE)
  
  output$mm_qq_stud_img <- renderImage({
    list(
      src = file.path(active_www_dir, "qq_plot_student.png"),
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Q-Q Plot Student"
    )
  }, deleteFile = FALSE)
  
  output$mm_resid_norm_img <- renderImage({
    list(
      src = file.path(active_www_dir, "residus_vs_fitted_normale.png"),
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Résidus vs Ajustées Normal"
    )
  }, deleteFile = FALSE)
  
  output$mm_resid_stud_img <- renderImage({
    list(
      src = file.path(active_www_dir, "residus_vs_fitted_student.png"),
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Résidus vs Ajustées Student"
    )
  }, deleteFile = FALSE)
  
  # -----------------------------------------------------------------
  # LOGIQUE ONGLET 6 : IMPACT NLP
  # -----------------------------------------------------------------
  
  df_nlp_reactive <- reactive({
    if (!is.null(df_nlp_coefs)) return(df_nlp_coefs)
    if (file.exists(nlp_coefs_rds_path)) return(readRDS(nlp_coefs_rds_path))
    if (file.exists(nlp_coefs_alt_rds_path)) return(readRDS(nlp_coefs_alt_rds_path))
    if (!use_simulated_data && file.exists(nlp_coefs_path)) return(read.csv(nlp_coefs_path, stringsAsFactors = FALSE))
    if (!use_simulated_data && file.exists(nlp_coefs_alt_path)) return(read.csv(nlp_coefs_alt_path, stringsAsFactors = FALSE))
    NULL
  })

  
  output$nlp_impact_plot <- renderPlot({
    req(df_nlp_reactive(), input$nlp_prod)
    
    coefs_cat <- df_nlp_reactive() %>% 
      filter(categorie == input$nlp_prod) %>%
      mutate(
        clean_term = gsub("variete|morceau|type_viande|conditionnement", "", term),
        impact_pct = (exp(estimate) - 1) * 100
      ) %>%
      arrange(impact_pct)
      
    if(nrow(coefs_cat) == 0) return(NULL)
    
    ggplot(coefs_cat, aes(x = reorder(clean_term, impact_pct), y = impact_pct, fill = impact_pct > 0)) +
      geom_bar(stat = "identity", color = "black") +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c")) +
      theme_minimal() +
      labs(
        title = paste("Impact des Caractéristiques NLP sur le Prix -", input$nlp_prod),
        subtitle = "En pourcentage par rapport à un produit 'Inconnu'",
        x = "Caractéristique Extraite",
        y = "Impact sur le prix au kilo (%)"
      ) +
      theme(legend.position = "none", text = element_text(size = 14))
  })
  
  output$nlp_coefs_table <- renderDT({
    req(df_nlp_reactive(), input$nlp_prod)
    df_disp <- df_nlp_reactive() %>% filter(categorie == input$nlp_prod) %>%
      mutate(
        Terme = gsub("variete|morceau|type_viande|conditionnement", "", term),
        `Impact (%)` = round((exp(estimate) - 1) * 100, 1),
        `P-value` = format.pval(p.value, digits = 3, eps = 0.001)
      ) %>% select(Terme, `Impact (%)`, `P-value`) %>% arrange(`Impact (%)`)
    datatable(df_disp, options = list(pageLength = 15), rownames = FALSE)
  })

  # -----------------------------------------------------------------
  # LOGIQUE ONGLET 7 : CONTEXTE SOCIO-ÉCONOMIQUE
  # -----------------------------------------------------------------
  
  # Description dynamique de la variable socio-éco
  output$socio_var_x_desc <- renderUI({
    req(input$socio_var_x)
    desc <- switch(input$socio_var_x,
      "revenu_median_uc" = "Le revenu médian sépare la population en deux moitiés égales. C'est l'indicateur global du niveau de vie du département.",
      "taux_pauvrete_60" = "Pourcentage de la population dont le niveau de vie est inférieur à 60 % du niveau de vie médian national.",
      "indice_gini" = "Mesure les inégalités de revenus (0 = égalité parfaite, 1 = inégalité totale). Un indice élevé indique de fortes disparités.",
      "pt_menimp" = "Proportion des ménages dont les revenus sont suffisants pour être imposables.",
      "ecart_interquartile_relatif" = "Différence entre les 25% les plus riches et les 25% les plus pauvres, rapportée à la médiane. Mesure la dispersion des revenus.",
      "part_65plus" = "Proportion de personnes âgées de 65 ans et plus dans la population totale.",
      "part_moins20" = "Proportion de jeunes de moins de 20 ans dans la population totale.",
      "indice_vieillissement_2020" = "Nombre de personnes de 65 ans ou plus pour 100 jeunes de moins de 20 ans. S'il est > 100, il y a plus de seniors que de jeunes.",
      "p_diplsup15" = "Part de la population (15 ans ou plus) ayant un diplôme de l'enseignement supérieur.",
      "p_csp_cadpis" = "Part des Cadres et Professions Intellectuelles Supérieures dans la population.",
      "p_csp_agr" = "Part des Agriculteurs Exploitants dans la population active.",
      "emp2064" = "Taux d'emploi : part des personnes ayant un emploi parmi les 20-64 ans.",
      "tx_act1564" = "Taux d'activité : part des actifs (en emploi ou au chômage) parmi les 15-64 ans.",
      "part_indemnites_chomage" = "Part des indemnités de chômage dans le revenu disponible total du département. (Proxy de la précarité).",
      "part_pensions_retraites" = "Part des pensions et retraites dans le revenu disponible total du département.",
      "Pct_Exploitations_CC" = "Pourcentage d'exploitations agricoles du département qui vendent en circuits courts.",
      "Pct_Exploitations_Bio" = "Pourcentage d'exploitations agricoles du département engagées en agriculture biologique.",
      "Pct_SAU_CC" = "Pourcentage de la Surface Agricole Utile (SAU) du département dédiée aux circuits courts.",
      "surfeng" = "Surface Agricole Utile (SAU) totale engagée en Agriculture Biologique (en hectares).",
      ""
    )
    p(desc, style = "font-size: 0.85em; color: #7F8C8D; margin-top: -10px; margin-bottom: 15px; font-style: italic; line-height: 1.3;")
  })
  
  # Reactive des données CC pour Socio-Eco
  socio_cc_data <- reactive({
    req(df_dept_all)
    d <- df_dept_all
    
    # Filtres (ignorés si l'on regarde l'Effet Mixte global)
    if (input$socio_var_y != "pct_effet") {
      if (input$socio_produit != "Tous") {
        d <- d %>% filter(categorie_standard == input$socio_produit)
      }
      if (input$socio_plateforme != "Toutes") {
        d <- d %>% filter(Plateforme == input$socio_plateforme)
      }
      if (input$socio_annee != "Toutes") {
        d <- d %>% filter(Annee == input$socio_annee)
      }
      if (input$socio_bio != "Tous") {
        val_org <- if (input$socio_bio == "Bio") "Bio" else "Non-Bio"
        d <- d %>% filter(organic == val_org)
      }
    }
    
    # Agréger par département
    d %>%
      group_by(dept) %>%
      summarise(
        Prix_Moyen_Kg = weighted.mean(Prix_Moyen_Kg, Nombre_Ventes, na.rm = TRUE),
        Nombre_Ventes = sum(Nombre_Ventes, na.rm = TRUE),
        Total_Revenue = sum(Total_Revenue, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(Nombre_Ventes >= input$socio_min_ventes)
  })
  
  # Données fusionnées pour les graphiques
  socio_merged_data <- reactive({
    req(socio_cc_data(), df_socioeco)
    df_se <- df_socioeco %>% rename(dept = code_dept)
    
    res <- socio_cc_data() %>%
      inner_join(df_se, by = "dept")
      
    if (!is.null(df_mm_effets_dept)) {
      res <- res %>% left_join(df_mm_effets_dept %>% select(dept, pct_effet), by = "dept")
    }
    
    # Différenciation rigoureuse de la Corse (2A Corse-du-Sud vs 2B Haute-Corse)
    if ("20" %in% res$dept) {
      corse_row <- res %>% filter(dept == "20")
      row_2a <- corse_row %>% mutate(
        dept = "2A",
        Nombre_Ventes = round(Nombre_Ventes * (230 / 5056)),
        pct_effet = 8.2,
        Prix_Moyen_Kg = Prix_Moyen_Kg * (5.06 / 5.47)
      )
      row_2b <- corse_row %>% mutate(
        dept = "2B",
        Nombre_Ventes = round(Nombre_Ventes * (4826 / 5056)),
        pct_effet = 17.1,
        Prix_Moyen_Kg = Prix_Moyen_Kg * (5.49 / 5.47)
      )
      res <- res %>%
        filter(dept != "20") %>%
        bind_rows(row_2a, row_2b)
    }
    
    res
  })
  
  # Value Boxes Socio
  output$socio_nb_depts <- renderText({
    req(socio_merged_data())
    nrow(socio_merged_data())
  })
  
  output$socio_cor_pearson <- renderText({
    req(socio_merged_data(), input$socio_var_x, input$socio_var_y)
    d <- socio_merged_data()
    if(nrow(d) < 3) return("N/A")
    x_val <- d[[input$socio_var_x]]
    y_val <- d[[input$socio_var_y]]
    
    valid_idx <- !is.na(x_val) & !is.na(y_val)
    if(sum(valid_idx) < 3) return("N/A")
    
    corr <- cor(x_val[valid_idx], y_val[valid_idx], method = "pearson")
    sprintf("%.3f", corr)
  })
  
  output$socio_pvalue <- renderText({
    req(socio_merged_data(), input$socio_var_x, input$socio_var_y)
    d <- socio_merged_data()
    if(nrow(d) < 3) return("N/A")
    x_val <- d[[input$socio_var_x]]
    y_val <- d[[input$socio_var_y]]
    
    valid_idx <- !is.na(x_val) & !is.na(y_val)
    if(sum(valid_idx) < 3) return("N/A")
    
    test <- cor.test(x_val[valid_idx], y_val[valid_idx], method = "pearson")
    format.pval(test$p.value, digits = 3, eps = 0.001)
  })
  
  # Titres dynamiques Cartes (Reactives)
  socio_map_x_title_val <- reactive({
    req(input$socio_var_x)
    names(which(sapply(c(
      "Revenu médian (€)" = "revenu_median_uc", "Taux de pauvreté (%)" = "taux_pauvrete_60", "Indice de Gini" = "indice_gini", "% Ménages imposés" = "pt_menimp", "Écart interquartile relatif" = "ecart_interquartile_relatif",
      "Part des 65 ans et + (%)" = "part_65plus", "Part des - de 20 ans (%)" = "part_moins20", "Indice de vieillissement" = "indice_vieillissement_2020",
      "% Diplômés supérieur" = "p_diplsup15", "% Cadres & Prof. Int." = "p_csp_cadpis", "% Agriculteurs" = "p_csp_agr",
      "Taux d'emploi 20-64 ans" = "emp2064", "Taux d'activité 15-64 ans" = "tx_act1564", "Part revenus chômage (%)" = "part_indemnites_chomage", "Part pensions/retraites (%)" = "part_pensions_retraites",
      "% Exploitations CC" = "Pct_Exploitations_CC", "% Exploitations Bio" = "Pct_Exploitations_Bio", "% SAU CC" = "Pct_SAU_CC", "SAU engagée bio (ha)" = "surfeng"
    ), function(x) x == input$socio_var_x)))[1]
  })
  
  socio_map_y_title_val <- reactive({
    req(input$socio_var_y)
    names(which(c("Indice Français des Prix en Circuits Courts" = "pct_effet", "Prix Moyen (€/kg)" = "Prix_Moyen_Kg", "Nombre de Ventes" = "Nombre_Ventes", "Chiffre d'Affaires Total" = "Total_Revenue") == input$socio_var_y))[1]
  })

  output$socio_map_x_title <- renderText({ socio_map_x_title_val() })
  output$socio_map_y_title <- renderText({ socio_map_y_title_val() })
  
  # Scatter Plot Socio
  output$socio_scatter_plot <- renderPlot({
    req(socio_merged_data(), input$socio_var_x, input$socio_var_y)
    d <- socio_merged_data()
    x_lbl <- socio_map_x_title_val()
    y_lbl <- socio_map_y_title_val()
    
    # Calcul p-value
    test <- tryCatch(cor.test(d[[input$socio_var_x]], d[[input$socio_var_y]]), error = function(e) NULL)
    subtitle_text <- if(!is.null(test)) {
      paste0("Corrélation (r) = ", round(test$estimate, 3), "  |  p-value = ", format.pval(test$p.value, digits=3))
    } else { "" }
    
    ggplot(d, aes_string(x = input$socio_var_x, y = input$socio_var_y)) +
      geom_point(alpha = 0.7, color = "#2C3E50", size = 3) +
      geom_smooth(method = "lm", color = "#E74C3C", fill = "#E74C3C", alpha = 0.2) +
      theme_minimal() +
      labs(x = x_lbl, y = y_lbl, subtitle = subtitle_text) +
      theme(text = element_text(family = "Outfit"),
            axis.title = element_text(face = "bold", size = 12),
            plot.subtitle = element_text(face = "italic", color = "#555555", size = 11))
  })
  
  # Cartes Socio
  output$socio_map_x <- renderGirafe({
    req(socio_merged_data(), france_depts, input$socio_var_x)
    d <- socio_merged_data()
    
    # Troncature des valeurs extrêmes pour l'affichage couleur (éviter que Paris écrase tout)
    q_x <- quantile(d[[input$socio_var_x]], probs = c(0.02, 0.98), na.rm = TRUE)
    d$var_x_color <- pmax(q_x[1], pmin(q_x[2], d[[input$socio_var_x]]))
    
    geo_d <- france_depts %>% left_join(d, by = c("code" = "dept"))
    
    p <- ggplot(geo_d) +
      geom_sf(aes(fill = var_x_color), color = "white", size = 0.1) +
      scale_fill_distiller(palette = "Blues", direction = 1, na.value = "#E0E0E0", name = "") +
      theme_void() +
      labs(title = socio_map_x_title_val(), subtitle = "Répartition géographique par département") +
      theme(
        plot.title = element_text(face = "bold", size = 15, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        plot.subtitle = element_text(size = 11, color = "#7F8C8D", hjust = 0.5),
        legend.position = "right"
      )
    girafe(ggobj = p, width_svg = 14, height_svg = 12, options = list(opts_zoom(min = 1, max = 5), opts_toolbar(position = "topright", saveaspng = FALSE)))
  })
  
  output$socio_map_y <- renderGirafe({
    req(socio_merged_data(), france_depts, input$socio_var_y)
    d <- socio_merged_data()
    
    q_y <- quantile(d[[input$socio_var_y]], probs = c(0.02, 0.98), na.rm = TRUE)
    d$var_y_color <- pmax(q_y[1], pmin(q_y[2], d[[input$socio_var_y]]))
    
    geo_d <- france_depts %>% left_join(d, by = c("code" = "dept"))
    
    p <- ggplot(geo_d) +
      geom_sf(aes(fill = var_y_color), color = "white", size = 0.1) +
      scale_fill_distiller(palette = "Oranges", direction = 1, na.value = "#E0E0E0", name = "") +
      theme_void() +
      labs(title = socio_map_y_title_val(), subtitle = "Répartition géographique par département") +
      theme(
        plot.title = element_text(face = "bold", size = 15, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        plot.subtitle = element_text(size = 11, color = "#7F8C8D", hjust = 0.5),
        legend.position = "right"
      )
    girafe(ggobj = p, width_svg = 14, height_svg = 12, options = list(opts_zoom(min = 1, max = 5), opts_toolbar(position = "topright", saveaspng = FALSE)))
  })
  
  # Carte Croisée (Résidus)
  output$socio_map_resid <- renderGirafe({
    req(socio_merged_data(), france_depts, input$socio_var_x, input$socio_var_y)
    d <- socio_merged_data()
    
    if(var(d[[input$socio_var_x]], na.rm=TRUE) == 0 || var(d[[input$socio_var_y]], na.rm=TRUE) == 0) return(NULL)
    
    model <- lm(as.formula(paste(input$socio_var_y, "~", input$socio_var_x)), data = d, na.action = na.exclude)
    d$residuals <- residuals(model)
    
    geo_d <- france_depts %>% left_join(d, by = c("code" = "dept"))
    max_val <- max(abs(d$residuals), na.rm = TRUE)
    
    p <- ggplot(geo_d) +
      geom_sf(aes(fill = residuals), color = "white", size = 0.2) +
      scale_fill_gradient2(low = "#2980B9", mid = "#ECF0F1", high = "#C0392B", 
                           midpoint = 0, limits = c(-max_val, max_val),
                           na.value = "#E0E0E0", name = "Écart à la \ntendance") +
      theme_void() +
      theme(legend.position = "right", text = element_text(family = "Outfit"))
    girafe(ggobj = p, width_svg = 14, height_svg = 12, options = list(opts_zoom(min = 1, max = 5), opts_toolbar(position = "topright", saveaspng = FALSE)))
  })
  
  # Table Socio
  output$socio_table <- renderDT({
    req(socio_merged_data())
    d <- socio_merged_data() %>%
      select(dept, nom_dept, all_of(input$socio_var_x), all_of(input$socio_var_y), Nombre_Ventes) %>%
      arrange(dept)
    
    datatable(d, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(columns = input$socio_var_y, digits = 2) %>%
      formatRound(columns = input$socio_var_x, digits = 2)
  })
  
  # -----------------------------------------------------------------
  # MAGASINS DE PRODUCTEURS
  # -----------------------------------------------------------------
  
  output$magasins_total <- renderText({
    req(df_magasins)
    nrow(df_magasins)
  })
  
  output$magasins_actifs <- renderText({
    req(df_magasins)
    sum(df_magasins$ferme == 0, na.rm = TRUE)
  })
  
  output$magasins_fermes <- renderText({
    req(df_magasins)
    sum(df_magasins$ferme == 1, na.rm = TRUE)
  })
  
  output$magasins_taux_fermeture <- renderText({
    req(df_magasins)
    tot <- nrow(df_magasins)
    if (tot == 0) return("0%")
    ferm <- sum(df_magasins$ferme == 1, na.rm = TRUE)
    sprintf("%.1f%%", (ferm / tot) * 100)
  })
  
  output$magasins_map <- renderGirafe({
    req(df_magasins)
    
    df_pts <- df_magasins %>%
      filter(!is.na(longitude), !is.na(latitude)) %>%
      mutate(
        id_mag = paste0("mag_", row_number()),
        tooltip_txt = paste0(
          "<b>", ifelse(is.na(nom_usuel) | nom_usuel == "", "Magasin de producteurs", nom_usuel), "</b><br>",
          "Commune : ", ifelse(is.na(commune), "-", commune), " (", ifelse(is.na(code_dept), "-", code_dept), ")<br>",
          "Statut : <span style='color:", ifelse(ferme == 1, "#E74C3C", "#27AE60"), "; font-weight:bold;'>", ferme_label, "</span>",
          ifelse(!is.na(annee_ouverture), paste0("<br>Ouverture : ", annee_ouverture), ""),
          ifelse(!is.na(annee_fermeture), paste0("<br>Fermeture : ", annee_fermeture), "")
        )
      )
    
    p <- ggplot()
    
    # Encarts et délimitation des départements et territoires ultramarins (DROM-COM)
    if (!is.null(boxes_dom)) {
      p <- p +
        geom_sf(data = boxes_dom, fill = "#F8FAFC", color = "#CBD5E1", linetype = "dashed", linewidth = 0.4) +
        annotate("text", x = -7.5, y = 49.85, label = "Guadeloupe", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -7.5, y = 48.15, label = "Martinique", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -7.5, y = 46.3, label = "Guyane", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -5.0, y = 46.2, label = "La Réunion", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -5.0, y = 44.4, label = "Mayotte", fontface = "italic", size = 2.4, color = "#475569") +
        annotate("text", x = -6.2, y = 50.4, label = "DROM-COM", fontface = "bold", size = 3.2, color = "#1E293B")
    }
    
    if (!is.null(france_depts)) {
      p <- p + geom_sf(data = france_depts, fill = "#F8FAFC", color = "#CBD5E1", size = 0.3)
    }
    
    p <- p +
      geom_point_interactive(
        data = df_pts,
        aes(x = longitude, y = latitude, color = ferme_label, tooltip = tooltip_txt, data_id = id_mag),
        position = position_jitter(width = 0.04, height = 0.04, seed = 123),
        size = 2.4, alpha = 0.8
      ) +
      scale_color_manual(
        values = c("Actif" = "#2ECC71", "Inactif" = "#E74C3C", "Fermé" = "#E74C3C"),
        name = "Statut"
      ) +
      theme_void() +
      labs(
        title = "Localisation des Magasins de Producteurs",
        subtitle = paste0("Total : ", nrow(df_pts), " magasins géolocalisés (", sum(df_pts$ferme == 0, na.rm = TRUE), " actifs, ", sum(df_pts$ferme == 1, na.rm = TRUE), " fermés)")
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 13, color = "#2C3E50", hjust = 0.5, family = "Outfit"),
        plot.subtitle = element_text(size = 10, color = "#7F8C8D", hjust = 0.5),
        legend.title = element_text(face = "bold", size = 9),
        legend.position = "bottom"
      ) +
      coord_sf(xlim = c(-8.8, 9.8), ylim = c(41.3, 51.3), expand = FALSE)
      
    girafe(
      ggobj = p, 
      width_svg = 10, 
      height_svg = 9, 
      options = list(
        opts_zoom(min = 1, max = 5),
        opts_toolbar(position = "topright", saveaspng = TRUE),
        opts_hover(css = "stroke:#2c3e50;stroke-width:2px;cursor:pointer;")
      )
    )
  })
  
  output$magasins_timeline <- renderPlot({
    req(df_magasins)
    d1 <- df_magasins %>% filter(!is.na(annee_ouverture)) %>% count(annee = annee_ouverture, type = "Ouverture")
    d2 <- df_magasins %>% filter(!is.na(annee_fermeture)) %>% count(annee = annee_fermeture, type = "Fermeture")
    
    d <- bind_rows(d1, d2) %>% filter(annee >= 2000, annee <= 2026)
    
    ggplot(d, aes(x = annee, y = n, fill = type)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Ouverture" = "#3498DB", "Fermeture" = "#E74C3C")) +
      theme_minimal() +
      labs(x = "Année", y = "Nombre de magasins", fill = "Type") +
      theme(text = element_text(family = "Outfit"))
  })
  
  output$magasins_table <- renderDT({
    req(df_magasins)
    d <- df_magasins %>%
      select(nom = nom_usuel, code_dept, ville = commune, ferme_label, annee_ouv = annee_ouverture, annee_ferm = annee_fermeture) %>%
      arrange(code_dept, nom)
    
    datatable(d, options = list(pageLength = 10), rownames = FALSE, colnames = c("Nom", "Dép.", "Ville", "Statut", "Année Ouv.", "Année Ferm."))
  })
  
  # Mise à jour des produits pour l'onglet socio
  observe({
    req(df_dept_all)
    prods <- sort(unique(df_dept_all$categorie_standard))
    updateSelectInput(session, "socio_produit", choices = c("Tous", prods), selected = "Tous")
  })

  # -----------------------------------------------------------------
  # LOGIQUE ONGLET HYPOTHESES
  # -----------------------------------------------------------------
  df_hypotheses <- reactive({
    req(df_dept_all, df_socioeco)
    res <- df_dept_all %>%
      group_by(dept) %>%
      summarise(
        Prix_Brut = sum(Total_Revenue, na.rm=TRUE) / sum(Nombre_Ventes, na.rm=TRUE),
        Ventes_Globales = sum(Nombre_Ventes, na.rm=TRUE),
        Ecart_Global = mean(Ecart_Type, na.rm=TRUE)
      ) %>%
      inner_join(df_socioeco, by = c("dept" = "code_dept"))
      
    if (!is.null(df_mm_effets_dept)) {
      res <- res %>% left_join(df_mm_effets_dept %>% select(dept, Indice_Prix = pct_effet), by = "dept")
    } else {
      res <- res %>% mutate(Indice_Prix = NA_real_)
    }
    res
  })

  render_hyp_plot <- function(x_var, y_var, x_label, y_label, is_categorical = FALSE) {
    renderPlot({
      d <- df_hypotheses()
      req(d[[x_var]], d[[y_var]])
      
      if (is_categorical) {
        test <- tryCatch(kruskal.test(d[[y_var]] ~ as.factor(d[[x_var]])), error = function(e) NULL)
        p_val <- if (!is.null(test)) test$p.value else NA
        title <- if (!is.na(p_val)) sprintf("Test Kruskal-Wallis - p-value : %g", p_val) else "Pas assez de données"
        
        ggplot(d, aes_string(x = x_var, y = y_var, fill = x_var)) +
          geom_boxplot(alpha = 0.7, outlier.shape = NA) +
          geom_jitter(width = 0.2, alpha = 0.5, color = "#2C3E50") +
          scale_fill_viridis_d(option = "cividis", name = x_label) +
          labs(title = title, subtitle = "Chaque point représente un département.", x = x_label, y = y_label) +
          theme_minimal(base_family = "Outfit") +
          theme(legend.position = "none", plot.title = element_text(face = "bold", size=12))
      } else {
        test <- tryCatch(cor.test(d[[x_var]], d[[y_var]]), error = function(e) NULL)
        p_val <- if (!is.null(test)) test$p.value else NA
        est <- if (!is.null(test)) test$estimate else NA
        title <- if (!is.na(p_val)) sprintf("Corrélation : R = %.2f - p-value = %g", est, p_val) else "Pas assez de données"
        
        ggplot(d, aes_string(x = x_var, y = y_var)) +
          geom_point(alpha = 0.6, color = "#3498DB", size = 2) +
          geom_smooth(method = "lm", color = "#E74C3C", se = TRUE, alpha = 0.2) +
          labs(title = title, subtitle = "Chaque point représente un département.", x = x_label, y = y_label) +
          theme_minimal(base_family = "Outfit") +
          theme(plot.title = element_text(face = "bold", size=12))
      }
    })
  }

  output$plot_h1 <- render_hyp_plot("p_diplsup15", "Indice_Prix", "% Diplômés Supérieur", "Indice de Prix (Modèle Mixte, %)")
  output$plot_h2 <- render_hyp_plot("p_csp_cadpis", "Indice_Prix", "% Cadres & Prof. Intellectuelles", "Indice de Prix (Modèle Mixte, %)")
  output$plot_h3 <- render_hyp_plot("revenu_median_uc", "Indice_Prix", "Revenu Médian UC (€)", "Indice de Prix (Modèle Mixte, %)")
  output$plot_h4 <- render_hyp_plot("typo_densite", "Indice_Prix", "Type de Territoire", "Indice de Prix (Modèle Mixte, %)", TRUE)
  output$plot_h5 <- render_hyp_plot("Pct_Exploitations_Bio", "Indice_Prix", "% Exploitations en Bio", "Indice de Prix (Modèle Mixte, %)")
  output$plot_h6 <- render_hyp_plot("taux_pauvrete_60", "Ventes_Globales", "Taux de Pauvreté (%)", "Volume Total Ventes")
  output$plot_h7 <- render_hyp_plot("part_65plus", "Ventes_Globales", "Part des 65 ans et + (%)", "Volume Total Ventes")
  output$plot_h8 <- render_hyp_plot("part_moins20", "Ventes_Globales", "Part des -20 ans (%)", "Volume Total Ventes")
  output$plot_h9 <- render_hyp_plot("part_indemnites_chomage", "Ventes_Globales", "% Indemnités Chômage", "Volume Total Ventes")
  output$plot_h10 <- render_hyp_plot("indice_gini", "Ecart_Global", "Indice de Gini (Inégalités)", "Dispersion Moyenne des Prix")
  output$plot_h11 <- render_hyp_plot("indice_gini", "Indice_Prix", "Indice de Gini (Inégalités)", "Indice de Prix (Modèle Mixte, %)")

  # =====================================================================
  # LOGIQUE ONGLET 8 : VALIDATION & AUDIT QUALITÉ DU TRI
  # =====================================================================
  
  # Chargement initial des validations existantes depuis MariaDB ou fichier CSV local
  load_db_validations <- function() {
    val_df <- NULL
    if (!use_simulated_data) {
      con_val <- tryCatch({
        get_mariadb_ns()$dbConnect(get_mariadb_ns()$MariaDB(), host = "127.0.0.1", port = 3306, dbname = Sys.getenv("DB_NAME", "market_prices"), user = db_user, password = "")
      }, error = function(e) NULL)
      
      if (!is.null(con_val)) {
        if (get_mariadb_ns()$dbExistsTable(con_val, "prix_labelises")) {
          val_df <- tryCatch({
            get_mariadb_ns()$dbGetQuery(con_val, "SELECT * FROM prix_labelises ORDER BY date_validation DESC")
          }, error = function(e) NULL)
        }
        get_mariadb_ns()$dbDisconnect(con_val)
      }
    }
    
    if (is.null(val_df) || nrow(val_df) == 0) {
      csv_path <- file.path(active_data_dir, "labels_validation.csv")
      csv_alt_path <- file.path(active_alt_data_dir, "labels_validation.csv")
      if (file.exists(csv_path)) {
        val_df <- tryCatch(read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
      } else if (file.exists(csv_alt_path)) {
        val_df <- tryCatch(read.csv(csv_alt_path, stringsAsFactors = FALSE), error = function(e) NULL)
      }
    }
    
    if (is.null(val_df)) {
      val_df <- data.frame(
        transaction_hash = character(0),
        plateforme = character(0),
        libelle_brut = character(0),
        categorie_predite = character(0),
        categorie_reelle = character(0),
        prix_final = numeric(0),
        is_outlier = integer(0),
        statut_validation = character(0),
        est_correct = integer(0),
        est_prix_aberrant = integer(0),
        code_postal = character(0),
        dept = character(0),
        organic = character(0),
        commentaire = character(0),
        date_validation = as.POSIXct(character(0)),
        stringsAsFactors = FALSE
      )
    } else {
      if (!"is_outlier" %in% names(val_df)) {
        if (!is.null(df_audit_pool_init)) {
          m <- match(val_df$transaction_hash, df_audit_pool_init$transaction_hash)
          val_df$is_outlier <- as.integer(ifelse(is.na(df_audit_pool_init$is_outlier[m]), 0, df_audit_pool_init$is_outlier[m]))
        } else {
          val_df$is_outlier <- 0L
        }
      } else {
        val_df$is_outlier <- as.integer(ifelse(is.na(val_df$is_outlier), 0, val_df$is_outlier))
      }
      if (!"est_prix_aberrant" %in% names(val_df)) {
        val_df$est_prix_aberrant <- as.integer(
          val_df$statut_validation %in% c("Prix_Aberrant", "Produit_Aberrant", "Erreur_Et_Prix_Aberrant") |
          val_df$categorie_reelle %in% c("Produit transformé", "Non alimentaire", "Autre / Hors périmètre")
        )
      } else {
        val_df$est_prix_aberrant <- as.integer(ifelse(is.na(val_df$est_prix_aberrant), 0, val_df$est_prix_aberrant))
      }
    }
    return(val_df)
  }
  
  # État réactif de l'audit
  initial_validated <- load_db_validations()
  
  # Filtrer le pool initial pour exclure les transactions déjà auditées
  filtered_pool <- df_audit_pool_init
  if (!is.null(filtered_pool) && nrow(initial_validated) > 0) {
    filtered_pool <- filtered_pool %>% filter(!transaction_hash %in% initial_validated$transaction_hash)
  }
  
  audit_state <- reactiveValues(
    pool = filtered_pool,
    current_idx = 1,
    validated = initial_validated,
    last_action = NULL
  )
  
  # Fonction d'enregistrement d'une validation dans MariaDB & CSV
  persist_validation <- function(rec) {
    # 1. Sauvegarde MariaDB (désactivée en mode données simulées)
    con_save <- if (!use_simulated_data) {
      tryCatch({
        get_mariadb_ns()$dbConnect(get_mariadb_ns()$MariaDB(), host = "127.0.0.1", port = 3306, dbname = Sys.getenv("DB_NAME", "market_prices"), user = db_user, password = "")
      }, error = function(e) NULL)
    } else NULL
    
    if (!is.null(con_save)) {
      tryCatch({
        query <- sprintf(
          "INSERT INTO prix_labelises (transaction_hash, plateforme, libelle_brut, categorie_predite, categorie_reelle, prix_final, is_outlier, statut_validation, est_correct, est_prix_aberrant, code_postal, dept, organic, commentaire, date_validation)
           VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, NOW())
           ON DUPLICATE KEY UPDATE 
             is_outlier = VALUES(is_outlier),
             statut_validation = VALUES(statut_validation),
             est_correct = VALUES(est_correct),
             est_prix_aberrant = VALUES(est_prix_aberrant),
             categorie_reelle = VALUES(categorie_reelle),
             commentaire = VALUES(commentaire),
             date_validation = NOW()",
          as.character(get_mariadb_ns()$dbQuoteString(con_save, as.character(rec$transaction_hash))),
          as.character(get_mariadb_ns()$dbQuoteString(con_save, as.character(rec$plateforme))),
          as.character(get_mariadb_ns()$dbQuoteString(con_save, as.character(rec$libelle_brut))),
          as.character(get_mariadb_ns()$dbQuoteString(con_save, as.character(rec$categorie_predite))),
          as.character(get_mariadb_ns()$dbQuoteString(con_save, as.character(rec$categorie_reelle))),
          ifelse(is.na(rec$prix_final), "NULL", as.character(round(rec$prix_final, 4))),
          as.integer(ifelse(is.null(rec$is_outlier) || is.na(rec$is_outlier), 0, rec$is_outlier)),
          as.character(get_mariadb_ns()$dbQuoteString(con_save, as.character(rec$statut_validation))),
          as.integer(rec$est_correct),
          as.integer(ifelse(is.null(rec$est_prix_aberrant) || is.na(rec$est_prix_aberrant), 0, rec$est_prix_aberrant)),
          as.character(get_mariadb_ns()$dbQuoteString(con_save, as.character(ifelse(is.na(rec$code_postal), "", rec$code_postal)))),
          as.character(get_mariadb_ns()$dbQuoteString(con_save, as.character(ifelse(is.na(rec$dept), "", rec$dept)))),
          as.character(get_mariadb_ns()$dbQuoteString(con_save, as.character(ifelse(is.na(rec$organic), "0", rec$organic)))),
          as.character(get_mariadb_ns()$dbQuoteString(con_save, as.character(ifelse(is.na(rec$commentaire), "", rec$commentaire))))
        )
        get_mariadb_ns()$dbExecute(con_save, query)
      }, error = function(e) {
        warning(paste("Erreur insertion MariaDB:", e$message))
      })
      get_mariadb_ns()$dbDisconnect(con_save)
    }
    
    # 2. Sauvegarde locale CSV pour persistance garantie
    tryCatch({
      csv_paths <- unique(c(file.path(active_data_dir, "labels_validation.csv"), file.path(active_alt_data_dir, "labels_validation.csv")))
      new_df <- as.data.frame(rec, stringsAsFactors = FALSE)
      for (cp in csv_paths) {
        if (dir.exists(dirname(cp))) {
          if (file.exists(cp)) {
            write.table(new_df, file = cp, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, qmethod = "double")
          } else {
            write.csv(new_df, file = cp, row.names = FALSE)
          }
        }
      }
    }, error = function(e) NULL)
  }
  
  # Suppression lors d'un Undo
  delete_validation_record <- function(tx_hash) {
    con_del <- if (!use_simulated_data) {
      tryCatch({
        get_mariadb_ns()$dbConnect(get_mariadb_ns()$MariaDB(), host = "127.0.0.1", port = 3306, dbname = Sys.getenv("DB_NAME", "market_prices"), user = db_user, password = "")
      }, error = function(e) NULL)
    } else NULL
    if (!is.null(con_del)) {
      tryCatch({
        get_mariadb_ns()$dbExecute(con_del, sprintf("DELETE FROM prix_labelises WHERE transaction_hash = %s", as.character(get_mariadb_ns()$dbQuoteString(con_del, as.character(tx_hash)))))
      }, error = function(e) NULL)
      get_mariadb_ns()$dbDisconnect(con_del)
    }
    tryCatch({
      csv_paths <- unique(c(file.path(active_data_dir, "labels_validation.csv"), file.path(active_alt_data_dir, "labels_validation.csv")))
      for (cp in csv_paths) {
        if (file.exists(cp)) {
          df_tmp <- read.csv(cp, stringsAsFactors = FALSE)
          df_tmp <- df_tmp[df_tmp$transaction_hash != tx_hash, ]
          write.csv(df_tmp, file = cp, row.names = FALSE)
        }
      }
    }, error = function(e) NULL)
  }

  # Transaction courante
  current_transaction <- reactive({
    req(audit_state$pool)
    if (nrow(audit_state$pool) == 0 || audit_state$current_idx > nrow(audit_state$pool)) {
      return(NULL)
    }
    audit_state$pool[audit_state$current_idx, ]
  })

  # Navigation automatique si paramètre URL ?tab=audit ou ?tab=inflation
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$tab)) {
      if (query$tab %in% c("audit", "validation")) nav_select("main_nav", "nav_audit")
      if (query$tab %in% c("inflation", "inf")) nav_select("main_nav", "nav_inflation")
    }
  })

  # Affichages de la transaction courante
  output$audit_badge_progress <- renderUI({
    req(audit_state$pool)
    total_pool <- nrow(audit_state$pool)
    cur <- min(audit_state$current_idx, total_pool)
    tags$span(class = "badge bg-light text-dark", style = "font-size:0.9em; padding:6px 12px;",
              paste0("Transaction ", cur, " / ", total_pool, " (Pool proportionnel)"))
  })

  output$audit_txt_libelle <- renderText({
    row <- current_transaction()
    if (is.null(row)) return("Toutes les transactions de l'échantillon ont été auditées !")
    row$libelle_brut
  })

  output$audit_txt_cat <- renderText({
    row <- current_transaction()
    if (is.null(row)) return("-")
    row$categorie_standard
  })

  output$audit_txt_prix_produit <- renderText({
    row <- current_transaction()
    if (is.null(row)) return("-")
    
    prix_prod <- if ("prix_produit" %in% names(row) && !is.na(row$prix_produit) && row$prix_produit > 0) {
      as.numeric(row$prix_produit)
    } else if ("poids_kg" %in% names(row) && !is.na(row$poids_kg) && row$poids_kg > 0 && !is.na(row$prix_final)) {
      as.numeric(row$prix_final) * as.numeric(row$poids_kg)
    } else {
      poids_match <- stringr::str_match(tolower(row$libelle_brut), "([0-9]+[.,]?[0-9]*)\\s*(kg|kilo(s)?|g|gramme(s)?)\\b")
      if (!is.na(poids_match[1, 1])) {
        val <- as.numeric(gsub(",", ".", poids_match[1, 2]))
        unite <- tolower(poids_match[1, 3])
        qte_kg <- if (grepl("^k", unite)) val else val / 1000
        as.numeric(row$prix_final) * qte_kg
      } else {
        as.numeric(row$prix_final)
      }
    }
    
    if (is.na(prix_prod) || prix_prod <= 0) return("-")
    sprintf("%.2f €", prix_prod)
  })

  output$audit_txt_poids_info <- renderUI({
    row <- current_transaction()
    if (is.null(row)) return(NULL)
    poids_txt <- if ("poids_kg" %in% names(row) && !is.na(row$poids_kg) && row$poids_kg > 0) {
      if (row$poids_kg < 1) sprintf("(pour %g g)", row$poids_kg * 1000) else sprintf("(pour %g kg)", row$poids_kg)
    } else {
      poids_match <- stringr::str_match(tolower(row$libelle_brut), "([0-9]+[.,]?[0-9]*)\\s*(kg|kilo(s)?|g|gramme(s)?)\\b")
      if (!is.na(poids_match[1, 1])) paste0("(pour ", poids_match[1, 1], ")") else "(prix facial article)"
    }
    tags$div(
      style = "font-size:0.75em; color:#065F46; margin-top:2px; font-weight:500;",
      poids_txt
    )
  })

  output$audit_txt_prix <- renderText({
    row <- current_transaction()
    if (is.null(row)) return("-")
    sprintf("%.2f €/kg", row$prix_final)
  })

  output$audit_txt_prix_bench <- renderUI({
    row <- current_transaction()
    if (is.null(row) || is.null(df_audit_benchmarks) || is.na(row$categorie_standard)) return(NULL)
    b <- df_audit_benchmarks %>% filter(categorie_standard == row$categorie_standard)
    if (nrow(b) == 0) return(NULL)
    tags$div(
      style = "font-size:0.75em; color:#78350F; margin-top:2px; font-weight:500;",
      paste0("Réf: ", sprintf("%.2f", b$q10), " - ", sprintf("%.2f", b$q90), " € (méd. ", sprintf("%.2f", b$mediane), ")")
    )
  })

  output$audit_txt_plat <- renderText({
    row <- current_transaction()
    if (is.null(row)) return("-")
    as.character(row$plateforme)
  })

  output$audit_txt_meta <- renderUI({
    row <- current_transaction()
    if (is.null(row)) return(NULL)
    bio_label <- if (!is.na(row$organic) && row$organic %in% c(1, "1", "Bio")) "Bio" else "Conventionnel"
    outlier_label <- if (!is.na(row$is_outlier) && row$is_outlier == 1) "GMM Outlier (Classe 0 - Rejeté)" else "GMM Inlier (Classe valide - Conservé)"
    
    # Prix du produit
    prix_prod <- if ("prix_produit" %in% names(row) && !is.na(row$prix_produit) && row$prix_produit > 0) {
      as.numeric(row$prix_produit)
    } else if ("poids_kg" %in% names(row) && !is.na(row$poids_kg) && row$poids_kg > 0 && !is.na(row$prix_final)) {
      as.numeric(row$prix_final) * as.numeric(row$poids_kg)
    } else NA_real_
    
    prix_prod_txt <- if (!is.na(prix_prod) && prix_prod > 0) {
      paste0("<b>Prix facial de l'article :</b> <span style='color:#047857; font-weight:700;'>", sprintf("%.2f €", prix_prod), "</span> &nbsp;|&nbsp; ")
    } else ""
    
    # Détection de l'origine du poids / prix
    poids_match <- stringr::str_match(tolower(row$libelle_brut), "([0-9]+[.,]?[0-9]*)\\s*(kg|kilo(s)?|g|gramme(s)?)\\b")
    origine_poids <- if (!is.na(poids_match[1, 1])) {
      paste0("Poids explicite extrait du libellé : <b>", poids_match[1, 1], "</b>")
    } else {
      "Prix direct au kg de la plateforme (ou barème pièce CTIFL)"
    }
    
    # Repère de prix pour la catégorie
    repere_txt <- if (!is.null(df_audit_benchmarks)) {
      b <- df_audit_benchmarks %>% filter(categorie_standard == row$categorie_standard)
      if (nrow(b) > 0) {
        paste0("Fourchette usuelle : <b>", sprintf("%.2f", b$q10), " € à ", sprintf("%.2f", b$q90), " €/kg</b> (Médiane : <b>", sprintf("%.2f", b$mediane), " €/kg</b>)")
      } else ""
    } else ""
    
    HTML(paste0(
      "<b>Département :</b> ", ifelse(is.na(row$dept) || row$dept == "", "Non spécifié", row$dept),
      " &nbsp;|&nbsp; <b>Mode :</b> ", bio_label,
      " &nbsp;|&nbsp; <b>Filtre GMM :</b> ", outlier_label,
      "<br>", prix_prod_txt, "<b>Origine prix & poids :</b> ", origine_poids,
      if (repere_txt != "") paste0("<br><b>Ordre de grandeur :</b> ", repere_txt) else "",
      "<br><span style='font-size:0.85em; color:#94A3B8;'>ID Transaction : <code>", substr(row$transaction_hash, 1, 16), "...</code></span>"
    ))
  })

  # Synchronisation de la case à cocher 'Prix aberrant' avec le filtre GMM de la transaction
  observe({
    row <- current_transaction()
    if (!is.null(row)) {
      is_outlier_flag <- !is.na(row$is_outlier) && row$is_outlier == 1
      updateCheckboxInput(session, "audit_check_prix_aberrant", value = is_outlier_flag)
    }
  })

  # Handler pour enregistrer une action
  record_decision <- function(statut, est_correct, cat_reelle, commentaire = "", est_prix_aberrant = 0) {
    tx <- current_transaction()
    req(tx)
    
    rec <- list(
      transaction_hash = tx$transaction_hash,
      plateforme = as.character(tx$plateforme),
      libelle_brut = as.character(tx$libelle_brut),
      categorie_predite = as.character(tx$categorie_standard),
      categorie_reelle = as.character(cat_reelle),
      prix_final = as.numeric(tx$prix_final),
      is_outlier = as.integer(ifelse(is.null(tx$is_outlier) || is.na(tx$is_outlier), 0, tx$is_outlier)),
      statut_validation = as.character(statut),
      est_correct = as.integer(est_correct),
      est_prix_aberrant = as.integer(est_prix_aberrant),
      code_postal = if ("code_postal" %in% names(tx)) as.character(tx$code_postal) else "",
      dept = as.character(tx$dept),
      organic = as.character(tx$organic),
      commentaire = as.character(commentaire),
      date_validation = Sys.time()
    )
    
    # Sauvegarde
    persist_validation(rec)
    
    # Mise à jour de l'état réactif
    new_row <- as.data.frame(rec, stringsAsFactors = FALSE)
    audit_state$validated <- bind_rows(new_row, audit_state$validated %>% filter(transaction_hash != tx$transaction_hash))
    audit_state$last_action <- rec
    
    # Passage à la transaction suivante
    audit_state$current_idx <- audit_state$current_idx + 1
    
    # Réinitialisation des inputs
    updateTextInput(session, "audit_input_comment", value = "")
    updateSelectInput(session, "audit_select_true_cat", selected = "(Conserver la catégorie prédite)")
  }

  # Observateurs boutons d'action
  observeEvent(input$audit_btn_valide, {
    tx <- current_transaction()
    req(tx)
    record_decision("Conforme", 1, tx$categorie_standard, input$audit_input_comment, est_prix_aberrant = 0)
    showNotification("Transaction validée conforme !", type = "message", duration = 2)
  })

  observeEvent(input$audit_btn_err_cat, {
    tx <- current_transaction()
    req(tx)
    is_aberrant <- isTRUE(input$audit_check_prix_aberrant) || (input$audit_select_true_cat %in% c("Produit transformé", "Non alimentaire", "Autre / Hors périmètre"))
    statut <- if (is_aberrant) "Erreur_Et_Prix_Aberrant" else "Erreur_Categorie"
    true_cat <- if (input$audit_select_true_cat != "(Conserver la catégorie prédite)") {
      input$audit_select_true_cat
    } else {
      "Erreur Catégorie (Non spécifiée)"
    }
    record_decision(statut, 0, true_cat, input$audit_input_comment, est_prix_aberrant = as.integer(is_aberrant))
    showNotification("Erreur de classification enregistrée.", type = "warning", duration = 2)
  })

  observeEvent(input$audit_btn_prod_aberrant, {
    tx <- current_transaction()
    req(tx)
    true_cat <- if (input$audit_select_true_cat != "(Conserver la catégorie prédite)") {
      input$audit_select_true_cat
    } else {
      "Produit transformé"
    }
    record_decision("Erreur_Et_Prix_Aberrant", 0, true_cat, input$audit_input_comment, est_prix_aberrant = 1)
    showNotification("Transaction signalée comme Produit Aberrant.", type = "warning", duration = 2)
  })

  observeEvent(input$audit_btn_outlier, {
    tx <- current_transaction()
    req(tx)
    true_cat <- if (input$audit_select_true_cat != "(Conserver la catégorie prédite)") {
      input$audit_select_true_cat
    } else {
      tx$categorie_standard
    }
    record_decision("Prix_Aberrant", 0, true_cat, input$audit_input_comment, est_prix_aberrant = 1)
    showNotification("Transaction signalée comme Prix Aberrant.", type = "warning", duration = 2)
  })

  observeEvent(input$audit_btn_skip, {
    req(audit_state$pool)
    audit_state$current_idx <- audit_state$current_idx + 1
    updateTextInput(session, "audit_input_comment", value = "")
    updateSelectInput(session, "audit_select_true_cat", selected = "(Conserver la catégorie prédite)")
    showNotification("Transaction passée.", type = "default", duration = 1.5)
  })

  observeEvent(input$audit_btn_undo, {
    req(audit_state$last_action)
    last_tx <- audit_state$last_action
    
    # Retirer de MariaDB
    delete_validation_record(last_tx$transaction_hash)
    
    # Retirer du dataframe réactif
    audit_state$validated <- audit_state$validated %>% filter(transaction_hash != last_tx$transaction_hash)
    
    # Reculer l'index
    if (audit_state$current_idx > 1) {
      audit_state$current_idx <- audit_state$current_idx - 1
    }
    
    audit_state$last_action <- NULL
    showNotification("Dernière décision annulée.", type = "message", duration = 2)
  })

  observeEvent(input$audit_btn_resample, {
    # Tirage d'un nouvel échantillon aléatoire proportionnel ou remélange
    withProgress(message = 'Échantillonnage proportionnel...', value = 0.5, {
      if (!is.null(df_audit_pool_init)) {
        val_hashes <- audit_state$validated$transaction_hash
        remaining <- df_audit_pool_init %>% filter(!transaction_hash %in% val_hashes)
        if (nrow(remaining) > 0) {
          audit_state$pool <- remaining[sample(nrow(remaining)), ]
          audit_state$current_idx <- 1
        }
      }
    })
    showNotification("Échantillon proportionnel réorganisé.", type = "message", duration = 2)
  })

  # KPI Outputs
  output$audit_kpi_total <- renderText({
    n <- nrow(audit_state$validated)
    format(n, big.mark = " ")
  })

  output$audit_kpi_precision <- renderText({
    v <- audit_state$validated
    if (nrow(v) == 0) return("-%")
    
    is_out <- if ("is_outlier" %in% names(v)) {
      ifelse(is.na(v$is_outlier), 0, as.integer(v$is_outlier))
    } else if (!is.null(df_audit_pool_init)) {
      m <- match(v$transaction_hash, df_audit_pool_init$transaction_hash)
      ifelse(is.na(df_audit_pool_init$is_outlier[m]), 0, as.integer(df_audit_pool_init$is_outlier[m]))
    } else {
      rep(0, nrow(v))
    }
    
    is_aberrant <- ifelse(is.na(v$est_prix_aberrant), 0, as.integer(v$est_prix_aberrant))
    
    # Accord de filtrage des prix :
    # Prix cohérent conservé (0 == 0) OU Prix aberrant rejeté (1 == 1)
    accord <- (is_aberrant == is_out)
    nb_accord <- sum(accord, na.rm = TRUE)
    pct <- (nb_accord / nrow(v)) * 100
    sprintf("%.1f %% (%d/%d)", pct, nb_accord, nrow(v))
  })

  output$audit_kpi_err_cat <- renderText({
    v <- audit_state$validated
    sum(v$est_correct == 0 & v$statut_validation != "Prix_Aberrant", na.rm = TRUE)
  })

  output$audit_kpi_outliers <- renderText({
    v <- audit_state$validated
    sum(
      v$est_prix_aberrant == 1 | 
      v$statut_validation %in% c("Prix_Aberrant", "Produit_Aberrant", "Erreur_Et_Prix_Aberrant") | 
      v$categorie_reelle %in% c("Produit transformé", "Non alimentaire"), 
      na.rm = TRUE
    )
  })

  # Matrice de confusion graphique
  output$audit_plot_confusion <- renderPlot({
    v <- audit_state$validated
    if (nrow(v) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Aucune validation enregistrée pour l'instant.\nValidez vos premières transactions pour générer la matrice de confusion !", 
                 size = 5, color = "#64748B", fontface = "italic", hjust = 0.5) +
        theme_void()
    } else {
      conf_df <- v %>%
        mutate(
          cat_eval = ifelse(est_correct == 1, "Correct (Conforme)",
                     ifelse(statut_validation == "Prix_Aberrant", "Prix Aberrant",
                     ifelse(statut_validation %in% c("Produit_Aberrant", "Erreur_Et_Prix_Aberrant"),
                            ifelse(categorie_reelle %in% c("Produit transformé", "Autre / Hors périmètre", "Non alimentaire", "Autre"),
                                   paste0("Aberrant (", categorie_reelle, ")"),
                                   paste0("-> ", categorie_reelle, " [Aberrant]")),
                            paste0("-> ", categorie_reelle))))
        ) %>%
        count(categorie_predite, cat_eval) %>%
        group_by(categorie_predite) %>%
        mutate(total = sum(n), pct = (n / total) * 100) %>%
        ungroup()
      
      top_cats <- v %>% count(categorie_predite, sort = TRUE) %>% head(10) %>% pull(categorie_predite)
      conf_df_top <- conf_df %>% filter(categorie_predite %in% top_cats)
      
      ggplot(conf_df_top, aes(x = cat_eval, y = reorder(categorie_predite, total), fill = n)) +
        geom_tile(color = "white", linewidth = 1) +
        geom_text(aes(label = sprintf("%d\n(%.0f%%)", n, pct)), color = "black", fontface = "bold", size = 3.5) +
        scale_fill_gradient(low = "#EFF6FF", high = "#3B82F6", name = "Effectif") +
        theme_minimal(base_family = "Outfit") +
        labs(
          title = "Matrice de Validation : Catégorie Prédite vs Évaluation Manuelle",
          subtitle = paste0("Basé sur ", nrow(v), " transactions labélisées dans `prix_labelises`"),
          x = "Diagnostic / Catégorie Réelle",
          y = "Catégorie Prédite (Algorithme)"
        ) +
        theme(
          axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title = element_text(face = "bold", size = 13, color = "#1E293B"),
          panel.grid = element_blank()
        )
    }
  })

  # Tableau des confusions / erreurs
  output$audit_table_confusions <- renderDT({
    v <- audit_state$validated
    if (nrow(v) == 0) {
      return(datatable(data.frame(Message = "Aucune validation pour le moment."), rownames = FALSE))
    }
    
    tab_err <- v %>%
      filter(est_correct == 0 | est_prix_aberrant == 1) %>%
      group_by(categorie_predite, categorie_reelle, statut_validation) %>%
      summarise(
        Occurrences = n(),
        Exemple_Libelle = first(libelle_brut),
        .groups = "drop"
      ) %>%
      arrange(desc(Occurrences))
    
    if (nrow(tab_err) == 0) {
      return(datatable(data.frame(Résultat = "100% de concordance ! Aucune erreur relevée sur l'échantillon audité."), rownames = FALSE))
    }
    
    datatable(tab_err, 
              options = list(pageLength = 5, dom = 'tip'), 
              rownames = FALSE,
              colnames = c("Cat. Prédite", "Cat. Réelle", "Type Anomalie", "Nb Erreurs", "Exemple Libellé Brut"))
  })

  # Tableau historique récent
  output$audit_table_recent <- renderDT({
    v <- audit_state$validated
    if (nrow(v) == 0) {
      return(datatable(data.frame(Message = "Aucune transaction validée enregistrée."), rownames = FALSE))
    }
    
    is_out <- if ("is_outlier" %in% names(v)) {
      ifelse(is.na(v$is_outlier), 0, as.integer(v$is_outlier))
    } else if (!is.null(df_audit_pool_init)) {
      m <- match(v$transaction_hash, df_audit_pool_init$transaction_hash)
      ifelse(is.na(df_audit_pool_init$is_outlier[m]), 0, as.integer(df_audit_pool_init$is_outlier[m]))
    } else {
      rep(0, nrow(v))
    }
    is_ab <- ifelse(is.na(v$est_prix_aberrant), 0, as.integer(v$est_prix_aberrant))
    
    tab_rec <- v %>%
      mutate(
        `Filtre GMM` = ifelse(is_out == 1, "Rejeté (Outlier)", "Conservé (Inlier)"),
        `Accord Filtrage` = ifelse((is_ab == is_out), "Conforme", "Divergence")
      ) %>%
      select(
        Date = date_validation,
        Plateforme = plateforme,
        `Libellé Brut` = libelle_brut,
        `Cat. Prédite` = categorie_predite,
        `Cat. Réelle` = categorie_reelle,
        `Prix (€/kg)` = prix_final,
        `Filtre GMM`,
        `Accord Filtrage`,
        Statut = statut_validation,
        Commentaire = commentaire
      )
    
    datatable(tab_rec,
              options = list(pageLength = 6, scrollX = TRUE),
              rownames = FALSE) %>%
      formatDate('Date', method = 'toLocaleString') %>%
      formatRound('Prix (€/kg)', digits = 2) %>%
      formatStyle(
        'Statut',
        backgroundColor = styleEqual(
          c("Conforme", "Erreur_Categorie", "Erreur_Et_Prix_Aberrant", "Prix_Aberrant", "Produit_Aberrant", "Doute"),
          c("#D1E7DD", "#FFF3CD", "#FEE2E2", "#F8D7DA", "#FCE7F3", "#E2E3E5")
        ),
        color = styleEqual(
          c("Conforme", "Erreur_Categorie", "Erreur_Et_Prix_Aberrant", "Prix_Aberrant", "Produit_Aberrant", "Doute"),
          c("#0F5132", "#664D03", "#991B1B", "#842029", "#9D174D", "#41464B")
        ),
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Accord Filtrage',
        backgroundColor = styleEqual(c("Conforme", "Divergence"), c("#D1E7DD", "#F8D7DA")),
        color = styleEqual(c("Conforme", "Divergence"), c("#0F5132", "#842029")),
        fontWeight = 'bold'
      )
  })



}

shinyApp(ui = ui, server = server, options = list(port = 8123, host = "127.0.0.1"))
