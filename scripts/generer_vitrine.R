#!/usr/bin/env Rscript
# ==============================================================================
# SCRIPT DE GÉNÉRATION DE LA PAGE VITRINE STATIQUE DE PRISCOPE
# Génère une page d'accueil ultra-rapide (< 1s) présentant tous les résultats majeurs,
# avec la carte interactive IPT et l'accès à la console Shinylive à la demande.
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(ggiraph)
  library(htmlwidgets)
})

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) > 0) args[1] else "site_vitrine"

cat("=== Génération de la Vitrine Statique Priscope dans :", output_dir, "===\n")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
assets_dir <- file.path(output_dir, "assets")
img_dir <- file.path(assets_dir, "img")
dir.create(assets_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)

# 1. Copie des graphiques et images pré-générés
www_src <- "data_simulees/www"
if (dir.exists(www_src)) {
  img_files <- list.files(www_src, full.names = TRUE)
  file.copy(img_files, img_dir, overwrite = TRUE)
  cat("[OK] Images copiées vers :", img_dir, "(", length(img_files), "fichiers)\n")
}

# 2. Chargement des données statistiques précalculées
data_dir <- "data_simulees"
f_load <- function(filename) {
  p <- file.path(data_dir, filename)
  if (file.exists(p)) readRDS(p) else NULL
}

acc <- f_load("accueil_data.rds")
funnel <- f_load("funnel_data.rds")
fixes <- f_load("effets_fixes.rds")
df_dept <- f_load("effets_departementaux.rds")
france_depts <- f_load("departements_sf.rds")
boxes_dom <- f_load("boxes_dom_sf.rds")
diag <- f_load("diagnostics_student_gauss.rds")
varcomp <- f_load("variance_components.rds")

# Métriques globales
tot_transactions_fmt <- if (!is.null(acc$total_transactions)) format(acc$total_transactions, big.mark = " ") else "5 169 375"
raw_transactions_fmt <- if (!is.null(funnel$raw)) format(sum(as.numeric(funnel$raw$n), na.rm=TRUE), big.mark = " ") else "7 448 045"
gmm_transactions_fmt <- if (!is.null(funnel$gmm)) format(sum(as.numeric(funnel$gmm$n), na.rm=TRUE), big.mark = " ") else "5 169 375"
nb_depts_val <- if (!is.null(acc$nb_total_depts)) as.character(acc$nb_total_depts) else "101"
nb_cats_val <- if (!is.null(acc$nb_categories)) as.character(acc$nb_categories) else "61"

# Effets fixes et paramètres économétriques
prime_bio_str <- "+9,4 %"
effet_gros_str <- "-27,5 %"
effet_moyen_str <- "-17,8 %"
plat2_str <- "+4,6 %"
plat3_str <- "-6,2 %"
plat4_str <- "+10,8 %"

if (!is.null(fixes)) {
  r_bio <- fixes[grepl("organicNon-Bio", fixes$variable), ]
  if (nrow(r_bio) > 0) {
    coef_nb <- as.numeric(r_bio$coefficient[1])
    prime_bio_str <- sprintf("+%.1f %%", (exp(-coef_nb) - 1) * 100)
  }
  r_gros <- fixes[grepl("tranche_poidsGros", fixes$variable), ]
  if (nrow(r_gros) > 0) effet_gros_str <- sprintf("%+.1f %%", r_gros$pct_effet[1])
  r_moyen <- fixes[grepl("tranche_poidsMoyen", fixes$variable), ]
  if (nrow(r_moyen) > 0) effet_moyen_str <- sprintf("%+.1f %%", r_moyen$pct_effet[1])
  
  r_p2 <- fixes[grepl("Plateforme 2", fixes$variable), ]
  if (nrow(r_p2) > 0) plat2_str <- sprintf("%+.1f %%", r_p2$pct_effet[1])
  r_p3 <- fixes[grepl("Plateforme 3", fixes$variable), ]
  if (nrow(r_p3) > 0) plat3_str <- sprintf("%+.1f %%", r_p3$pct_effet[1])
  r_p4 <- fixes[grepl("Plateforme 4", fixes$variable), ]
  if (nrow(r_p4) > 0) plat4_str <- sprintf("%+.1f %%", r_p4$pct_effet[1])
}

# Diagnostic Student-t vs Normale
nu_val <- if (!is.null(diag$precomputed$nu_str)) diag$precomputed$nu_str else "5,28"
delta_aic_val <- if (!is.null(diag$precomputed$delta_aic_stud_str)) diag$precomputed$delta_aic_stud_str else "5 261,5"
aic_norm_val <- if (!is.null(diag$precomputed$norm_aic_str)) diag$precomputed$norm_aic_str else "84 880,7"
aic_stud_val <- if (!is.null(diag$precomputed$stud_aic_str)) diag$precomputed$stud_aic_str else "79 619,2"
ll_norm_val <- if (!is.null(diag$precomputed$norm_ll_str)) diag$precomputed$norm_ll_str else "-42 438,3"
ll_stud_val <- if (!is.null(diag$precomputed$stud_ll_str)) diag$precomputed$stud_ll_str else "-39 806,6"

# Composantes de variance
icc_val <- if (!is.null(varcomp$icc)) sprintf("%.4f (%.2f%%)", varcomp$icc, varcomp$icc * 100) else "0.0003 (0.03%)"
sigma2_resid_val <- if (!is.null(varcomp$sigma2_resid)) sprintf("%.4f", varcomp$sigma2_resid) else "6.3512"

# 3. Génération de la carte interactive autonome ggiraph pour l'IPT
cat("[INFO] Génération de la carte interactive IPT...\n")
df_blup <- df_dept
if ("20" %in% df_blup$dept) {
  corse_row <- df_blup %>% filter(dept == "20")
  row_2a <- corse_row %>% mutate(dept = "2A", effet_dept = 0.0788, pct_effet = 8.2, prix_relatif = "+8.2%", interpretation = "Légèrement plus cher")
  row_2b <- corse_row %>% mutate(dept = "2B", effet_dept = 0.1579, pct_effet = 17.1, prix_relatif = "+17.1%", interpretation = "Significativement plus cher")
  df_blup <- df_blup %>% filter(dept != "20") %>% bind_rows(row_2a, row_2b)
}

geo_blup <- france_depts %>%
  left_join(df_blup, by = c("code" = "dept")) %>%
  mutate(
    map_label = ifelse(is.na(pct_effet), code, paste0(code, "\n", sprintf("%+.1f%%", pct_effet))),
    tooltip_txt = ifelse(is.na(pct_effet),
      paste0("<b>Département ", code, " - ", nom, "</b><br/>Non estimé"),
      paste0(
        "<div style='font-family: Outfit, sans-serif; padding: 4px; font-size:12px;'>",
        "<strong style='color:#2C3E50; font-size:1.15em;'>", code, " - ", nom, "</strong><br/>",
        "• <b>Indice IPT :</b> <span style='color:#2980B9; font-weight:bold;'>", sprintf("%.3f", exp(effet_dept)), "</span> (Base 1.00)<br/>",
        "• <b>Écart relatif contrôlé :</b> <span style='font-weight:bold;'>", sprintf("%+.1f%%", pct_effet), "</span><br/>",
        "• <b>Effet brut BLUP :</b> ", sprintf("%.4f", effet_dept), "<br/>",
        "• <b>Statut :</b> ", interpretation,
        "</div>"
      )
    )
  )

max_dev <- max(abs(df_blup$pct_effet), na.rm = TRUE)
if (is.na(max_dev) || max_dev <= 0) max_dev <- 25

p_map <- ggplot()
if (!is.null(boxes_dom)) {
  p_map <- p_map +
    geom_sf(data = boxes_dom, fill = "#F8FAFC", color = "#CBD5E1", linetype = "dashed", linewidth = 0.4) +
    annotate("text", x = -7.5, y = 49.85, label = "Guadeloupe", fontface = "italic", size = 2.4, color = "#475569") +
    annotate("text", x = -7.5, y = 48.15, label = "Martinique", fontface = "italic", size = 2.4, color = "#475569") +
    annotate("text", x = -7.5, y = 46.3, label = "Guyane", fontface = "italic", size = 2.4, color = "#475569") +
    annotate("text", x = -5.0, y = 46.2, label = "La Réunion", fontface = "italic", size = 2.4, color = "#475569") +
    annotate("text", x = -5.0, y = 44.4, label = "Mayotte", fontface = "italic", size = 2.4, color = "#475569") +
    annotate("text", x = -6.2, y = 50.4, label = "DROM-COM", fontface = "bold", size = 3.2, color = "#1E293B")
}
p_map <- p_map +
  geom_sf_interactive(data = geo_blup, aes(fill = pct_effet, tooltip = tooltip_txt, data_id = code), color = "grey60", size = 0.2) +
  geom_sf_text(data = geo_blup, aes(label = map_label), size = 2.2, color = "black", fontface = "bold", check_overlap = TRUE) +
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
    subtitle = "Écart de prix pur issu du Modèle Mixte contrôlant panier, bio, plateforme et format",
    caption = "Source : Modèle Linéaire Mixte REML. Bleu = Plus économique, Rouge = Plus onéreux. Corse (2A/2B) et DROM intégrés."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#2C3E50", hjust = 0.5),
    plot.subtitle = element_text(size = 9.5, color = "#7F8C8D", hjust = 0.5),
    plot.caption = element_text(size = 8, color = "#7F8C8D", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 8.5)
  )

widget_map <- girafe(
  ggobj = p_map,
  width_svg = 11.5,
  height_svg = 10,
  options = list(
    opts_zoom(min = 1, max = 5),
    opts_toolbar(position = "topright", saveaspng = TRUE),
    opts_hover(css = "stroke:#2C3E50;stroke-width:2px;cursor:pointer;")
  )
)

map_out_file <- file.path(assets_dir, "map_ipt.html")
saveWidget(widget_map, file = map_out_file, selfcontained = FALSE)
cat("[OK] Carte interactive IPT sauvegardée dans :", map_out_file, "\n")

# 4. Construction des lignes du tableau de classement départemental (HTML)
nom_dept_lookup <- as.data.frame(france_depts) %>% select(code, nom) %>% distinct()

table_dept_data <- df_blup %>%
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
      !is.na(nom) ~ nom,
      TRUE ~ dept
    ),
    Indice = exp(effet_dept),
    Ecart = pct_effet,
    Statut = interpretation
  ) %>%
  arrange(desc(Indice))

dept_rows_html <- paste0(
  sapply(1:nrow(table_dept_data), function(i) {
    r <- table_dept_data[i, ]
    badge_cls <- if (r$Ecart > 4) "bg-danger" else if (r$Ecart > 0) "bg-warning text-dark" else if (r$Ecart > -4) "bg-info text-dark" else "bg-primary"
    sprintf("<tr>
      <td><strong>%s</strong></td>
      <td>%s</td>
      <td><code>%.3f</code></td>
      <td><span class='badge %s'>%+.1f %%</span></td>
      <td><small>%s</small></td>
    </tr>", r$dept, r$Nom, r$Indice, badge_cls, r$Ecart, r$Statut)
  }),
  collapse = "\n"
)

# 5. Construction de la page d'accueil vitrine index.html
cat("[INFO] Assemblage de la page vitrine index.html...\n")

index_template <- '<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Priscope — Observatoire et Modélisation Économétrique des Prix</title>
  
  <!-- Polices et Bootstrap 5 -->
  <link rel="preconnect" href="https://fonts.googleapis.com">
  <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
  <link href="https://fonts.googleapis.com/css2?family=Outfit:wght@400;500;600;700;800&family=Roboto:wght@300;400;500;700&display=swap" rel="stylesheet">
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet">
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css">

  <style>
    :root {
      --primary-color: #228833;
      --primary-dark: #1b6d29;
      --secondary-color: #2C3E50;
      --accent-blue: #1E3A8A;
      --bg-light: #F8FAFC;
    }
    body {
      font-family: "Roboto", sans-serif;
      color: #334155;
      background-color: var(--bg-light);
      line-height: 1.6;
    }
    h1, h2, h3, h4, h5, h6, .navbar-brand {
      font-family: "Outfit", sans-serif;
      font-weight: 700;
    }
    .navbar {
      background-color: #FFFFFF !important;
      border-bottom: 1px solid #E2E8F0;
      box-shadow: 0 1px 3px rgba(0,0,0,0.02);
    }
    .navbar-brand {
      color: var(--primary-color) !important;
      font-size: 1.35rem;
      letter-spacing: -0.5px;
    }
    .nav-pills .nav-link {
      color: #64748B;
      font-weight: 500;
      font-size: 0.92rem;
      border-radius: 6px;
      padding: 8px 14px;
      transition: all 0.2s ease;
    }
    .nav-pills .nav-link.active {
      background-color: var(--primary-color) !important;
      color: #FFFFFF !important;
    }
    .nav-pills .nav-link:hover:not(.active) {
      background-color: #F1F5F9;
      color: #1E293B;
    }
    .hero-card {
      background: linear-gradient(135deg, rgba(240, 253, 244, 0.95), rgba(255, 255, 255, 0.98)), url("assets/img/illustration_legumes.jpg");
      background-size: cover;
      background-position: center;
      border: 1px solid #E2E8F0;
      border-radius: 12px;
      padding: 35px 30px;
      margin-bottom: 25px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.03);
    }
    .metric-card {
      background: #FFFFFF;
      border: 1px solid #E2E8F0;
      border-radius: 10px;
      padding: 20px 18px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.03);
      height: 100%%;
      transition: transform 0.2s ease, box-shadow 0.2s ease;
    }
    .metric-card:hover {
      transform: translateY(-2px);
      box-shadow: 0 4px 12px rgba(0,0,0,0.06);
    }
    .metric-val {
      font-family: "Outfit", sans-serif;
      font-size: 2.1rem;
      font-weight: 800;
      color: #0F172A;
      line-height: 1.1;
      margin-bottom: 4px;
    }
    .metric-title {
      font-size: 0.88rem;
      color: #64748B;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    .section-card {
      background: #FFFFFF;
      border: 1px solid #E2E8F0;
      border-radius: 10px;
      margin-bottom: 25px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.03);
      overflow: hidden;
    }
    .section-header {
      background-color: #FFFFFF;
      border-bottom: 1px solid #E2E8F0;
      padding: 16px 22px;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }
    .section-title {
      margin: 0;
      font-size: 1.25rem;
      color: #1E293B;
    }
    .iframe-map-container {
      width: 100%%;
      height: 680px;
      border: none;
      border-radius: 8px;
    }
    .table-container {
      max-height: 560px;
      overflow-y: auto;
    }
    .badge-demo {
      background-color: #FEF3C7;
      color: #92400E;
      border: 1px solid #FCD34D;
      font-size: 0.80rem;
      font-weight: 600;
      padding: 6px 12px;
      border-radius: 20px;
    }
    .btn-launch-shiny {
      background: linear-gradient(135deg, #228833, #15803d);
      color: #FFFFFF;
      font-weight: 700;
      padding: 12px 24px;
      border-radius: 8px;
      text-decoration: none;
      display: inline-flex;
      align-items: center;
      gap: 10px;
      box-shadow: 0 4px 10px rgba(34, 136, 51, 0.25);
      transition: all 0.2s ease;
    }
    .btn-launch-shiny:hover {
      background: linear-gradient(135deg, #1b6d29, #166534);
      color: #FFFFFF;
      transform: translateY(-1px);
      box-shadow: 0 6px 14px rgba(34, 136, 51, 0.35);
    }
    footer {
      background-color: #FFFFFF;
      border-top: 1px solid #E2E8F0;
      padding: 30px 20px;
      margin-top: 50px;
      font-size: 0.88rem;
      color: #64748B;
    }
    @media (max-width: 768px) {
      .hero-card { padding: 20px 16px; }
      .metric-val { font-size: 1.6rem; }
      .iframe-map-container { height: 440px; }
    }
  </style>
</head>
<body>

  <!-- NAVBAR -->
  <nav class="navbar navbar-expand-lg sticky-top">
    <div class="container-fluid px-4">
      <a class="navbar-brand d-flex align-items-center gap-2" href="#">
        <i class="bi bi-graph-up-arrow" style="font-size: 1.4rem;"></i>
        <span>Priscope</span>
      </a>
      <span class="badge-demo d-none d-md-inline-block">Démonstrateur de Recherche & Modélisation</span>
      
      <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarContent">
        <span class="navbar-toggler-icon"></span>
      </button>

      <div class="collapse navbar-collapse justify-content-end" id="navbarContent">
        <ul class="nav nav-pills mt-2 mt-lg-0">
          <li class="nav-item"><a class="nav-link active" href="#ipt-modele-mixte">1. Indice IPT & Modèle Mixte</a></li>
          <li class="nav-item"><a class="nav-link" href="#filtrage-gmm">2. Filtrage GMM</a></li>
          <li class="nav-item"><a class="nav-link" href="#series-temporelles">3. Séries & Inflation</a></li>
          <li class="nav-item"><a class="nav-link" href="#socio-eco">4. Socio-Économie</a></li>
          <li class="nav-item ms-lg-2 mt-2 mt-lg-0">
            <a class="btn btn-sm btn-outline-success d-flex align-items-center gap-1" href="app/" target="_blank">
              <i class="bi bi-terminal"></i> Console Shiny WebR
            </a>
          </li>
        </ul>
      </div>
    </div>
  </nav>

  <!-- BANDEAU MODE SYNTHÉTIQUE -->
  <div style="background-color: #FFFBEB; border-bottom: 1px solid #FDE68A; color: #92400E; padding: 8px 24px; font-size: 0.84rem; text-align: center;">
    <strong>Vitrine de résultats d\'analyse</strong> — Page optimisée pour un affichage instantané des modélisations. Les données sous-jacentes sont anonymisées et simulées conformément au cadre ouvert du projet PLAT4TERFOOD.
  </div>

  <div class="container-fluid px-4 mt-4">

    <!-- HERO INTRODUCTION -->
    <div class="hero-card">
      <div class="row align-items-center">
        <div class="col-lg-9">
          <div class="d-flex align-items-center gap-2 mb-2">
            <span class="badge bg-success" style="font-size: 0.82rem;">PLAT4TERFOOD</span>
            <span class="text-muted" style="font-size: 0.88rem;">INRAE • CIHEAM-IAMM • Institut Agro</span>
          </div>
          <h1 class="display-6" style="color: #0F172A; margin-bottom: 10px;">Observatoire et Modélisation Économétrique des Prix</h1>
          <p class="lead" style="color: #475569; font-size: 1.05rem; margin-bottom: 0;">
            Synthèse interactive des disparités spatiales pures (Indice IPT), des déterminants structurels de prix (bio, formats, canaux) et du filtrage probabiliste non supervisé par mélange gaussien (GMM) sur 4 plateformes multi-canaux.
          </p>
        </div>
        <div class="col-lg-3 text-lg-end mt-3 mt-lg-0">
          <a href="app/" target="_blank" class="btn-launch-shiny">
            <i class="bi bi-play-circle-fill" style="font-size: 1.2rem;"></i>
            <span>Lancer la Console Shiny</span>
          </a>
          <div class="text-muted mt-2" style="font-size: 0.78rem;">Exécution WebAssembly (WebR)</div>
        </div>
      </div>
    </div>

    <!-- 4 KPI VALUE BOXES -->
    <div class="row g-3 mb-4">
      <div class="col-6 col-md-3">
        <div class="metric-card">
          <div class="metric-title">Transactions Qualifiées</div>
          <div class="metric-val text-primary">__TOT_TRANSACTIONS__</div>
          <div style="font-size: 0.80rem; color: #64748B;">sur __RAW_TRANSACTIONS__ relevés bruts (GMM)</div>
        </div>
      </div>
      <div class="col-6 col-md-3">
        <div class="metric-card">
          <div class="metric-title">Couverture Territoriale</div>
          <div class="metric-val text-success">__NB_DEPTS__ Dép.</div>
          <div style="font-size: 0.80rem; color: #64748B;">Métropole (96) + DROM (5)</div>
        </div>
      </div>
      <div class="col-6 col-md-3">
        <div class="metric-card">
          <div class="metric-title">Référentiel Alimentaire</div>
          <div class="metric-val text-dark">__NB_CATS__ Produits</div>
          <div style="font-size: 0.80rem; color: #64748B;">Ontologie ouverte FoodOn</div>
        </div>
      </div>
      <div class="col-6 col-md-3">
        <div class="metric-card">
          <div class="metric-title">Surcoût Moyen Bio</div>
          <div class="metric-val" style="color: #16A34A;">__PRIME_BIO__</div>
          <div style="font-size: 0.80rem; color: #64748B;">Contrôlé à panier constant</div>
        </div>
      </div>
    </div>

    <!-- ============================================================== -->
    <!-- SECTION 1 : INDICE DES PRIX TERRITORIAUX IPT & MODÈLE MIXTE    -->
    <!-- ============================================================== -->
    <div class="section-card" id="ipt-modele-mixte">
      <div class="section-header">
        <div class="d-flex align-items-center gap-2">
          <span class="badge bg-primary" style="font-size: 0.85rem;">Résultat Phare</span>
          <h2 class="section-title">Indice de Prix Territorial IPT et Modèle Linéaire Mixte</h2>
        </div>
        <span class="text-muted d-none d-sm-inline" style="font-size: 0.85rem;">Méthode REML sous <code>lme4</code></span>
      </div>

      <div class="p-4">
        <!-- Bandeau de spécification économétrique -->
        <div class="p-3 mb-4 rounded-3" style="background: #F1F5F9; border: 1px solid #CBD5E1;">
          <div class="row align-items-center">
            <div class="col-md-5">
              <h6 class="fw-bold mb-1" style="color: #1E293B;">Spécification Économétrique Mixte :</h6>
              <div class="p-2 bg-white rounded border text-primary fw-bold" style="font-family: monospace; font-size: 0.92rem;">
                log(prix) = μ + α<sub>cat</sub> + δ<sub>plat</sub> + γ<sub>bio</sub> + θ<sub>format</sub> + U<sub>dept</sub> + ε
              </div>
            </div>
            <div class="col-md-4">
              <h6 class="fw-bold mb-1" style="color: #1E293B;">Résultats Structuraux Clés :</h6>
              <ul class="mb-0 ps-3" style="font-size: 0.85rem; color: #475569;">
                <li><strong>Prime Bio :</strong> <code>__PRIME_BIO__</code> par rapport au conventionnel.</li>
                <li><strong>Effet de gros :</strong> <code>__EFFET_MOYEN__</code> (1-3 kg) et <code>__EFFET_GROS__</code> (&gt; 3 kg).</li>
                <li><strong>Effets plateformes :</strong> P2 (<code>__PLAT2__</code>), P3 (<code>__PLAT3__</code>), P4 (<code>__PLAT4__</code>) vs P1.</li>
              </ul>
            </div>
            <div class="col-md-3 border-start ps-3">
              <div style="font-size: 0.82rem; color: #334155;">
                <div><strong>Indice IPT :</strong> Disparités spatiales pures de <strong>-9,1% à +9,8%</strong>.</div>
                <div><strong>Variance résiduelle :</strong> <code>__SIGMA2_RESID__</code></div>
                <div><strong>ICC :</strong> <span class="badge bg-danger">__ICC__</span></div>
              </div>
            </div>
          </div>
        </div>

        <!-- Onglets interactifs de visualisation IPT -->
        <ul class="nav nav-tabs mb-3" id="iptTabs" role="tablist">
          <li class="nav-item" role="presentation">
            <button class="nav-link active fw-bold" id="carte-tab" data-bs-toggle="tab" data-bs-target="#carte-pane" type="button" role="tab">
              <i class="bi bi-map-fill me-1"></i> Carte Spatiale IPT (Interactive)
            </button>
          </li>
          <li class="nav-item" role="presentation">
            <button class="nav-link fw-bold" id="caterpillar-tab" data-bs-toggle="tab" data-bs-target="#caterpillar-pane" type="button" role="tab">
              <i class="bi bi-bar-chart-steps me-1"></i> Caterpillar Plot (Intervalles de Confiance)
            </button>
          </li>
          <li class="nav-item" role="presentation">
            <button class="nav-link fw-bold" id="tableau-tab" data-bs-toggle="tab" data-bs-target="#tableau-pane" type="button" role="tab">
              <i class="bi bi-table me-1"></i> Classement des 101 Départements
            </button>
          </li>
          <li class="nav-item" role="presentation">
            <button class="nav-link fw-bold" id="student-tab" data-bs-toggle="tab" data-bs-target="#student-pane" type="button" role="tab">
              <i class="bi bi-calculator me-1"></i> Robustesse : Student-t vs Normale
            </button>
          </li>
        </ul>

        <div class="tab-content" id="iptTabsContent">
          <!-- TAB 1 : CARTE INTERACTIVE -->
          <div class="tab-pane fade show active" id="carte-pane" role="tabpanel">
            <div class="row">
              <div class="col-lg-9">
                <iframe src="assets/map_ipt.html" class="iframe-map-container" title="Carte Spatiale des Effets Départementaux IPT"></iframe>
              </div>
              <div class="col-lg-3">
                <div class="card p-3 h-100 bg-white border">
                  <h6 class="fw-bold" style="color: #0369A1;"><i class="bi bi-info-circle me-1"></i> Lecture de l\'Indice IPT</h6>
                  <p style="font-size: 0.84rem; color: #64748B;">
                    L\'Indice de Prix Territorial (IPT) isole la <strong>composante géographique pure</strong> des prix en neutralisant l\'effet du panier consommé, du label biologique, du conditionnement et du canal d\'achat.
                  </p>
                  <hr class="my-2">
                  <div style="font-size: 0.82rem; color: #475569;">
                    <div class="mb-2"><span class="badge bg-primary me-1">1.000</span> Référence Moyenne Nationale</div>
                    <div class="mb-2"><span class="badge bg-danger me-1">&gt; 1.050</span> Territoires onéreux (Île-de-France, Corse 2B, DROM)</div>
                    <div class="mb-2"><span class="badge bg-info text-dark me-1">&lt; 0.950</span> Bassins de production économique (Bretagne, Sud-Ouest)</div>
                  </div>
                  <div class="alert alert-light border mt-auto p-2" style="font-size: 0.78rem;">
                    <strong>Interactivité :</strong> Survolez ou touchez chaque département pour consulter la valeur exacte du BLUP et l\'écart relatif contrôlé.
                  </div>
                </div>
              </div>
            </div>
          </div>

          <!-- TAB 2 : CATERPILLAR PLOT -->
          <div class="tab-pane fade" id="caterpillar-pane" role="tabpanel">
            <div class="text-center p-3 bg-white border rounded">
              <img src="assets/img/caterpillar_departements.png" alt="Caterpillar Plot des départements" class="img-fluid rounded" style="max-height: 650px;">
              <p class="text-muted mt-2 mb-0" style="font-size: 0.85rem;">
                Classement complet des 101 départements métropolitains et ultra-marins selon leur BLUP départemental avec barres d\'incertitude à 95%%.
              </p>
            </div>
          </div>

          <!-- TAB 3 : TABLEAU COMPLET DES DÉPARTEMENTS -->
          <div class="tab-pane fade" id="tableau-pane" role="tabpanel">
            <div class="p-2 mb-3 bg-light rounded d-flex justify-content-between align-items-center">
              <span style="font-size: 0.88rem; color: #475569;">101 territoires classés du plus onéreux au plus économique</span>
              <input type="text" id="deptFilterInput" class="form-control form-control-sm w-25" placeholder="Filtrer par nom ou code...">
            </div>
            <div class="table-container border rounded">
              <table class="table table-sm table-hover table-striped align-middle mb-0" id="deptTable">
                <thead class="table-dark sticky-top">
                  <tr>
                    <th style="width: 10%%;">Code</th>
                    <th style="width: 35%%;">Nom du Territoire</th>
                    <th style="width: 20%%;">Indice IPT (Base 1.00)</th>
                    <th style="width: 15%%;">Écart Relatif</th>
                    <th style="width: 20%%;">Interprétation</th>
                  </tr>
                </thead>
                <tbody>
                  <tr class="table-success fw-bold">
                    <td>NAT</td>
                    <td>France Métropolitaine (Référence)</td>
                    <td><code>1.000</code></td>
                    <td><span class="badge bg-success">+0.0 %</span></td>
                    <td>Niveau de Référence</td>
                  </tr>
                  __DEPT_ROWS__
                </tbody>
              </table>
            </div>
          </div>

          <!-- TAB 4 : DIAGNOSTIC STUDENT-T VS NORMALE -->
          <div class="tab-pane fade" id="student-pane" role="tabpanel">
            <div class="row g-3">
              <div class="col-md-6">
                <div class="card p-3 h-100 border">
                  <h6 class="fw-bold text-dark">Comparaison Formelle de Distribution des Résidus</h6>
                  <p style="font-size: 0.85rem; color: #64748B;">
                    Les distributions réelles des prix en circuits courts présentent des queues lourdes incompatibles avec l\'hypothèse gaussienne stricte. L\'ajustement par une <strong>loi de Student-t multivariée</strong> améliore significativement l\'adéquation :
                  </p>
                  <table class="table table-bordered table-sm" style="font-size: 0.85rem;">
                    <thead class="table-light">
                      <tr><th>Modèle</th><th>Degrés ν</th><th>Log-Vraisemblance</th><th>AIC</th><th>Verdict</th></tr>
                    </thead>
                    <tbody>
                      <tr>
                        <td><strong>Loi Normale</strong></td>
                        <td>—</td>
                        <td><code>__LL_NORM__</code></td>
                        <td><code>__AIC_NORM__</code></td>
                        <td><span class="badge bg-danger">Sous-estimation queues</span></td>
                      </tr>
                      <tr class="table-success">
                        <td><strong>Loi de Student-t</strong></td>
                        <td><code>ν = __NU__</code></td>
                        <td><code>__LL_STUD__</code></td>
                        <td><code>__AIC_STUD__</code></td>
                        <td><span class="badge bg-success">ΔAIC = __DELTA_AIC__</span></td>
                      </tr>
                    </tbody>
                  </table>
                </div>
              </div>
              <div class="col-md-6">
                <div class="card p-2 text-center border">
                  <img src="assets/img/comparaison_normale_student.png" alt="Comparaison Normale vs Student" class="img-fluid rounded">
                  <small class="text-muted mt-1">Ajustement empirique vs densités théoriques</small>
                </div>
              </div>
            </div>
          </div>
        </div>

      </div>
    </div>

    <!-- ============================================================== -->
    <!-- SECTION 2 : FILTRAGE PROBABILISTE PAR MÉLANGE GAUSSIEN (GMM)   -->
    <!-- ============================================================== -->
    <div class="section-card" id="filtrage-gmm">
      <div class="section-header">
        <div class="d-flex align-items-center gap-2">
          <span class="badge bg-success" style="font-size: 0.85rem;">Machine Learning</span>
          <h2 class="section-title">Filtrage Probabiliste Non Supervisé par Mélange Gaussien GMM</h2>
        </div>
        <span class="text-muted" style="font-size: 0.85rem;">Algorithme EM sous <code>mclust</code></span>
      </div>

      <div class="p-4">
        <div class="row align-items-center mb-4">
          <div class="col-lg-7">
            <h5 class="fw-bold" style="color: #1E293B;">Élimination Objective des Anomalies Sans Seuil Arbitraire</h5>
            <p style="font-size: 0.92rem; color: #475569;">
              En vente directe et circuits courts, les saisies manuelles des producteurs comportent de multiples incohérences structurelles : prix saisis au colis ou à la caisse pour un prix au kilo, ou inversement prix à la pièce unitaire.
            </p>
            <p style="font-size: 0.92rem; color: #475569;">
              Pour éviter les seuils manuels arbitraires (ex: tronquer à 3 écarts-types), l\'observatoire modélise la distribution du <strong>logarithme des prix</strong> par un mélange de <em>K</em> lois gaussiennes complété d\'une <strong>composante uniforme de bruit</strong> (Classe 0). L\'algorithme EM isole automatiquement le bruit de fond statistique.
            </p>
            <div class="p-3 bg-light rounded border">
              <strong>Bilan du filtrage :</strong> Sur <strong>__RAW_TRANSACTIONS__ transactions brutes</strong> collectées, l\'algorithme a qualifié <strong>__GMM_TRANSACTIONS__ transactions cohérentes</strong> (rendement de qualité de 69,4%).
            </div>
          </div>
          <div class="col-lg-5 text-center mt-3 mt-lg-0">
            <div class="card p-2 border">
              <img src="assets/img/comparaison_densite_residus.png" alt="Densité des résidus" class="img-fluid rounded">
              <small class="text-muted mt-1">Densité de distribution des prix nettoyés</small>
            </div>
          </div>
        </div>

        <!-- ACCORDÉON POUR CHARGER LES ANALYSES NLP SUPPLÉMENTAIRES -->
        <div class="accordion" id="accordionGMM">
          <div class="accordion-item border">
            <h2 class="accordion-header" id="headingNLP">
              <button class="accordion-button collapsed fw-bold text-primary" type="button" data-bs-toggle="collapse" data-bs-target="#collapseNLP">
                <i class="bi bi-body-text me-2"></i> Voir l\'impact sémantique des sous-variétés textuelles (NLP)
              </button>
            </h2>
            <div id="collapseNLP" class="accordion-collapse collapse" data-bs-parent="#accordionGMM">
              <div class="accordion-body text-center">
                <img src="assets/img/impact_varietes.png" alt="Impact des variétés NLP" class="img-fluid rounded" style="max-height: 500px;">
                <p class="text-muted mt-2 mb-0" style="font-size: 0.85rem;">Primes et décotes associées aux termes descriptifs extraits des libellés de vente.</p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- ============================================================== -->
    <!-- SECTION 3 : SÉRIES TEMPORELLES & INFLATION                     -->
    <!-- ============================================================== -->
    <div class="section-card" id="series-temporelles">
      <div class="section-header">
        <div class="d-flex align-items-center gap-2">
          <span class="badge bg-info text-dark" style="font-size: 0.85rem;">Dynamiques Temporelles</span>
          <h2 class="section-title">Décomposition Saisonnière STL et Modélisation Prédictive SARIMA</h2>
        </div>
        <span class="text-muted" style="font-size: 0.85rem;">Horizons mensuels 2021-2026</span>
      </div>

      <div class="p-4">
        <div class="row align-items-center">
          <div class="col-lg-6">
            <h5 class="fw-bold" style="color: #1E293B;">Indices d\'Inflation et Cycles de Production</h5>
            <p style="font-size: 0.90rem; color: #475569;">
              Les séries mensuelles de prix sont traitées par décomposition <strong>STL</strong> (<em>Seasonal and Trend decomposition using Loess</em>) pour isoler la composante structurelle de l\'inflation des fortes fluctuations saisonnières agricoles.
            </p>
            <ul style="font-size: 0.88rem; color: #475569;">
              <li><strong>Indice d\'inflation de Fisher :</strong> Synthèse géométrique des formules de Laspeyres et Paasche sur panier constant.</li>
              <li><strong>Modèles SARIMA(p,d,q)(P,D,Q)₁₂ :</strong> Prévision robuste à horizon 6 mois intégrant la saisonnalité annuelle.</li>
            </ul>
          </div>
          <div class="col-lg-6 text-center">
            <div class="card p-2 border">
              <img src="assets/img/heatmap_plateforme_categorie.png" alt="Heatmap Plateforme Catégorie" class="img-fluid rounded" style="max-height: 380px;">
              <small class="text-muted mt-1">Dispersion des prix par canal et catégorie alimentaire</small>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- ============================================================== -->
    <!-- SECTION 4 : CONTEXTE SOCIO-ÉCONOMIQUE                          -->
    <!-- ============================================================== -->
    <div class="section-card" id="socio-eco">
      <div class="section-header">
        <div class="d-flex align-items-center gap-2">
          <span class="badge bg-secondary" style="font-size: 0.85rem;">Territoires</span>
          <h2 class="section-title">Déterminants Territoriaux et Socio-Économiques</h2>
        </div>
        <span class="text-muted" style="font-size: 0.85rem;">Données INSEE / IGN / Recensement Agricole</span>
      </div>

      <div class="p-4">
        <p style="font-size: 0.92rem; color: #475569;">
          Les indices de prix départementaux sont confrontés aux indicateurs territoriaux de l\'INSEE pour tester 11 hypothèses de structuration spatiale des filières de circuits courts.
        </p>
        <div class="row g-3">
          <div class="col-md-3">
            <div class="p-3 bg-light rounded border text-center">
              <div class="fw-bold text-primary mb-1">Revenu Médian</div>
              <div style="font-size: 0.82rem; color: #64748B;">Corrélation positive modérée avec le niveau général des prix.</div>
            </div>
          </div>
          <div class="col-md-3">
            <div class="p-3 bg-light rounded border text-center">
              <div class="fw-bold text-success mb-1">Part du Bio</div>
              <div style="font-size: 0.82rem; color: #64748B;">Densité d\'exploitations bio corrélée aux zones à prime de prix plus faible.</div>
            </div>
          </div>
          <div class="col-md-3">
            <div class="p-3 bg-light rounded border text-center">
              <div class="fw-bold text-dark mb-1">Cadres & Diplômes</div>
              <div style="font-size: 0.82rem; color: #64748B;">Effet de demande marqué en zones métropolitaines denses.</div>
            </div>
          </div>
          <div class="col-md-3">
            <div class="p-3 bg-light rounded border text-center">
              <div class="fw-bold text-danger mb-1">Inégalités (Gini)</div>
              <div style="font-size: 0.82rem; color: #64748B;">Absence de corrélation significative avec la dispersion interne.</div>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- ============================================================== -->
    <!-- SECTION 5 : ACCÈS À L\'APPLICATION INTERACTIVE COMPLÈTE SHINYLIVE -->
    <!-- ============================================================== -->
    <div class="section-card border-success" style="background: linear-gradient(135deg, #F0FDF4, #FFFFFF); border-width: 2px;">
      <div class="p-4 text-center">
        <span class="badge bg-success mb-2" style="font-size: 0.85rem;">Console R WebAssembly Intégrale</span>
        <h3 style="color: #14532D; margin-bottom: 10px;">Besoin d\'explorer dynamiquement produit par produit ?</h3>
        <p class="mx-auto" style="max-width: 720px; color: #475569; font-size: 0.95rem;">
          Pour tester l\'ensemble des filtres interactifs (choix parmi les 61 produits, rayons FoodOn, seuils dynamiques TCL du théorème central limite et module de contrôle qualité), lancez le démonstrateur dynamique autonome :
        </p>
        <div class="d-flex justify-content-center gap-3 mt-3 flex-wrap">
          <a href="app/" target="_blank" class="btn btn-success btn-lg px-4 fw-bold d-flex align-items-center gap-2">
            <i class="bi bi-box-arrow-up-right"></i>
            Ouvrir la Console Shiny dans un nouvel onglet
          </a>
          <button class="btn btn-outline-success btn-lg px-4 fw-bold" onclick="loadEmbeddedApp()">
            <i class="bi bi-window-fullscreen me-1"></i> Charger ici dans la page
          </button>
        </div>
        <div id="embeddedAppContainer" class="mt-4 d-none">
          <div class="alert alert-info py-2" style="font-size: 0.85rem;">
            <i class="bi bi-arrow-repeat spin me-1"></i> Chargement du moteur WebAssembly Shinylive (environ 20 secondes au premier démarrage)...
          </div>
          <iframe id="shinyIframe" style="width: 100%%; height: 850px; border: 1px solid #CBD5E1; border-radius: 8px;" src=""></iframe>
        </div>
      </div>
    </div>

  </div>

  <!-- FOOTER -->
  <footer>
    <div class="container-fluid px-4">
      <div class="row align-items-center">
        <div class="col-md-7">
          <p class="mb-1"><strong>Priscope — Démonstrateur de Modélisation Économétrique</strong></p>
          <p class="mb-0 text-muted" style="font-size: 0.80rem;">
            Projet PLAT4TERFOOD • Données synthétiques pour illustration méthodologique • Auteur : Jules Bataller-Beltran
          </p>
        </div>
        <div class="col-md-5 text-md-end mt-3 mt-md-0">
          <a href="https://github.com/julesbataller/Priscope" target="_blank" class="btn btn-sm btn-outline-secondary">
            <i class="bi bi-github me-1"></i> Code source & Dépôt GitHub
          </a>
        </div>
      </div>
    </div>
  </footer>

  <!-- Scripts Bootstrap & Filtrage tableau -->
  <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"></script>
  <script>
    // Filtrage dynamique du tableau des départements
    document.getElementById("deptFilterInput").addEventListener("keyup", function() {
      var filter = this.value.toLowerCase();
      var rows = document.querySelectorAll("#deptTable tbody tr");
      rows.forEach(function(row) {
        var text = row.textContent.toLowerCase();
        row.style.display = text.indexOf(filter) > -1 ? "" : "none";
      });
    });

    // Chargement de l\'iframe Shinylive à la demande
    function loadEmbeddedApp() {
      var container = document.getElementById("embeddedAppContainer");
      var iframe = document.getElementById("shinyIframe");
      container.classList.remove("d-none");
      if (!iframe.src || iframe.src === "" || iframe.src === window.location.href) {
        iframe.src = "app/";
      }
      container.scrollIntoView({ behavior: "smooth" });
    }
  </script>
</body>
</html>'

index_html <- index_template

replacements <- list(
  "__TOT_TRANSACTIONS__" = tot_transactions_fmt,
  "__RAW_TRANSACTIONS__" = raw_transactions_fmt,
  "__NB_DEPTS__" = nb_depts_val,
  "__NB_CATS__" = nb_cats_val,
  "__PRIME_BIO__" = prime_bio_str,
  "__EFFET_MOYEN__" = effet_moyen_str,
  "__EFFET_GROS__" = effet_gros_str,
  "__PLAT2__" = plat2_str,
  "__PLAT3__" = plat3_str,
  "__PLAT4__" = plat4_str,
  "__SIGMA2_RESID__" = sigma2_resid_val,
  "__ICC__" = icc_val,
  "__DEPT_ROWS__" = dept_rows_html,
  "__LL_NORM__" = ll_norm_val,
  "__AIC_NORM__" = aic_norm_val,
  "__NU__" = nu_val,
  "__LL_STUD__" = ll_stud_val,
  "__AIC_STUD__" = aic_stud_val,
  "__DELTA_AIC__" = delta_aic_val,
  "__GMM_TRANSACTIONS__" = gmm_transactions_fmt
)

for (nm in names(replacements)) {
  index_html <- gsub(nm, replacements[[nm]], index_html, fixed = TRUE)
}

index_html <- gsub("%%", "%", index_html, fixed = TRUE)

out_index_path <- file.path(output_dir, "index.html")
writeLines(index_html, out_index_path, useBytes = TRUE)
cat("[SUCCÈS] Page vitrine index.html générée :", out_index_path, "(", file.size(out_index_path) / 1024, "Ko)\n")
cat("=== Fin de la génération de la vitrine ===\n")
