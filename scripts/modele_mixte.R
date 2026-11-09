# =============================================================================
# MODÈLE LINÉAIRE MIXTE - Spécification Enrichie (Bio, Conditionnement, Territoire)
# =============================================================================
# Modèle Optimal : log(p_idsbc) = mu + alpha_i (catégorie) + delta_s (plateforme) 
#                                 + gamma_b (bio) + theta_c (conditionnement) 
#                                 + u_d (département) + epsilon_idsbc
#
# Où u_d ~ N(0, sigma²_u) est l'effet aléatoire du département d
# =============================================================================

library(lme4)
library(dplyr)
library(ggplot2)
library(RMariaDB)
library(tidyr)
library(data.table)

cat("=== MODÈLE LINÉAIRE MIXTE ENRICHI - ANALYSE DES PRIX ===\n")
cat("Date :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 1. EXTRACTION DES DONNÉES
# ─────────────────────────────────────────────────────────────────────────────
cat("--- Étape 1 : Chargement des données désaisonnalisées ---\n")

agg_path <- if (file.exists("data/df_agg_corrige.rds")) "data/df_agg_corrige.rds" else "data_simulees/df_agg_corrige.rds"
if (!file.exists(agg_path)) {
  stop("Le fichier df_agg_corrige.rds est introuvable dans data/ ou data_simulees/.")
}
df_agg <- readRDS(agg_path)
plat_map <- if (file.exists("data/mapping_plateformes.rds")) readRDS("data/mapping_plateformes.rds") else NULL

df_agg <- na.omit(df_agg)
df_agg$dept <- as.factor(df_agg$dept)
df_agg$Plateforme <- as.factor(df_agg$Plateforme)
df_agg$categorie_standard <- as.factor(df_agg$categorie_standard)
df_agg$organic <- as.factor(df_agg$organic)
df_agg$tranche_poids <- as.factor(df_agg$tranche_poids)
df_agg$poids <- df_agg$Nombre_Ventes

cat("   Cellules agrégées valides (dept × plat × cat × bio × poids) :", nrow(df_agg), "\n")
cat("   Départements :", length(unique(df_agg$dept)), "\n")
cat("   Plateformes  :", paste(unique(df_agg$Plateforme), collapse = ", "), "\n")
cat("   Catégories   :", length(unique(df_agg$categorie_standard)), "\n")
cat("   Total ventes :", format(sum(df_agg$Nombre_Ventes), big.mark = " "), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 2. COMPARAISON STATISTIQUE DES SPÉCIFICATIONS (ML)
# ─────────────────────────────────────────────────────────────────────────────
cat("--- Étape 2 : Comparaison des critères d'information (AIC / BIC) ---\n")

mod_base_ml <- lmer(log_prix ~ Plateforme + categorie_standard + (1 | dept), 
                    data = df_agg, weights = poids, REML = FALSE)
mod_bio_ml  <- lmer(log_prix ~ Plateforme + categorie_standard + organic + (1 | dept), 
                    data = df_agg, weights = poids, REML = FALSE)
mod_opt_ml  <- lmer(log_prix ~ Plateforme + categorie_standard + organic + tranche_poids + (1 | dept), 
                    data = df_agg, weights = poids, REML = FALSE)
mod_cont_ml <- lmer(log_prix ~ Plateforme + categorie_standard + organic + mean_log_poids + (1 | dept), 
                    data = df_agg, weights = poids, REML = FALSE)

aic_tab <- AIC(mod_base_ml, mod_bio_ml, mod_opt_ml, mod_cont_ml)
bic_tab <- BIC(mod_base_ml, mod_bio_ml, mod_opt_ml, mod_cont_ml)

df_comparaison_modeles <- data.frame(
  Specification = c("1. Sans Bio ni Conditionnement", 
                    "2. + Label Biologique", 
                    "3. + Bio + Tranches de Conditionnement (Discret)", 
                    "4. + Bio + Log(Poids) moyen (Continu)"),
  Variables_Incluses = c("Plateforme + Catégorie",
                         "Plateforme + Catégorie + Bio",
                         "Plateforme + Catégorie + Bio + Tranche Poids",
                         "Plateforme + Catégorie + Bio + Log(Poids)"),
  df = aic_tab$df,
  AIC = round(aic_tab$AIC, 1),
  BIC = round(bic_tab$BIC, 1),
  Delta_AIC = round(aic_tab$AIC - min(aic_tab$AIC), 1),
  stringsAsFactors = FALSE
)

print(df_comparaison_modeles)
cat("\n")

# ─────────────────────────────────────────────────────────────────────────────
# 3. AJUSTEMENT DU MODÈLE OPTIMAL (REML)
# ─────────────────────────────────────────────────────────────────────────────
cat("--- Étape 3 : Estimation REML du modèle optimal (Modèle 3 Discret) ---\n")

mod_mixte <- lmer(
  log_prix ~ Plateforme + categorie_standard + organic + tranche_poids + (1 | dept),
  data = df_agg,
  weights = poids,
  REML = TRUE
)

cat("   Modèle ajusté avec succès.\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4. EXTRACTION DES EFFETS ET DE LA VARIANCE
# ─────────────────────────────────────────────────────────────────────────────
cat("--- Étape 4 : Décomposition de la variance et effets fixes ---\n")

vc <- as.data.frame(VarCorr(mod_mixte))
sigma2_dept <- vc$vcov[vc$grp == "dept"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]
sigma2_total <- sigma2_dept + sigma2_resid
icc <- sigma2_dept / sigma2_total

cat(sprintf("   Variance inter-départements (σ²_u)     : %.6f\n", sigma2_dept))
cat(sprintf("   Variance résiduelle (σ²_ε)             : %.6f\n", sigma2_resid))
cat(sprintf("   Variance totale                        : %.6f\n", sigma2_total))
cat(sprintf("   ICC (Intraclass Correlation Coeff.)    : %.4f (%.2f%%)\n\n", icc, icc * 100))

fe <- fixef(mod_mixte)

# 4.1 Effets Bio
coef_bio <- fe[grep("^organic", names(fe))]
cat("   Label Biologique :\n")
for (nm in names(coef_bio)) {
  val <- coef_bio[nm]
  cat(sprintf("     %s : %+.4f (log) → %+.1f%% vs Bio de référence\n", nm, val, (exp(val) - 1) * 100))
}

# 4.2 Effets Conditionnement
coef_poids <- fe[grep("^tranche_poids", names(fe))]
cat("\n   Effet de Conditionnement / Gros :\n")
cat("     Petit (<1kg) : Référence (0 %)\n")
for (nm in names(coef_poids)) {
  val <- coef_poids[nm]
  cat(sprintf("     %s : %+.4f (log) → %+.1f%% vs Petit (<1kg)\n", sub("tranche_poids", "", nm), val, (exp(val) - 1) * 100))
}

# 4.3 Effets Plateformes
plat_effects <- fe[grep("^Plateforme", names(fe))]
ref_plat <- levels(df_agg$Plateforme)[1]
cat(sprintf("\n   Plateforme de référence : %s (0 %%)\n", ref_plat))
for (nm in names(plat_effects)) {
  plat_name <- sub("Plateforme", "", nm)
  eff <- plat_effects[nm]
  pct <- (exp(eff) - 1) * 100
  cat(sprintf("     %s vs %s : %+.4f (log) → %+.1f%%\n", plat_name, ref_plat, eff, pct))
}

# 4.4 Effets Aléatoires Départementaux (BLUPs / IPT)
re_dept <- ranef(mod_mixte)$dept
re_dept$dept <- rownames(re_dept)
colnames(re_dept)[1] <- "effet_dept"
re_dept <- re_dept %>%
  mutate(pct_effet = (exp(effet_dept) - 1) * 100) %>%
  arrange(desc(effet_dept))

cat("\n   Top 5 départements les plus chers (IPT) :\n")
print(head(re_dept %>% select(dept, effet_dept, pct_effet), 5))

cat("\n   Top 5 départements les moins chers (IPT) :\n")
print(tail(re_dept %>% select(dept, effet_dept, pct_effet), 5))

# ─────────────────────────────────────────────────────────────────────────────
# 5. GRAPHIQUES DIAGNOSTIQUES
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Étape 5 : Production des graphiques diagnostiques ---\n")
dir.create("resultats_modele_mixte", showWarnings = FALSE, recursive = TRUE)

# 5.1 Caterpillar plot des effets départementaux
re_ci <- as.data.frame(ranef(mod_mixte, condVar = TRUE)$dept)
re_ci$dept <- rownames(re_ci)
colnames(re_ci)[1] <- "effet"
pv <- attr(ranef(mod_mixte, condVar = TRUE)$dept, "postVar")
re_ci$se <- sqrt(pv[1, 1, ])
re_ci$lower <- re_ci$effet - 1.96 * re_ci$se
re_ci$upper <- re_ci$effet + 1.96 * re_ci$se
re_ci <- re_ci %>% arrange(effet)
re_ci$dept <- factor(re_ci$dept, levels = re_ci$dept)

p1 <- ggplot(re_ci, aes(x = dept, y = effet)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#7F8C8D", linewidth = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3, color = "#BDC3C7") +
  geom_point(aes(color = effet), size = 2.5) +
  scale_color_gradient2(low = "#2ECC71", mid = "#F39C12", high = "#E74C3C", midpoint = 0,
                        name = "Effet\ndépartemental") +
  coord_flip() +
  labs(
    title = "Effets aléatoires départementaux sur les log-prix (IPT épuré)",
    subtitle = "Modèle mixte : log(prix) ~ Plateforme + Catégorie + Bio + Conditionnement + (1|Département)",
    x = "Département",
    y = "Effet aléatoire (log-échelle) ± IC 95%"
  ) +
  theme_minimal(base_family = "Outfit", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
    plot.subtitle = element_text(size = 10, color = "#7F8C8D"),
    axis.text.y = element_text(size = 6)
  )
ggsave("resultats_modele_mixte/caterpillar_departements.png", p1, width = 10, height = 14, dpi = 150)

# 5.2 Barplot des effets Plateformes épurés
df_plat <- data.frame(
  plateforme = c(ref_plat, sub("Plateforme", "", names(plat_effects))),
  coef = c(0, unname(plat_effects)),
  stringsAsFactors = FALSE
) %>%
  mutate(
    pct = (exp(coef) - 1) * 100,
    plateforme = if (!is.null(plat_map) && any(plateforme %in% names(plat_map))) {
      ifelse(plateforme %in% names(plat_map), unname(plat_map[plateforme]), plateforme)
    } else {
      plateforme
    }
  )

p2 <- ggplot(df_plat, aes(x = reorder(plateforme, pct), y = pct, fill = pct)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "solid", color = "#2C3E50", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f%%", pct)), hjust = ifelse(df_plat$pct >= 0, -0.1, 1.1),
            size = 4, fontface = "bold", color = "#2C3E50") +
  scale_fill_gradient2(low = "#2ECC71", mid = "#F1C40F", high = "#E74C3C", midpoint = 0) +
  coord_flip() +
  labs(
    title = "Effet de la plateforme sur les prix (contrôlé pour bio et conditionnement)",
    subtitle = sprintf("Plateforme de référence : %s", ref_plat),
    x = NULL,
    y = "Écart de prix relatif (%)"
  ) +
  theme_minimal(base_family = "Outfit", base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
    plot.subtitle = element_text(size = 10, color = "#7F8C8D")
  )
ggsave("resultats_modele_mixte/effets_plateforme.png", p2, width = 8, height = 4, dpi = 150)

# 5.3 Diagnostics résidus
res <- residuals(mod_mixte)
fitted_vals <- fitted(mod_mixte)

p3 <- ggplot(data.frame(sample = res), aes(sample = sample)) +
  stat_qq(color = "#2980B9", alpha = 0.5, size = 1.5) +
  stat_qq_line(color = "#E74C3C", linewidth = 0.8) +
  labs(
    title = "Q-Q Plot des résidus du modèle mixte enrichi",
    subtitle = "Vérification de la normalité des résidus",
    x = "Quantiles théoriques",
    y = "Quantiles observés (résidus)"
  ) +
  theme_minimal(base_family = "Outfit", base_size = 12)
ggsave("resultats_modele_mixte/qq_plot_residus.png", p3, width = 7, height = 6, dpi = 150)

p4 <- ggplot(data.frame(fitted = fitted_vals, resid = res), aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.2, color = "#2C3E50", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "#3498DB", se = FALSE, linewidth = 0.8) +
  labs(
    title = "Résidus vs Valeurs Ajustées",
    subtitle = "Vérification de l'homoscédasticité et linéarité",
    x = "Valeurs ajustées (log-prix)",
    y = "Résidus"
  ) +
  theme_minimal(base_family = "Outfit", base_size = 12)
ggsave("resultats_modele_mixte/residus_vs_fitted.png", p4, width = 8, height = 6, dpi = 150)

# 5.4 Heatmap des écarts plateforme × catégorie
df_ecarts <- df_agg %>%
  group_by(categorie_standard, Plateforme) %>%
  summarise(
    prix_moyen_log = weighted.mean(log_prix, poids, na.rm = TRUE),
    n_ventes = sum(Nombre_Ventes),
    .groups = "drop"
  ) %>%
  group_by(categorie_standard) %>%
  mutate(
    prix_ref = mean(prix_moyen_log),
    ecart_pct = (exp(prix_moyen_log - prix_ref) - 1) * 100
  ) %>%
  ungroup()

df_heatmap <- df_ecarts %>%
  mutate(
    plateforme_label = if (!is.null(plat_map) && any(as.character(Plateforme) %in% names(plat_map))) {
      ifelse(as.character(Plateforme) %in% names(plat_map), unname(plat_map[as.character(Plateforme)]), as.character(Plateforme))
    } else {
      as.character(Plateforme)
    }
  )

p5 <- ggplot(df_heatmap, aes(x = plateforme_label, y = reorder(categorie_standard, ecart_pct, FUN = mean), fill = ecart_pct)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%+.0f%%", ecart_pct)), size = 2.8, color = "black") +
  scale_fill_gradient2(
    low = "#2ECC71", mid = "#FFFFFF", high = "#E74C3C", midpoint = 0,
    limits = c(-50, 50), oob = scales::squish,
    name = "Écart au prix\nmoyen produit (%)"
  ) +
  labs(
    title = "Écart de prix par catégorie et plateforme",
    subtitle = "Écart en % par rapport à la moyenne nationale toutes plateformes confondues",
    x = "Plateforme",
    y = "Catégorie de produit"
  ) +
  theme_minimal(base_family = "Outfit", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#2C3E50"),
    plot.subtitle = element_text(size = 9, color = "#7F8C8D"),
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank()
  )
ggsave("resultats_modele_mixte/heatmap_plateforme_categorie.png", p5, width = 9, height = 10, dpi = 150)

# ─────────────────────────────────────────────────────────────────────────────
# 6. EXPORT DES RÉSULTATS POUR L'APPLICATION ET LE RAPPORT
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Étape 6 : Exportation des objets RDS et CSV ---\n")

saveRDS(df_comparaison_modeles, "resultats_modele_mixte/comparaison_modeles_aic_bic.rds")
write.csv(df_comparaison_modeles, "resultats_modele_mixte/comparaison_modeles_aic_bic.csv", row.names = FALSE, fileEncoding = "UTF-8")

saveRDS(mod_mixte, "resultats_modele_mixte/modele_mixte_lmer.rds")

re_export <- re_dept %>%
  mutate(
    prix_relatif = sprintf("%+.1f%%", pct_effet),
    interpretation = case_when(
      pct_effet > 5 ~ "Significativement plus cher",
      pct_effet > 2 ~ "Légèrement plus cher",
      pct_effet > -2 ~ "Dans la moyenne nationale",
      pct_effet > -5 ~ "Légèrement moins cher",
      TRUE ~ "Significativement moins cher"
    )
  )
saveRDS(re_export, "resultats_modele_mixte/effets_departementaux.rds")

fe_export <- data.frame(
  variable = names(fe),
  coefficient = unname(fe),
  pct_effet = (exp(unname(fe)) - 1) * 100,
  stringsAsFactors = FALSE
)
saveRDS(fe_export, "resultats_modele_mixte/effets_fixes.rds")

variance_export <- list(
  sigma2_dept = sigma2_dept,
  sigma2_resid = sigma2_resid,
  icc = icc,
  n_obs = nrow(df_agg),
  n_dept = length(unique(df_agg$dept)),
  n_plat = length(unique(df_agg$Plateforme)),
  n_cat = length(unique(df_agg$categorie_standard)),
  total_ventes = sum(df_agg$Nombre_Ventes)
)
saveRDS(variance_export, "resultats_modele_mixte/variance_components.rds")
saveRDS(df_ecarts, "resultats_modele_mixte/ecarts_plateforme_categorie.rds")
saveRDS(df_heatmap, "resultats_modele_mixte/heatmap_data.rds")

# Synchronisation avec le dossier data/ de l'app Shiny si présent
if (dir.exists("app_visu/data")) {
  saveRDS(re_export, "app_visu/data/effets_departementaux.rds")
  saveRDS(fe_export, "app_visu/data/effets_fixes.rds")
  saveRDS(variance_export, "app_visu/data/variance_components.rds")
  saveRDS(df_ecarts, "app_visu/data/ecarts_plateforme_categorie.rds")
  saveRDS(df_heatmap, "app_visu/data/heatmap_data.rds")
  saveRDS(df_comparaison_modeles, "app_visu/data/comparaison_modeles_aic_bic.rds")
}
if (dir.exists("data")) {
  saveRDS(re_export, "data/effets_departementaux.rds")
  saveRDS(fe_export, "data/effets_fixes.rds")
  saveRDS(variance_export, "data/variance_components.rds")
  saveRDS(df_ecarts, "data/ecarts_plateforme_categorie.rds")
  saveRDS(df_heatmap, "data/heatmap_data.rds")
  saveRDS(df_comparaison_modeles, "data/comparaison_modeles_aic_bic.rds")
}

# Synchronisation des figures avec le rapport
if (dir.exists("rapport/figures")) {
  file.copy("resultats_modele_mixte/caterpillar_departements.png", "rapport/figures/caterpillar_departements.png", overwrite = TRUE)
  file.copy("resultats_modele_mixte/effets_plateforme.png", "rapport/figures/effets_plateforme.png", overwrite = TRUE)
  file.copy("resultats_modele_mixte/heatmap_plateforme_categorie.png", "rapport/figures/heatmap_plateforme_categorie.png", overwrite = TRUE)
  file.copy("resultats_modele_mixte/qq_plot_residus.png", "rapport/figures/qq_plot_residus.png", overwrite = TRUE)
  file.copy("resultats_modele_mixte/residus_vs_fitted.png", "rapport/figures/residus_vs_fitted.png", overwrite = TRUE)
}

# Synchronisation des figures avec l'application Shiny (app_visu/www)
www_dirs <- c("app_visu/www", "www")
for (wd in www_dirs) {
  if (dir.exists(wd)) {
    file.copy("resultats_modele_mixte/caterpillar_departements.png", file.path(wd, "caterpillar_departements.png"), overwrite = TRUE)
    file.copy("resultats_modele_mixte/effets_plateforme.png", file.path(wd, "effets_plateforme.png"), overwrite = TRUE)
    file.copy("resultats_modele_mixte/heatmap_plateforme_categorie.png", file.path(wd, "heatmap_plateforme_categorie.png"), overwrite = TRUE)
    file.copy("resultats_modele_mixte/qq_plot_residus.png", file.path(wd, "qq_plot_residus.png"), overwrite = TRUE)
    file.copy("resultats_modele_mixte/residus_vs_fitted.png", file.path(wd, "residus_vs_fitted.png"), overwrite = TRUE)
  }
}

cat("   [✓] Tous les objets et graphiques ont été exportés et synchronisés.\n")
cat("\n=== MODÈLE MIXTE TERMINÉ AVEC SUCCÈS ===\n")
