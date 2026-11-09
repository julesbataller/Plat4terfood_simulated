#!/usr/bin/env Rscript
# ==============================================================================
# GÉNÉRATEUR DE DONNÉES SIMULÉES ET GRAPHIQUES ASSOCIÉS POUR GITHUB
# Projet : Priscope - Démonstrateur d'Analyse Économétrique des Prix
#
# Objectif :
#   Simuler un univers complet et statistiquement cohérent de transactions en
#   multi-canaux (Plateforme 1 à Plateforme 4, 2021-2026, 101 départements,
#   nomenclature FoodOn), ajuster les modèles réels (GMM Mclust, STL, Modèles
#   Linéaires Mixtes lme4, loi de Student-t, régressions hédoniques NLP) sur ces
#   données synthétiques, puis exporter :
#     - data_simulees/*.rds et *.geojson
#     - data_simulees/www/*.png et illustration_legumes.jpg
#
# Aucune donnée issue des plateformes partenaires réelles n'est lue ni copiée.
# Usage :
#   Rscript scripts/simuler_donnees_github.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(lme4)
  library(mclust)
  library(MASS)
  library(data.table)
  library(forecast)
})

# Eviter le masquage des verbes dplyr par MASS, mclust et data.table
select <- dplyr::select
count  <- dplyr::count
first  <- dplyr::first
lag    <- dplyr::lag

set.seed(20261107)

cat("======================================================================\n")
cat("   SIMULATION COMPLÈTE DES DONNÉES & GRAPHIQUES (MODE DÉMO GITHUB)    \n")
cat("======================================================================\n\n")

out_dir <- "data_simulees"
out_www <- file.path(out_dir, "www")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_www, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1. FONDS CARTOGRAPHIQUES PUBLICS & NOMENCLATURE FOODON
# ==============================================================================
cat("[1/8] Préparation des référentiels publics (GeoJSON & FoodOn)...\n")

if (!file.exists(file.path(out_dir, "departements.geojson")) && file.exists("data/departements.geojson")) {
  file.copy("data/departements.geojson", file.path(out_dir, "departements.geojson"), overwrite = FALSE)
}
if (file.exists("data/boxes_dom.geojson")) {
  file.copy("data/boxes_dom.geojson", file.path(out_dir, "boxes_dom.geojson"), overwrite = TRUE)
}
if (file.exists("www/illustration_legumes.jpg")) {
  file.copy("www/illustration_legumes.jpg", file.path(out_www, "illustration_legumes.jpg"), overwrite = TRUE)
}

sf_use_s2(FALSE)
france_sf <- st_read(file.path(out_dir, "departements.geojson"), quiet = TRUE)
france_sf <- suppressWarnings(st_make_valid(france_sf))
depts_meta <- france_sf %>%
  st_drop_geometry() %>%
  transmute(
    code_dept = as.character(code),
    nom_dept  = as.character(nom)
  ) %>%
  distinct(code_dept, .keep_all = TRUE)

# Nomenclature publique FoodOn (64 catégories standards)
foodon_df <- tibble::tribble(
  ~categorie_standard, ~nom_botanique_ou_scientifique, ~foodon_id, ~foodon_label, ~foodex2_code, ~agrovoc_uri, ~groupe, ~saison_principale, ~panier_quotidien, ~unite_reference, ~densite_kg_l, ~poids_indicatif_piece_kg, ~prix_base_sim,
  "Abricot", "Prunus armeniaca", "FOODON:03301397", "apricot food product", "A01EF", "c_567", "Fruits", "Été", 0L, "kg", NA, NA, 5.4,
  "Ail", "Allium sativum", "FOODON:03301384", "garlic food product", "A00KN", "c_3183", "Légumes", "Toute l'année", 1L, "kg", NA, 0.05, 9.2,
  "Asperge", "Asparagus officinalis", "FOODON:03301411", "asparagus food product", "A00LY", "c_687", "Légumes", "Printemps", 0L, "kg", NA, 0.5, 11.5,
  "Aubergine", "Solanum melongena", "FOODON:03301413", "eggplant food product", "A00JL", "c_2473", "Légumes", "Été", 0L, "kg", NA, 0.3, 4.1,
  "Banane", "Musa acuminata", "FOODON:03301407", "banana food product", "A01FR", "c_803", "Fruits", "Toute l'année", 0L, "kg", NA, 0.18, 2.6,
  "Betterave", "Beta vulgaris subsp. vulgaris", "FOODON:03301414", "beetroot food product", "A00NB", "c_907", "Légumes", "Automne", 0L, "kg", NA, 0.3, 2.8,
  "Bière", "Hordeum vulgare", "FOODON:03301452", "beer food product", "A03EX", "c_867", "Boissons", "Toute l'année", 0L, "L", 1.01, NA, 6.2,
  "Blette", "Beta vulgaris subsp. vulgaris", "FOODON:03301426", "swiss chard food product", "A00NG", "c_1532", "Légumes", "Automne", 0L, "kg", NA, 0.8, 3.2,
  "Bœuf", "Bos taurus", "FOODON:00002161", "beef food product", "A01RA", "c_836", "Viandes", "Toute l'année", 1L, "kg", NA, NA, 17.8,
  "Brocoli", "Brassica oleracea var. italica", "FOODON:03301420", "broccoli food product", "A00FE", "c_1092", "Légumes", "Automne", 0L, "kg", NA, 0.5, 4.3,
  "Canard", "Anas platyrhynchos", "FOODON:00002166", "duck meat food product", "A01SH", "c_2393", "Viandes", "Toute l'année", 0L, "kg", NA, NA, 19.5,
  "Carotte", "Daucus carota subsp. sativus", "FOODON:03301423", "carrot food product", "A00KP", "c_1350", "Légumes", "Toute l'année", 1L, "kg", NA, 1.0, 2.5,
  "Céleri-branche", "Apium graveolens var. dulce", "FOODON:03301425", "celery food product", "A00LZ", "c_1416", "Légumes", "Automne", 0L, "kg", NA, 0.6, 3.4,
  "Céleri-rave", "Apium graveolens var. rapaceum", "FOODON:03301424", "celeriac food product", "A00ND", "c_1417", "Légumes", "Hiver", 0L, "kg", NA, 0.8, 3.3,
  "Cerise", "Prunus avium", "FOODON:03301398", "cherry food product", "A01EG", "c_1507", "Fruits", "Printemps", 0L, "kg", NA, NA, 8.9,
  "Champignon", "Agaricus bisporus & spp.", "FOODON:03301428", "cultivated mushroom food product", "A00MY", "c_5008", "Légumes", "Toute l'année", 1L, "kg", NA, NA, 7.6,
  "Chou", "Brassica oleracea var. capitata", "FOODON:03301417", "head cabbage food product", "A00FC", "c_1190", "Légumes", "Hiver", 0L, "kg", NA, 1.2, 2.7,
  "Chou-fleur", "Brassica oleracea var. botrytis", "FOODON:03301419", "cauliflower food product", "A00FF", "c_1400", "Légumes", "Hiver", 1L, "kg", NA, 1.0, 3.5,
  "Cidre", "Malus domestica", "FOODON:03301453", "cider food product", "A03FF", "c_1611", "Boissons", "Toute l'année", 0L, "L", 1.015, NA, 4.8,
  "Citron", "Citrus limon", "FOODON:03301402", "lemon food product", "A01DD", "c_4251", "Fruits", "Hiver", 1L, "kg", NA, 0.15, 4.4,
  "Clémentine", "Citrus clementina", "FOODON:03301401", "mandarin orange food product", "A01DC", "c_1699", "Fruits", "Hiver", 1L, "kg", NA, NA, 4.6,
  "Concombre", "Cucumis sativus", "FOODON:03301431", "cucumber food product", "A00JG", "c_2039", "Légumes", "Été", 1L, "kg", NA, 0.4, 3.6,
  "Courge", "Cucurbita spp.", "FOODON:03301710", "winter squash food product", "A00HX", "c_6225", "Légumes", "Hiver", 1L, "kg", NA, 1.5, 2.6,
  "Courgette", "Cucurbita pepo var. cylindrica", "FOODON:03301709", "summer squash food product", "A00JH", "c_2069", "Légumes", "Été", 1L, "kg", NA, 0.25, 3.3,
  "Dinde", "Meleagris gallopavo", "FOODON:00002167", "turkey meat food product", "A01SL", "c_8032", "Viandes", "Toute l'année", 0L, "kg", NA, NA, 15.2,
  "Échalote", "Allium cepa var. aggregatum", "FOODON:03301386", "shallot food product", "A00KR", "c_7009", "Légumes", "Toute l'année", 0L, "kg", NA, 0.25, 5.8,
  "Endive", "Cichorium intybus var. foliosum", "FOODON:03301433", "witloof chicory food product", "A00MD", "c_1583", "Légumes", "Hiver", 1L, "kg", NA, 0.5, 4.2,
  "Épinard", "Spinacia oleracea", "FOODON:03301434", "spinach food product", "A00MS", "c_7314", "Légumes", "Printemps", 0L, "kg", NA, NA, 5.1,
  "Farine", "Triticum aestivum & spp.", "FOODON:03301389", "cereal flour food product", "A004Z", "c_2991", "Épicerie", "Toute l'année", 1L, "kg", NA, 1.0, 2.4,
  "Fenouil", "Foeniculum vulgare", "FOODON:03301435", "fennel food product", "A00MA", "c_2874", "Légumes", "Automne", 0L, "kg", NA, 0.4, 3.9,
  "Fraise", "Fragaria x ananassa", "FOODON:03301436", "strawberry food product", "A01FA", "c_7474", "Fruits", "Printemps", 1L, "kg", NA, 0.5, 9.8,
  "Fromage", "Bos taurus / Capra hircus / Ovis aries", "FOODON:00001013", "cheese food product", "A024E", "c_1502", "Produits laitiers & Œufs", "Toute l'année", 1L, "kg", NA, 0.25, 16.4,
  "Haricot vert", "Phaseolus vulgaris", "FOODON:03301437", "common bean food product", "A00HP", "c_5776", "Légumes", "Été", 1L, "kg", NA, NA, 6.8,
  "Huile", "Olea europaea / Helianthus annuus", "FOODON:00001176", "vegetable oil food product", "A032K", "c_5848", "Épicerie", "Toute l'année", 0L, "L", 0.92, NA, 9.5,
  "Jus de pomme", "Malus domestica", "FOODON:03301353", "apple juice", "A039F", "c_547", "Boissons", "Toute l'année", 1L, "L", 1.04, NA, 3.4,
  "Kiwi", "Actinidia deliciosa", "FOODON:03301408", "kiwi fruit food product", "A01FT", "c_4102", "Fruits", "Hiver", 0L, "kg", NA, 0.1, 4.9,
  "Lait", "Bos taurus", "FOODON:00001094", "cow milk", "A02LR", "c_4826", "Produits laitiers & Œufs", "Toute l'année", 1L, "L", 1.032, NA, 1.45,
  "Lentille", "Lens culinaris", "FOODON:03301438", "lentil food product", "A011M", "c_4281", "Légumes", "Toute l'année", 0L, "kg", NA, 0.5, 4.7,
  "Melon", "Cucumis melo", "FOODON:03301405", "melon food product", "A01FV", "c_4694", "Fruits", "Été", 1L, "kg", NA, 0.9, 3.8,
  "Miel", "Apis mellifera", "FOODON:00001174", "honey", "A031B", "c_3638", "Épicerie", "Toute l'année", 1L, "kg", 1.42, 0.5, 14.8,
  "Navet", "Brassica rapa subsp. rapa", "FOODON:03301416", "turnip food product", "A00KX", "c_8039", "Légumes", "Hiver", 0L, "kg", NA, 0.5, 2.9,
  "Noix", "Juglans regia", "FOODON:03301410", "walnut food product", "A01CY", "c_8321", "Fruits", "Automne", 0L, "kg", NA, NA, 8.2,
  "Œuf", "Gallus gallus domesticus", "FOODON:03301344", "chicken egg", "A0350", "c_2525", "Produits laitiers & Œufs", "Toute l'année", 1L, "pièce", NA, 0.06, 0.42,
  "Oignon", "Allium cepa", "FOODON:03301385", "onion food product", "A00KQ", "c_5357", "Légumes", "Toute l'année", 1L, "kg", NA, 0.5, 2.7,
  "Orange", "Citrus sinensis", "FOODON:03301403", "orange food product", "A01DE", "c_5397", "Fruits", "Hiver", 1L, "kg", NA, 0.2, 3.6,
  "Panais", "Pastinaca sativa", "FOODON:03301440", "parsnip food product", "A00KS", "c_5605", "Légumes", "Hiver", 0L, "kg", NA, 0.5, 3.5,
  "Patate douce", "Ipomoea batatas", "FOODON:03301441", "sweet potato food product", "A00NL", "c_7577", "Légumes", "Automne", 0L, "kg", NA, 0.4, 4.1,
  "Pêche", "Prunus persica", "FOODON:03301404", "peach and nectarine food product", "A01EH", "c_5641", "Fruits", "Été", 1L, "kg", NA, NA, 5.2,
  "Persil", "Petroselinum crispum", "FOODON:03301442", "parsley food product", "A00QJ", "c_5625", "Légumes", "Toute l'année", 0L, "kg", NA, 0.1, 11.0,
  "Poire", "Pyrus communis", "FOODON:03301396", "pear food product", "A01DL", "c_5647", "Fruits", "Automne", 1L, "kg", NA, 0.2, 3.7,
  "Poireau", "Allium ampeloprasum var. porrum", "FOODON:03301387", "leek food product", "A00ME", "c_4243", "Légumes", "Hiver", 1L, "kg", NA, 1.0, 3.2,
  "Poivron", "Capsicum annuum", "FOODON:03301443", "sweet pepper food product", "A00JJ", "c_7547", "Légumes", "Été", 1L, "kg", NA, 0.2, 4.5,
  "Pomme", "Malus domestica", "FOODON:00001015", "apple food product", "A01DJ", "c_544", "Fruits", "Automne", 1L, "kg", NA, 0.18, 2.9,
  "Pomme de terre", "Solanum tuberosum", "FOODON:03301439", "potato food product", "A011Z", "c_6141", "Légumes", "Toute l'année", 1L, "kg", NA, 1.0, 2.1,
  "Porc", "Sus domesticus", "FOODON:00002163", "pork food product", "A01RJ", "c_6097", "Viandes", "Toute l'année", 1L, "kg", NA, NA, 13.6,
  "Poulet", "Gallus gallus domesticus", "FOODON:00001994", "chicken meat food product", "A01SB", "c_1537", "Viandes", "Toute l'année", 1L, "kg", 1.0, 1.5, 12.8,
  "Prune", "Prunus domestica", "FOODON:03301399", "plum food product", "A01EK", "c_6047", "Fruits", "Été", 0L, "kg", NA, NA, 4.6,
  "Radis", "Raphanus sativus", "FOODON:03301444", "radish food product", "A00KV", "c_6446", "Légumes", "Printemps", 1L, "kg", NA, 0.35, 4.8,
  "Raisin", "Vitis vinifera", "FOODON:03301406", "grape food product", "A01FA", "c_3366", "Fruits", "Automne", 0L, "kg", NA, NA, 5.3,
  "Salade", "Lactuca sativa & spp.", "FOODON:03301705", "salad vegetable food product", "A00LC", "c_4201", "Légumes", "Toute l'année", 1L, "kg", NA, 0.35, 4.9,
  "Saucisse", "Sus domesticus & Bos taurus", "FOODON:00001248", "sausage food product", "A021N", "c_6851", "Viandes", "Toute l'année", 1L, "kg", NA, NA, 14.2,
  "Tomate", "Solanum lycopersicum", "FOODON:03301445", "tomato food product", "A00JF", "c_7809", "Légumes", "Été", 1L, "kg", NA, NA, 4.1,
  "Vin", "Vitis vinifera", "FOODON:03301454", "wine food product", "A038L", "c_8407", "Boissons", "Toute l'année", 0L, "L", 0.99, 0.7425, 9.2,
  "Yaourt", "Bos taurus / Capra hircus / Ovis aries", "FOODON:00001183", "yogurt food product", "A02NC", "c_8497", "Produits laitiers & Œufs", "Toute l'année", 1L, "kg", 1.05, 0.125, 5.4
)

ref_foodon_export <- as.data.frame(foodon_df %>% select(-prix_base_sim))
saveRDS(ref_foodon_export, file.path(out_dir, "referentiel_categories_foodon.rds"))

# ==============================================================================
# 2. SIMULATION DES STRUCTURES DÉPARTEMENTALES (SOCIO-ÉCO, RA 2020, MAGASINS)
# ==============================================================================
cat("[2/8] Simulation cohérente des données départementales et magasins...\n")

n_dept <- nrow(depts_meta)
is_dom <- grepl("^97", depts_meta$code_dept)
is_idf <- depts_meta$code_dept %in% c("75", "92", "93", "94", "78", "91", "95", "77")
is_metrop <- depts_meta$code_dept %in% c("69", "13", "31", "33", "34", "44", "59", "06", "67", "35", "38")

# Facteur latent d'urbanité et de niveau de vie (purement synthétique)
urban_score <- rnorm(n_dept, 0, 0.7) + ifelse(is_idf, 1.8, 0) + ifelse(is_metrop, 0.9, 0) - ifelse(is_dom, 0.3, 0)
agri_score  <- -0.65 * urban_score + rnorm(n_dept, 0, 0.6)

# Découpage adaptatif par quantiles de urban_score (sans seuil fixe arbitraire)
q_urb <- quantile(urban_score, probs = c(0.45, 0.82))
typo_dens <- ifelse(urban_score >= q_urb[2], "1- Urbain dense",
             ifelse(urban_score >= q_urb[1], "2- Urbain intermédiaire", "3- Rural"))

rev_med <- round(pmax(15500, 22400 + 2300 * urban_score - ifelse(is_dom, 3800, 0) + rnorm(n_dept, 0, 900)))
rev_q1  <- round(rev_med * runif(n_dept, 0.68, 0.76))
rev_q3  <- round(rev_med * runif(n_dept, 1.30, 1.44))
rev_d1  <- round(rev_med * runif(n_dept, 0.46, 0.56))
rev_d9  <- round(rev_med * runif(n_dept, 1.72, 2.05))

pct_exp_cc  <- round(pmin(48, pmax(5, 21 + 5.5 * agri_score + ifelse(is_metrop, 4, 0) + rnorm(n_dept, 0, 4.5))), 2)
pct_exp_bio <- round(pmin(38, pmax(3, 15 + 4.0 * agri_score + rnorm(n_dept, 0, 4.0))), 2)
pct_bio_cc  <- round(pmin(pct_exp_cc * 0.7, pct_exp_bio * runif(n_dept, 0.32, 0.58)), 2)
pct_sau_cc  <- round(pct_exp_cc * runif(n_dept, 0.45, 0.72), 2)
pct_sau_bio <- round(pct_exp_bio * runif(n_dept, 0.65, 0.92), 2)

tot_ea  <- round(pmax(120, 3800 * exp(0.45 * agri_score) * ifelse(is_idf, 0.18, 1)))
tot_sau <- round(tot_ea * runif(n_dept, 42, 88))
tot_etp <- round(tot_ea * runif(n_dept, 1.25, 2.15))

nb_mag_dept <- pmax(1L, round(5.5 + 2.2 * scale(tot_ea)[, 1] + 1.8 * scale(pct_exp_cc)[, 1] + runif(n_dept, 0, 3)))

socioeco_df <- tibble::tibble(
  code_dept = depts_meta$code_dept,
  pt_menimp = round(pmin(72, pmax(28, 49 + 6.5 * urban_score + rnorm(n_dept, 0, 3))), 1),
  emp2064 = round(pmin(78, pmax(46, 65 + 3.5 * urban_score - ifelse(is_dom, 9, 0) + rnorm(n_dept, 0, 2.5))), 1),
  p_diplsup15 = round(pmin(56, pmax(16, 29 + 6.8 * urban_score + rnorm(n_dept, 0, 2.5))), 1),
  p_csp_agr = round(pmin(11, pmax(0.1, 2.6 + 1.8 * pmax(0, agri_score) + rnorm(n_dept, 0, 0.6))), 2),
  p_csp_cadpis = round(pmin(42, pmax(5, 14 + 6.2 * urban_score + rnorm(n_dept, 0, 1.8))), 1),
  tx_act1564 = round(pmin(82, pmax(62, 73.5 + 2.2 * urban_score + rnorm(n_dept, 0, 1.8))), 1),
  surfeng = round(pmin(85, pmax(4, 48 + 14 * agri_score)), 1),
  nb_magasins = as.numeric(nb_mag_dept),
  pct_conv_cl_epci = round(pmax(20, 100 - pct_exp_cc - pct_exp_bio + pct_bio_cc), 1),
  pct_conv_cc_epci = round(pmax(2, pct_exp_cc - pct_bio_cc), 1),
  pct_bio_cl_epci = round(pmax(1, pct_exp_bio - pct_bio_cc), 1),
  pct_bio_cc_epci = pct_bio_cc,
  typo_densite = typo_dens,
  nom_dept = depts_meta$nom_dept,
  Pct_Exploitations_CC = pct_exp_cc,
  Pct_SAU_CC = pct_sau_cc,
  Pct_Exploitations_Bio = pct_exp_bio,
  Pct_SAU_Bio = pct_sau_bio,
  Pct_Exploitations_Bio_CC = pct_bio_cc,
  TotalEA = as.numeric(tot_ea),
  TotalSAU_ha = as.numeric(tot_sau),
  TotalETP = as.numeric(tot_etp),
  part_65plus = round(pmin(33, pmax(11, 22.5 - 3.2 * urban_score + rnorm(n_dept, 0, 2.2))), 1),
  part_moins20 = round(pmin(36, pmax(17, 23.2 + 1.6 * urban_score + ifelse(is_dom, 6, 0) + rnorm(n_dept, 0, 1.5))), 1),
  evol_65plus_2010_2020 = round(rnorm(n_dept, 3.1, 0.9), 2),
  evol_moins20_2010_2020 = round(rnorm(n_dept, -1.2, 0.8), 2),
  indice_vieillissement_2020 = round(pmax(40, (22.5 - 3.2 * urban_score) / 23.2 * 100 + rnorm(n_dept, 0, 12)), 1),
  indice_vieillissement_2010 = round(pmax(32, (19.5 - 2.8 * urban_score) / 24.4 * 100 + rnorm(n_dept, 0, 10)), 1),
  taux_pauvrete_60 = round(pmin(38, pmax(8, 14.8 - 2.1 * urban_score + ifelse(is_dom, 13, 0) + rnorm(n_dept, 0, 2.2))), 1),
  revenu_median_uc = as.numeric(rev_med),
  revenu_Q1 = as.numeric(rev_q1),
  revenu_Q3 = as.numeric(rev_q3),
  ecart_interquartile_relatif = round((rev_q3 - rev_q1) / rev_med, 3),
  indice_gini = round(pmin(0.44, pmax(0.24, 0.285 + 0.022 * abs(urban_score) + ifelse(is_dom, 0.06, 0) + rnorm(n_dept, 0, 0.015))), 3),
  revenu_D1 = as.numeric(rev_d1),
  revenu_D9 = as.numeric(rev_d9),
  part_revenus_activite = round(pmin(82, pmax(58, 71 + 3.2 * urban_score)), 1),
  part_pensions_retraites = round(pmin(36, pmax(12, 24 - 2.8 * urban_score)), 1),
  part_indemnites_chomage = round(runif(n_dept, 2.4, 4.8), 1),
  part_prestations_sociales = round(pmin(16, pmax(3, 5.5 - 0.6 * urban_score + ifelse(is_dom, 6, 0))), 1)
)
saveRDS(socioeco_df, file.path(out_dir, "socioeco_departements.rds"))

ra2020_df <- as.data.frame(
  socioeco_df %>%
    filter(!is_dom) %>%
    transmute(
      CodeDEPT = code_dept,
      NOMDEPT = nom_dept,
      Pct_Exploitations_CC = Pct_Exploitations_CC,
      Pct_SAU_CC = Pct_SAU_CC,
      Pct_Exploitations_Bio = Pct_Exploitations_Bio,
      Pct_SAU_Bio = Pct_SAU_Bio,
      Pct_Exploitations_Bio_CC = Pct_Exploitations_Bio_CC,
      TotalEA = TotalEA,
      TotalSAU = TotalSAU_ha,
      TotalETP = TotalETP
    )
)
saveRDS(ra2020_df, file.path(out_dir, "ra2020_departements.rds"))

# Simulation spatiale des magasins de producteurs fictifs dans les vrais polygones
prefixes_mag <- c("Halle Paysanne", "Comptoir Fermier", "Panier des Producteurs", "Échoppe Rurale", "Coopérative Locale", "Marché de la Vallée")
pts_list <- list()
for (i in seq_len(nrow(france_sf))) {
  cd <- as.character(france_sf$code[i])
  row_se <- socioeco_df %>% filter(code_dept == cd)
  if (nrow(row_se) == 0) next
  k_pts <- max(3L, min(10L, as.integer(round(row_se$nb_magasins[1]))))
  poly_cur <- france_sf[i, ]
  
  # Échantillonnage spatial avec dispersion optimisée (régulier ou aléatoire)
  sampled_pts <- tryCatch({
    pts_reg <- suppressWarnings(st_sample(poly_cur, size = k_pts, type = "regular"))
    if (length(pts_reg) < k_pts) {
      pts_rnd <- suppressWarnings(st_sample(poly_cur, size = k_pts * 3L, type = "random"))
      if (length(pts_rnd) >= k_pts) pts_rnd[seq_len(k_pts)] else c(pts_reg, pts_rnd)[seq_len(min(k_pts, length(pts_reg) + length(pts_rnd)))]
    } else {
      pts_reg[seq_len(k_pts)]
    }
  }, error = function(e) NULL)
  
  if (is.null(sampled_pts) || length(sampled_pts) < 2) {
    cent <- suppressWarnings(st_centroid(st_geometry(poly_cur)))
    cent_c <- st_coordinates(cent)
    bb <- st_bbox(poly_cur)
    span_x <- max(0.08, (bb$xmax - bb$xmin) * 0.32)
    span_y <- max(0.08, (bb$ymax - bb$ymin) * 0.32)
    angles <- seq(0, 2 * pi, length.out = k_pts + 1)[seq_len(k_pts)]
    coords <- matrix(0, nrow = k_pts, ncol = 2)
    coords[, 1] <- cent_c[1, 1] + span_x * cos(angles) * runif(k_pts, 0.45, 0.95)
    coords[, 2] <- cent_c[1, 2] + span_y * sin(angles) * runif(k_pts, 0.45, 0.95)
  } else {
    coords <- st_coordinates(sampled_pts)
  }
  n_k <- nrow(coords)
  is_closed <- rbinom(n_k, 1, 0.14)
  pts_list[[length(pts_list) + 1L]] <- tibble::tibble(
    id_cfs = sprintf("SIM_MAG_%s_%02d", cd, seq_len(n_k)),
    nom_usuel = paste(sample(prefixes_mag, n_k, replace = TRUE), "-", row_se$nom_dept[1], seq_len(n_k)),
    numero_siret = as.numeric(sprintf("9%02d%05d%06d", i %% 99L, sample(10000L:99999L, n_k, replace = TRUE), sample(100000L:999999L, n_k, replace = TRUE))),
    annee_ouverture = sample(2005:2023, n_k, replace = TRUE),
    annee_fermeture = ifelse(is_closed == 1, sample(2020:2024, n_k, replace = TRUE), NA_real_),
    etat_en_2024 = ifelse(is_closed == 1, "inactif", "actif"),
    adresse = paste(sample(1:95, n_k, replace = TRUE), "Route des Maraîchers"),
    code_postal = as.numeric(paste0(ifelse(nchar(cd) == 2 & !grepl("[AB]", cd), cd, "34"), sprintf("%03d", sample(100:890, n_k, replace = TRUE)))),
    code_insee = paste0(cd, sprintf("%03d", sample(1:250, n_k, replace = TRUE))),
    commune = paste("Commune Simulée", cd, seq_len(n_k)),
    code_departement = cd,
    departement = row_se$nom_dept[1],
    region = "Région Simulée",
    longitude = coords[, 1],
    latitude = coords[, 2],
    precision_geocodage = sample(c("Numéro de rue", "Rue / Route", "Commune", "Lieu-dit"), n_k, replace = TRUE, prob = c(0.55, 0.25, 0.12, 0.08)),
    code_statut_juridique_etablissement = 5499,
    statut_juridique_etablissement = "Société coopérative ou SARL agricole simulée",
    code_naf_etablissement = 4721,
    libelle_naf_etablissement = "Commerce de détail de produits agricoles en magasin spécialisé",
    code_tranche_effectif_etablissement_2022 = sample(c("01", "02", "03"), n_k, replace = TRUE),
    tranche_effectif_etablissement_2022 = sample(c("1 ou 2 salariés", "3 à 5 salariés", "6 à 9 salariés"), n_k, replace = TRUE),
    statut_fermeture = ifelse(is_closed == 1, "Fermé", "Ouvert"),
    a_employe_salaries = sample(c("Oui", "Non"), n_k, replace = TRUE, prob = c(0.75, 0.25)),
    employeur_avant_fermeture = "Oui",
    employeur_actuel = ifelse(is_closed == 1, "Non", "Oui"),
    date_interrogation_api = as.Date("2026-06-01"),
    code_epci = as.numeric(200000000 + i * 100 + seq_len(n_k)),
    nom_epci = paste("Intercommunalité Simulée", row_se$nom_dept[1]),
    population_commune = round(exp(rnorm(n_k, 8.6, 1.1))),
    surface_commune_km2 = round(runif(n_k, 8, 55), 1),
    code_departement.1 = cd,
    code_region = as.numeric((i %% 13) + 1),
    densite_commune = round(exp(rnorm(n_k, 5.2, 1.2)), 1),
    statut_enrichissement = "OK",
    pt_menimp = row_se$pt_menimp[1],
    surfeng = row_se$surfeng[1],
    superf_commune = round(runif(n_k, 800, 5500)),
    emp2064 = row_se$emp2064[1],
    p_diplsup15 = row_se$p_diplsup15[1],
    p_csp_agr = row_se$p_csp_agr[1],
    p_csp_cadpis = row_se$p_csp_cadpis[1],
    tx_act1564 = row_se$tx_act1564[1],
    typo_densite = row_se$typo_densite[1],
    exploitations.conventionnelles.en.circuit.long = as.character(round(row_se$pct_conv_cl_epci[1], 1)),
    exploitations.conventionnelles.en.circuit.court = as.character(round(row_se$pct_conv_cc_epci[1], 1)),
    exploitations.biologiques.en.circuit.long = as.character(round(row_se$pct_bio_cl_epci[1], 1)),
    exploitations.biologiques.en.circuit.court = as.character(round(row_se$pct_bio_cc_epci[1], 1)),
    code_dept = cd,
    ferme = as.integer(is_closed),
    ferme_label = ifelse(is_closed == 1, "Inactif", "Actif")
  )
}
magasins_df <- bind_rows(pts_list)
saveRDS(magasins_df, file.path(out_dir, "magasins_producteurs.rds"))

# ==============================================================================
# 3. SIMULATION DU CORPUS DE TRANSACTIONS MULTI-PLATEFORMES (2021-2026)
# ==============================================================================
cat("[3/8] Génération du corpus synthétique de transactions (Plateforme 1 à 4)...\n")

# Sélection des 61 catégories de produits suivies (hors Vin, Bière, Cidre pour le cœur ou 61 produits principaux)
cats_sim <- foodon_df %>%
  filter(!categorie_standard %in% c("Vin", "Cidre", "Huile")) %>%
  arrange(categorie_standard)

plateformes <- c("Plateforme 1", "Plateforme 2", "Plateforme 3", "Plateforme 4")
plat_effects_true <- c("Plateforme 1" = 0.00, "Plateforme 2" = 0.048, "Plateforme 3" = -0.068, "Plateforme 4" = 0.115)
plat_weights <- c(0.34, 0.24, 0.22, 0.20)

# Calibrations empiriques précises alignées sur les tests statistiques de l'UI (H1 à H11)
sc_vec <- function(x) (x - mean(x)) / sd(x)
rev_s_cal   <- sc_vec(socioeco_df$revenu_median_uc)
cad_s_cal   <- sc_vec(socioeco_df$p_csp_cadpis)
dipl_s_cal  <- sc_vec(socioeco_df$p_diplsup15)
bio_s_cal   <- sc_vec(socioeco_df$Pct_Exploitations_Bio)
gini_s_cal  <- sc_vec(socioeco_df$indice_gini)
dens_f_cal  <- as.factor(socioeco_df$typo_densite)
bio_ortho_cal <- residuals(lm(bio_s_cal ~ rev_s_cal))

# 1. Volume départemental (H6: R=-0.29, H7: R=-0.38, H8: R=+0.22, H9: R=+0.01, p=0.87)
set.seed(13252)
eps_vol <- rnorm(nrow(socioeco_df), 0, 1.0)
lvol <- -0.18 * sc_vec(socioeco_df$taux_pauvrete_60) - 
        0.28 * sc_vec(socioeco_df$part_65plus) + 
        0.14 * sc_vec(socioeco_df$part_moins20) + 
        0.010 * sc_vec(socioeco_df$part_indemnites_chomage) + 
        0.90 * eps_vol
dept_probs <- exp(lvol) / sum(exp(lvol))

# 2. Effet prix départemental (H1: R=+0.35, H2: R=+0.40, H3: R=+0.45, H4: KW p=0.051, H5: R=0.00, H11: R=+0.20, p=0.048)
set.seed(704064)
eps_p <- rnorm(nrow(socioeco_df), 0, 1.0)
lp_raw <- 0.46 * rev_s_cal + 0.10 * cad_s_cal + 0.08 * dipl_s_cal + 0.14 * gini_s_cal + 0.10 * bio_ortho_cal + 0.72 * eps_p
lp_within <- lp_raw - ave(lp_raw, dens_f_cal, FUN = mean)
dens_target <- ifelse(socioeco_df$typo_densite == "1- Urbain dense", 0.35,
               ifelse(socioeco_df$typo_densite == "2- Urbain intermédiaire", 0.0, -0.16))
lp_cal <- lp_within + dens_target
dept_true_eff <- setNames(0.045 * sc_vec(lp_cal), socioeco_df$code_dept)

# 3. Facteur de dispersion par département (H10: Gini vs Dispersion, R=+0.10, p=0.33)
set.seed(103451)
eps_disp <- rnorm(nrow(socioeco_df), 0, 1.0)
ec_target <- 2.5 + 0.10 * gini_s_cal + 0.98 * eps_disp
disp_dept <- setNames(1.0 + 0.08 * sc_vec(ec_target), socioeco_df$code_dept)

# Grille temporelle mensuelle 2021-01 à 2026-09 (69 mois)
months_grid <- seq(as.Date("2021-01-01"), as.Date("2026-09-01"), by = "month")
n_months <- length(months_grid)

# Courbe macroéconomique d'inflation cumulée réaliste (base 0 en 2021-01)
t_idx <- seq_len(n_months)
inflation_curve <- cumsum(c(
  0,
  rep(0.0020, 11),  # 2021 : +2.4%
  rep(0.0072, 12),  # 2022 : poussée inflationniste ~ +9%
  rep(0.0055, 12),  # 2023 : +6.8%
  rep(0.0022, 12),  # 2024 : accalmie ~ +2.7%
  rep(0.0015, 12),  # 2025 : stabilisation ~ +1.8%
  rep(0.0014, 9)    # 2026 : ~ +1.7% rythme annuel
))

# Phase saisonnière selon la saison principale du produit (creux de prix en pleine récolte)
get_season_offset <- function(saison_str, month_num) {
  switch(saison_str,
    "Été"       = -0.14 * cos(2 * pi * (month_num - 8) / 12),
    "Printemps" = -0.13 * cos(2 * pi * (month_num - 5) / 12),
    "Automne"   = -0.10 * cos(2 * pi * (month_num - 10) / 12),
    "Hiver"     = -0.11 * cos(2 * pi * (month_num - 1) / 12),
    -0.03 * cos(2 * pi * (month_num - 7) / 12)
  )
}

# Dictionnaire synthétique de termes métiers pour libellés réalistes
varietes_dict <- list(
  "Pomme de terre" = c("Agata", "Bintje", "Charlotte", "Monalisa", "Agria", "Désirée"),
  "Tomate"         = c("Ancienne", "Cœur de Bœuf", "Cerise", "Roma", "Ronde", "Noire de Crimée"),
  "Carotte"        = c("Nantaise", "Nouvelle", "Mix Couleurs", "Conservation", "Sables"),
  "Salade"         = c("Batavia", "Feuille de chêne", "Laitue", "Mâche", "Roquette", "Scarole"),
  "Porc"           = c("Filet", "Côte", "Échine", "Rôti", "Saucisse", "Poitrine", "Haché"),
  "Bœuf"           = c("Steak", "Bourguignon", "Pot au feu", "Côte", "Rôti", "Haché"),
  "Poulet"         = c("Entier", "Filet/Blanc", "Cuisse", "Aile", "Fermier")
)

tx_list <- vector("list", nrow(cats_sim))

for (ic in seq_len(nrow(cats_sim))) {
  cat_row <- cats_sim[ic, ]
  cat_name <- cat_row$categorie_standard
  base_p <- cat_row$prix_base_sim
  saison_p <- cat_row$saison_principale
  is_panier <- cat_row$panier_quotidien == 1L

  # Nombre de transactions simulées proportionnel à l'importance du produit (total ~ 210 000 lignes)
  n_tx <- if (is_panier) sample(3800:4800, 1) else sample(2000:2800, 1)

  m_idx <- sample(seq_len(n_months), n_tx, replace = TRUE,
                  prob = seq(0.82, 1.18, length.out = n_months))
  dates_tx <- months_grid[m_idx] + sample(0:27, n_tx, replace = TRUE)
  months_num <- as.integer(format(dates_tx, "%m"))

  plat_tx <- sample(plateformes, n_tx, replace = TRUE, prob = plat_weights)
  dept_tx <- sample(socioeco_df$code_dept, n_tx, replace = TRUE, prob = dept_probs)
  bio_tx  <- sample(c("Bio", "Non-Bio"), n_tx, replace = TRUE, prob = c(0.42, 0.58))

  # Conditionnement corrélé aux modes de prix (Gros >3kg, Moyen 1-3kg, Petit <1kg)
  tranche_tx <- sample(
    c("Petit (<1kg)", "Moyen (1-3kg)", "Gros (>3kg)"),
    n_tx, replace = TRUE, prob = c(0.32, 0.48, 0.20)
  )
  poids_kg_tx <- ifelse(
    tranche_tx == "Petit (<1kg)", round(runif(n_tx, 0.25, 0.95), 2),
    ifelse(tranche_tx == "Moyen (1-3kg)", round(runif(n_tx, 1.0, 2.8), 2),
           round(runif(n_tx, 3.2, 8.5), 2))
  )

  # Effets structurels sur le log-prix
  eff_plat  <- plat_effects_true[plat_tx]
  eff_bio   <- ifelse(bio_tx == "Non-Bio", -0.098, 0.0)
  eff_poids <- ifelse(tranche_tx == "Moyen (1-3kg)", -0.21,
               ifelse(tranche_tx == "Gros (>3kg)", -0.34, 0.0))
  eff_dept  <- dept_true_eff[dept_tx]
  eff_sais  <- get_season_offset(saison_p, months_num)
  eff_infl  <- inflation_curve[m_idx]

  # Structure multi-modale réaliste (4 régimes de prix distincts avec leurs propres dispersions) :
  # 1 : Vrac / Gros volume / Déclassé / Anti-gaspi (~22%)
  # 2 : Vente au détail standard / Calibre moyen (~46%)
  # 3 : Primeur / Sélection Haute Qualité / Découpe noble (~26%)
  # 4 : Épicerie Fine / Coffret Prestige / Traiteur artisanal (~6%)
  regime_tx <- sample(1:4, n_tx, replace = TRUE, prob = c(0.22, 0.46, 0.26, 0.06))
  
  regime_shift <- ifelse(regime_tx == 1, -0.44,
                  ifelse(regime_tx == 3, +0.50,
                  ifelse(regime_tx == 4, +0.92, 0.0)))
  
  regime_sigma <- ifelse(regime_tx == 1, 0.16,
                  ifelse(regime_tx == 3, 0.20,
                  ifelse(regime_tx == 4, 0.25, 0.14)))
  
  # Bruit spécifique au régime avec queues lourdes (Student-t df = 3.6)
  noise_tx <- rt(n_tx, df = 3.6) * regime_sigma * disp_dept[dept_tx]

  # ~5.6 % d'anomalies réalistes et erreurs de saisie :
  # - Erreur unité vs kilo (barquette / pièce au lieu du kg, ou inverse)
  # - Facteur 10 ou virgule mal positionnée
  # - Facteur caisse ou colis entier (x4 à x8)
  anom_type <- sample(0:3, n_tx, replace = TRUE, prob = c(0.944, 0.024, 0.018, 0.014))
  anom_shift <- ifelse(anom_type == 1, sample(c(-1.55, +1.45), n_tx, replace = TRUE) + rnorm(n_tx, 0, 0.15),
                ifelse(anom_type == 2, sample(c(-log(10), +log(10)), n_tx, replace = TRUE) + rnorm(n_tx, 0, 0.06),
                ifelse(anom_type == 3, log(runif(n_tx, 4.2, 8.2)), 0.0)))

  log_p <- log(base_p) + eff_plat + eff_bio + eff_poids + eff_dept + eff_sais + eff_infl + regime_shift + noise_tx + anom_shift
  prix_final <- round(pmax(0.12, exp(log_p)), 2)

  # Libellés synthétiques informatifs enrichis selon le profil de prix et le régime
  vars_pool <- if (cat_name %in% names(varietes_dict)) varietes_dict[[cat_name]] else c("Terroir", "Fermier", "Plein Champ", "Sélection", "Récolte Locale")
  var_chosen <- sample(vars_pool, n_tx, replace = TRUE)
  
  regime_desc <- ifelse(regime_tx == 1, sample(c("Vrac Caisse 5kg", "Lot Déclassé Anti-gaspi", "Sac Éco 5kg", "Calibre 2 Gros Format"), n_tx, replace = TRUE),
                 ifelse(regime_tx == 3, sample(c("Sélection Terroir Primeur", "Variété Ancienne d'Exception", "Extra Bouton", "Découpe Fine"), n_tx, replace = TRUE),
                 ifelse(regime_tx == 4, sample(c("Coffret Dégustation", "Spécialité Traiteur Artisanale", "Édition Prestige"), n_tx, replace = TRUE),
                        sample(c("Standard Calibre 1", "Filet 1.5kg", "Sachet Vente Directe", "Barquette Fraîcheur"), n_tx, replace = TRUE))))
  
  bio_mot <- ifelse(bio_tx == "Bio", "Agriculture Biologique", "Production Raisonnée")
  anom_mot <- ifelse(anom_type == 1, "- Prix Pièce Unitaire",
              ifelse(anom_type == 2, "- Anomalie Décimale Saisie",
              ifelse(anom_type == 3, "- Colis Caisse Complète", "")))
  
  libelle_sim <- trimws(paste(cat_name, var_chosen, "-", regime_desc, "-", bio_mot, anom_mot))

  tx_list[[ic]] <- tibble::tibble(
    transaction_hash = sprintf("sim_%02d_%06d", ic, seq_len(n_tx)),
    plateforme = plat_tx,
    categorie_standard = cat_name,
    date = dates_tx,
    annee = format(dates_tx, "%Y"),
    annee_mois = format(dates_tx, "%Y-%m"),
    dept = dept_tx,
    organic = bio_tx,
    tranche_poids = tranche_tx,
    poids_kg = poids_kg_tx,
    prix_final = prix_final,
    eff_sais_true = eff_sais,
    variete_sim = var_chosen,
    libelle_brut = libelle_sim
  )
}

df_tx <- bind_rows(tx_list)
cat(sprintf("   -> %s transactions synthétiques générées sur %d catégories.\n",
            format(nrow(df_tx), big.mark = " "), length(unique(df_tx$categorie_standard))))

# ==============================================================================
# 4. ENTRAÎNEMENT GMM RÉEL (MCLUST) SANS SEUIL ARBITRAIRE & DIAGNOSTICS MOTS-CLÉS
# ==============================================================================
cat("[4/8] Ajustement des mélanges gaussiens (Mclust) sur les données simulées...\n")

gmm_models_list <- list()
gmm_counts_list <- list()
gmm_keywords_list <- list()

df_tx$classe <- 1L
df_tx$nb_classes <- 1L
df_tx$is_outlier <- 0L

stopwords_sim <- c("agriculture", "biologique", "production", "raisonnée", "avec", "dans", "pour")

for (cat_name in sort(unique(df_tx$categorie_standard))) {
  idx <- which(df_tx$categorie_standard == cat_name)
  sub_p <- df_tx$prix_final[idx]
  lp <- log(sub_p)

  # Initialisation adaptative invariante d'échelle basée sur la médiane et le MAD (zéro seuil fixe sur les prix)
  med_lp <- median(lp)
  mad_lp <- mad(lp, constant = 1.4826)
  z_rob  <- if (mad_lp > 0) abs(lp - med_lp) / mad_lp else rep(0, length(lp))
  # Seuil théorique gaussien corrigé de Bonferroni léger / extrême queue empirique
  noise_init <- z_rob > qnorm(1 - 0.015 / 2)

  mod_gmm <- tryCatch(
    Mclust(lp, G = 1:4, modelNames = "E", initialization = list(noise = noise_init), verbose = FALSE),
    error = function(e) NULL
  )

  if (is.null(mod_gmm)) {
    mod_gmm <- Mclust(lp, G = 1:3, modelNames = "E", verbose = FALSE)
  }

  mod_slim <- mod_gmm
  mod_slim$data <- NULL
  mod_slim$z <- NULL
  mod_slim$uncertainty <- NULL
  mod_slim$density <- NULL
  gmm_models_list[[cat_name]] <- mod_slim
  cls_vec <- as.integer(mod_gmm$classification)
  k_val   <- as.integer(mod_gmm$G)
  out_vec <- as.integer(cls_vec == 0L)

  df_tx$classe[idx]     <- cls_vec
  df_tx$nb_classes[idx] <- k_val
  df_tx$is_outlier[idx] <- out_vec

  # Comptages par classe (mise à l'échelle représentative d'observatoire x25 pour affichage réaliste des volumes)
  scale_vol <- 25L
  sub_df <- df_tx[idx, ]
  cnt_df <- sub_df %>%
    group_by(categorie_standard, classe, is_outlier) %>%
    summarise(
      n_ventes = as.numeric(n() * scale_vol),
      prix_moyen = round(mean(prix_final), 2),
      prix_min = round(min(prix_final), 2),
      prix_max = round(max(prix_final), 2),
      .groups = "drop"
    ) %>%
    arrange(classe)
  gmm_counts_list[[cat_name]] <- as.data.frame(cnt_df)

  # Construction de gmm_class_keywords pour l'onglet d'exploration sémantique
  tot_v_cat <- sum(cnt_df$n_ventes)
  cls_cards <- list()
  inlier_means <- cnt_df %>% filter(classe > 0) %>% arrange(prix_moyen)

  for (r_i in seq_len(nrow(cnt_df))) {
    c_id  <- cnt_df$classe[r_i]
    is_o  <- cnt_df$is_outlier[r_i]
    sub_c <- sub_df %>% filter(classe == c_id)
    libs_lower <- tolower(sub_c$libelle_brut)
    words_vec <- unlist(strsplit(gsub("[[:punct:]0-9]", " ", libs_lower), "\\s+"))
    words_vec <- words_vec[nchar(words_vec) >= 4 & !words_vec %in% c(stopwords_sim, tolower(cat_name))]
    tb_w <- sort(table(words_vec), decreasing = TRUE)
    tb_top <- head(tb_w, 6)
    top_df <- if (length(tb_top) > 0) {
      data.frame(
        mot = names(tb_top),
        occurrences = as.numeric(tb_top),
        pct = round(100 * as.numeric(tb_top) / nrow(sub_c), 1),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(mot = character(), occurrences = numeric(), pct = numeric(), stringsAsFactors = FALSE)
    }

    pct_pm <- round(100 * cnt_df$n_ventes[r_i] / tot_v_cat, 1)
    label_met <- if (is_o == 1L) {
      paste(cat_name, "- anomalies de saisie ou lots atypiques")
    } else if (nrow(inlier_means) >= 2 && cnt_df$prix_moyen[r_i] == min(inlier_means$prix_moyen)) {
      paste(cat_name, "- segment vrac & gros conditionnement")
    } else if (nrow(inlier_means) >= 2 && cnt_df$prix_moyen[r_i] == max(inlier_means$prix_moyen)) {
      paste(cat_name, "- segment primeur, découpe ou petit format")
    } else {
      paste(cat_name, "- segment standard détail")
    }

    cls_cards[[as.character(c_id)]] <- list(
      classe = as.integer(c_id),
      is_outlier = as.integer(is_o),
      statut = if (is_o == 1L) "Rejetée (Outlier)" else "Conservée (Inlier)",
      n_ventes = cnt_df$n_ventes[r_i],
      n_echantillon = nrow(sub_c),
      prix_moyen = cnt_df$prix_moyen[r_i],
      pct_faux_amis = if (is_o == 1L) 14.5 else 0.0,
      pct_lots = round(100 * mean(grepl("lot|caisse", libs_lower)), 1),
      pct_part_marche = pct_pm,
      pct_impute = round(runif(1, 6, 22), 1),
      diagnostic = sprintf("%.1f %% des ventes", pct_pm),
      code_diag = if (is_o == 1L) "rejet" else "inlier",
      top_mots = top_df,
      exemples = head(unique(sub_c$libelle_brut), 3),
      label_metier = label_met,
      facteur_discriminant = label_met
    )
  }

  gmm_keywords_list[[cat_name]] <- list(
    categorie = cat_name,
    nb_classes = k_val,
    total_echantillon = nrow(sub_df),
    classes = cls_cards
  )
}

saveRDS(gmm_models_list, file.path(out_dir, "gmm_models.rds"), compress = "xz")
saveRDS(bind_rows(gmm_counts_list) %>% as.data.frame(), file.path(out_dir, "gmm_class_counts.rds"))
saveRDS(gmm_keywords_list, file.path(out_dir, "gmm_class_keywords.rds"))

# Échantillon des classes et pool d'audit
sample_classes_df <- df_tx %>%
  select(plateforme, categorie_standard, prix_final, classe, nb_classes, organic, is_outlier)
saveRDS(sample_classes_df, file.path(out_dir, "sample_classes.rds"))

audit_pool_df <- df_tx %>%
  slice_sample(n = min(2000L, nrow(df_tx))) %>%
  transmute(
    transaction_hash = transaction_hash,
    plateforme = plateforme,
    categorie_standard = categorie_standard,
    prix_final = prix_final,
    poids_kg = poids_kg,
    prix_produit = round(prix_final * poids_kg, 2),
    is_outlier = as.integer(is_outlier),
    dept = dept,
    organic = ifelse(organic == "Bio", 1L, 0L),
    libelle_brut = libelle_brut
  ) %>%
  as.data.frame()
saveRDS(audit_pool_df, file.path(out_dir, "pool_audit_transactions.rds"))

# Entonnoir d'ingestion (funnel_data.rds) cohérent avec les volumes simulés x25
funnel_gmm <- df_tx %>%
  filter(is_outlier == 0L) %>%
  count(plateforme, annee, name = "n") %>%
  mutate(n = as.numeric(n * 25L))
funnel_valid <- df_tx %>%
  count(plateforme, annee, name = "n") %>%
  mutate(n = as.numeric(n * 25L))
funnel_cat <- funnel_valid %>%
  mutate(n = round(n * runif(n(), 1.08, 1.15)))
funnel_raw <- funnel_cat %>%
  mutate(n = round(n * runif(n(), 1.18, 1.30)))

saveRDS(
  list(raw = funnel_raw, categorized = funnel_cat, valid = funnel_valid, gmm = funnel_gmm),
  file.path(out_dir, "funnel_data.rds")
)

# ==============================================================================
# 5. AGRÉGATIONS DÉPARTEMENTALES, SÉRIES TEMPORELLES & DÉCOMPOSITION STL RÉELLE
# ==============================================================================
cat("[5/8] Calcul des agrégats territoriaux, indices d'inflation et STL...\n")

df_clean <- df_tx %>%
  filter(is_outlier == 0L) %>%
  mutate(
    total_ligne = prix_final * poids_kg,
    Plateforme = plateforme,
    Annee = annee
  )

# Construction des 4 combinaisons marginales attendues par app.R (identique à scripts/aggregation_departements.R)
df_with_org <- bind_rows(df_clean, df_clean %>% mutate(organic = "Tous"))
df_with_all <- bind_rows(df_with_org, df_with_org %>% mutate(Annee = "Toutes"))

summarise_cells <- function(grouped_df) {
  grouped_df %>%
    summarise(
      Nombre_Ventes = as.integer(n() * 25L),
      Prix_Moyen_Kg = round(sum(total_ligne) / sum(poids_kg), 3),
      Ecart_Type = round(sqrt(pmax(0.01, sum(poids_kg * (prix_final - sum(total_ligne) / sum(poids_kg))^2) / sum(poids_kg))), 3),
      Prix_Min = round(min(prix_final), 2),
      Prix_Max = round(max(prix_final), 2),
      Total_Revenue = round(sum(total_ligne) * 25, 2),
      Date_Debut = min(date),
      Date_Fin = max(date),
      .groups = "drop"
    )
}

agg_1 <- df_with_all %>% group_by(categorie_standard, dept, Plateforme, Annee, organic) %>% summarise_cells()
agg_2 <- df_with_all %>% group_by(categorie_standard, dept, Annee, organic) %>% summarise_cells() %>% mutate(Plateforme = "Toutes")
agg_3 <- df_with_all %>% group_by(categorie_standard, Plateforme, Annee, organic) %>% summarise_cells() %>% mutate(dept = "National")
agg_4 <- df_with_all %>% group_by(categorie_standard, Annee, organic) %>% summarise_cells() %>% mutate(dept = "National", Plateforme = "Toutes")

resultats_dept_df <- bind_rows(agg_1, agg_2, agg_3, agg_4) %>%
  select(categorie_standard, dept, Plateforme, Annee, organic, Nombre_Ventes,
         Prix_Moyen_Kg, Ecart_Type, Prix_Min, Prix_Max, Total_Revenue, Date_Debut, Date_Fin) %>%
  as.data.frame()
saveRDS(resultats_dept_df, file.path(out_dir, "resultats_departements.rds"), compress = "xz")

# Génération des métriques légères pour affichage instantané de l'Accueil (WebR / Shiny)
total_trans_acc <- resultats_dept_df %>%
  filter(dept == "National", Plateforme == "Toutes", Annee == "Toutes", organic == "Tous") %>%
  summarise(s = sum(Nombre_Ventes, na.rm = TRUE)) %>%
  pull(s)

metro_trans_acc <- resultats_dept_df %>%
  filter(dept != "National", !grepl("^9[78]", dept), Plateforme == "Toutes", Annee == "Toutes", organic == "Tous") %>%
  summarise(s = sum(Nombre_Ventes, na.rm = TRUE)) %>%
  pull(s)

depts_acc <- unique(resultats_dept_df$dept[resultats_dept_df$dept != "National"])
dom_depts_acc <- depts_acc[grepl("^9[78]", depts_acc)]
metro_depts_acc <- setdiff(depts_acc, dom_depts_acc)
nb_metro_acc <- if ("20" %in% metro_depts_acc) length(setdiff(metro_depts_acc, "20")) + 2 else length(metro_depts_acc)
nb_dom_acc <- length(dom_depts_acc)
nb_tot_depts_acc <- if ("20" %in% depts_acc) length(setdiff(depts_acc, "20")) + 2 else length(depts_acc)

df_pie_product_acc <- resultats_dept_df %>%
  filter(dept != "National", Plateforme != "Toutes", Annee == "Toutes", organic == "Tous") %>%
  group_by(categorie_standard, Plateforme) %>%
  summarise(Count = sum(Nombre_Ventes, na.rm = TRUE), .groups = "drop")

gmm_tot_acc <- sum(as.numeric(funnel_gmm$n), na.rm = TRUE)

accueil_data <- list(
  total_transactions = total_trans_acc,
  metro_transactions = metro_trans_acc,
  nb_categories = length(unique(resultats_dept_df$categorie_standard)),
  nb_total_depts = nb_tot_depts_acc,
  nb_metro_depts = nb_metro_acc,
  nb_dom_depts = nb_dom_acc,
  gmm_tot = gmm_tot_acc,
  non_geo = max(0, gmm_tot_acc - total_trans_acc),
  pct_geo = round(total_trans_acc / gmm_tot_acc * 100, 1),
  df_pie_product = df_pie_product_acc
)
saveRDS(accueil_data, file.path(out_dir, "accueil_data.rds"))

# Sérialisation SF RDS rapide des contours départementaux (80x plus rapide que GeoJSON en WebAssembly)
if (file.exists(file.path(out_dir, "departements.geojson"))) {
  sf_dept_map <- sf::st_read(file.path(out_dir, "departements.geojson"), quiet = TRUE)
  saveRDS(sf_dept_map, file.path(out_dir, "departements_sf.rds"), compress = "xz")
}
if (file.exists(file.path(out_dir, "boxes_dom.geojson"))) {
  sf_boxes_dom <- sf::st_read(file.path(out_dir, "boxes_dom.geojson"), quiet = TRUE)
  saveRDS(sf_boxes_dom, file.path(out_dir, "boxes_dom_sf.rds"), compress = "xz")
}

# Séries mensuelles circuits courts
series_mensuelles_df <- df_clean %>%
  group_by(categorie_standard, annee_mois) %>%
  summarise(
    prix_moyen_pondere = round(sum(total_ligne) / sum(poids_kg), 3),
    prix_median = round(median(prix_final), 3),
    ecart_type = round(sd(prix_final), 3),
    nb_transactions = as.integer(n() * 25L),
    volume_kg = round(sum(poids_kg) * 25, 1),
    date = as.Date(paste0(first(annee_mois), "-01")),
    .groups = "drop"
  ) %>%
  arrange(categorie_standard, date)
saveRDS(series_mensuelles_df, file.path(out_dir, "series_mensuelles_cc.rds"))

# ==============================================================================
# PRÉ-CALCUL DES MODÈLES SARIMA (POUR WEBR / SHINYPUBLIC)
# ==============================================================================
cat("   Pré-calcul des prévisions SARIMA (6 mois) pour WebR...\n")
products_sarima <- unique(series_mensuelles_df$categorie_standard)
ls_sarima <- list()
for (p_name in products_sarima) {
  cat_data <- series_mensuelles_df %>% 
    filter(categorie_standard == p_name) %>%
    arrange(date)
  if (nrow(cat_data) < 18) {
    ls_sarima[[p_name]] <- list(status = "insufficient_data")
    next
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
  mod_arima <- tryCatch({
    forecast::auto.arima(ts_prix, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
  }, error = function(e) NULL)
  if (is.null(mod_arima)) {
    ls_sarima[[p_name]] <- list(status = "model_error")
    next
  }
  fc <- forecast::forecast(mod_arima, h = 6)
  n_hist <- length(ts_prix)
  dates_hist <- cat_complete$date
  dates_fc <- seq.Date(max(dates_hist) + months(1), by = "month", length.out = 6)
  df_fc <- data.frame(
    date = c(dates_hist, dates_fc),
    prix = c(as.numeric(ts_prix), as.numeric(fc$mean)),
    type = c(rep("Historique", n_hist), rep("Prévision (SARIMA)", 6)),
    lo80 = c(rep(NA, n_hist), as.numeric(fc$lower[, 1])),
    hi80 = c(rep(NA, n_hist), as.numeric(fc$upper[, 1])),
    lo95 = c(rep(NA, n_hist), as.numeric(fc$lower[, 2])),
    hi95 = c(rep(NA, n_hist), as.numeric(fc$upper[, 2]))
  )
  ord <- forecast::arimaorder(mod_arima)
  model_label <- sprintf("ARIMA(%d,%d,%d)×(%d,%d,%d)[12]", 
                         ord[1], ord[2], ord[3],
                         mod_arima$arma[3], mod_arima$arma[4], mod_arima$arma[7])
  ls_sarima[[p_name]] <- list(status = "ok", df_fc = df_fc, model_label = model_label)
}
saveRDS(ls_sarima, file.path(out_dir, "precomputed_sarima.rds"), compress = "xz")
cat("   -> SARIMA pré-calculé avec succès pour", length(ls_sarima), "catégories.\n")

series_plat_df <- df_clean %>%
  group_by(categorie_standard, plateforme, annee_mois) %>%
  summarise(
    prix_moyen_pondere = round(sum(total_ligne) / sum(poids_kg), 3),
    nb_transactions = as.integer(n() * 25L),
    date = as.Date(paste0(first(annee_mois), "-01")),
    .groups = "drop"
  ) %>%
  arrange(categorie_standard, plateforme, date)
saveRDS(series_plat_df, file.path(out_dir, "series_par_plateforme.rds"))

# Séries mensuelles de référence hors circuits courts (simulées avec décote GMS cohérente)
ref_hors_cc_df <- series_mensuelles_df %>%
  group_by(categorie_standard) %>%
  mutate(
    ratio_gms = runif(1, 0.78, 0.93),
    prix_moyen_kg = round(prix_moyen_pondere * ratio_gms * (1 + rnorm(n(), 0, 0.025)), 2),
    date_mois = annee_mois,
    source = "Référence simulée"
  ) %>%
  ungroup() %>%
  select(categorie_standard, date_mois, prix_moyen_kg, source) %>%
  as.data.frame()
saveRDS(ref_hors_cc_df, file.path(out_dir, "prix_reference_hors_cc.rds"))

# Décomposition STL réelle et synthèse de saisonnalité sur chaque série mensuelle simulée
mois_fr <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
             "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
stl_list <- list()
synth_sais_list <- list()

for (cat_name in unique(series_mensuelles_df$categorie_standard)) {
  sub_m <- series_mensuelles_df %>%
    filter(categorie_standard == cat_name) %>%
    arrange(date)
  if (nrow(sub_m) < 24) next

  y_start <- as.integer(format(min(sub_m$date), "%Y"))
  m_start <- as.integer(format(min(sub_m$date), "%m"))
  ts_p <- ts(sub_m$prix_moyen_pondere, start = c(y_start, m_start), frequency = 12)
  stl_fit <- stl(ts_p, s.window = "periodic", robust = TRUE)
  comp_mat <- stl_fit$time.series

  stl_df_cat <- data.frame(
    seasonal = as.numeric(comp_mat[, "seasonal"]),
    trend = as.numeric(comp_mat[, "trend"]),
    remainder = as.numeric(comp_mat[, "remainder"]),
    date = sub_m$date,
    original = sub_m$prix_moyen_pondere,
    categorie_standard = cat_name,
    stringsAsFactors = FALSE
  )
  stl_list[[cat_name]] <- stl_df_cat

  # Synthèse saisonnière issue directement de la décomposition STL
  s_by_m <- stl_df_cat %>%
    mutate(m = as.integer(format(date, "%m"))) %>%
    group_by(m) %>%
    summarise(s_mean = mean(seasonal), .groups = "drop")
  m_pic   <- s_by_m$m[which.max(s_by_m$s_mean)]
  m_creux <- s_by_m$m[which.min(s_by_m$s_mean)]
  amp_abs <- max(s_by_m$s_mean) - min(s_by_m$s_mean)
  p_mean  <- mean(sub_m$prix_moyen_pondere)
  lm_tr   <- lm(trend ~ seq_along(trend), data = stl_df_cat)

  synth_sais_list[[cat_name]] <- data.frame(
    categorie = cat_name,
    amplitude_saisonniere = round(amp_abs, 3),
    amplitude_relative_pct = round(100 * amp_abs / p_mean, 1),
    prix_moyen_produit = round(p_mean, 2),
    mois_pic = as.integer(m_pic),
    mois_creux = as.integer(m_creux),
    tendance_pente = round(coef(lm_tr)[2] * 12, 4),
    sarima_order = "ARIMA(1,1,1)(1,0,0)[12]",
    aic = round(AIC(lm_tr), 1),
    mois_pic_nom = mois_fr[m_pic],
    mois_creux_nom = mois_fr[m_creux],
    stringsAsFactors = FALSE
  )
}

saveRDS(bind_rows(stl_list) %>% as.data.frame(), file.path(out_dir, "decomposition_stl.rds"))
saveRDS(bind_rows(synth_sais_list) %>% as.data.frame(), file.path(out_dir, "synthese_saisonnalite.rds"))

# ==============================================================================
# 6. MODÈLES HÉDONIQUES NLP SUR LES LIBELLÉS SYNTHÉTIQUES
# ==============================================================================
cat("[6/8] Estimation des effets NLP sur les caractéristiques simulées...\n")

nlp_specs <- list(
  "Pomme de terre" = c("varieteCharlotte" = 0.16, "varieteRatte" = 0.28, "varieteAgria" = 0.07, "varieteMonalisa" = -0.04, "varieteBintje" = -0.09, "conditionnementVrac" = -0.14, "conditionnementFilet" = -0.08),
  "Porc"           = c("morceauFilet" = 0.31, "morceauCôte" = 0.12, "morceauRôti" = 0.08, "morceauSaucisse" = -0.05, "morceauÉchine" = -0.09, "morceauPoitrine" = -0.18),
  "Bœuf"           = c("morceauCôte" = 0.38, "morceauSteak" = 0.19, "morceauRôti" = 0.14, "morceauHaché" = -0.08, "morceauBourguignon" = -0.19, "morceauPot au feu" = -0.26),
  "Volaille"       = c("morceauFilet/Blanc" = 0.34, "morceauCuisse" = 0.06, "morceauEntier" = -0.12, "morceauAile" = -0.22),
  "Tomate"         = c("varieteCerise" = 0.42, "varieteAncienne" = 0.24, "varieteNoire de Crimée" = 0.21, "varieteCœur de Bœuf" = 0.18, "varieteRoma" = -0.11, "varieteRonde" = -0.14),
  "Carotte"        = c("varieteNouvelle" = 0.22, "conditionnementBotte" = 0.18, "varieteMix Couleurs" = 0.14, "varieteNantaise" = -0.04, "conditionnementVrac" = -0.15),
  "Salade"         = c("varieteRoquette" = 0.35, "varieteMâche" = 0.29, "varieteFeuille de chêne" = 0.06, "varieteBatavia" = -0.05, "varieteScarole" = -0.09)
)

nlp_rows <- list()
for (cat_nlp in names(nlp_specs)) {
  terms_v <- nlp_specs[[cat_nlp]]
  n_sim_nlp <- 900L
  x_mat <- sapply(names(terms_v), function(nm) rbinom(n_sim_nlp, 1, 0.18))
  y_sim <- 1.5 + as.numeric(x_mat %*% unname(terms_v)) + rnorm(n_sim_nlp, 0, 0.22)
  df_lm <- as.data.frame(x_mat)
  colnames(df_lm) <- names(terms_v)
  df_lm$y <- y_sim
  fit_nlp <- lm(y ~ ., data = df_lm)
  sm <- summary(fit_nlp)$coefficients
  sm <- sm[rownames(sm) != "(Intercept)", , drop = FALSE]
  clean_rn <- gsub("^`|`$", "", rownames(sm))
  nlp_rows[[cat_nlp]] <- data.frame(
    term = clean_rn,
    estimate = round(sm[, "Estimate"], 4),
    std.error = round(sm[, "Std. Error"], 4),
    p.value = sm[, "Pr(>|t|)"],
    categorie = cat_nlp,
    stringsAsFactors = FALSE
  )
}
coefs_nlp_df <- bind_rows(nlp_rows)
rownames(coefs_nlp_df) <- NULL
saveRDS(coefs_nlp_df, file.path(out_dir, "coefs_nlp.rds"))

# ==============================================================================
# 7. MODÈLE LINÉAIRE MIXTE (LME4) & LOI DE STUDENT SUR LES DONNÉES SIMULÉES
# ==============================================================================
cat("[7/8] Ajustement du Modèle Linéaire Mixte (lme4) et de la loi de Student...\n")

# Agrégation désaisonnalisée par cellule (dept x Plateforme x categorie x organic x tranche_poids)
df_agg_sim <- df_clean %>%
  mutate(
    log_p_desais = log(prix_final) - eff_sais_true,
    Plateforme = factor(Plateforme, levels = c("Plateforme 1", "Plateforme 2", "Plateforme 3", "Plateforme 4")),
    organic = factor(organic, levels = c("Bio", "Non-Bio")),
    tranche_poids = factor(tranche_poids, levels = c("Petit (<1kg)", "Moyen (1-3kg)", "Gros (>3kg)"))
  ) %>%
  group_by(dept, Plateforme, categorie_standard, organic, tranche_poids) %>%
  summarise(
    Nombre_Ventes = as.integer(n() * 25L),
    log_prix = mean(log_p_desais),
    mean_log_poids = mean(log(poids_kg)),
    poids = as.integer(n() * 25L),
    .groups = "drop"
  ) %>%
  as.data.table()

saveRDS(df_agg_sim, file.path(out_dir, "df_agg_corrige.rds"))

df_mm <- as.data.frame(df_agg_sim)
df_mm$dept <- as.factor(df_mm$dept)
df_mm$categorie_standard <- as.factor(df_mm$categorie_standard)

mod_1 <- lmer(log_prix ~ Plateforme + categorie_standard + (1 | dept), data = df_mm, weights = poids, REML = FALSE)
mod_2 <- lmer(log_prix ~ Plateforme + categorie_standard + organic + (1 | dept), data = df_mm, weights = poids, REML = FALSE)
mod_3 <- lmer(log_prix ~ Plateforme + categorie_standard + organic + tranche_poids + (1 | dept), data = df_mm, weights = poids, REML = FALSE)
mod_4 <- lmer(log_prix ~ Plateforme + categorie_standard + organic + mean_log_poids + (1 | dept), data = df_mm, weights = poids, REML = FALSE)

aic_tab <- AIC(mod_1, mod_2, mod_3, mod_4)
bic_tab <- BIC(mod_1, mod_2, mod_3, mod_4)

df_comp_mm <- data.frame(
  Specification = c(
    "1. Sans Bio ni Conditionnement",
    "2. + Label Biologique",
    "3. + Bio + Tranches de Conditionnement (Discret)",
    "4. + Bio + Log(Poids) moyen (Continu)"
  ),
  Variables_Incluses = c(
    "Plateforme + Catégorie",
    "Plateforme + Catégorie + Bio",
    "Plateforme + Catégorie + Bio + Tranche Poids",
    "Plateforme + Catégorie + Bio + Log(Poids)"
  ),
  df = aic_tab$df,
  AIC = round(aic_tab$AIC, 1),
  BIC = round(bic_tab$BIC, 1),
  Delta_AIC = round(aic_tab$AIC - min(aic_tab$AIC), 1),
  stringsAsFactors = FALSE
)
saveRDS(df_comp_mm, file.path(out_dir, "comparaison_modeles_aic_bic.rds"))

mod_mixte <- lmer(
  log_prix ~ Plateforme + categorie_standard + organic + tranche_poids + (1 | dept),
  data = df_mm,
  weights = poids,
  REML = TRUE
)
saveRDS(mod_mixte, file.path(out_dir, "modele_mixte_lmer.rds"))

vc <- as.data.frame(VarCorr(mod_mixte))
sigma2_dept  <- vc$vcov[vc$grp == "dept"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]
icc_val      <- sigma2_dept / (sigma2_dept + sigma2_resid)

variance_export <- list(
  sigma2_dept = sigma2_dept,
  sigma2_resid = sigma2_resid,
  icc = icc_val,
  n_obs = as.integer(nrow(df_mm)),
  n_dept = as.integer(length(unique(df_mm$dept))),
  n_plat = as.integer(length(unique(df_mm$Plateforme))),
  n_cat = as.integer(length(unique(df_mm$categorie_standard))),
  total_ventes = as.integer(sum(df_mm$Nombre_Ventes))
)
saveRDS(variance_export, file.path(out_dir, "variance_components.rds"))

fe <- fixef(mod_mixte)
fe_export <- data.frame(
  variable = names(fe),
  coefficient = unname(fe),
  pct_effet = (exp(unname(fe)) - 1) * 100,
  stringsAsFactors = FALSE
)
saveRDS(fe_export, file.path(out_dir, "effets_fixes.rds"))

# Qualification adaptative des BLUPs départementaux via erreurs-types conditionnelles (sans seuil arbitraire)
re_obj <- ranef(mod_mixte, condVar = TRUE)$dept
pv_arr <- attr(re_obj, "postVar")
se_blup <- sqrt(pv_arr[1, 1, ])

re_export <- data.frame(
  effet_dept = re_obj[, 1],
  dept = rownames(re_obj),
  se_blup = se_blup,
  stringsAsFactors = FALSE
) %>%
  mutate(
    z_stat = effet_dept / pmax(se_blup, 1e-6),
    pct_effet = (exp(effet_dept) - 1) * 100,
    prix_relatif = sprintf("%+.1f%%", pct_effet),
    interpretation = case_when(
      z_stat >  1.96 ~ "Significativement plus cher",
      z_stat >  0.85 ~ "Légèrement plus cher",
      z_stat >= -0.85 ~ "Dans la moyenne nationale",
      z_stat >= -1.96 ~ "Légèrement moins cher",
      TRUE ~ "Significativement moins cher"
    )
  ) %>%
  select(effet_dept, dept, pct_effet, prix_relatif, interpretation) %>%
  arrange(desc(effet_dept))
saveRDS(re_export, file.path(out_dir, "effets_departementaux.rds"))

df_ecarts <- df_mm %>%
  group_by(categorie_standard, Plateforme) %>%
  summarise(
    prix_moyen_log = weighted.mean(log_prix, poids, na.rm = TRUE),
    n_ventes = as.integer(sum(Nombre_Ventes)),
    .groups = "drop"
  ) %>%
  group_by(categorie_standard) %>%
  mutate(
    prix_ref = mean(prix_moyen_log),
    ecart_pct = (exp(prix_moyen_log - prix_ref) - 1) * 100
  ) %>%
  ungroup()
saveRDS(df_ecarts, file.path(out_dir, "ecarts_plateforme_categorie.rds"))

df_heatmap <- df_ecarts %>%
  mutate(plateforme_label = as.character(Plateforme))
saveRDS(df_heatmap, file.path(out_dir, "heatmap_data.rds"))

# Ajustement par maximum de vraisemblance de la loi de Student sur les résidus simulés
res_gauss <- residuals(mod_mixte)
fit_gauss <- fitted(mod_mixte)
t_fit <- tryCatch(
  suppressWarnings(MASS::fitdistr(res_gauss, densfun = "t", start = list(m = mean(res_gauss), s = sd(res_gauss) * 0.75, df = 5))),
  error = function(e) list(estimate = c(m = mean(res_gauss), s = sd(res_gauss) * 0.8, df = 4.8), loglik = sum(dt(res_gauss / sd(res_gauss), df = 4.8, log = TRUE)))
)
m_hat  <- unname(t_fit$estimate["m"])
s_hat  <- unname(t_fit$estimate["s"])
nu_hat <- unname(t_fit$estimate["df"])
res_stud_std <- (res_gauss - m_hat) / s_hat

rg <- as.numeric(res_gauss)
rg <- rg[is.finite(rg)]
nu_v <- as.numeric(nu_hat)
s_v  <- as.numeric(s_hat)
m_v  <- as.numeric(m_hat)
mu_n <- mean(rg)
sd_n <- sd(rg)
ll_n <- sum(dnorm(rg, mean = mu_n, sd = sd_n, log = TRUE))
ll_t <- sum(dt((rg - m_v) / s_v, df = nu_v, log = TRUE) - log(s_v))
aic_n <- 4 - 2 * ll_n
aic_t <- 6 - 2 * ll_t
fmt_fr <- function(x, d = 1) gsub(".", ",", fixed = TRUE, format(round(x, d), big.mark = " ", nsmall = d, trim = TRUE))
precomputed_obj <- list(
  n_res_str = format(length(rg), big.mark = " ", trim = TRUE),
  nu_str = fmt_fr(nu_v, 2),
  delta_aic_stud_str = fmt_fr(max(0, aic_n - aic_t), 1),
  norm_params_str = sprintf("μ = %s ; σ = %s", fmt_fr(mu_n, 3), fmt_fr(sd_n, 3)),
  norm_ll_str = fmt_fr(ll_n, 1),
  norm_aic_str = fmt_fr(aic_n, 1),
  stud_params_str = sprintf("μ = %s ; σ = %s (s = %s, ν = %s)", fmt_fr(m_v, 3), fmt_fr(sd_n, 3), fmt_fr(s_v, 3), fmt_fr(nu_v, 2)),
  stud_ll_str = fmt_fr(ll_t, 1),
  stud_aic_str = fmt_fr(aic_t, 1)
)

diag_student <- list(
  precomputed = precomputed_obj,
  nu = c(df = nu_hat),
  scale = c(s = s_hat),
  location = c(m = m_hat)
)
saveRDS(diag_student, file.path(out_dir, "diagnostics_student_gauss.rds"), compress = "xz")

# ==============================================================================
# 8. GÉNÉRATION DE TOUS LES GRAPHIQUES PNG EN FONCTION DES DONNÉES SIMULÉES
# ==============================================================================
cat("[8/8] Génération des graphiques PNG dans data_simulees/www/...\n")

theme_sim <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#2C3E50"),
    plot.subtitle = element_text(size = 9.5, color = "#64748B"),
    panel.grid.minor = element_blank()
  )

# 8.1 Effets plateformes (Plateforme 1 à 4)
plat_fe <- fe[grep("^Plateforme", names(fe))]
df_plat_plot <- data.frame(
  plateforme = c("Plateforme 1", sub("^Plateforme", "", names(plat_fe))),
  coef = c(0, unname(plat_fe)),
  stringsAsFactors = FALSE
) %>%
  mutate(pct = (exp(coef) - 1) * 100)

p_plat <- ggplot(df_plat_plot, aes(x = reorder(plateforme, pct), y = pct, fill = pct)) +
  geom_col(width = 0.58, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "#2C3E50", linewidth = 0.45) +
  geom_text(aes(label = sprintf("%+.1f%%", pct)),
            hjust = ifelse(df_plat_plot$pct >= 0, -0.15, 1.15),
            size = 4, fontface = "bold", color = "#2C3E50") +
  scale_fill_gradient2(low = "#2ECC71", mid = "#F1C40F", high = "#E74C3C", midpoint = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.18, 0.18))) +
  coord_flip() +
  labs(
    title = "Effet propre de la plateforme sur les prix simulés",
    subtitle = "Modèle mixte contrôlé pour la catégorie, le bio, le conditionnement et le département | Référence : Plateforme 1",
    x = NULL,
    y = "Écart de prix relatif (%)"
  ) +
  theme_sim
ggsave(file.path(out_www, "effets_plateforme.png"), p_plat, width = 8.5, height = 4.2, dpi = 150, bg = "white")

# 8.2 Heatmap Plateforme x Catégorie
p_hm <- ggplot(df_heatmap, aes(x = plateforme_label, y = reorder(categorie_standard, ecart_pct, FUN = mean), fill = ecart_pct)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%+.0f%%", ecart_pct)), size = 2.6, color = "#1E293B") +
  scale_fill_gradient2(
    low = "#2ECC71", mid = "#FFFFFF", high = "#E74C3C", midpoint = 0,
    limits = c(-35, 35), oob = scales::squish,
    name = "Écart au prix\nmoyen (%)"
  ) +
  labs(
    title = "Écart de prix par catégorie et plateforme sur les données simulées",
    subtitle = "Écart relatif (%) par rapport à la moyenne toutes plateformes confondues",
    x = "Plateforme anonymisée",
    y = "Catégorie de produit"
  ) +
  theme_sim +
  theme(axis.text.y = element_text(size = 7.2), panel.grid = element_blank())
ggsave(file.path(out_www, "heatmap_plateforme_categorie.png"), p_hm, width = 9, height = 11, dpi = 150, bg = "white")

# 8.3 Caterpillar plot des effets aléatoires départementaux
re_cat_df <- data.frame(
  dept = rownames(re_obj),
  effet = re_obj[, 1],
  se = se_blup,
  stringsAsFactors = FALSE
) %>%
  mutate(lower = effet - 1.96 * se, upper = effet + 1.96 * se) %>%
  arrange(effet)
re_cat_df$dept <- factor(re_cat_df$dept, levels = re_cat_df$dept)

p_cat <- ggplot(re_cat_df, aes(x = dept, y = effet)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#7F8C8D", linewidth = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3, color = "#CBD5E1") +
  geom_point(aes(color = effet), size = 2.2) +
  scale_color_gradient2(low = "#2ECC71", mid = "#F39C12", high = "#E74C3C", midpoint = 0, name = "BLUP\n(log-prix)") +
  coord_flip() +
  labs(
    title = "Effets aléatoires départementaux sur les log-prix simulés",
    subtitle = "BLUPs estimés ± intervalle de confiance à 95 % | Modèle mixte REML",
    x = "Département",
    y = "Effet aléatoire territorial (échelle logarithmique)"
  ) +
  theme_sim +
  theme(axis.text.y = element_text(size = 6.2))
ggsave(file.path(out_www, "caterpillar_departements.png"), p_cat, width = 9.5, height = 13.5, dpi = 150, bg = "white")

# 8.4 Diagnostics Normal vs Student-t sur les résidus simulés
set.seed(123)
idx_sub <- sample(seq_along(res_gauss), min(8000L, length(res_gauss)))
res_sub <- sort(res_gauss[idx_sub])
std_sub <- sort(res_stud_std[idx_sub])
n_s     <- length(res_sub)
prob_q  <- ppoints(n_s)
theo_norm <- qnorm(prob_q, mean = mean(res_gauss), sd = sd(res_gauss))
theo_stud <- qt(prob_q, df = nu_hat)

p_qq_norm <- ggplot(data.frame(theo = qnorm(prob_q), obs = (res_sub - mean(res_gauss)) / sd(res_gauss)),
                    aes(x = theo, y = obs)) +
  geom_point(color = "#2980B9", alpha = 0.55, size = 1.2) +
  geom_abline(slope = 1, intercept = 0, color = "#E74C3C", linewidth = 0.9) +
  labs(
    title = "Q-Q Plot sous hypothèse Normale",
    subtitle = "Déviation en S caractéristique de queues lourdes sur les résidus simulés",
    x = "Quantiles théoriques N(0,1)",
    y = "Quantiles observés standardisés"
  ) +
  theme_sim
ggsave(file.path(out_www, "qq_plot_residus.png"), p_qq_norm, width = 7, height = 5.8, dpi = 150, bg = "white")

p_qq_stud <- ggplot(data.frame(theo = theo_stud, obs = std_sub), aes(x = theo, y = obs)) +
  geom_point(color = "#16A34A", alpha = 0.55, size = 1.2) +
  geom_abline(slope = 1, intercept = 0, color = "#E74C3C", linetype = "dashed", linewidth = 0.9) +
  labs(
    title = sprintf("Q-Q Plot sous loi de Student - nu estimé = %.2f", nu_hat),
    subtitle = "Alignement linéaire sur la première bissectrice après prise en compte de l'épaisseur des queues",
    x = sprintf("Quantiles théoriques Student-t, nu = %.2f", nu_hat),
    y = "Quantiles observés standardisés"
  ) +
  theme_sim
ggsave(file.path(out_www, "qq_plot_student.png"), p_qq_stud, width = 7, height = 5.8, dpi = 150, bg = "white")

# Densités comparées Normal vs Student-t avec vrais critères AIC calculés sur l'échantillon
ll_norm  <- sum(dnorm(res_gauss, mean = mean(res_gauss), sd = sd(res_gauss), log = TRUE))
ll_stud  <- sum(dt((res_gauss - m_hat) / s_hat, df = nu_hat, log = TRUE) - log(s_hat))
aic_norm <- round(2 * 2 - 2 * ll_norm)
aic_stud <- round(2 * 3 - 2 * ll_stud)

x_grid <- seq(quantile(res_gauss, 0.002), quantile(res_gauss, 0.998), length.out = 400)
dens_df <- bind_rows(
  data.frame(
    x = x_grid,
    y = dt((x_grid - m_hat) / s_hat, df = nu_hat) / s_hat,
    Loi = sprintf("Loi de Student-t : AIC = %s, nu = %.2f", format(aic_stud, big.mark = " "), nu_hat)
  ),
  data.frame(
    x = x_grid,
    y = dnorm(x_grid, mean = mean(res_gauss), sd = sd(res_gauss)),
    Loi = sprintf("Loi Normale : AIC = %s", format(aic_norm, big.mark = " "))
  )
)

p_dens <- ggplot(data.frame(r = res_gauss[res_gauss >= min(x_grid) & res_gauss <= max(x_grid)]), aes(x = r)) +
  geom_histogram(aes(y = after_stat(density)), bins = 75, fill = "#CBD5E1", color = "white", alpha = 0.7) +
  geom_line(data = dens_df, aes(x = x, y = y, color = Loi, linetype = Loi), linewidth = 1.05) +
  scale_color_manual(values = c("#16A34A", "#E74C3C")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Ajustement des densités théoriques sur l'histogramme des résidus simulés",
    subtitle = "Comparaison par maximum de vraisemblance entre loi Normale et loi de Student",
    x = "Résidus du modèle mixte",
    y = "Densité"
  ) +
  theme_sim +
  theme(legend.position = "bottom")
ggsave(file.path(out_www, "comparaison_densite_residus.png"), p_dens, width = 8.5, height = 5.2, dpi = 150, bg = "white")

p_combined <- (p_qq_norm | p_qq_stud) / p_dens + plot_layout(heights = c(1.05, 0.95))
ggsave(file.path(out_www, "comparaison_normale_student.png"), p_combined, width = 12, height = 9.5, dpi = 150, bg = "white")

# Résidus vs valeurs ajustées (Normal et Student standardisés)
df_rvf <- data.frame(
  fitted = fit_gauss[idx_sub],
  res_n  = (res_gauss[idx_sub] - mean(res_gauss)) / sd(res_gauss),
  res_t  = res_stud_std[idx_sub]
)

p_rvf_norm <- ggplot(df_rvf, aes(x = fitted, y = res_n)) +
  geom_point(alpha = 0.22, color = "#2C3E50", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
  geom_smooth(method = "loess", formula = y ~ x, color = "#2980B9", se = FALSE, linewidth = 0.85) +
  labs(
    title = "Résidus standardisés vs Valeurs ajustées - Échelle Normale",
    subtitle = "Vérification de la linéarité et de l'homoscédasticité sur les cellules simulées",
    x = "Valeurs ajustées (log-prix)",
    y = "Résidus standardisés"
  ) +
  theme_sim
ggsave(file.path(out_www, "residus_vs_fitted.png"), p_rvf_norm, width = 8, height = 5.5, dpi = 150, bg = "white")
ggsave(file.path(out_www, "residus_vs_fitted_normale.png"), p_rvf_norm, width = 8, height = 5.5, dpi = 150, bg = "white")

p_rvf_stud <- ggplot(df_rvf, aes(x = fitted, y = res_t)) +
  geom_point(alpha = 0.22, color = "#15803D", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
  geom_smooth(method = "loess", formula = y ~ x, color = "#16A34A", se = FALSE, linewidth = 0.85) +
  labs(
    title = sprintf("Résidus réduits Student vs Valeurs ajustées - nu = %.2f", nu_hat),
    subtitle = "Stabilisation de la dispersion sous pondération robuste Student-t",
    x = "Valeurs ajustées (log-prix)",
    y = "Résidus réduits (échelle Student)"
  ) +
  theme_sim
ggsave(file.path(out_www, "residus_vs_fitted_student.png"), p_rvf_stud, width = 8, height = 5.5, dpi = 150, bg = "white")

# 8.5 Figure globale NLP impact_varietes.png
p_nlp_all <- coefs_nlp_df %>%
  mutate(
    clean_term = gsub("^(variete|morceau|conditionnement)", "", term),
    pct = (exp(estimate) - 1) * 100
  ) %>%
  ggplot(aes(x = reorder(clean_term, pct), y = pct, fill = pct >= 0)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "#334155", linewidth = 0.4) +
  coord_flip() +
  facet_wrap(~ categorie, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#2ECC71")) +
  labs(
    title = "Primes et décotes hédoniques estimées par variété, morceau et conditionnement",
    subtitle = "Régressions log-linéaires sur les libellés synthétiques",
    x = NULL,
    y = "Impact relatif sur le prix (%)"
  ) +
  theme_sim
ggsave(file.path(out_www, "impact_varietes.png"), p_nlp_all, width = 11, height = 9, dpi = 150, bg = "white")

cat("\n======================================================================\n")
cat("   SIMULATION TERMINÉE AVEC SUCCÈS !\n")
cat(sprintf("   - Fichiers RDS/GeoJSON générés dans : %s/\n", out_dir))
cat(sprintf("   - Graphiques PNG générés dans       : %s/\n", out_www))
cat("======================================================================\n")
