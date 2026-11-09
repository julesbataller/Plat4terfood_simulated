suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

cat("=== GENERATION DU GEOJSON COMPLET AVEC DOM-TOM (INSERTS A GAUCHE) ===\n")

d_all <- st_read("scratch/departements_avec_outre_mer.geojson", quiet = TRUE)
metro <- d_all %>% filter(!code %in% c("971", "972", "973", "974", "976"))

place_dom <- function(geom, target_cx, target_cy, target_width, target_height) {
  bbox <- st_bbox(geom)
  orig_cx <- (bbox["xmin"] + bbox["xmax"]) / 2
  orig_cy <- (bbox["ymin"] + bbox["ymax"]) / 2
  orig_w <- bbox["xmax"] - bbox["xmin"]
  orig_h <- bbox["ymax"] - bbox["ymin"]
  
  scale_factor <- min(target_width / orig_w, target_height / orig_h)
  geom_shifted <- (geom - c(orig_cx, orig_cy)) * scale_factor + c(target_cx, target_cy)
  st_set_crs(geom_shifted, st_crs(geom))
}

dom_configs <- list(
  "971" = list(name = "Guadeloupe", cx = -7.5, cy = 49.0, w = 1.7, h = 1.4),
  "972" = list(name = "Martinique", cx = -7.5, cy = 47.3, w = 1.6, h = 1.4),
  "973" = list(name = "Guyane",     cx = -7.5, cy = 45.1, w = 2.1, h = 2.1),
  "974" = list(name = "La Réunion", cx = -5.0, cy = 45.3, w = 1.7, h = 1.5),
  "976" = list(name = "Mayotte",    cx = -5.0, cy = 43.6, w = 1.5, h = 1.3)
)

dom_list <- list()
box_list <- list()

for (code_dom in names(dom_configs)) {
  cfg <- dom_configs[[code_dom]]
  dom_feat <- d_all %>% filter(code == code_dom)
  if (nrow(dom_feat) > 0) {
    new_geom <- place_dom(st_geometry(dom_feat), cfg$cx, cfg$cy, cfg$w * 0.82, cfg$h * 0.82)
    dom_placed <- dom_feat
    st_geometry(dom_placed) <- new_geom
    dom_list[[code_dom]] <- dom_placed
    
    # Cadre de délimitation
    box_poly <- st_polygon(list(matrix(c(
      cfg$cx - cfg$w/2, cfg$cy - cfg$h/2,
      cfg$cx + cfg$w/2, cfg$cy - cfg$h/2,
      cfg$cx + cfg$w/2, cfg$cy + cfg$h/2,
      cfg$cx - cfg$w/2, cfg$cy + cfg$h/2,
      cfg$cx - cfg$w/2, cfg$cy - cfg$h/2
    ), ncol = 2, byrow = TRUE)))
    
    box_sf <- st_sf(
      code = code_dom,
      nom = cfg$name,
      geometry = st_sfc(box_poly, crs = st_crs(d_all))
    )
    box_list[[code_dom]] <- box_sf
  }
}

dom_all_placed <- bind_rows(dom_list)
boxes_all <- bind_rows(box_list)
france_complete <- bind_rows(metro, dom_all_placed)

cat(sprintf("-> Total départements : %d (dont %d DOM-TOM placés à gauche)\n", 
            nrow(france_complete), nrow(dom_all_placed)))

# Sauvegardes
paths_france <- c("data/departements.geojson", "app_visu/data/departements.geojson")
paths_boxes <- c("data/boxes_dom.geojson", "app_visu/data/boxes_dom.geojson")

for (p in paths_france) {
  st_write(france_complete, p, delete_dsn = TRUE, quiet = TRUE)
  cat(sprintf("[✓] Écrit : %s\n", p))
}

for (p in paths_boxes) {
  st_write(boxes_all, p, delete_dsn = TRUE, quiet = TRUE)
  cat(sprintf("[✓] Écrit : %s\n", p))
}

cat("=== TERMINÉ AVEC SUCCÈS ===\n")
