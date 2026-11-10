# Priscope — Observatoire et Modélisation Économétrique des Prix

Application R Shiny d'exploration spatiale, de filtrage probabiliste et de modélisation économétrique des distributions et dynamiques de prix.

Démonstrateur autonome développé par Jules Bataller-Beltran dédié aux méthodes quantitatives et à la modélisation statistique de la dispersion des prix multi-canaux.

## Démo interactive en ligne via GitHub Pages et WebR

Le projet dispose d'un double mode d'accès déployé automatiquement par GitHub Actions :

- **Page vitrine instantanée (< 1 s)** : `https://julesbataller.github.io/Priscope/`
  Synthèse ultra-rapide des résultats économétriques majeurs, carte spatiale interactive autonome de l'Indice de Prix Territorial (IPT), comparatifs Student-t et classements complets pré-calculés, sans aucun temps de chargement initial WebAssembly.
- **Console interactive Shinylive intégrale** : `https://julesbataller.github.io/Priscope/app/`
  Application Shiny complète exécutée directement dans le navigateur via **WebR** pour explorer dynamiquement les 61 produits alimentaires, ajuster les filtres et consulter l'intégralité des 8 modules thématiques.

> Note sur la console WebR : GitHub Pages étant un hébergeur statique sans serveur R, la console s'exécute directement dans votre navigateur grâce à **WebR**. Le premier démarrage de la console télécharge le moteur R WebAssembly et les bibliothèques cartographiques `sf`, `ragg` et `ggiraph`.

## Avertissement sur les données et cadre d'utilisation

Afin de garantir une stricte confidentialité et une totale indépendance :

- **Aucune donnée réelle n'est incluse dans ce dépôt public** : l'ensemble des fichiers présents dans `data_simulees/` ainsi que les figures diagnostiques dans `data_simulees/www/` sont **intégralement générés par simulation Monte Carlo** via le script `scripts/simuler_donnees_github.R`.
- Les canaux de commercialisation sont anonymisés sous les intitulés neutres `Plateforme 1` à `Plateforme 4`.
- Seuls les fonds cartographiques départementaux publics issus de l'IGN et de l'INSEE (`departements.geojson`, `boxes_dom.geojson`) et le référentiel ontologique ouvert **FoodOn** sont utilisés.
- Ce dépôt constitue un projet personnel à vocation de portfolio technique et d'illustration méthodologique. Il est totalement indépendant et n'engage aucune organisation tierce ni aucun partenaire commercial.

## Architecture méthodologique et statistique

Le pipeline analytique et le générateur de données simulées reproduisent fidèlement la chaîne de traitement de l'observatoire :

1. **Standardisation sémantique et ontologique** : rattachement des libellés bruts à 61 catégories de référence assorties de métadonnées de conditionnement.
2. **Filtrage probabiliste des anomalies par Modèles de Mélange Gaussien** : estimation sans seuil arbitraire sur le logarithme des prix via `mclust` avec composante de bruit uniforme pour isoler les erreurs de saisie et conditionnements atypiques, complétée par un diagnostic sémantique par classe.
3. **Indices d'inflation et séries temporelles** : calcul d'indices agrégés de Laspeyres, Paasche et Fisher, décomposition saisonnière robuste **STL** et prévisions **SARIMA** à 6 mois.
4. **Modélisation économétrique multiniveau** : estimation par maximum de vraisemblance restreint **REML** sous `lme4` des effets propres des canaux de distribution, des caractéristiques de certification, des économies d'échelle liées au conditionnement et des **Indices de Prix Territoriaux** départementaux via les prédicteurs empiriques bayésiens BLUP, complétée par un ajustement de **loi de Student-t** sur les résidus à queues lourdes.
5. **Régression hédonique sur caractéristiques textuelles NLP** : quantification des primes et décotes associées aux attributs textuels, sous-variétés et modes de conditionnement.

## Installation et exécution locale sous R

Pour une exécution instantanée côté serveur local :

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "DT", "dplyr", "tidyr", "stringr", "lubridate",
  "sf", "ggplot2", "ggrepel", "ggiraph", "patchwork", "forecast",
  "shinycssloaders", "ragg", "lme4", "mclust", "MASS", "data.table"
))
```

### Régénérer les données simulées et les graphiques

```bash
Rscript scripts/simuler_donnees_github.R
```

### Lancer l'application Shiny

```r
shiny::runApp("app.R")
```

## Structure principale du projet

- `app.R` : interface utilisateur `bslib` et serveur Shiny complet à 8 onglets thématiques.
- `.github/workflows/deploy-shinylive.yml` : déploiement GitHub Pages automatisant la génération de la vitrine statique d'accueil et la compilation Shinylive vers la branche `gh-pages`.
- `scripts/generer_vitrine.R` : générateur de la page vitrine d'accueil ultra-rapide pré-calculée et de la carte vectorielle interactive autonome `ggiraph`.
- `scripts/simuler_donnees_github.R` : générateur reproductible des données simulées et des graphiques économétriques associés.
- `scripts/modele_mixte.R` : estimation du modèle linéaire mixte REML et diagnostics résiduels gaussiens vs Student-t.
- `scripts/analyse_saisonnalite.R` : décomposition STL et modélisation SARIMA des séries mensuelles.
- `scripts/build_france_dom_geojson.R` : préparation cartographique incluant les départements d'outre-mer.
- `data_simulees/` : tables `.rds`, contours `.geojson` et figures `.png` générés sur données synthétiques.
