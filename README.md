
<!-- README.md est généré à partir de README.Rmd. Modifier README.Rmd uniquement,
     puis lancer : devtools::build_readme() -->

# pack.divers

<!-- badges: start -->

![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)
<!-- badges: end -->

Package R regroupant des fonctions utilitaires inclassables : recherche
récursive de fichiers, lecture d’onglets Excel, indicateurs d’évaluation
de modèles, mise en forme de p-values et de numéros de téléphone,
lecture de shapefiles depuis un serveur FTP.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("Amaury974/pack.divers")
```

### Packages optionnels

Certaines fonctions nécessitent des packages supplémentaires, vérifiés à
l’appel :

| Package       | Utilisé par     |
|---------------|-----------------|
| `XLConnect`   | `lire_onglet()` |
| `RCurl`, `sf` | `st_read_ftp()` |

``` r
install.packages(c("XLConnect", "RCurl", "sf"))
```

## Fonctions

| Fonction | Description |
|----|----|
| `loca_dossier()` | Localise récursivement les fichiers ou dossiers correspondant à une expression régulière |
| `lire_onglet()` | Lit un onglet d’un classeur Excel (via `XLConnect`), avec récupération des numériques non arrondis |
| `indic()` | Indicateurs d’évaluation de modèles : RMSE, RRMSE, MAE, efficience, biais, MAPE, RMSPE (quantitatif) ; spécificité, sensibilité, taux de bien prédits, indice de Youden (qualitatif) |
| `pvalue_signif()` | Convertit une p-value en symbole de significativité (`' '`, `.`, `*`, `**`, `***`) |
| `format_tel()` | Met en forme des numéros de téléphone français, avec gestion des numéros réunionnais |
| `st_read_ftp()` | Télécharge puis lit un shapefile depuis un serveur FTP |

La documentation détaillée de chaque fonction est accessible via
`?nom_de_la_fonction`.

## Exemples

``` r
library(pack.divers)
```

### Localiser des fichiers

`loca_dossier()` descend récursivement dans les dossiers jusqu’à trouver
ceux dont le chemin correspond au motif `marqueur` (expression
régulière). Les dossiers correspondant à `Stop` sont ignorés.

``` r
# arborescence d'exemple
racine <- file.path(tempdir(), "exemple_loca")
dir.create(file.path(racine, "projet_A", "data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(racine, "projet_B", "data"), recursive = TRUE, showWarnings = FALSE)
file.create(file.path(racine, "projet_A", "data", "mesures_2024.csv"),
            file.path(racine, "projet_B", "data", "mesures_2025.csv"),
            file.path(racine, "projet_B", "data", "notes.txt"))
#> [1] TRUE TRUE TRUE

# tous les fichiers « mesures »
loca_dossier(racine, marqueur = "mesures")
#> [1] "C:\\Users\\DELAGA~1\\AppData\\Local\\Temp\\RtmpGwVFXn/exemple_loca/projet_A/data/mesures_2024.csv"
#> [2] "C:\\Users\\DELAGA~1\\AppData\\Local\\Temp\\RtmpGwVFXn/exemple_loca/projet_B/data/mesures_2025.csv"

# en excluant le projet B
loca_dossier(racine, marqueur = "mesures", Stop = "projet_B")
#> [1] "C:\\Users\\DELAGA~1\\AppData\\Local\\Temp\\RtmpGwVFXn/exemple_loca/projet_A/data/mesures_2024.csv"
```

### Indicateurs d’évaluation de modèles

Variables quantitatives :

``` r
obs <- rnorm(100, 10, 2)
sim <- obs + rnorm(100, 0.3, 0.8)

sapply(c("RMSE", "RRMSE", "MAE", "eff", "biais", "MAPE", "RMSPE"),
       function(i) indic(obs, sim, i))
#>        RMSE       RRMSE         MAE         eff       biais        MAPE 
#>  0.75557453  0.07506928  0.58974098  0.86705862 -0.23001303  6.16072979 
#>       RMSPE 
#>  8.50816385
```

Avec des poids :

``` r
poids <- runif(100)
indic(obs, sim, "RMSE", .poids = poids)
#> [1] 0.7884529
```

Variables qualitatives (observé et simulé booléens) :

``` r
obs_bin <- obs > 10
sim_bin <- sim > 10

sapply(c("spe", "sens", "tbp", "J"),
       function(i) indic(obs_bin, sim_bin, i))
#>        spe       sens        tbp          J 
#>  0.8043478  0.9444444 88.0000000  0.7487923
```

### Significativité d’une p-value

`pvalue_signif()` accepte une valeur unique ; on l’applique à un vecteur
avec `sapply()`.

``` r
sapply(c(0.2, 0.07, 0.03, 0.005, 0.0001), pvalue_signif)
#> [1] " "   "."   "*"   "**"  "***"
```

### Numéros de téléphone

Format français par défaut (`06 ## ## ## ##`) ; les numéros réunionnais
(`0262` / `0692`) sont formatés `0262 ## ## ##`. Le 0 initial est
facultatif.

``` r
format_tel(c("0745623118", 262856231, " 0692856214"))
#> [1] "07 45 62 31 18" "0262 85 62 31"  "0692 85 62 14"
```

### Lire un onglet Excel

Nécessite `XLConnect` (et Java). Exemple non exécuté.

``` r
# tableau avec en-têtes
df <- lire_onglet("mon_classeur.xlsx", onglet = "Feuil1")

# plage libre, sans en-têtes (numériques non arrondis)
brut <- lire_onglet("mon_classeur.xlsx", onglet = "Feuil1", tableau = FALSE)
```

### Lire un shapefile depuis un serveur FTP

Nécessite `RCurl` et `sf`, et un serveur FTP accessible. Exemple non
exécuté.

``` r
# identifiants intégrés à l'URL
couche <- st_read_ftp("ftp://utilisateur:motdepasse@serveur.exemple.fr/dossier")

# ou fournis séparément
couche <- st_read_ftp("ftp://serveur.exemple.fr/dossier",
                      identification = "utilisateur:motdepasse")
```

## Licence

Distribué sous licence [GNU GPL (≥ 3)](LICENSE.md).

## Auteur

Amaury Jorant ([@Amaury974](https://github.com/Amaury974)) —
[issues](https://github.com/Amaury974/pack.divers/issues)
