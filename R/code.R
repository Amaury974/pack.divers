


#' Localise un fichier ou un dossier
#'
#' Fonction recursive :) premettant d'identifier tous les dossiers et/ou fichiers
#' correspondant au marqueur renseigné.
#'
#' @param Dossiers La direction du dossier dans lequel chercher.
#' @param marqueur Regex identifiant le/les fichiers cherchées.
#' @param Stop Regex pour identifier les dossiers dans lesquels ne pas chercher.
#'
#' @returns vecteur des fichiers identifiés
#' @export
loca_dossier <- function(Dossiers, marqueur, Stop = "\\*"){

  Dossiers <- Dossiers[!stringr::str_detect(Dossiers, Stop)]

  Dossiers_valide <- Dossiers[stringr::str_detect(Dossiers, marqueur)]

  if(!length(Dossiers_valide) & length(Dossiers)){
    # element = Dossiers[1]
    for (element in Dossiers){
      Dossiers_E <- paste0(element,'/', list.files(element),recycle0 = TRUE)

      Dossiers_valide <- c(Dossiers_valide, loca_dossier(Dossiers_E, marqueur, Stop))
    }
  }

  Dossiers_valide
}


#' Lire l'onglet d'un classeur Excel
#'
#' Appels de `XLConnect::readWorksheet` pré-parametré
#'
#' @param classeur Nom du Classeur Excel.
#' @param onglet Nom de l'onglet à importer.
#' @param tableau Si `TRUE`, force la récupération des numériques non arrondi
#'   même lorsque leurs colones contiennent du texte.
#'
#' @export
lire_onglet <- function(classeur, onglet, tableau = TRUE){

  if (!requireNamespace("XLConnect", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"XLConnect\" pour utiliser cette fonction.",
      call. = FALSE
    )
  }

  suppressWarnings({
    df_onglet <- XLConnect::readWorksheet(classeur,
                                          onglet,
                                          startRow =1, autofitRow = tableau,
                                          startCol = 1, autofitCol = FALSE,
                                          colTypes = unlist(list(character(0),'character')[c(tableau, !tableau)]),
                                          header = tableau,
                                          useCachedValues = TRUE,
                                          dateTimeFormat = '%d/%m/%Y')


    if(!tableau){

      # on force la récupération au format numeric pour éviter les arrondis
      df_onglet_2 <- XLConnect::readWorksheet(classeur,
                                              onglet,
                                              startRow =1, autofitRow = tableau,
                                              startCol = 1, autofitCol = FALSE,
                                              colTypes = 'numeric',
                                              header = tableau,
                                              useCachedValues = TRUE,
                                              dateTimeFormat = '%d/%m/%Y')



      # on remplace les petits arrondis par les valeur complètes
      for(i in 1:ncol(df_onglet)) {
        df_onglet[,i] <-  ifelse(is.na(df_onglet_2[,i]), df_onglet[,i], df_onglet_2[,i])
      }
    }
  })

  as.data.frame(df_onglet)
}


#' Indicateurs évaluation des modèles
#'
#'Calculs les indicateurs utile à l'évaluation de la performance des modèles.
#'
#' @param obs Vecteur des valeurs observées.
#' @param sim Vecteur des valeurs simulées.
#' @param indicateur Nom de l'indicateur à calculer. Non sensible à la casse.
#'   * variables quantitatives: RMSE, RRMSE, MAE, Efficience (eff), Biais, MAPE, RMSPE.
#'   * variables qualitatives: Specificite (spe), Sensibilite (sen, sens),
#'     Taux_bien_predit (tbp), Youden_index (J)
#' @param .poids Poids à attribuer aux valeur dans le calcul de l'indicateur.
#' @export
#'
#' @details
#' Indicateurs pour variables Quantitatives
#' * RMSE : Root Mean Square Error. L'indicateur le plus courant.
#'   Erreur moyenne en augmentant le poids des grosses erreurs via le carré.
#' * RRMSE : RMSE / moyenne des observations. Idem mais transformé en indice
#'   pour pouvoir être comparé à des modèles utilisant des données de moyenne différente.
#' * MAE : Mean Absolute Error. Erreur moyenne sans augmenter le poids des
#'   grosses erreurs.
#' * Efficience (ou eff) : erreur au carré moyenne / equart à la moyenne au carré.
#'   A quel point le modèle est meilleur que de juste utiliser la moyenne.
#' * Biais : moyenne des erreurs. Le modèle dévit-il ?
#' * MAPE : Mean Absolute Percentage Error.
#'   Equivalent MAE pour des pourcentages d'erreur.
#'   Utile pour des données ayant une grande amplitude,
#'   typiquement des modèles de croissance.
#' * RMSPE : Root Mean Squared Percentage Error (indicateur perso)
#'   Equivalent RMSE du log de la variable (approche à privilégier)
#'   RMSE pour des pourcentages d'erreur.
#'   Utile pour des données ayant une grande amplitude,
#'   typiquement des modèles de croissance.
#'
#'  Indicateurs pour variables Qualitatives
#'  * Specificite (ou spe) : taux de prévision des négatifs
#'  * Sensibilite (ou sen, sens) : taux de prévision des positifs
#'  * Taux_bien_predit (ou tbp) : Classique mais trompeur si les données sont
#'    biaisées en faveur des positifs ou des négatifs
#'  * Youden_index (ou J) : sensibilité + spécificité - 1
#'    Indice de performance global, insensible au bias des données d'entrainement
indic <- function(obs, sim, indicateur, .poids = NULL){
  if(is.null(.poids)) .poids = 1/sum(!(is.na(obs)|is.na(obs))) else .poids = .poids / sum(.poids)
  if(is.infinite(.poids[1])) return(NA)

  if(toupper(indicateur) == 'RMSE')
    return(sqrt(sum(.poids * (obs-sim)^2, na.rm = TRUE)))

  if(toupper(indicateur) == 'RRMSE')
    return(sqrt(sum(.poids * (obs-sim)^2, na.rm = TRUE))/mean(obs, na.rm = TRUE))

  if(toupper(indicateur) == 'MAE')
    return(sum(.poids * abs(obs-sim), na.rm = TRUE))

  if(toupper(indicateur) %in% c('EFFICIENCE', 'EFF'))
    return( 1-sum(.poids * (obs-sim)^2, na.rm = TRUE)/sum(.poids * (obs-mean(obs, na.rm = TRUE))^2, na.rm = TRUE))

  if(toupper(indicateur) == 'BIAIS')
    return(sum(.poids * (obs-sim), na.rm = TRUE))

  if(toupper(indicateur) == 'MAPE')
    # return(mean(abs(obs-sim)/obs, na.rm = TRUE)*100)
    return(sum(.poids * abs(obs-sim)/((obs+sim)/2), na.rm = TRUE)*100) # la moyenne obs-sim au lieu de l'obs

  if(toupper(indicateur) == 'RMSPE') # indicateur perso ? Root mean squared percentage error
    return(sqrt(sum(.poids * ((obs-sim)/((obs+sim)/2)*100)^2, na.rm = TRUE)))

  # ~~~~{    Indicateur pour variable Qualitative     }~~~~
  .poids <- .poids * length(obs)

  N  <- sum(.poids)
  VN <- sum(as.numeric(!obs & !sim) * .poids)
  VP <- sum(as.numeric(obs & sim) * .poids)
  FN <- sum(as.numeric(obs & !sim) * .poids)
  FP <- sum(as.numeric(!obs & sim) * .poids)

  if(toupper(indicateur) %in% c('SPECIFICITE', 'SPE'))
    return(VN/(FP+VN))

  if(toupper(indicateur) %in% c('SENSIBILITE', 'SEN', 'SENS'))
    return(VP/(FN+VP))

  if(toupper(indicateur) %in% c('TAUX_BIEN_PREDITS', 'TBP'))
    return((VN+VP)/N)

  if(toupper(indicateur) %in% c('YOUDEN_INDEX', 'J'))
    return(VP/(FN+VP) + VN/(FP+VN) -1) #sens + spe -1


  warning('Indicateur non reconnu')
  NA
}


#' Significativité d'une pvalue
#'
#' Mise en forme de la significativité d'une pvalue
#'
#' @param pvalue valeur unique
#' @export
#'
#' @details
#' * pvalue > 0.1 : ' '
#' * 0.1 >= pvalue > 0.05 : .
#' * 0.05 >= pvalue > 0.01 : *
#' * 0.01 >= pvalue > 0.001 : **
#' * 0.001 >= pvalue : ***
pvalue_signif <- function(pvalue){
  seuils <- c(' '=0.1, '.'=0.05, '*'=0.01, '**'=0.001,'***'=0)
  names(seuils[seuils < pvalue][1])

}

#' Formatage numéros de téléphone
#'
#' Mise en forme de numéros de téléphones.
#'
#' @param num Vecteur de numéros de télephone en numérique ou caractère,
#'   le 0 initial est facultatif
#' @export
#'
#' @details
#' Defaut au format français: 06 ## ## ## ##.
#' Détection des numéros réunionnais, commençant par 0262 ou 0692 et
#' formaté ainsi: 0262 ## ## ##.
#'
#' @examples
#'   format_tel(c('0745623118', 262856231, ' 0692856214'))
format_tel <- function(num){

  tel_numeric <- num |>
    stringr::str_remove_all('\\s') |>
    as.numeric()

  tel_france <- tel_numeric |>
    format(big.mark = ' ', big.interval = 2L, trim=TRUE) |>
    dplyr::na_if('NA') |>
    stringr::str_pad(width = 14, side = 'left', pad = 0) # ajout du 0 devant qui saute en numérique

  tel_format <- dplyr::if_else(
    stringr::str_detect(tel_numeric, '^(262|692)'),
    stringr::str_remove(tel_france, '\\s'),
    tel_france)

  tel_format
}

























#' Lecture de shapefile depuis un serveur FTP
#'
#' Détection, téléchargement depuis un serveur FTP puis lecture des fichiers shapefile.
#'
#' @param url_dossier URL du dossier contenant le shapefile,
#'   format `ftp://username:password@ftppath` seul ou `ftp://ftppath`
#'   en précisant `identification` si nécessaire
#' @param identification `username:password` du FTP si nécéssaire
#'   uniquement si ils ne sont pas intégrés dans `url_dossier`
#'
#' @returns objet de classe `sf`, voir doc du package `sf`
#'
#' @export
#'
#' @details
#' La détéction des fichiers se fait avec `RCurl::getURL`.
#' Ils sont téléchargé dans une direction temporaire et
#' lu avec `sf::st_read`
st_read_ftp <- function(url_dossier, identification = NULL){

  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"RCurl\" pour utiliser cette fonction.",
      call. = FALSE
    )
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"sf\" pour utiliser cette fonction.",
      call. = FALSE
    )
  }
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"utils\" pour utiliser cette fonction.",
      call. = FALSE
    )
  }

  # url_dossier doit finir par un /
  url_dossier <- paste0(url_dossier, '/') |> # on en rajoute un
    stringr::str_remove('(?<=/)/$') # et on en enlève un si il y en a deux

  # standardisation de l'url
  if(stringr::str_detect(url_dossier, '@')){
    # url au format ftp://username:password@ftppath

    url_id <- url_dossier
    url_simple <- paste0("ftp://", stringr::str_extract(url_dossier, '[^@]+$'))
    identification <- stringr::str_extract(url_dossier, '(?<=ftp:..)[^@]+')

  } else {
    # url au format ftp://ftppath

    if(!is.null(identification)){
      # avec identification
      url_simple <- url_dossier
      url_id <- paste0("ftp://",
                       identification,
                       '@', stringr::str_remove(url_dossier, "ftp:.."))

    } else url_id <- url_simple <- url_dossier # sans identification
  }

  # url_simple = ftp://ftppath
  # url_id = ftp://username:password@ftppath

  # récupération de la liste des fichiers du dossier
  fichiers <- RCurl::getURL(url_simple, userpwd = identification, dirlistonly = TRUE)        # <- à voir si ça marche avec identification = NULL
  liste_fichiers <- strsplit(fichiers, "\r\n")[[1]]

  # uniquement les fichier du shapefile
  fichiers_shapefile <-
    liste_fichiers[stringr::str_detect(liste_fichiers,"((shp)|(shx)|(dbf)|(prj)|(cpg))$")]

  # téléchargement des fichiers dans un dossier temporaire
  temp_dir <- tempdir()

  for(fichier_i in fichiers_shapefile) {
    utils::download.file(
      paste0(url_id, fichier_i),
      file.path(temp_dir, fichier_i),
      mode = "wb"
    )
  }

  # Lecture du shapefile depuis le dossier temporaire
  couche_sf <- sf::st_read(file.path(temp_dir,
                                     fichiers_shapefile[stringr::str_detect(fichiers_shapefile, 'shp$')]))

}


