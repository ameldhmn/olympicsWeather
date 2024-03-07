library(devtools)
library(httr2)
library(jsonlite)
library(usethis)
library(htmltools)
library(tibble)
library(purrr)
library(leaflet)
library(sf)
library(testthat)
library(tidygeocoder)
library(roxygen2)
library(lubridate)
library(dplyr)
library(ggplot2)


#' Requête pour récupérer les données de prévisions météorologiques
#'
#' @param latitude Latitude (numérique)
#' @param longitude Longitude (numérique)
#' @return Un tibble contenant les données de prévisions météorologiques
#' @export
perform_request <- function(latitude, longitude){
  url <- "https://api.open-meteo.com/v1/forecast"
  request(url) |>
    req_url_query(latitude = latitude,
                  longitude = longitude,
                  hourly= c("temperature_2m",
                            "apparent_temperature",
                            "precipitation_probability",
                            "precipitation"),
                  .multi = "comma") |>
    req_perform() |>
    resp_body_json() |>
    as_tibble()
}


#' Décompacte les données de réponse des prévisions météorologiques.
#'
#' @param resp Résultat de la fonction précédente, tibble 5x9
#' @return Un nouveau tibble comprenant 168 observations et 5 variables
#' @export
unnest_response <- function(resp){
  hourly_donnees <- resp$hourly
  if (length(hourly_donnees) == 0) {
    stop("Aucune donnée dans la colonne 'hourly'.")
  }
  output_tibble <- tibble(
    "heure au tz UTC" = with_tz(unlist(hourly_donnees[[1]]), tzone = "UTC"),
    "données de température" = unlist(hourly_donnees[[2]]),
    "données de température ressentie" = unlist(hourly_donnees[[3]]),
    "probabilité de pluie" = unlist(hourly_donnees[[4]]),
    "precipitation en mm" = unlist(hourly_donnees[[5]])
  )
  return(output_tibble)
}


#' Convertit une adresse en coordonnées GPS.
#'
#' @param adresse Adresse à géocoder
#' @return Coordonnées GPS obtenues à partir de l'adresse
address_to_gps <- function(adresse) {
  df_adresse <- data.frame("nom" = character(), addr = character(), stringsAsFactors = FALSE)

  df_adresse <- rbind(df_adresse, data.frame(addr = adresse), stringsAsFactors = FALSE)

  resultat_geocodage <- df_adresse |>
    geocode(addr, method = 'arcgis')

  df_adresse <- resultat_geocodage

  return(df_adresse)
  print(df_adresse)
}


#' Obtient les coordonnées GPS à partir d'une adresse.
#'
#' @param adresse Adresse à géocoder
#' @return Coordonnées GPS obtenues à partir de l'adresse
#' @export
get_gps_coordinate <- function(adresse) {
  resultat_geocodage <- address_to_gps(adresse)

  coordonnees <- c(resultat_geocodage$lat, resultat_geocodage$long)

  return(coordonnees)
  print(coordonnees)
}


#' Obtient les prévisions météorologiques à partir de coordonnées GPS.
#'
#' @param xy Vecteur numérique avec x les latitudes et y les longitudes
#' @return Un tibble 168x5
#' @export
get_forecast.numeric <- function(xy, ...) {
  if (!is.numeric(xy) || length(xy) != 2) {
    stop("Erreur ! L'argument xy doit être un vecteur numérique de taille 2 !")
  }

  response_table <- perform_request(xy[2], xy[1], ...)

  unnested_table <- unnest_response(response_table)

  return(unnested_table)
}


#' Prévisions météorologiques à partir d'une adresse.
#'
#' @param adresse Adresse à géocoder
#' @return Les prévisions météorologiques obtenues
#' @export
forecast.character <- function(adresse) {
  if (!is.character(adresse) || length(adresse) != 1) {
    stop("L'argument address doit être de type character et de taille 1.")
  }
}

#' Obtient les prévisions météorologiques à partir d'une adresse.
#'
#' @param adresse Adresse à géocoder
#' @return Les prévisions météorologiques obtenues
#' @export
get_forecast.character <- function(adresse) {
  forecast.character(adresse)

  coordinates <- get_gps_coordinate(adresse)

  resultat_previsions <- perform_request(latitude = coordinates[1], longitude = coordinates[2])
  resultat_traitement <- unnest_response(resultat_previsions)

  return(resultat_traitement)
}


#' get_forecast
#'
#' Cette fonction générique permet d'obtenir des prévisions météorologiques en fonction d'une localisation.
#' Elle a deux implémentations spécifiques :
#'   - get_forecast.character : pour obtenir des prévisions à partir d'un nom de site olympique ou d'une adresse.
#'   - get_forecast.numeric : pour obtenir des prévisions à partir des coordonnées GPS.
#'
#' @param location Emplacement pour obtenir les prévisions (Adresse, nom de site olympique ou coordonnées GPS).
#'   - Pour get_forecast.character : un caractère de taille 1.
#'   - Pour get_forecast.numeric : un vecteur numérique de taille 2 (latitude, longitude).
#' @return Un tibble contenant les prévisions météorologiques.
#' @examples Exemples
#' \dontrun{
#'   # Exemples d'utilisation
#'   get_forecast("Nantes")
#'   get_forecast(c(48.85, 2.35))
#' }
#'
#' @seealso
#' \code{\link{get_forecast.character}}, \code{\link{get_forecast.numeric}},
#' \code{\link{perform_request}}, \code{\link{unnest_response}}
#'
#' @export
get_forecast <- function(x) {
  if (is.numeric(x)) {
    get_forecast.numeric(x)
  } else if (is.character(x)) {
    get_forecast.character(x)
  } else {
    stop("L'argument doit être de type numeric (coordonnées) ou character (adresse).")
  }
}




#' Visualise les emplacements des prévisions météorologiques sur une carte Leaflet
#'
#' @param forecast_locations Un data frame contenant les emplacements des prévisions météorologiques avec au moins deux colonnes : long (longitude) et lat (latitude).
#' @return Une carte Leaflet avec des marqueurs pour les emplacements des prévisions météorologiques.
#' @export
visualiser_carte <- function(forecast_locations) {
  require(leaflet)

  ma_carte <- leaflet() %>%
    addTiles() %>%
    addMarkers(lng = forecast_locations$long, lat = forecast_locations$lat)

  return(ma_carte)
}


#' Visualise les prévisions météorologiques
#'
#' @param forecast_data Un data frame contenant les données de prévisions météorologiques avec au moins deux colonnes : date et température.
#' @return Un objet ggplot représentant les prévisions météorologiques.
#' @export
visualiser_previsions <- function(forecast_data) {
  require(ggplot2)

  ggplot(data = forecast_data, aes(x = date, y = temperature)) +
    geom_line() +
    labs(title = "Prévision de la température", x = "Date", y = "Température (°C)")
}

