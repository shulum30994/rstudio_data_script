library(geojsonsf)
library(sf)
library(leaflet)
library(leaflet.extras)
library(dbscan)
library(dplyr)
library(geosphere)
library(openrouteservice)
library(magrittr)

ors_api_key('eyJvcmciOiI1YjNjZTM1OTc4NTExMTAwMDFjZjYyNDgiLCJpZCI6ImNjOWY2NTg1MjRlYzQyNGRiYTIwYzJjOGM4ZTU4ZjIxIiwiaCI6Im11cm11cjY0In0=')

sawah_sf <- geojson_sf("https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_JEMBER/JEMBER_TANJUNG.geojson")

sumur_sf <- geojson_sf("https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_COORDINATE.geojson")

basemap_sf <- leaflet() %>%
  addProviderTiles(
    "OpenStreetMap",
    group = "OpenStreetMap"
  ) %>%
  addProviderTiles(
    "Esri.WorldStreetMap",
    group = "Esri.WorldStreetMap"
  ) %>%
  addProviderTiles(
    "CartoDB.Positron",
    group = "CartoDB.Positron"
  ) %>%
  addProviderTiles(
    "Esri.WorldImagery",
    group = "Esri.WorldImagery"
  ) %>%
  addLayersControl(
    baseGroups = c(
      "OpenStreetMap", "Stamen.Toner", "Esri.WorldStreetMap", "Wikimedia", "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
    ),
    position = "topleft"
  )

icon.spesial <- makeAwesomeIcon(
  icon = "flag",
  markerColor = "blue",
  library = "fa",
  iconColor = "black"
)

basemap_sf %>%
  addAwesomeMarkers(
    data = sumur_sf,
    icon = icon.spesial
  )

basemap_sf %>%
  addCircleMarkers(data = sumur_sf,color = "red") %>%
  addCircles(data = sumur_sf,radius = 50,color = "blue",opacity = 1,fillColor = "yellow",fillOpacity = 0.25,popup =~as.character(FARMER))

drivetime <- ors_isochrones(
  locations = c(-8.340278494320252, 113.56051957155863),
  profile = "cycling-regular",
  range = 1200)

st_intersection(drivetime,sumur_sf)
