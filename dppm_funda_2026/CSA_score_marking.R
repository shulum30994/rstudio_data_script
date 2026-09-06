library(googlesheets4)
library(dplyr)
library(sf)
library(tmap)

raw <- read_sheet('https://docs.google.com/spreadsheets/d/1ApGjPu5hZuKqBw3k107kAimv0BX53nDiSycXJWmb7t0/edit?gid=258546240#gid=258546240', sheet='DRPM CSA dan SA Padi 2026')

koordinat <- read_sheet('https://docs.google.com/spreadsheets/d/1EvmIAri2-BDz43-BhR1tSwzd94xS2jF78LGYHnlm1Tc/edit?gid=0#gid=0')

will_up <- st_read('https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/KLAMBU_WILALUNG/KLAMBU_WILALUNG_UPSTREAMS.geojson')

will_mid <- st_read('https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/KLAMBU_WILALUNG/KLAMBU_WILALUNG_MIDDLE.geojson')

will_down <- st_read('https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/KLAMBU_WILALUNG/KLAMBU_WILALUNG_DOWNSTREAM.geojson')

csa_score_responden <- raw %>% select(`_index`,'CSSA_score')
koordinat_kudus <- koordinat %>% filter(REGENCY=="KUDUS")
CSA_score_kudus <- left_join(koordinat_kudus, csa_score_responden, by=c('KOBO_PARENT'='_index'))
CSA_score_kudus_coord <- st_as_sf(CSA_score_kudus, coords = c('LONG','LAT'), crs=4326)

tm_shape(will_up)+
  tm_polygons()+
  tm_shape(will_mid)+
  tm_polygons()+
  tm_shape(will_down)+
  tm_polygons()+
  tm_shape(CSA_score_kudus_coord)+
  #tm_dots(
   # col='CSSA_score',
  #  size='CSSA_score'
 # )+
  tm_dots(
    fill = 'CSSA_score',
    fill.scale = tm_scale_intervals(
      breaks=c(25,44,64,84,104,125),
      values=c("#FF0000","#FFA500","#FFFF00","#78C679","#238443")
   ),
    size = 'CSSA_score',
    size.scale=tm_scale_intervals(
      breaks=c(25,44,64,84,104,125),
      values=c(0.5, 1, 1.5, 2, 2.5)
    )
  )+
  tm_view(
    bbox = c(
      xmin=110.739927,
      ymin=-7.022552,
      xmax=110.894620,
      ymax=-6.853096
    )
  )
