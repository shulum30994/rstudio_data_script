library(googlesheets4)
library(dplyr)
library(sf)
library(ggplot2)
library(tmap)

lokasi_pasar <- read_sheet('https://docs.google.com/spreadsheets/d/1VnaaNz6oO9Mix80bsxhnUkVgMwrNjIjDbg6m_QTMYgo/edit?gid=0#gid=0')
variables <- read_sheet('https://docs.google.com/spreadsheets/d/1VnaaNz6oO9Mix80bsxhnUkVgMwrNjIjDbg6m_QTMYgo/edit?gid=0#gid=0', sheet='variables')

idn_g2 <- st_read('/home/shohibul/KERJA/Pemetaan/gadm41_IDN_shp/gadm41_IDN_2.shp')

koor_pasar <- st_as_sf(lokasi_pasar, coords = c('LONG','LAT'), crs=4326)

jatim <- idn_g2 %>%
  filter(NAME_1=='Jawa Timur')

centroids <- st_centroid(jatim)

##### Jointed dataset #####
jajoined <- left_join(jatim, variables, by=c('GID_2'='GID_2'))

cenvar <- left_join(centroids,variables,by=c('GID_2'='GID_2'))

ggplot()+
  geom_sf(data=jajoined, aes(fill=protas_ton_ha_2025_BPS))+
  scale_fill_distiller(palette = "Blues", direction = 1)+
  geom_sf(data=cenvar, aes(size=IKG_2025_BPS))+
  scale_size(range = c(1,5))+
  #geom_sf_text(data=koor_pasar, aes(label=PASAR))+
  #ggsflabel::geom_sf_label_repel(data=cenvar %>% filter(IKG_2025_BPS>= 0.5), aes(label=NAME_2.x))+
  labs(
    title = "Produktivitas padi (ton/hektar) dan Indeks Kesenjangan Gender (IKG) tahun 2025",
    caption = "Sumber : Badan Pusat Statistik Propinsi Jawa Timur (2026)",
    fill='Produktivitas (ton/hektar) :',
    size='IKG :'
  )+
  theme_void()+
  theme(
    legend.position = "left",
    plot.title= element_text(
      family = "mono",
      size = 12,
      face = 'bold'
    ),
    plot.caption = element_text(
      family = "mono",
      size = 10,
      face = 'italic'
    ),
    legend.title = element_text(
      family = "mono",
      size = 11,
      face = 'bold'
    ),
    legend.text = element_text(
      family = "mono",
      size = 10,
      face = 'bold'
    )
  )
  
tm_shape(jajoined)+
  tm_borders(alpha=0.3)+
  tm_polygons(fill = 'IPM_2025_BPS',
palette='Blues',
title='IPM :')+
  tm_shape(cenvar)+
  tm_dots(
    size = 'IKG_2025_BPS',
    title.size='IKG :'
  )+
  tm_compass(
    type = 'arrow',
    position = c('left','top')
  )+
  tm_scalebar(
    position = c('right','bottom')
  )+
  tm_layout(
    legend.position=c('right','top'),
    fontfamily='Mono',
    fontsize=12
  )
