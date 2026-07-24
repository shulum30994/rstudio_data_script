library(googlesheets4)
library(sf)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(tmap)

data <- read_sheet('https://docs.google.com/spreadsheets/d/1VnaaNz6oO9Mix80bsxhnUkVgMwrNjIjDbg6m_QTMYgo/edit?gid=39761894#gid=39761894', sheet='variables')

spasial <- st_read('G:\\SPATIAL\\idn_GADM\\gadm41_IDN_2.shp')

jatim <- spasial %>% filter(NAME_1=='Jawa Timur')

##### PERTANIAN, PENGOLAHAN, TRANSPORTASI, INFOKOM #####
# Unstacked
PPTI_unstacked<-data %>%
  select(ID_2,Pertanian_hutan_ikan_ADHB_miliarRp_2025_BPS, Ind_pengolahan_ADHB_miliaRp_2025_BPS, 	Transport_log_ADHB_miliarRp_2025_BPS, 	Infokom_ADHB_miliarRp_2025_BPS) %>%
  mutate(
    across(c(Pertanian_hutan_ikan_ADHB_miliarRp_2025_BPS, Ind_pengolahan_ADHB_miliaRp_2025_BPS, Transport_log_ADHB_miliarRp_2025_BPS, Infokom_ADHB_miliarRp_2025_BPS), ~round(./1000, 1))
  ) %>%
  rename(Pertanian=Pertanian_hutan_ikan_ADHB_miliarRp_2025_BPS,
         Industri=Ind_pengolahan_ADHB_miliaRp_2025_BPS,
         Transport=Transport_log_ADHB_miliarRp_2025_BPS,
         Infokom=Infokom_ADHB_miliarRp_2025_BPS)

# Stacked
PPTI<-data %>%
  select(ID_2,Pertanian_hutan_ikan_ADHB_miliarRp_2025_BPS, Ind_pengolahan_ADHB_miliaRp_2025_BPS, 	Transport_log_ADHB_miliarRp_2025_BPS, 	Infokom_ADHB_miliarRp_2025_BPS) %>%
  gather(key="CATEGORY", value="VAL",-ID_2)

PPTI$CATEGORY <- replace(PPTI$CATEGORY, PPTI$CATEGORY %in% c("Pertanian_hutan_ikan_ADHB_miliarRp_2025_BPS","Ind_pengolahan_ADHB_miliaRp_2025_BPS","Transport_log_ADHB_miliarRp_2025_BPS","Infokom_ADHB_miliarRp_2025_BPS"), c("Pertanian","Industri","Transport","Infokom"))
  
PPTI_jatim <- left_join(jatim, PPTI, by=c('GID_2'='ID_2'))
PPTI_unstacked_jatim <- left_join(jatim, PPTI_unstacked, by=c('GID_2'='ID_2'))

jatim %>%
  tm_shape()+
  tm_borders(alpha=0.3)+
  tm_shape(PPTI_jatim %>%
             filter(CATEGORY=="Industri"))+
  tm_polygons("VAL")

PPTI_jatim %>%
  tm_shape()+
  tm_polygons("VAL")+
  tm_facets_wrap(by="CATEGORY")

warna <- colorBin(
  "Blues",
  domain = PPTI_unstacked_jatim$Transport,
  bins = 4
)

popup <- paste0(
  '<b>',PPTI_unstacked_jatim$NAME_2,'</br><br>',
  "Rp",PPTI_unstacked_jatim$Transport," triliun"
)

leaflet(PPTI_unstacked_jatim) %>%
  addProviderTiles('Esri.WorldPhysical') %>%
  addPolygons(
    fillColor = ~warna(Transport),
    fillOpacity = 0.8,
    color = "black",
    weight = 0.5,
    popup = popup,
    label=~NAME_2
  ) %>%
  addLegend(
    pal = warna,
    values = ~Transport,
    title = 'PDRB Transportasi:',
    position = "topright"
  )
