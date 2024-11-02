library(tidyverse)
library(sf)
library(tmap)

ind_kec <- read_sf("G:/SPATIAL/idn_GADM/gadm41_IDN_3.shp") %>% st_transform(4326)

ind_jatim <- ind_kec %>%
  filter(NAME_1=="Jawa Timur"|NAME_1=="Jawa Tengah"|NAME_2=="Cirebon")

ind_jawa_prop <- ind_prop %>%
  filter(NAME_1=="Jawa Timur"|NAME_1=="Jawa Tengah"|NAME_1=="Jawa Barat"|NAME_1=="Yogyakarta"|NAME_1=="Banten")

ko_pg <- koordinat %>%
  st_as_sf(coords = c("LONG","LAT"),crs=4326)

wilker <- st_buffer(subset(ko_pg),10000)

ind_jatim$terdekat <- ifelse(sf::st_intersects(ind_jatim,
                                               wilker,
                                               sparse = F),
                             "Yes", 
                             "No")
ggplot()+
  geom_sf(data=ind_jatim,aes(size=0.2,fill=terdekat))+
  geom_sf(data=ko_pg,aes(color="black"))+
  geom_sf(data = wilker,fill=alpha("red",0.2),color="red")+
  theme_minimal()+
  theme(legend.position = "none")

tm_shape(ind_jawa_prop)+
  tm_borders(alpha = 0.7)+
  tm_shape(ko_pg)+
  tm_symbols(size = "TCD",
             palette = "Reds")
