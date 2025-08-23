library(tidyverse)
library(sf)
library(tmap)

##### Read Shapefile #####
blukon <- read_sf("G:\\2024\\Sumur Tanjungrejo\\FARMLAND LUMAJANG [SHP]\\NEW\\LUMAJANG_BLUKON.shp")

tanjung <- read_sf("G:\\2024\\Sumur Tanjungrejo\\FARMLAND JEMBER [SHP]\\NEW\\JEMBER_TANJUNG.shp")

##### Data Filter #####
jbr_water <- water %>%
  filter(kab=="JEMBER")

lmj_water <- water %>%
  filter(kab=="LUMAJANG")

##### Join data #####
jbr_water_shp <- left_join(tanjung,jbr_water,by=c("descriptio"="kode"))

##### Plotting #####
tm_shape(tanjung)+
  tm_polygons()+
tm_shape(jbr_water_shp)+
  #tm_polygons("air", fill.scale = tm_scale(values="greens"))
  tm_fill(fill="air")+
  tm_scale_intervals(style = "equal")+
  tm_facets_wrap(by="label")
