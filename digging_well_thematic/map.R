library(tidyverse)
library(sf)
library(tmap)

##### Read Shapefile #####
blukon <- read_sf("G:\\2024\\Sumur Tanjungrejo\\FARMLAND LUMAJANG [SHP]\\NEW\\LUMAJANG_BLUKON.shp")

sal_tanjung <- read_sf("G:\\2024\\Sumur Tanjungrejo\\Pimer bedadung pendek.kml")

tanjung <- read_sf("G:\\2024\\Sumur Tanjungrejo\\FARMLAND JEMBER [SHP]\\NEW\\JEMBER_TANJUNG.shp")

##### Data Filter #####
jbr_water <- water %>%
  filter(kab=="JEMBER")

lmj_water <- water %>%
  filter(kab=="LUMAJANG")

##### Join data #####
jbr_water_all <- left_join(tanjung,jbr_water,by=c("descriptio"="kode"))

jbr_water_mt1 <- left_join(tanjung,jbr_water %>% filter(musim=="MT1"),by=c("descriptio"="kode"))

jbr_water_mt2 <- left_join(tanjung,jbr_water %>% filter(musim=="MT2"),by=c("descriptio"="kode"))

jbr_water_mt3 <- left_join(tanjung,jbr_water %>% filter(musim=="MT3"),by=c("descriptio"="kode"))

##### Plotting #####
tm_shape(tanjung)+
  tm_polygons()+
tm_shape(jbr_water_all)+
  #tm_polygons("air", fill.scale = tm_scale(values="greens"))
  tm_fill(fill="air")+
  tm_scale_intervals(style = "equal")+
  tm_facets(by="label")

jbr_mt1 <-
  #tm_shape(tanjung)+
  #tm_polygons()+
  tm_shape(jbr_water_mt1)+
  tm_polygons("air",
              lwd=0.4,
              fill.scale = tm_scale_continuous(values = "blues",value.na="white"),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Wet Season (m3) :",
                na.show = F))+
  #tm_fill(fill="air",colorNA=NULL)+
  #tm_scale_intervals(style = "equal")
  tm_layout(legend.show = T)

jbr_mt2 <-
  tm_shape(jbr_water_mt2)+
  tm_polygons("air",
              lwd=0.4,
              fill.scale = tm_scale_continuous(values = "blues",value.na="white"),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Dry Season I (m3) :",
                na.show = F))+
  tm_shape(sal_tanjung)+
  tm_lines(col = "blue",
           lwd=2)+
  tm_labels("Secondary Canal",
            ymod = -0.5,
          angle=40,
          size = 0.5)+
  tm_compass(type = "8star",
             position = c("left","top"))+
  tm_layout(legend.show = T)

jbr_mt3 <-
  #tm_shape(tanjung)+
  #tm_polygons()+
  tm_shape(jbr_water_mt3)+
  tm_polygons("air",
              lwd=0.4,
              fill.scale = tm_scale_continuous(values = "blues",value.na="white"),
              col_alpha = 0.5,
              fill.legend = tm_legend(
                title = "Dry Season II (m3) :",
                na.show = F))+
  #tm_fill(fill="air",colorNA=NULL)+
  #tm_scale_intervals(style = "equal")
  tm_layout(legend.show = T)

##### Show and arrange #####
jbr_mt1
jbr_mt2
jbr_mt3
