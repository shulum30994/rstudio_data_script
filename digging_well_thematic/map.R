library(tidyverse)
library(sf)
library(tmap)

##### Read Shapefile #####
blukon <- read_sf("https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_LUMAJANG/LUMAJANG_BLUKON.geojson")

sal_tanjung <- read_sf("https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_JEMBER/Primary%20Canal.geojson")

sal_blukon <- read_sf("https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_LUMAJANG/Secondary%20Canal.geojson")

tanjung <- read_sf("https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_JEMBER/JEMBER_TANJUNG.geojson")

##### Data Filter #####
water$label<-factor(water$label,
                    ordered=T,
                    levels=c(
  "Wet Season (Jan - April)",
  "Dry Season I (May – July/Aug)",
  "Dry Season II (Sept – Dec)"
))

jbr_water <- water %>%
  filter(kab=="JEMBER")

lmj_water <- water %>%
  filter(kab=="LUMAJANG")

##### Join data #####
jbr_water_all <- left_join(tanjung,jbr_water,by=c("descriptio"="kode"))

lmj_water_all <- left_join(blukon,lmj_water,by=c("descriptio"="kode"))

jbr_water_mt1 <- left_join(tanjung,jbr_water %>% filter(musim=="MT1"),by=c("descriptio"="kode"))

jbr_water_mt2 <- left_join(tanjung,jbr_water %>% filter(musim=="MT2"),by=c("descriptio"="kode"))

jbr_water_mt3 <- left_join(tanjung,jbr_water %>% filter(musim=="MT3"),by=c("descriptio"="kode"))

##### Plotting #####
#---- Facetted Jember ----
tm_shape(jbr_water_all)+
  tm_polygons("air",
              lwd=0.4,
              fill.scale = tm_scale_continuous(values = "storm",
                                               ticks = c(500,1000,5000,10000,15000,26000),
                                               midpoint = 12757,
                                               value.na="white"),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Water Volume (cubic meter) :",
                na.show = F,
                position=tm_pos_out("center","bottom"),
                orientation = "landscape",
                title.fontfamily = "mono",
                title.fontface = "bold",
                text.fontfamily = "mono",
                text.fontface = "plain"
                )
              )+
  tm_facets_wrap(by="label",
                 nrow = 1,
                 drop.NA.facets=T)+
  tm_shape(sal_tanjung)+
  tm_lines(col = "blue",
           lwd=2)+
  tm_labels("Secondary Canal",
            ymod = -0.5,
            angle=40,
            size = 0.5,
            col = "blue",
            fontfamily = "mono",
            fontface = "bold.italic")+
  tm_compass(type = "8star",
             position = c("left","top"))+
  tm_scalebar(position = c('right','top'),
              text.size = 0.7)+
  tm_layout(legend.show = T,
            panel.label.fontfamily = "mono",
            panel.label.fontface = "bold")

#---- Facetted Lumajang ----
tm_shape(lmj_water_all)+
  tm_polygons("air",
              lwd=0.4,
              fill.scale = tm_scale_continuous(values = "storm",
                                               ticks = c(500,10000,50000,100000,150000,200000,260000),
                                               midpoint = 128136,
                                               value.na="white"),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Water Volume (cubic meter) :",
                na.show = F,
                position=tm_pos_out("center","bottom"),
                orientation = "landscape",
                title.fontfamily = "mono",
                title.fontface = "bold",
                text.fontfamily = "mono",
                text.fontface = "plain"
              )
  )+
  tm_facets_wrap(by="label",
                 nrow = 1,
                 drop.NA.facets=T)+
  tm_shape(sal_blukon)+
  tm_lines(col = "blue",
           lwd=2)+
  tm_labels("Secondary Canal",
            ymod = -0.5,
            angle=40,
            size = 0.5,
            col = "blue",
            fontfamily = "mono",
            fontface = "bold.italic")+
  tm_compass(type = "8star",
             position = c("left","top"))+
  tm_scalebar(position = c('right','top'),
              text.size = 0.7)+
  tm_layout(legend.show = T,
            panel.label.fontfamily = "mono",
            panel.label.fontface = "bold")

#---- One-by-one ------
  jbr_mt1 <-
  tm_shape(jbr_water_mt1)+
  tm_polygons("air",
              lwd=0.4,
              fill.scale = tm_scale_continuous(values = "blues",value.na="white"),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Wet Season (cubic meter) :",
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

jbr_mt2 <-
  tm_shape(jbr_water_mt2)+
  tm_polygons("air",
              lwd=0.4,
              fill.scale = tm_scale_continuous(values = "blues",value.na="white"),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Dry Season I (cubic meter) :",
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
  tm_shape(jbr_water_mt3)+
  tm_polygons("air",
              lwd=0.4,
              fill.scale = tm_scale_continuous(values = "blues",value.na="white"),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Dry Season II (cubic meter) :",
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

##### Show and arrange #####
jbr_mt1
jbr_mt2
jbr_mt3

tmap_arrange(jbr_mt1,
             jbr_mt2,
             jbr_mt3)
