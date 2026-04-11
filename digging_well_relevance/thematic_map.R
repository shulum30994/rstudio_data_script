library(tidyverse)
library(sf)
library(tmap)

##### Read Shapefile #####
blukon <- read_sf("https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_LUMAJANG/LUMAJANG_BLUKON.geojson")

sal_tanjung <- read_sf("https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_JEMBER/Primary%20Canal.geojson")

sal_blukon <- read_sf("https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_LUMAJANG/Secondary%20Canal.geojson")

tanjung <- read_sf("https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_JEMBER/JEMBER_TANJUNG.geojson")

one_df = rbind(mt1_clean,mt2_clean)

one_df$season<-factor(one_df$season,
                    ordered=T,
                    levels=c(
  "Wet Season (Jan - April)",
  "Dry Season (May – July/Aug)"
))

blukon_wev <- left_join(blukon,one_df %>% select(Code,Land_val_USD,Water_val_USD,season),by=c("descriptio"="Code"))

tanjung_wev <- left_join(tanjung,one_df %>% select(Code,Land_val_USD,Water_val_USD,season),by=c("descriptio"="Code"))


##### Plotting #####
#---- Facetted Jember ----
tm_shape(tanjung_wev)+
  tm_polygons("Land_val_USD",
              lwd=0.4,
              fill.scale = tm_scale_continuous(values = "storm",
                                               ticks = c(-0.6243404,0.3824438,1.108198,8.586101),
                                               midpoint = 0.7195517,
                                               value.na="white"),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Land Economic Value (USD/sqm) :",
                na.show = F,
                position=tm_pos_out("center","bottom"),
                orientation = "landscape",
                title.fontfamily = "mono",
                title.fontface = "bold",
                text.fontfamily = "mono",
                text.fontface = "plain"
                )
              )+
  tm_facets_wrap(by="season",
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
