library(sf)
library(tmap)
library(RColorBrewer)

indo1 <- read_sf("C:/Users/riset/Documents/1. PERFORMA CENDEKIA/2023/idn_GADM/gadm41_IDN_1.shp")

# joined
indo1_konv <- left_join(indo1,RASIO_KONV,by=c("CC_1"="PROP"))
indo1_konverter <- left_join(indo1,KONVERTER,by=c("CC_1"="PROP"))

indo1_konv %>%
  tm_shape()+
  tm_polygons("RASIO_KONVERSI",style="quantile",palette="Greys")+
  tm_facets(by="KONVERSI_LAHAN",nrow = 3)+
  tm_layout(panel.show = TRUE,
            panel.label.bg.color = NA)

indo1_konverter %>%
  na.omit() %>%
  tm_shape()+
  tm_polygons("PERC",style="quantile",palette="Greys")+
  tm_facets(by="KONVERSI_LAHAN",nrow = 4)+
  tm_layout(panel.show = TRUE,
            panel.label.bg.color = NA)
