library(tidyverse)
library(sf)
library(tmap)

map <- read_sf("G:\\SPATIAL\\Besuki+Lumajang\\besuki+lumajang.shp")

map_indo<- read_sf("G:\\SPATIAL\\idn_GADM\\gadm41_IDN_0.shp")

map_kabupaten <- read_sf("G:\\SPATIAL\\idn_GADM\\gadm41_IDN_2.shp")

data <- lg %>%
  select(GID_3,KAB,Status)
data <- data %>% mutate(Choice=if_else(Status=="Basis",1,0))

map_lq <- left_join(map,data,by=c("GID_3"="GID_3"))

map_lq_selected <- map_lq %>%
  filter(Status=="Basis")

tm_shape(map_lq)+
  tm_polygons("Status",
              fill.scale = tm_scale_discrete(
                values= c("0"="#6495ED",
                          "1"="#cce3de"),
                labels = c("1"="Basis",
                           "0"="Non Basis")
              ),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Status :",
                na.show = F,
                #position=c("center","bottom"),
                #orientation = "horizontal",
                title.fontfamily = "mono",
                title.fontface = "bold",
                text.fontfamily = "mono",
                text.fontface = "plain",
                text.size = 0.8
              ))+
  tm_facets_wrap(by="KAB")+
  tm_legend(na.show = F)+
  tm_layout(legend.show = T,
            panel.label.fontfamily = "mono",
            panel.label.fontface = "bold",
            panel.label.size = 1.5)

##### JEMBER ######
tm_shape(map_lq %>%
           filter(NAME_2=="Jember"))+
  tm_polygons("Status",
              fill.scale = tm_scale_discrete(
                values= c("0"="#6495ED",
                          "1"="#cce3de"),
                labels = c("1"="Basis",
                           "0"="Non Basis")
              ),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Status :",
                na.show = F,
                position=c("right","bottom"),
                #orientation = "horizontal",
                title.fontfamily = "mono",
                title.fontface = "bold",
                text.fontfamily = "mono",
                text.fontface = "plain",
                text.size = 0.8
              ))+
  tm_shape(map_lq_selected %>%
             filter(NAME_2=="Jember"))+
  tm_text("NAME_3",
          size = 0.8,
          fontface = "bold", 
          fontfamily = "Mono", 
          shadow = TRUE)+
  tm_legend(na.show = F)+
  tm_compass(type = "8star",
             position = c("left","top"))+
  tm_layout(legend.show = T,
            panel.label.fontfamily = "mono",
            panel.label.fontface = "bold",
            panel.label.size = 1.5)
###################
##### LUMAJANG ######
tm_shape(map_lq %>%
           filter(NAME_2=="Lumajang"))+
  tm_polygons("Status",
              fill.scale = tm_scale_discrete(
                values= c("0"="#6495ED",
                          "1"="#cce3de"),
                labels = c("1"="Basis",
                           "0"="Non Basis")
              ),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Status :",
                na.show = F,
                position=c("right","bottom"),
                #orientation = "horizontal",
                title.fontfamily = "mono",
                title.fontface = "bold",
                text.fontfamily = "mono",
                text.fontface = "plain",
                text.size = 0.8
              ))+
  tm_shape(map_lq_selected %>%
             filter(NAME_2=="Lumajang"))+
  tm_text("NAME_3",
          size = 0.8,
          fontface = "bold", 
          fontfamily = "Mono", 
          shadow = TRUE)+
  tm_legend(na.show = F)+
  tm_compass(type = "8star",
             position = c("left","top"))+
  tm_layout(legend.show = T,
            panel.label.fontfamily = "mono",
            panel.label.fontface = "bold",
            panel.label.size = 1.5)
###################
##### BONDOWOSO ######
tm_shape(map_lq %>%
           filter(NAME_2=="Bondowoso"))+
  tm_polygons("Status",
              fill.scale = tm_scale_discrete(
                values= c("0"="#6495ED",
                          "1"="#cce3de"),
                labels = c("1"="Basis",
                           "0"="Non Basis")
              ),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Status :",
                na.show = F,
                position=c("left","bottom"),
                #orientation = "horizontal",
                title.fontfamily = "mono",
                title.fontface = "bold",
                text.fontfamily = "mono",
                text.fontface = "plain",
                text.size = 0.8
              ))+
  tm_shape(map_lq_selected %>%
             filter(NAME_2=="Bondowoso"))+
  tm_text("NAME_3",
          size = 0.8,
          fontface = "bold", 
          fontfamily = "Mono",
          remove.overlap=T,
          shadow = TRUE)+
  tm_legend(na.show = F)+
  tm_compass(type = "8star",
             position = c("right","top"))+
  tm_layout(legend.show = T,
            panel.label.fontfamily = "mono",
            panel.label.fontface = "bold",
            panel.label.size = 1.5)
###################
##### SITUBONDO ######
tm_shape(map_lq %>%
           filter(NAME_2=="Situbondo"))+
  tm_polygons("Status",
              fill.scale = tm_scale_discrete(
                values= c("0"="#6495ED",
                          "1"="#cce3de"),
                labels = c("1"="Basis",
                           "0"="Non Basis")
              ),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Status :",
                na.show = F,
                position=c("center","bottom"),
                #orientation = "horizontal",
                title.fontfamily = "mono",
                title.fontface = "bold",
                text.fontfamily = "mono",
                text.fontface = "plain",
                text.size = 0.8
              ))+
  tm_shape(map_lq_selected %>%
             filter(NAME_2=="Situbondo"))+
  tm_text("NAME_3",
          size = 0.8,
          fontface = "bold", 
          fontfamily = "Mono",
          remove.overlap=T,
          shadow = TRUE)+
  tm_legend(na.show = F)+
  tm_compass(type = "8star",
             position = c("right","top"))+
  tm_layout(legend.show = T,
            panel.label.fontfamily = "mono",
            panel.label.fontface = "bold",
            panel.label.size = 1.5)
###################
##### BANYUWANGI ######
tm_shape(map_lq %>%
           filter(NAME_2=="Banyuwangi"))+
  tm_polygons("Status",
              fill.scale = tm_scale_discrete(
                values= c("0"="#6495ED",
                          "1"="#cce3de"),
                labels = c("1"="Basis",
                           "0"="Non Basis")
              ),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Status :",
                na.show = F,
                position=c("left","top"),
                #orientation = "horizontal",
                title.fontfamily = "mono",
                title.fontface = "bold",
                text.fontfamily = "mono",
                text.fontface = "plain",
                text.size = 0.8
              ))+
  tm_shape(map_lq_selected %>%
             filter(NAME_2=="Banyuwangi"))+
  tm_text("NAME_3",
          size = 0.8,
          fontface = "bold", 
          fontfamily = "Mono",
          remove.overlap=T,
          shadow = TRUE)+
  tm_legend(na.show = F)+
  tm_compass(type = "8star",
             position = c("right","top"))+
  tm_layout(legend.show = T,
            panel.label.fontfamily = "mono",
            panel.label.fontface = "bold",
            panel.label.size = 1.5)
###################
tm_shape(map_lq)+
  tm_polygons("Status",
              fill.scale = tm_scale_discrete(
                values= c("0"="#6495ED",
                          "1"="#cce3de"),
                labels = c("1"="Basis",
                           "0"="Non Basis")
              ),
              col_alpha = 0.9,
              fill.legend = tm_legend(
                title = "Status :",
                na.show = F,
                #position=c("center","bottom"),
                #orientation = "horizontal",
                title.fontfamily = "mono",
                title.fontface = "bold",
                text.fontfamily = "mono",
                text.fontface = "plain",
                text.size = 0.8
              ))+
  tm_shape(map_lq_selected)+
  tm_text("NAME_3",
          size = 0.5,
          fontface = "bold", 
          fontfamily = "Mono", 
          shadow = TRUE)+
  tm_shape(map_indo)+
  tm_borders()+
  tm_legend(na.show = F)+
  tm_compass(type = "8star",
             position = c("right","top"))+
  tm_layout(legend.show = T,
            panel.label.fontfamily = "mono",
            panel.label.fontface = "bold",
            panel.label.size = 1.5)


##### Sekarkijang #####
tm_shape(map_kabupaten %>%
           filter(NAME_2=="Lumajang"|NAME_2=="Jember"|NAME_2=="Bondowoso"|NAME_2=="Situbondo"|NAME_2=="Banyuwangi"))+
  tm_polygons()+
  tm_text("NAME_2",
          size = 0.9,
          fontface = "bold", 
          fontfamily = "Mono", 
          shadow = TRUE)+
  tm_shape(map_indo)+
  tm_borders()+
  tm_legend(na.show = F)+
  tm_compass(type = "8star",
             position = c("right","top"))+
  tm_layout(legend.show = F,
            panel.label.fontfamily = "mono",
            panel.label.fontface = "bold",
            panel.label.size = 1.5)
