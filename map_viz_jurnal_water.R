library(sf)
library(tmap)
library(ragg)
library(RColorBrewer)

indo1 <- read_sf("/media/shohibul/e52b78ee-7bb4-410a-8fb5-f0306ebdbef4/Pemetaan/propinsi/gadm36_IDN_1.shp")

indo1_area <- left_join(indo1,area_prop,by=c("CC_1"="PROP"))

indo1_area %>%
  mutate(AREA_HA=AREA/10000)%>%
  tm_shape() +
  tm_borders(alpha = 0.3)+
  tm_fill("AREA_HA",title = "Harvest Area (ha) :",palette = "Blues",style = "quantile")+
  tm_facets(by="CATEGORY",nrow = 2)+
  tm_layout(legend.outside = FALSE,
            #legend.outside.position ="bottom
            legend.position = c("left","bottom"),
            panel.labels=c("A","B"),
            panel.label.fontfamily="serif",
            panel.label.fontface = "bold",
            panel.label.size = 1.1,
            panel.label.bg.color=NA,
            legend.text.fontfamily = "monospace",
            legend.text.size = 0.8,
            legend.title.fontfamily="serif",
            legend.title.fontface = "bold",
            legend.title.size = 1)

indo1_area %>%
  mutate(AREA_HA=AREA/10000) %>%
  ggplot()+
  geom_sf(aes(fill=AREA_HA))+
  geom_text(indo1_area %>%
              group_by(CATEGORY) %>%
              mutate(label=if_else(CATEGORY=="IRRIGATION","A.","B.")),
            mapping=aes(label=label,x=92,y=5.2),color="black",fontface="bold")+
  scale_fill_distiller(palette = "Blues",direction = 1)+
  facet_grid(rows = vars(CATEGORY))+
  labs(fill="Harvest Area (ha) :",x="",y="")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_blank(),
        legend.position = c(0.1,0.15))
