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

#### ET map VIZ ####
# et_joined = after joined grouped dataset with spatial
et_joined %>%
  ggplot()+
  geom_sf(aes(fill=perc))+
  geom_text(et_grouped %>% mutate(label_baru=paste0(status,labels,","))%>% group_by(label_baru) %>% mutate(huruf=case_when(label_baru=="IRRIGATEDURBAN,"~"A.",label_baru=="IRRIGATEDVILLAGES/COUNTRYSIDE,"~"B.",label_baru=="NON-IRRIGATEDURBAN,"~"C.",label_baru=="NON-IRRIGATEDVILLAGES/COUNTRYSIDE,"~"D.")),
            mapping = aes(label=huruf,x=92,y=5.2),color="black",fontface="bold")+
  scale_fill_gradient2(low = "lightblue",midpoint=50,high = "darkblue",na.value = "gray71",breaks=c(25,50,75))+
  facet_wrap(status ~ labels,nrow = 4,ncol = 1)+
  labs(fill="Technical Efficiency (%) :",x="",y="")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_blank(),
        legend.title = element_text(face = "bold",
                                    size = 11,
                                    family = "monospace"),
        legend.text = element_text(family = "monospace",
                                   size=10.5),
        legend.position = "bottom")
