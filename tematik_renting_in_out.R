library(ragg)
library(tmap)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(hrbrthemes)

id_map <- read_sf("C:/Users/riset/Documents/1. PERFORMA CENDEKIA/2023/idn_GADM/gadm41_IDN_1.shp")
siap04 <- read.csv("/home/shohibul/R Wokspace/spp_riska/siap04.csv")
siap04 <- siap04 %>% mutate(TRANSFER_RATIO=(RENTIN+RENTOUT)/B5R1DK5)
siap14 <- read.csv("/home/shohibul/R Wokspace/spp_riska/siap14.csv")

siap04 %>%
  group_by(PROP) %>%
  summarise(across(c(IN_RATIO),.fns=mean)) %>%
  rename(VALUE=IN_RATIO) %>%
  add_column(KATEGORI="RENTING IN RATIO",TAHUN="2004")

siap04 %>%
  group_by(PROP) %>%
  summarise(across(c(OUT_RATIO),.fns=mean))%>%
  rename(VALUE=OUT_RATIO) %>%
  add_column(KATEGORI="RENTING OUT RATIO",TAHUN="2004")

siap04 %>%
  group_by(PROP) %>%
  summarise(across(c(TRANSFER_RATIO),.fns=mean))%>%
  rename(VALUE=TRANSFER_RATIO) %>%
  add_column(KATEGORI="LAND TRANSFER RATIO",TAHUN="2004")

siap14 %>%
  group_by(PROP_x) %>%
  summarise(across(c(IN_RATIO),.fns=mean)) %>%
  rename(VALUE=IN_RATIO,PROP=PROP_x) %>%
  add_column(KATEGORI="RENTING IN RATIO",TAHUN="2014")

siap14 %>%
  group_by(PROP_x) %>%
  summarise(across(c(OUT_RATIO),.fns=mean))%>%
  rename(VALUE=OUT_RATIO,PROP=PROP_x) %>%
  add_column(KATEGORI="RENTING OUT RATIO",TAHUN="2014")

siap14 %>%
  group_by(PROP_x) %>%
  summarise(across(c(TRANSFER_RATIO),.fns=mean))%>%
  rename(VALUE=TRANSFER_RATIO,PROP=PROP_x) %>%
  add_column(KATEGORI="LAND TRANSFER RATIO",TAHUN="2014")

trans_all<-bind_rows(
  siap04 %>%
    group_by(PROP) %>%
    summarise(across(c(IN_RATIO),.fns=mean)) %>%
    rename(VALUE=IN_RATIO) %>%
    add_column(KATEGORI="RENTING IN RATIO",TAHUN="2004")%>%
    na.omit(),
  siap04 %>%
    group_by(PROP) %>%
    summarise(across(c(OUT_RATIO),.fns=mean))%>%
    rename(VALUE=OUT_RATIO) %>%
    add_column(KATEGORI="RENTING OUT RATIO",TAHUN="2004")%>%
    na.omit(),
  siap04 %>%
    group_by(PROP) %>%
    summarise(across(c(TRANSFER_RATIO),.fns=mean))%>%
    rename(VALUE=TRANSFER_RATIO) %>%
    add_column(KATEGORI="LAND TRANSFER RATIO",TAHUN="2004")%>%
    na.omit(),
  siap14 %>%
    group_by(PROP_x) %>%
    summarise(across(c(IN_RATIO),.fns=mean)) %>%
    rename(VALUE=IN_RATIO,PROP=PROP_x) %>%
    add_column(KATEGORI="RENTING IN RATIO",TAHUN="2014")%>%
    na.omit(),
  siap14 %>%
    group_by(PROP_x) %>%
    summarise(across(c(OUT_RATIO),.fns=mean))%>%
    rename(VALUE=OUT_RATIO,PROP=PROP_x) %>%
    add_column(KATEGORI="RENTING OUT RATIO",TAHUN="2014")%>%
    na.omit(),
  siap14 %>%
    group_by(PROP_x) %>%
    summarise(across(c(TRANSFER_RATIO),.fns=mean))%>%
    rename(VALUE=TRANSFER_RATIO,PROP=PROP_x) %>%
    add_column(KATEGORI="LAND TRANSFER RATIO",TAHUN="2014")%>%
    na.omit()
)

siap14 %>%
  group_by(PROP_x) %>%
  summarise(across(c(R501B_K6),.fns=sum))

siap14 %>%
  group_by(PROP_x) %>%
  summarise(across(c(R501C_K6),.fns=sum))

siap14 %>%
  group_by(PROP_x) %>%
  summarise(across(c(R501D_K6),.fns=sum))

id_map %>%
  ggplot()+geom_sf()

#### Plot The Map ####
map04 <- left_join(id_map,trans_2004,by=c("CC_1"="PROP"))
thm_map <- left_join(id_map,trans_all,by=c("CC_1"="PROP"))

tm_shape(id_map)+
  tm_borders(alpha = 0.1)+
  tm_shape(map04%>%na.omit)+
  tm_fill("VALUE",style = "quantile",title = "Acreage :",palette = "Blues")+
  tm_facets(by="KATEGORI")


ggplot()+
  geom_sf(data=id_map)+
  geom_sf(data=thm_map%>%na.omit,aes(fill=VALUE))+
  scale_fill_distiller(palette = "Blues",direction = 1)+
  labs(fill="Ratio :")+
  facet_wrap(KATEGORI~TAHUN,nrow = 3,ncol = 2)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
