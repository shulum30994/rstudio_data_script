library(tmap)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(hrbrthemes)

id_map <- read_sf("/home/shohibul/Pemetaan/gadm41_IDN_shp/gadm41_IDN_1.shp")
siap04 <- read.csv("/home/shohibul/R Wokspace/spp_riska/siap04.csv")
siap04 <- siap04 %>% mutate(TRANSFER_RATIO=(RENTIN+RENTOUT)/B5R1DK5)
siap14 <- read.csv("/home/shohibul/R Wokspace/spp_riska/siap14.csv")

siap04 %>%
  group_by(PROP) %>%
  summarise(across(c(RENTIN),.fns=sum)) %>%
  rename(VALUE=RENTIN) %>%
  add_column(KATEGORI="RENTING IN")

siap04 %>%
  group_by(PROP) %>%
  summarise(across(c(RENTOUT),.fns=sum))%>%
  rename(VALUE=RENTOUT) %>%
  add_column(KATEGORI="RENTING OUT")

siap04 %>%
  group_by(PROP) %>%
  summarise(across(c(B5R1DK5),.fns=sum)) %>%
  rename(VALUE=B5R1DK5) %>%
  add_column(KATEGORI="ACREAGE")

trans_2004<-bind_rows(
  siap04 %>%
    group_by(PROP) %>%
    summarise(across(c(RENTIN),.fns=sum)) %>%
    rename(VALUE=RENTIN) %>%
    add_column(KATEGORI="RENTING IN")%>%
    na.omit(),
  siap04 %>%
    group_by(PROP) %>%
    summarise(across(c(RENTOUT),.fns=sum))%>%
    rename(VALUE=RENTOUT)%>%
    add_column(KATEGORI="RENTING OUT")%>%
    na.omit(),
  siap04 %>%
    group_by(PROP) %>%
    summarise(across(c(B5R1DK5),.fns=sum)) %>%
    rename(VALUE=B5R1DK5)%>%
    add_column(KATEGORI="ACREAGE")%>%
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
tm_shape(id_map)+
  tm_borders(alpha = 0.1)+
  tm_shape(map04%>%na.omit)+
  tm_fill("VALUE",style = "quantile",title = "Acreage :",palette = "Blues")+
  tm_facets(by="KATEGORI")


ggplot()+
  geom_sf(data=id_map)+
  geom_sf(data=map04%>%na.omit,aes(fill=VALUE/10000))+
  scale_fill_distiller(palette = "Blues",direction = 1)+
  facet_wrap(~KATEGORI,nrow = 2,ncol = 2)+
  theme_linedraw()
