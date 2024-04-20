library(tmap)
library(dplyr)
library(sf)
library(RColorBrewer)

# load the dataset and spatial data
df04 <- read.csv("/home/shohibul/R Wokspace/EnPress_mans/data04.csv")
df14 <- read.csv("/home/shohibul/R Wokspace/EnPress_mans/data14.csv")
idn01 <- read_sf("/home/shohibul/KERJA/Pemetaan/propinsi/gadm36_IDN_1.shp")

# count the average land conversion and group by respective province
df04 %>%
  filter(JENIS_KONVERSI!=0) %>%
  mutate(NOM_KONVERSI=B5R2B1+B5R2B2+B5R2B3+B5R2B4+B5R2B5+B5R2B6)%>%
  group_by(PROP) %>%
  summarise(across(c("NOM_KONVERSI"),.fns=mean))

df14 %>%
  filter(JENIS_KONVERSI!=0) %>%
  mutate(NOM_KONVERSI=R503B_I1+R503B_I2+R503B_II1+R503B_II2+R503B_III1+R503B_III2)%>%
  group_by(PROP)%>%
  summarise(across(c("NOM_KONVERSI"),.fns=mean))

# join both dataset(s) with spatial
idn0104 <- left_join(idn01,
                     df04 %>%
                       filter(JENIS_KONVERSI!=0) %>%
                       mutate(NOM_KONVERSI=B5R2B1+B5R2B2+B5R2B3+B5R2B4+B5R2B5+B5R2B6)%>%
                       group_by(PROP) %>%
                       summarise(across(c("NOM_KONVERSI"),.fns=mean)),by=c("CC_1"="PROP"))

idn0114 <- left_join(idn01,
                     df14 %>%
                       filter(JENIS_KONVERSI!=0) %>%
                       mutate(NOM_KONVERSI=R503B_I1+R503B_I2+R503B_II1+R503B_II2+R503B_III1+R503B_III2)%>%
                       group_by(PROP)%>%
                       summarise(across(c("NOM_KONVERSI"),.fns=mean)),by=c("CC_1"="PROP"))

# plotting by using tmap
idn0104 %>%
  tm_shape()+
  tm_borders(alpha = 0.5)+
  tm_fill("NOM_KONVERSI",palette = "Blues",title = "Average Land Conv (SQM):",colorNA = NULL)+
  tm_layout(legend.outside = T,legend.outside.position = c("bottom","left"))+
  tm_credits("SPP 2004 (C) BPS",position = c("right","top"))-> land04

idn0114 %>%
  tm_shape()+
  tm_borders(alpha = 0.5)+
  tm_fill("NOM_KONVERSI",palette = "Blues",title = "Average Land Conv (SQM):",colorNA = NULL)+
  tm_layout(legend.outside = T,legend.outside.position = c("bottom","left"))+
  tm_credits("SPP 2014 (C) BPS",position = c("right","top"))-> land14

tmap_arrange(land04,land14)            
