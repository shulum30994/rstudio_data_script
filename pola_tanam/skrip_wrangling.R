library(foreign)
library(dplyr)
library(udpipe)

data1 <- read.dbf("/home/shohibui/KERJA/DATA/SPP_2014/DATA/3400/blok1,4-6.dbf") #padi
data2 <- read.dbf("/home/shohibui/KERJA/DATA/SPP_2014/DATA/3400/blok7-8.dbf") #hortikultura
data3 <- read.dbf("/home/shohibui/KERJA/DATA/SPP_2014/DATA/3400/blok12-13.dbf") # kehutanan
tanaman <- read.csv("KODE_TAN_BPS.csv")

pangan <- data1 %>% select(ID_RUTA,PROP,KAB,B6_K2:B6D_2_K7)
horti <- data2 %>% select(ID_RUTA,KAB,B7_NAMA_K2:B7D_K7)
perkebunan <- data2 %>% select(ID_RUTA,KAB,B8_NAMA_K2:B8D_K7)
kehutanan <- data3 %>% select(ID_RUTA,KAB,B12_NAMAK2:B12D_2_K7)

pola_tanam <-
  pangan %>%
  select(ID_RUTA,
         KAB,
         B6_K2,
         B6_K3,
         B6_KODE_K4,
         B6_KODE_K5,
         B6_KODE_K6,
         B6_KODE_K7) %>%
  left_join(horti %>%
              select(ID_RUTA,
                     B7_KODE_K2,
                     B7_KODE_K3,
                     B7_KODE_K4,
                     B7_KODE_K5,
                     B7_KODE_K6,
                     B7_KODE_K7), by="ID_RUTA") %>%
  left_join(perkebunan %>%
              select(ID_RUTA,
                     B8_KODE_K2,
                     B8_KODE_K3,
                     B8_KODE_K4,
                     B8_KODE_K5,
                     B8_KODE_K6,
                     B8_KODE_K7,), by="ID_RUTA") %>%
  left_join(kehutanan %>%
              select(ID_RUTA,
                     B12_KODEK2,
                     B12_KODEK3,
                     B12_KODEK4,
                     B12_KODEK5,
                     B12_KODEK6,
                     B12_KODE_K
              ), by="ID_RUTA")

pola_tanam<-pola_tanam %>%
  mutate(PADI=case_when(
    B6_K2!=0~"PADI",
    B6_K3!=0~"PADI",
    .default = NA
  ),
  PALAWIJA=case_when(
    B6_KODE_K4!=0~"PALAWIJA",
    B6_KODE_K5!=0~"PALAWIJA",
    B6_KODE_K6!=0~"PALAWIJA",
    B6_KODE_K7!=0~"PALAWIJA",
    .default = NA
  ),
  HORTI=case_when(
    B7_KODE_K2!=0~"HORTI",
    B7_KODE_K3!=0~"HORTI",
    B7_KODE_K4!=0~"HORTI",
    B7_KODE_K5!=0~"HORTI",
    B7_KODE_K6!=0~"HORTI",
    B7_KODE_K7!=0~"HORTI",
    .default = NA
  ),
  PERKEBUNAN=case_when(
    B8_KODE_K2!=0~"PERKEBUNAN",
    B8_KODE_K3!=0~"PERKEBUNAN",
    B8_KODE_K4!=0~"PERKEBUNAN",
    B8_KODE_K5!=0~"PERKEBUNAN",
    B8_KODE_K6!=0~"PERKEBUNAN",
    B8_KODE_K7!=0~"PERKEBUNAN",
    .default = NA
  ),
  KEHUTANAN=case_when(
    B12_KODEK2!=0~"KEHUTANAN",
    B12_KODEK3!=0~"KEHUTANAN",
    B12_KODEK4!=0~"KEHUTANAN",
    B12_KODEK5!=0~"KEHUTANAN",
    B12_KODEK6!=0~"KEHUTANAN",
    B12_KODE_K!=0~"KEHUTANAN",
    .default = NA
  )
         )
pola_tanam<-pola_tanam %>%
  mutate(POLA=txt_paste(PADI,PALAWIJA,HORTI,PERKEBUNAN,KEHUTANAN,na.rm = T))

pola_tanam %>%
  select(POLA) %>%
  na.omit() %>%
  group_by(POLA) %>%
  count(POLA, sort = T) %>%
  mutate(PERC=(n/376650)*100) %>%
  print(n=32)

pola_tanam %>%
  select(POLA) %>%
  filter(grepl("PADI",POLA)) %>%
  na.omit() %>%
  group_by(POLA) %>%
  count(POLA, sort = T) %>%
  print(n=32)

pola_tanam %>%
  filter(grepl("PERKEBUNAN",POLA)) %>%
  group_by(PROP_x) %>%
  count(PROP_x)

POLA_HUT <- pola_tanam %>%
  filter(grepl("KEHUTANAN",POLA)) %>%
  group_by(PROP_x) %>%
  count(PROP_x)

POLA_HUT <- left_join(POLA_HUT,kode_propinisi,by=c("PROP_x"="kode"))
