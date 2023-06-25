library(dplyr)
library(ResourceSelection)
library(DescTools)
library(stargazer)
library(foreign)

spp204 <- read.dbf("/home/shohibul/KERJA/DATA/Buku Analisis Lahan/Referensi/spp2.dbf")
demogspp <- read.dbf("/home/shohibul/KERJA/DATA/Buku Analisis Lahan/Referensi/demogspp.dbf")

mentahan <- spp204 %>% select(IDENTITAS,PROP,B16RF,B17R5) %>% na.omit()

# hitung kepala jumlah anggota keluarga
keluarga<-demogspp %>%
  select(IDENTITAS,B3K3) %>%
  filter(B3K3!=1) %>%
  group_by(IDENTITAS) %>%
  count(IDENTITAS)

pendidikan<-demogspp %>%
  filter(B3K3==1) %>%
  select(IDENTITAS,B3K6)
  
# gabungkan data
mentahan <- merge(mentahan,keluarga,by="IDENTITAS",all = TRUE)
mentahan <- mentahan%>%rename(AGG_KEL=n)
mentahan <- merge(mentahan,pendidikan,by="IDENTITAS",all = TRUE)
mentahan[is.na(mentahan)]<-0

# variabel baru dari variabel yang telah ada
