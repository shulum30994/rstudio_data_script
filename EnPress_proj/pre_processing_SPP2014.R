library(tidyverse)
# library(descr)

master14 <- read.csv("/home/shohibul/KERJA/DATA/gabung_file_spp/gabung170722ok.csv",sep = ";")

master14 %>% glimpse

# Dependent :
# R503A : Selama 5 tahun yang lalu, dari lahan yang dimiliki pernah mengalami konversi
# R503B_I1 : Luas lahan sawah menjadi lahan pertanian bukan sawah
# R503B_I2 : Luas lahan sawah menjadi lahan bukan pertanian
# R503B_II1 : Luas lahan pertanian bukan sawah menjadi lahan sawah
# R503B_II2 : Luas lahan pertanian bukan sawah menjadi lahan bukan pertanian
# R503B_III1 : Luas lahan bukan pertanian menjadi lahan sawah
# R503B_III2 : Luas lahan bukan pertanian menjadi lahan pertanian bukan sawah

# Independent :
# B17_R1711A : Apakah mengalami kesulitan dalam menjual hasil pertanian?
# R501D_K6 : Jumlah lahan yang dikuasai (a + b + c) (m2)
# B3_K3 : Hubungan dengan KRT
# B3_K5 : Umur (tahun)
# B3_K7 : STTB tertinggi yang dimiliki
# jumlah_anggota_kel : Jumlah anggota keluarga (orang)

# R501E1_K6 : Jumlah penggunaan lahan pertanian yang dikuasai (1) Diusahakan
# B16F_K2 : Jumlah pendapatan rumah tangga (000 Rp)
# B16A_K2 : Pendapatan/Penerimaan (000 Rp) Usaha Sektor Pertanian R. (A.1 s.d A.18)
# R502A : Luas lahan (m2) yang bersertifikat (SHM,SHGB, SHP, SSRS)
# R502B :  Berapa luas lahan (m2) dengan bukti kepemilikan lainnya (Girik, Akta jual beli notaris/PPAT)?
# R1710A : Apakah ada anggota rumah tangga yang memanfaatkan fasilitas koperasi
# R1709A : Apakah ada anggota rumah tangga yang anggota kelompok tani?
# R1704 : Sumber pembiayaan dalam usaha pertanian
# RASIO : Rasio antara panjang jalan dengan luas propinsi

data <- master14 %>%
  select(ID_RUTA,
         #PROP_x,
         #KAB_x,
         R503A,
         R503B_I1,
         R503B_I2,
         R503B_II1,
         R503B_II2,
         R503B_III1,
         R503B_III2,
         B17_R1711A,
         R501D_K6,
         B3_K3,
         B3_K5,
         B3_K7,
         jumlah_anggota_kel,
         R501E1_K6,
         B16F_K2,
         B16A_K2,
         R502A,
         R1710A,
         R1709A,
         R1704)

ready <- read.csv("status_urban_ready.csv")

data %>%
  glimpse

nrow(
  data %>%
  select(-c("KAB_c"))
)

data<-data %>% 
  mutate(
    KATEGORI = case_when(
      R503B_I1!=0 ~ "1",
      R503B_I2!=0 ~ "2",
      R503B_II1!=0 ~ "3",
      R503B_II2!=0 ~ "4",
      R503B_III1!=0 ~ "5",
      R503B_III2!=0 ~ "6",
      TRUE ~ "X"
    )
  )

data %>% 
  group_by(KATEGORI) %>% 
  summarise(across(c("R503B_I1","R503B_I2","R503B_II1","R503B_II2","R503B_III1","R503B_III2"),.fns=mean))

data %>%
  group_by(KATEGORI) %>%
  count(KATEGORI)

data %>%
  select(KATEGORI,R503B_I1,R503B_I2,R503B_II1,R503B_II2,R503B_III1,R503B_III2) %>%
  filter(KATEGORI=="1") %>%
  arrange(desc(R503B_II2))

# data<- data %>%
#  mutate(KAB_c = if_else(KAB_x<10,paste("0",KAB_x,sep = ""),NA))

# data %>%
#  mutate(KOD_KAB=paste(PROP_x,KAB_c,sep = ""))

data14 <- left_join(ready,data,by=c("ID"="ID_RUTA"))
data14<-left_join(data14,id_rasio,by=c("PROP"="CC_1"))
data14 <- left_join(data14,tanah2,by=c("ID"="ID_RUTA"))
