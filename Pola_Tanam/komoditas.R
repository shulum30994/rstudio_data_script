padi_palawija <- gabung %>%
  select(ID_RUTA,
         PROP_x,
         KAB_x,
         B6_K2:B6D_2_K7)

# BG_K2 : Kode Padi Sawah
# B6_K3 : Kode Padi Ladang
# B6C_K2 : Pendapatan padi ladang
# B6C_K3 : Pendapatan padi sawah
# B6_NAMA_K4 : Nama Komoditas Palawija
# B6_KODE_K4 : Kode Komoditas Palawija
# B6_NAMA_K5 : Nama Komoditas Palawija 2
# B6_KODE_K5 : Kode Komoditas Palawija 2
# B6_NAMA_K6 : Nama Komoditas Palawija 3
# B6_KODE_K6 : Kode Komoditas Palawija 3
# B6_NAMA_K7 : Nama Komoditas Palawija 4
# B6_KODE_K7 : Kode Komoditas Palawija 4

padi_palawija<-padi_palawija %>%
  mutate(KOMODITAS=case_when(B6C_K2!=0|B6C_K2!=0~"PADI",
                             B6_KODE_K4!=0~"PALAWIJA",
                             B6_KODE_K5!=0~"PALAWIJA",
                             B6_KODE_K6!=0~"PALAWIJA",
                             B6_KODE_K7!=0~"PALAWIJA",
         B6_KODE_K4!=0|B6C_K2!=0~"PADI-PALAWIJA",
         B6_KODE_K5!=0|B6C_K2!=0~"PADI-PALAWIJA",
         B6_KODE_K6!=0|B6C_K2!=0~"PADI-PALAWIJA",
         B6_KODE_K7!=0|B6C_K2!=0~"PADI-PALAWIJA",
         B6_KODE_K4!=0|B6C_K3!=0~"PADI-PALAWIJA",
         B6_KODE_K5!=0|B6C_K3!=0~"PADI-PALAWIJA",
         B6_KODE_K6!=0|B6C_K3!=0~"PADI-PALAWIJA",
         B6_KODE_K7!=0|B6C_K3!=0~"PADI-PALAWIJA"))

padi_palawija %>%
  distinct(B6_KODE_K5)

padi_palawija %>%
  replace(is.na(padi_palawija),"")%>%
  #na.omit()%>%
  group_by(ID_RUTA) %>%
  select(ID_RUTA,KOMODITAS1,KOMODITAS2,KOMODITAS3,KOMODITAS4,KOMODITAS5) %>%
  mutate(POLA=paste(KOMODITAS1,KOMODITAS2,KOMODITAS3,KOMODITAS4,KOMODITAS5,sep = "-"))%>%
  distinct(POLA)
