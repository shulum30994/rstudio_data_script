library(ResourceSelection)
library(DescTools)
library(stargazer)
library(nnet)

# Pedoman Konversi

data14<-data14 %>%
  mutate(
    KONVERSI_LAHAN = if_else(R503A==1,1,0),
    JENIS_KONVERSI = if_else(
      R503B_I1!=0|R503B_I2!=0,1,
      if_else(R503B_II1!=0|R503B_II2!=0,2,
              if_else(R503B_III1!=0|R503B_III2!=0,3,0))
    ),
    #JENIS_KONVERSI2 = case_when(
    #	(R503B_I1!=0 & R503B_I2!=0 & R503B_II1!=0 & R503B_II2!=0 & R503B_III1!=0 & R503B_III2!=0) ~ 1,
    #	(R503B_I1!=0 & R503B_II1!=0) ~ 2,
    #	(R503B_I2!=0 & R503B_III1!=0) ~ 3,
    #	(R503B_II2!=0 & R503B_III2!=0) ~ 4,
    #	TRUE ~ 0
    #	),
    AKSES_PASAR = if_else(B17_R1711A==1,1,0),
    SERTIPIKAT=if_else(R502A!=0|R502B!=0,1,0),
    KOPERASI=if_else(R1710A==1,1,0),
    POKTAN=if_else(R1709A==1,1,0),
    KREDIT=if_else(R1705A==1,1,0),
    URBAN=if_else(URBAN_STAT=="DESA",1,0),
    ISLAND=case_when(
      PROP=="31" ~ 1,
      PROP=="32" ~ 1,
      PROP=="33" ~ 1,
      PROP=="34" ~ 1,
      PROP=="35" ~ 1,
      PROP=="36" ~ 1,
      TRUE ~ 0
    ),
    DSUMA = case_when(
      PROP=="11" ~ 1,
      PROP=="12" ~ 1,
      PROP=="13" ~ 1,
      PROP=="14" ~ 1,
      PROP=="15" ~ 1,
      PROP=="16" ~ 1,
      PROP=="17" ~ 1,
      PROP=="18" ~ 1,
      PROP=="19" ~ 1,
      PROP=="21" ~ 1,
      TRUE ~ 0
    ),
    DBALI = case_when(
      PROP=="51" ~ 1,
      PROP=="52" ~ 1,
      PROP=="53" ~ 1,
      TRUE ~ 0
    ),
    DKALI = case_when(
      PROP=="61" ~ 1,
      PROP=="62" ~ 1,
      PROP=="63" ~ 1,
      PROP=="64" ~ 1,
      TRUE ~ 0
    ),
    DSULA = case_when(
      PROP=="71" ~ 1,
      PROP=="72" ~ 1,
      PROP=="73" ~ 1,
      PROP=="74" ~ 1,
      PROP=="75" ~ 1,
      PROP=="76" ~ 1,
      TRUE ~ 0
    ),
    DMALU = case_when(
      PROP=="81" ~ 1,
      PROP=="82" ~ 1,
      TRUE ~ 0,
    ),
    DPAPU = case_when(
      PROP=="91" ~ 1,
      PROP=="94" ~ 1,
      TRUE ~ 0,
    ),
    S_NS=if_else(R503B_I1!=0,"S_NS",""),
    S_NP=if_else(R503B_I2!=0,"S_NP",""),
    NS_S=if_else(R503B_II1!=0,"NS_S",""),
    NS_NP=if_else(R503B_II2!=0,"NS_NP",""),
    NP_S=if_else(R503B_III1!=0,"NP_S",""),
    NP_NS=if_else(R503B_III2!=0,"NP_NS",""),
    KOMBINASI = gsub(",{2,}", ",", 
                     gsub("^,+|,+$", "", paste(S_NS,S_NP,NS_S,NS_NP,NP_S,NP_NS,sep = ","))),
    KONVERSI_LAHAN = case_when(
	  KOMBINASI == 'NP_NS'~1,
    KOMBINASI == 'S_NS'~2,
    KOMBINASI == 'NS_NP'~3,
    KOMBINASI == 'NS_S'~1,
    KOMBINASI == 'S_NP'~2,
    KOMBINASI == 'NP_S'~1,
    KOMBINASI == 'NS_NP,NP_NS'~1,
    KOMBINASI == 'S_NS,S_NP'~1,
    KOMBINASI == 'S_NS,NS_S'~1,
    KOMBINASI == 'S_NS,NP_NS'~1,
    KOMBINASI == 'NP_S,NP_NS'~1,
    KOMBINASI == 'S_NS,NS_NP'~1,
    KOMBINASI == 'NS_S,NP_NS'~1,
    KOMBINASI == 'S_NP,NS_NP'~2,
    KOMBINASI == 'S_NP,NP_NS'~1,
    KOMBINASI == 'NS_S,NS_NP'~2,
    KOMBINASI == 'S_NP,NS_S'~2,
    KOMBINASI == 'NS_S,NP_S'~1,
    KOMBINASI == 'S_NS,NP_S'~1,
    KOMBINASI == 'S_NS,S_NP,NP_NS'~1,
    KOMBINASI == 'S_NS,S_NP,NS_S'~1,
    KOMBINASI == 'S_NP,NP_S'~1,
    KOMBINASI == 'S_NS,NS_NP,NP_NS'~3,
    KOMBINASI == 'S_NS,NS_S,NP_NS'~1,
    KOMBINASI == 'S_NS,S_NP,NS_NP'~2,
    KOMBINASI == 'NS_NP,NP_S,NP_NS'~1,
    KOMBINASI == 'NS_S,NS_NP,NP_NS'~1,
    KOMBINASI == 'S_NP,NS_NP,NP_NS'~2,
    KOMBINASI == 'S_NS,NP_S,NP_NS'~1,
    KOMBINASI == 'S_NS,S_NP,NS_NP,NP_NS'~2,
    KOMBINASI == 'S_NS,S_NP,NS_S,NS_NP'~1,
    KOMBINASI == 'NS_NP,NP_S'~3,
    KOMBINASI == 'S_NP,NP_S,NP_NS'~1,
    KOMBINASI == 'S_NP,NS_NP,NP_S'~3,
    KOMBINASI == 'S_NP,NS_S,NP_S'~3,
    KOMBINASI == 'S_NS,NS_S,NP_S'~3,
    KOMBINASI == 'S_NS,NS_S,NS_NP'~1,
    KOMBINASI == 'S_NS,S_NP,NS_S,NP_NS'~1,
    KOMBINASI == 'S_NS,S_NP,NS_S,NS_NP,NP_S,NP_NS'~1,
    TRUE ~ 0
	),
	RASIO_KONVERSI = (((R503B_I1+R503B_I2+R503B_II1+R503B_II2+R503B_III1+R503B_III2)/R501D_K6)*100)
  )
data14 %>% group_by(KOMBINASI) %>% count(KOMBINASI) %>% arrange(desc(n)) %>% print(n=40)

data14 %>% group_by(URBAN_STAT) %>% count(as.factor(KONVERSI_LAHAN)) # status urban

data14 %>% group_by(B3_K7) %>% count(as.factor(KONVERSI_LAHAN)) # spendidikan

data14 %>% group_by(SERTIPIKAT) %>% count(as.factor(KONVERSI_LAHAN)) # sertifikat tanah

data14 %>% group_by(KOPERASI) %>% count(as.factor(KONVERSI_LAHAN)) # koperasi

data14 %>% group_by(POKTAN) %>% count(as.factor(KONVERSI_LAHAN)) # poktan

data14 %>% group_by(KREDIT) %>% count(as.factor(KONVERSI_LAHAN)) # poktan

MODEL1 <- glm(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+R501D_K6+R501E1_K6+B3_K5+as.factor(B3_K7)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+RASIO+as.factor(ISLAND)+as.factor(URBAN),data14 %>% filter(PROP!="31") %>% na.omit(),family = "binomial")

MULTINOM1 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+R501D_K6+R501E1_K6+B3_K5+as.factor(B3_K7)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+Total_Panjang+as.factor(DSUMA)+as.factor(DKALI)+as.factor(DBALI)+as.factor(DSULA)+as.factor(DPAPU)+as.factor(DPAPU)+as.factor(URBAN),data=data14 %>% na.omit())

stargazer(MULTINOM1,
          #poktan.remit.urban,
          #koperasi.remit.urban,
          title = "Reference:Tidak Konversi, 1=Konversi Sawah, 2=Konversi Non sawah, 3=Konversi Non Pertanian",
          #column.labels = c(),
          intercept.bottom = FALSE,
          #apply.coef = exp,
          p.auto = FALSE,
          t.auto = FALSE,
          digits = 2,
          report = ('vc*p'),
          type = "text",
          out = "multinom1c.txt")
