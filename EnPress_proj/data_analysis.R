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
    )
  )

MODEL1 <- glm(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+R501D_K6+R501E1_K6+B3_K5+as.factor(B3_K7)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+RASIO+as.factor(ISLAND)+as.factor(URBAN),data14 %>% filter(PROP!="31") %>% na.omit(),family = "binomial")

MULTINOM1 <- multinom(as.factor(JENIS_KONVERSI)~as.factor(AKSES_PASAR)+R501D_K6+R501E1_K6+B3_K5+as.factor(B3_K7)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+Total_Panjang+as.factor(DSUMA)+as.factor(DKALI)+as.factor(DBALI)+as.factor(DSULA)+as.factor(DPAPU)+as.factor(DPAPU)+as.factor(URBAN),data=data14 %>% filter(PROP!="31") %>% na.omit())

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
