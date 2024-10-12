library(dplyr)
library(stargazer) # saved result to .txt files
library(nnet) # multinomial estimation package

master14 <- read.csv("/gabung170722ok.csv",sep = ";") # joined SPP 2014 csv file
urban_status <- read.csv("/status_urban_ready.csv") # urban status csv file

# regrooup cropping patterns categories into five
pola_tanam<-pola_tanam %>%
  mutate(DOMINAN=case_when(
    POLA=="PERKEBUNAN" ~ "SINGLE",
    POLA=="PADI" ~ "SINGLE",
    POLA=="PADI PERKEBUNAN" ~ "DOUBLE",
    POLA=="HORTI PERKEBUNAN" ~ "DOUBLE",
    POLA=="PADI HORTI PERKEBUNAN" ~ "TRIPLE",
    POLA=="PALAWIJA HORTI" ~ "DOUBLE",
    POLA=="HORTI" ~ "SINGLE",
    POLA=="PALAWIJA PERKEBUNAN" ~ "DOUBLE",
    POLA=="PALAWIJA HORTI PERKEBUNAN" ~ "TRIPLE",
    POLA=="PADI PALAWIJA HORTI" ~ "TRIPLE",
    POLA=="PADI PALAWIJA HORTI PERKEBUNAN" ~ "QUADRUPLE",
    POLA=="PALAWIJA" ~ "SINGLE",
    POLA=="PADI HORTI" ~ "DOUBLE",
    POLA=="PADI PALAWIJA PERKEBUNAN" ~ "TRIPLE",
    POLA=="PADI PALAWIJA" ~ "DOUBLE",
    POLA=="PADI PERKEBUNAN KEHUTANAN" ~ "TRIPLE",
    POLA=="PADI HORTI PERKEBUNAN KEHUTANAN" ~ "QUADRUPLE",
    POLA=="PERKEBUNAN KEHUTANAN"~ "DOUBLE",
    POLA=="PADI PALAWIJA PERKEBUNAN KEHUTANAN" ~ "QUADRUPLE",
    POLA=="KEHUTANAN" ~ "SINGLE",
    POLA=="PALAWIJA HORTI PERKEBUNAN KEHUTANAN" ~ "QUADRUPLE",
    POLA=="PADI PALAWIJA HORTI PERKEBUNAN KEHUTANAN" ~ "ALL",
    POLA=="HORTI PERKEBUNAN KEHUTANAN" ~ "TRIPLE",
    POLA=="PADI KEHUTANAN"~ "DOUBLE",
    POLA=="HORTI KEHUTANAN" ~ "DOUBLE",
    POLA=="PADI HORTI KEHUTANAN" ~ "TRIPLE",
    POLA=="PADI PALAWIJA HORTI KEHUTANAN" ~ "QUADRUPLE",
    POLA=="PADI PALAWIJA KEHUTANAN" ~ "TRIPLE",
    POLA=="PALAWIJA PERKEBUNAN KEHUTANAN" ~ "TRIPLE",
    POLA=="PALAWIJA KEHUTANAN"~ "DOUBLE",
    POLA=="PALAWIJA HORTI KEHUTANAN" ~ "TRIPLE",
    TRUE ~ "NONE"
  ))

ready <- data %>% select(ID_RUTA,PROP_x,KAB_x,DOMINAN) # select certain column from regrouped category
# select independen variables from master14 dataframe
independen <- master14 %>%
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
         B3_K4,
         B3_K3,
         B3_K5,
         B3_K7,
         jumlah_anggota_kel,
         R501E1_K6,
         B16F_K2,
         B16A_K2,
         B17_R1711A,
         R502A,
         R502B,
         R1710A,
         R1709A,
         R1704,
         R1705A
         )

data_fix <- left_join(ready,independen,by=c("ID_RUTA"="ID_RUTA")) # join independen variables and crop pattern dataframe
data_fix <- left_join(data_fix,urban_status,by=c("ID_RUTA"="ID")) # join with urban status dataframe
# recode the independen variables into new column(s)
data_fix<-data_fix %>%
  na.omit() %>%
  mutate(
    GENDER = if_else(B3_K4==1,1,0),
    KONVERSI_LAHAN = if_else(R503A==1,1,0),
    JENIS_KONVERSI = if_else(
      R503B_I1!=0|R503B_I2!=0,1,
      if_else(R503B_II1!=0|R503B_II2!=0,2,
              if_else(R503B_III1!=0|R503B_III2!=0,3,0))
    ),
    ELEMENTARY = case_when(B3_K7==2 ~ 1, TRUE ~ 0),
    MID = case_when((B3_K7==3)|(B3_K7==4) ~ 1, TRUE ~ 0),
    UNIV = case_when((B3_K7==5)|(B3_K7==6)|(B3_K7==7)|(B3_K7==8) ~ 1, TRUE ~ 0),
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
    ))

#### MODEL ESTIMATION ####
data_fix$DOMINAN <- factor(
  data_fix$DOMINAN,
  ordered = T,
  levels = c("NONE","SINGLE","DOUBLE","TRIPLE","QUADRUPLE","ALL")) # re-level category in Y variable
cropat1 <- multinom(data=data_fix %>% na.omit(),
                    DOMINAN~as.factor(AKSES_PASAR)+
                      as.factor(GENDER)+
                      R501D_K6+
                      R501E1_K6+
                      B3_K5+
                      as.factor(ELEMENTARY)+
                      as.factor(MID)+
                      as.factor(UNIV)+
                      jumlah_anggota_kel+
                      B16F_K2+
                      B16A_K2+
                      as.factor(SERTIPIKAT)+
                      as.factor(KOPERASI)+
                      as.factor(POKTAN)+
                      as.factor(KREDIT)+
                      as.factor(DSUMA)+
                      as.factor(DKALI)+
                      as.factor(DBALI)+
                      as.factor(DSULA)+
                      as.factor(DPAPU)+
                      as.factor(DMALU)+
                      as.factor(URBAN))

summary(cropat1) # show the estimation result

stargazer(cropat1,
          #poktan.remit.urban,
          #koperasi.remit.urban,
          title = "Reference:TIDAK menanam/berusaha tani",
          #column.labels = c(),
          intercept.bottom = FALSE,
          #apply.coef = exp,
          p.auto = FALSE,
          t.auto = FALSE,
          digits = 2,
          report = ('vc*p'),
          type = "text",
          out = "multikurtur.txt") # saved estimeation result to .txt file
