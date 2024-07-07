library(nnet)
library(dplyr)

data14 <- read.csv("/home/shohibul/R Wokspace/EnPress_mans/data14.csv")

data14$KONVERSI_LAHAN <- as.factor(data14$KONVERSI_LAHAN)

data14$KONVERSI_LAHAN <- factor(
	data14$KONVERSI_LAHAN,
	ref=0)
	
MODEL_FIX <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+Total_Panjang+as.factor(DSUMA)+as.factor(DKALI)+as.factor(DBALI)+as.factor(DSULA)+as.factor(DPAPU)+as.factor(DMALU)+as.factor(URBAN),data=data14 %>% na.omit)

MODEL_1<- multinom(as.factor(KONVERSI_LAHAN)~1,data=data14 %>% na.omit)

MODEL_2 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR),data=data14 %>% na.omit)

MODEL_3 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER),data=data14 %>% na.omit)

MODEL_4 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6,data=data14 %>% na.omit)

MODEL_5 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6,data=data14 %>% na.omit)

MODEL_6 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5,data=data14 %>% na.omit)

MODEL_7 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY),data=data14 %>% na.omit)

MODEL_8 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID),data=data14 %>% na.omit)

MODEL_9 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV),data=data14 %>% na.omit)

MODEL_10 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel,data=data14 %>% na.omit)

MODEL_11 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2,data=data14 %>% na.omit)

MODEL_12 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2,data=data14 %>% na.omit)

MODEL_13 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT),data=data14 %>% na.omit)

MODEL_14 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI),data=data14 %>% na.omit)

MODEL_15 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN),data=data14 %>% na.omit)

MODEL_16 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT),data=data14 %>% na.omit)

MODEL_17 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+Total_Panjang,data=data14 %>% na.omit)

MODEL_18 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+Total_Panjang+as.factor(DSUMA),data=data14 %>% na.omit)

MODEL_19 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+Total_Panjang+as.factor(DSUMA)+as.factor(DKALI),data=data14 %>% na.omit)

MODEL_20 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+Total_Panjang+as.factor(DSUMA)+as.factor(DKALI)+as.factor(DBALI),data=data14 %>% na.omit)

MODEL_21 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+Total_Panjang+as.factor(DSUMA)+as.factor(DKALI)+as.factor(DBALI)+as.factor(DSULA),data=data14 %>% na.omit)

MODEL_22 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+Total_Panjang+as.factor(DSUMA)+as.factor(DKALI)+as.factor(DBALI)+as.factor(DSULA)+as.factor(DPAPU),data=data14 %>% na.omit)

MODEL_23 <- multinom(as.factor(KONVERSI_LAHAN)~as.factor(AKSES_PASAR)+as.factor(GENDER)+R501D_K6+R501E1_K6+B3_K5+as.factor(ELEMENTARY)+as.factor(MID)+as.factor(UNIV)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(SERTIPIKAT)+as.factor(KOPERASI)+as.factor(POKTAN)+as.factor(KREDIT)+Total_Panjang+as.factor(DSUMA)+as.factor(DKALI)+as.factor(DBALI)+as.factor(DSULA)+as.factor(DPAPU)+as.factor(DMALU),data=data14 %>% na.omit)

anova(MODEL_1,
      MODEL_2,
      MODEL_3,
      MODEL_4,
      MODEL_5,
      MODEL_6,
      MODEL_7,
      MODEL_8,
      MODEL_9,
      MODEL_10,
      MODEL_11,
      MODEL_12,
      MODEL_13,
      MODEL_14,
      MODEL_15,
      MODEL_16,
      MODEL_17,
      MODEL_18,
      MODEL_19,
      MODEL_20,
      MODEL_21,
      MODEL_22,
      MODEL_23)

pred.multinom <- predict(MODEL_FIX,data14)

head(pred.multinom)

pprob <- predict(MODEL_FIX,data14,type="p")
head(pprob)

pred_accuracy <- mean(pred.multinom==data14$KONVERSI_LAHAN)
pred_accuracy
