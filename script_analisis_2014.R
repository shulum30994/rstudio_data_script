library(DescTools)
library(data.table)
library(dplyr)
library(ResourceSelection)
library(stargazer)

spp14 <- read.csv("C:/Users/riset/Documents/1. PERFORMA CENDEKIA/2023/khusus_spp2014/gabung170722ok.csv",sep = ";")

LAHAN14 <-fread("C:/Users/riset/Documents/1. PERFORMA CENDEKIA/2023/khusus_spp2014/gabung170722ok.csv",sep = ";",select = c("ID_RUTA","R501B_K6","R501C_K6","R501D_K6","B16A_K2"))

LAHAN14 <- spp14 %>% select(ID_RUTA,R501B_K6,R501C_K6,R501D_K6,B16A_K2)
siap14 <-
  siap14 %>% mutate(TRANSFER_LAHAN=if_else(R501B_K6!=0 & R501C_K6!=0,1,if_else(R501B_K6!=0 & R501C_K6==0,1,if_else(R501B_K6==0 & R501C_K6!=0,1,0))))
siap14 <- siap14 %>% mutate(AGR_RATIO = R501E1_K6/R501D_K6)

mentah <- spp14 %>% select(ID_RUTA,PROP_x,R504A,B3_K4,B3_K5,B3_K7,B16F_K2,jumlah_anggota_kel,R1709A,R1710A,R1705A) %>% na.omit()

mentah <- left_join(mentah,sertipikat,by=c("ID_RUTA"="ID_RUTA"))

siap14<-mentah %>%
  mutate(
    TRANSFER = case_when(R504A==2~0,R504A==1~1),
    DSD = if_else(B3_K7==2,1,0),
    DSMP = if_else(B3_K7==3,1,0),
    DSMA = if_else(B3_K7==4,1,0),
    DPT = if_else(B3_K7 %in% c(5,6,7,8),1,0),
    GENDER = if_else(B3_K4==1,1,0),
    KREDIT = if_else(R1705A==1,1,0),
    KELOMPOK = if_else(R1709A==1,1,0),
    KOPERASI = if_else(R1710A==1,1,0),
    SERTIPIKAT=case_when(R502A==0|R502B==0 ~ 0,R502A!=0|R502B==0 ~ 1,R502A==0|R502B!=0 ~ 1),
	DSUMATRA = if_else(PROP_x %in% c(11,12,13,14,15,16,17,18,18,21),1,0),
	DKLISUL = if_else(PROP_x %in% c(61,62,63,64,65,71,72,73,74,75,76),1,0),
	DBALINT = if_else(PROP_x %in% c(51,52,53),1,0),
	DMAPU = if_else(PROP_x %in% c(81,82,91,92),1,0)
  )
siap14 <- siap14 %>% mutate(DJAWA = if_else(PROP_x %in% c(36,34,31,32,33,35),1,0))
siap14<-siap14 %>% mutate(
  IN_RATIO=if_else(R501B_K6!=0|R501D_K6!=0,R501B_K6/R501D_K6,0),
  OUT_RATIO=if_else(R501C_K6!=0|R501D_K6!=0,R501C_K6/R501D_K6,0),
  IN_OUT=R501B_K6+R501C_K6,
  TRANSFER_RATIO= if_else(IN_OUT!=0|R501D_K6!=0,IN_OUT/R501D_K6,0)
  )

# Model
REG_NUL14 <- glm(as.factor(TRANSFER_LAHAN) ~ 1, family="binomial",data=siap14)
MODEL14 <- glm(as.factor(TRANSFER) ~  B3_K5+R501D_K6+as.factor(GENDER)+as.factor(DSD)+as.factor(DSMP)+as.factor(DSMA)+as.factor(DPT)+jumlah_anggota_kel+B16F_K2+as.factor(DSUMATRA)+as.factor(DKLISUL)+as.factor(DBALINT)+as.factor(DMAPU)+as.factor(KOPERASI)+as.factor(KELOMPOK)+as.factor(KREDIT)+as.factor(SERTIPIKAT), family="binomial",data=siap14)

# Proposed Model 1
MODEL14_1 <- glm(as.factor(TRANSFER_LAHAN) ~  R501D_K6+R501E1_K6+B3_K5+as.factor(GENDER)+jumlah_anggota_kel+B16F_K2+B16A_K2+as.factor(DSD)+as.factor(DSMP)+as.factor(DSMA)+as.factor(DPT)+as.factor(DSUMATRA)+as.factor(DKLISUL)+as.factor(DBALINT)+as.factor(DMAPU)+as.factor(KOPERASI)+as.factor(KELOMPOK)+as.factor(KREDIT)+as.factor(SERTIPIKAT), family="binomial",data=siap14)

# Proposed Model 2
MODEL14_2 <- glm(as.factor(TRANSFER_LAHAN) ~  R501D_K6+R501E1_K6+B3_K5+as.factor(GENDER)+jumlah_anggota_kel+B16F_K2+as.factor(DSD)+as.factor(DSMP)+as.factor(DSMA)+as.factor(DPT)+as.factor(DSUMATRA)+as.factor(DKLISUL)+as.factor(DBALINT)+as.factor(DMAPU)+as.factor(KOPERASI)+as.factor(KELOMPOK)+as.factor(KREDIT)+as.factor(SERTIPIKAT), family="binomial",data=siap14)

# Proposed Model 3
MODEL14_3 <- glm(as.factor(TRANSFER_LAHAN) ~ AGR_RATIO+B3_K5+as.factor(GENDER)+jumlah_anggota_kel+B16A_K2+as.factor(DSD)+as.factor(DSMP)+as.factor(DSMA)+as.factor(DPT)+as.factor(DSUMATRA)+as.factor(DKLISUL)+as.factor(DBALINT)+as.factor(DMAPU)+as.factor(KOPERASI)+as.factor(KELOMPOK)+as.factor(KREDIT)+as.factor(SERTIPIKAT), family="binomial",data=siap14)

stargazer(MODEL14_1, type="text")
PseudoR2(MODEL14_1,which = c("McFadden","McFaddenAdj","Negelkerke","Coxsnell"))
