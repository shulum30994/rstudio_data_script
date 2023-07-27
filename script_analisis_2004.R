library(dplyr)
library(ResourceSelection)
library(DescTools)
library(stargazer)
library(foreign)

spp104 <- read.dbf("C:/Users/riset/Documents/1. PERFORMA CENDEKIA/2023/spp04/spp1.dbf")
spp204 <- read.dbf("C:/Users/riset/Documents/1. PERFORMA CENDEKIA/2023/spp04/spp2.dbf")
demogspp <- read.dbf("C:/Users/riset/Documents/1. PERFORMA CENDEKIA/2023/spp04/demogspp.dbf")

mentahan04 <- spp204 %>% select(IDENTITAS,PROP,B16RF,B17R5) %>% na.omit()
transfer_lahan <- spp104 %>% select(IDENTITAS,B5R3A) %>% na.omit()
LAHAN04 <- spp104 %>% select(IDENTITAS,B5R1DK5,B5R1BK5,B5R1CK5) %>% mutate(TRANSFER_LAHAN = case_when(B5R1BK5==0|B5R1CK5==0~0,B5R1BK5!=0|B5R1CK5!=0~1,B5R1BK5==0|B5R1CK5!=0~1,B5R1BK5!=0|B5R1CK5==0~1))
siap04$B16RA <- as.numeric(levels(siap04$B16RA)[siap04$B16RA])
siap04$RENTIN <- as.numeric(levels(siap04$B5R1BK5)[siap04$B5R1BK5])
siap04$RENTOUT <- as.numeric(levels(siap04$B5R1CK5)[siap04$B5R1CK5])
siap04$B5R1DK5 <- as.numeric(levels(siap04$B5R1DK5)[siap04$B5R1DK5])
pendapatan <- spp204 %>% select(IDENTITAS,B16RA)
siap04 <- siap04 %>% mutate(TRANSFER_LAHAN=if_else(RENTIN!=0 & RENTOUT!=0,1,if_else(RENTIN!=0 & RENTOUT==0,1,if_else(RENTIN==0 & RENTOUT!=0,1,0))))
siap04 <- siap04 %>% mutate(AGR_RATIO=B5R1E1K5/B5R1DK5)

# hitung kepala jumlah anggota keluarga
keluarga<-demogspp %>%
  select(IDENTITAS,B3K3) %>%
  filter(B3K3!=1) %>%
  group_by(IDENTITAS) %>%
  count(IDENTITAS)

demo<-demogspp %>%
  filter(B3K3==1) %>%
  select(IDENTITAS,B3K4,B3K5,B3K6) %>%
  na.omit()
  
# gabungkan data
mentahan04 <- merge(mentahan04,transfer_lahan,by="IDENTITAS",all = TRUE)
mentahan04 <- merge(mentahan04,keluarga,by="IDENTITAS",all = TRUE)
mentahan04 <- mentahan04%>%rename(AGG_KEL=n)
mentahan04 <- merge(mentahan04,demo,by="IDENTITAS",all = TRUE)
mentahan04[is.na(mentahan04)]<-0
siap04 <- mentahan04 %>% na.omit()

# variabel baru dari variabel yang telah ada
siap04<-siap04 %>%
  mutate(TRANSFER=if_else(B5R3A==1,1,0),
         GENDER=if_else(B3K4==1,1,0),
         DSD = if_else(B3K6==2,1,0),
         DSMP=if_else(B3K6==3,1,0),
         DSMA=if_else(B3K6==4,1,0),
         DPT=if_else(B3K6 %in% c(5,6,7),1,0),
         DKREDIT=if_else(B17R5==2,1,0),
         DSUMATRA = if_else(PROP %in% c(11,12,13,14,15,16,17,18,18,21),1,0),
         DKLISUL = if_else(PROP %in% c(61,62,63,64,65,71,72,73,74,75,76),1,0),
         DBALINT = if_else(PROP %in% c(51,52,53),1,0),
         DMAPU = if_else(PROP %in% c(81,82,91,92),1,0))
siap04 <- siap04 %>% mutate(DJAWA=if_else(PROP %in% c(36,34,31,32,33,35),1,0))
siap04<-siap04 %>% mutate(
  IN_RATIO = if_else(RENTIN!=0|B5R1DK5!=0,RENTIN/B5R1DK5,0),
  OUT_RATIO= if_else(RENTOUT!=0|B5R1DK5!=0,RENTOUT/B5R1DK5,0),
  IN_OUT = RENTIN+RENTOUT,
  TRANSFER_RATIO= if_else(IN_OUT!=0|B5R1DK5!=0,IN_OUT/B5R1DK5,0)
  )
# model
REG_NUL04 <- glm(as.factor(TRANSFER_LAHAN) ~ 1,family = "binomial",data = siap04)

MODEL04 <- glm(as.factor(TRANSFER) ~ B3K5+B5R1E1K5+as.factor(GENDER)+as.factor(DSD)+as.factor(DSMP)+as.factor(DSMA)+as.factor(DPT)+as.factor(DSUMATRA)+as.factor(DKLISUL)+as.factor(DBALINT)+as.factor(DMAPU)+AGG_KEL+B16RF+as.factor(DKREDIT),family = "binomial",data = siap04)

# Proposed Model 1
MODEL04_1 <- glm(as.factor(TRANSFER_LAHAN) ~ B5R1DK5+B5R1E1K5+B3K5+as.factor(GENDER)+AGG_KEL+B16RF+B16RA+as.factor(DSD)+as.factor(DSMP)+as.factor(DSMA)+as.factor(DPT)+as.factor(DSUMATRA)+as.factor(DKLISUL)+as.factor(DBALINT)+as.factor(DMAPU)+as.factor(DKREDIT),family = "binomial",data = siap04)

# Proposed Model 2
MODEL04_2 <- glm(as.factor(TRANSFER_LAHAN) ~ B5R1E1K5+B3K5+as.factor(GENDER)+AGG_KEL+B16RA+as.factor(DSD)+as.factor(DSMP)+as.factor(DSMA)+as.factor(DPT)+as.factor(DSUMATRA)+as.factor(DKLISUL)+as.factor(DBALINT)+as.factor(DMAPU)+as.factor(DKREDIT),family = "binomial",data = siap04)

# hasil estimasi
stargazer(MODEL04_1,type = "text")

# fitting model
anova(REG_NUL04,MODEL04_1,test = "LRT")
hoslem.test(siap$TRANSFER,fitted(MODEL04),g=10)
PseudoR2(MODEL04_1,which = c("McFadden","McFaddenAdj","Negelkerke","Coxsnell"))
