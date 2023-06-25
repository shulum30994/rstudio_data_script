library(dplyr)
library(ResourceSelection)
library(DescTools)
library(stargazer)
library(foreign)

spp104 <- read.dbf("/home/shohibul/KERJA/DATA/Buku Analisis Lahan/Referensi/spp1.dbf")
spp204 <- read.dbf("/home/shohibul/KERJA/DATA/Buku Analisis Lahan/Referensi/spp2.dbf")
demogspp <- read.dbf("/home/shohibul/KERJA/DATA/Buku Analisis Lahan/Referensi/demogspp.dbf")

mentahan <- spp204 %>% select(IDENTITAS,PROP,B16RF,B17R5) %>% na.omit()
transfer_lahan <- spp104 %>% select(IDENTITAS,B5R3A)

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
mentahan <- merge(mentahan,transfer_lahan,by="IDENTITAS",all = TRUE)
mentahan <- merge(mentahan,keluarga,by="IDENTITAS",all = TRUE)
mentahan <- mentahan%>%rename(AGG_KEL=n)
mentahan <- merge(mentahan,demo,by="IDENTITAS",all = TRUE)
mentahan[is.na(mentahan)]<-0
siap <- mentahan %>% na.omit()

# variabel baru dari variabel yang telah ada
siap<-siap %>%
  mutate(TRANSFER=if_else(B5R3A==1,1,0),GENDER=if_else(B3K4==1,1,0),DSD = if_else(B3K6==2,1,0),DSMP=if_else(B3K6==3,1,0),DSMA=if_else(B3K6==4,1,0),DPT=if_else(B3K6 %in% c(5,6,7),1,0),DKREDIT=if_else(B17R5==2,1,0))

# model
REG_NUl <- glm(as.factor(TRANSFER) ~ 1,family = "binomial",data = siap)
MODEL <- glm(as.factor(TRANSFER) ~ B3K5+as.factor(GENDER)+as.factor(DSD)+as.factor(DSMP)+as.factor(DSMA)+as.factor(DPT)+AGG_KEL+B16RF+as.factor(DKREDIT),family = "binomial",data = siap)

# hasil estimasi
stargazer(MODEL,type = "text")

# fitting model
anova(REG_NUl,MODEL,test = "LRT")
hoslem.test(siap$TRANSFER,fitted(MODEL),g=10)
PseudoR2(MODEL,which = c("McFadden","McFaddenAdj","Negelkerke","Coxsnell"))
