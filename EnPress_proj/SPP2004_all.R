library(foreign)

spp104 <- read.dbf("/home/shohibul/KERJA/DATA/SPP_2004/spp1.dbf")
demog04 <- read.dbf("/home/shohibul/KERJA/DATA/SPP_2004/demogspp.dbf")
spp204<- read.dbf("/home/shohibul/KERJA/DATA/SPP_2004/spp2.dbf")

# B5R2B1 : Luas lahan sawah yang mengalami konversi menjadi lahan pertanian bukan sawah
# B5R2B2 : Luas lahan sawah yang mengalami konversi menjadi lahan bukan pertanian
# B5R2B3 : Luas lahan pertanian bukan sawah yang mengalami konversi menjadi lahan sawah
# B5R2B4 : Luas lahan pertanian bukan sawah yang mengalami konversi menjadi lahan bukan pertanian
# B5R2B5 : Luas lahan bukan pertanian yang mengalami konversi menjadi lahan sawah
# B5R2B6 : Luas lahan bukan pertanian yang mengalami konversi menjadi lahan pertanian bukan sawah
# B5R1DK5 : Jumlah penguasaan lahan yang dikuasai pada saat pencacahan
# B5R1E1K5 : Jumlah penggunaan lahan pertanian yang diusahakan pada saat pencacahan

spp0104 <- spp104 %>% select(IDENTITAS,PROP,KAB,B5R2A,B5R2B1,B5R2B2,B5R2B3,B5R2B4,B5R2B4,B5R2B5,B5R2B6,B5R1DK5)

spp0104 %>% group_by(PROP) %>% count()

spp0104<-spp0104 %>% mutate(across(c("B5R2B1","B5R2B2","B5R2B3","B5R2B4","B5R2B4","B5R2B5","B5R2B6","B5R1DK5"),as.numeric))

spp0104<-spp0104 %>% mutate(
  TS=if_else(B5R2A=="2","TK",""),
  S_NS=if_else(B5R2B1>=10,"S_NS",""),
  S_NP=if_else(B5R2B2>=10,"S_NP",""),
  NS_S=if_else(B5R2B3>=10,"NS_S",""),
  NS_NP=if_else(B5R2B4>=10,"NS_NP",""),
  NP_S=if_else(B5R2B5>=10,"NP_S",""),
  NP_NS=if_else(B5R2B6>=10,"NP_NS",""),
  KOMBINASI = gsub(",{2,}", ",", 
                   gsub("^,+|,+$", "", paste(S_NS,S_NP,NS_S,NS_NP,NP_S,NP_NS,sep = ","))),
  JENIS_KONVERSI = case_when(
    KOMBINASI=='NP_NS' ~ 1,
    KOMBINASI=='S_NS' ~ 2,
    KOMBINASI=='NS_NP' ~ 2,
    KOMBINASI=='NS_S' ~ 1,
    KOMBINASI=='S_NP' ~ 2,
    KOMBINASI=='NS_NP,NP_NS' ~ 3,
    KOMBINASI=='NP_S' ~ 1,
    KOMBINASI=='S_NP,NP_NS' ~ 3,
    KOMBINASI=='S_NS,S_NP' ~ 2,
    KOMBINASI=='S_NS,NS_NP' ~ 2,
    KOMBINASI=='S_NS,NS_S' ~ 3,
    KOMBINASI=='NP_S,NP_NS' ~ 1,
    KOMBINASI=='S_NP,NS_NP' ~ 2,
    KOMBINASI=='S_NS,NP_NS' ~ 3,
    KOMBINASI=='NS_S,NP_NS' ~ 3,
    KOMBINASI=='S_NS,S_NP,NP_NS' ~ 2,
    KOMBINASI=='NS_S,NS_NP' ~ 3,
    KOMBINASI=='S_NS,NS_S,NS_NP,NP_NS '~ 3,
    KOMBINASI=='S_NP,NS_S' ~ 3,
    KOMBINASI=='S_NP,NP_S' ~ 3,
    KOMBINASI=='S_NP,NS_NP,NP_NS' ~ 2,
    KOMBINASI=='S_NS,S_NP,NS_NP,NP_NS' ~ 2,
    KOMBINASI=='NS_NP,NP_S,NP_NS' ~ 3,
    KOMBINASI=='NS_S,NS_NP,NP_NS' ~ 3,
    KOMBINASI=='NS_S,NS_NP,NP_S,NP_NS' ~ 3,
    KOMBINASI=='S_NS,NS_NP,NP_NS' ~ 3,
    KOMBINASI=='S_NS,NS_S,NP_NS' ~ 3,
    KOMBINASI=='S_NS,S_NP,NS_S,NS_NP,NP_S,NP_NS' ~ 3,
    KOMBINASI=='NS_NP,NP_S' ~ 3,
    KOMBINASI=='NS_S,NP_S' ~ 1,
    KOMBINASI=='S_NP,NP_S,NP_NS' ~ 3,
    KOMBINASI=='S_NP,NS_S,NP_NS' ~ 3,
    KOMBINASI=='S_NS,NP_S,NP_NS' ~ 3,
    KOMBINASI=='S_NS,S_NP,NP_S' ~ 3,
    KOMBINASI=='S_NS,S_NP,NS_NP' ~ 2,
    KOMBINASI=='S_NS,S_NP,NS_S,NS_NP' ~ 3,
    TRUE ~ 0
  )
)

spp0104 %>% group_by(KOMBINASI) %>% count(KOMBINASI) %>% arrange(desc(n)) %>% print(n=37)

# Pedoman Kategori
# 2NP_NS ~ 1
# 3S_NS ~ 2
# 4NS_NP ~ 2
# 5NS_S ~ 1
# 6S_NP ~ 2
# 7NS_NP,NP_NS ~ 3
# 8NP_S ~ 1
# 9S_NP,NP_NS ~ 3
# 10S_NS,S_NP ~ 2
# 11S_NS,NS_NP ~ 2
# 12S_NS,NS_S ~ 3
# 13NP_S,NP_NS ~ 1
# 14S_NP,NS_NP ~ 2
# 15S_NS,NP_NS ~ 3
# 16NS_S,NP_NS ~ 3
# 17S_NS,S_NP,NP_NS ~ 2
# 18NS_S,NS_NP ~ 3
# 19S_NS,NS_S,NS_NP,NP_NS ~ 3
# 20S_NP,NS_S ~ 3
# 21S_NP,NP_S ~ 3
# 22S_NP,NS_NP,NP_NS ~ 2
# 23S_NS,S_NP,NS_NP,NP_NS ~ 2
# 24NS_NP,NP_S,NP_NS ~ 3
# 25NS_S,NS_NP,NP_NS ~ 3
# 26NS_S,NS_NP,NP_S,NP_NS ~ 3
# 27S_NS,NS_NP,NP_NS ~ 3
# 28S_NS,NS_S,NP_NS ~ 3
# 29S_NS,S_NP,NS_S,NS_NP,NP_S,NP_NS ~ 3
# 30NS_NP,NP_S ~ 3
# 31NS_S,NP_S ~ 1
# 32S_NP,NP_S,NP_NS ~ 3
# 33S_NP,NS_S,NP_NS ~ 3
# 34S_NS,NP_S,NP_NS ~ 3
# 35S_NS,S_NP,NP_S ~ 3
# 36S_NS,S_NP,NS_NP ~ 2
# 37S_NS,S_NP,NS_S,NS_NP ~ 3

demo04 <- demog04 %>% select(IDENTITAS,B3K4,B3K5,B3K6)

# B3K4 : Jenis Kelamin (1 : Laki-laki, 2:Perempuan)
# B3K5 : Umur
# B3K6 : Pendidikan

spp0204 <- spp204 %>% select(IDENTITAS,B17R6A,B16RA,B16RF)
# B16RA : Pendapatan usaha sektor pertanian
# B16Rf : Pendapatan rumah tangga
data04 <- left_join(spp0104,spp0204,by=c("IDENTITAS"="IDENTITAS"))
data04 <- left_join(data04,demo04,by=c("IDENTITAS"="IDENTITAS"))
data04 <- left_join(data04,spp104%>%select(IDENTITAS,B5R1E1K2),by=c("IDENTITAS"="IDENTITAS"))


data04 <- data04 %>% mutate(
  GENDER = if_else(B3K4==1,1,0),
  KREDIT = if_else(B17R6A==1,1,0),
  DSUMA=case_when(
    PROP == 11 ~ 1,
    PROP == 12 ~ 1,
    PROP == 13 ~ 1,
    PROP == 14 ~ 1,
    PROP == 15 ~ 1,
    PROP == 16 ~ 1,
    PROP == 17 ~ 1,
    PROP == 18 ~ 1,
    PROP == 19 ~ 1,
    PROP == 21 ~ 1,
    TRUE ~ 0
  ),
  DKALI = case_when(
    PROP == 61 ~ 1,
    PROP == 62 ~ 1,
    PROP == 63 ~ 1,
    PROP == 64 ~ 1,
    TRUE ~ 0
  ),
  DBALI = case_when(
    PROP == 51 ~ 1,
    PROP == 52 ~ 1,
    PROP == 53 ~ 1,
    TRUE ~ 0
  ),
  DSULA = case_when(
    PROP == 71 ~ 1,
    PROP == 72 ~ 1,
    PROP == 73 ~ 1,
    PROP == 74 ~ 1,
    PROP == 75 ~ 1,
    PROP == 76 ~ 1,
    TRUE ~ 0
  ),
  DMALU = case_when(
    PROP == 81 ~ 1,
    PROP == 82 ~ 1,
    TRUE ~ 0
  ),
  DPAPU = case_when(
    PROP == 94 ~ 1,
    TRUE ~ 0
  ),
  ELEMENTARY = case_when(
    B3K6==2 ~ 1,
    TRUE ~ 0
  ),
  MID = case_when(
    B3K6==3 ~ 1,
    B3K6==4 ~ 1,
    TRUE ~ 0
  ),
  UNIV = case_when(
    B3K6==5 ~ 1,
    B3K6==6 ~ 1,
    B3K6==7 ~ 1,
    TRUE ~ 0
  )
)
