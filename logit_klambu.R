library(DescTools)
library(ResourceSelection)
library(googlesheets4)

# STATUS     : chr [1:40] "1" "1" "1" "1" ...
# PRODUKSI   : num [1:40] 1830 570 2000 756 4800 4000 1070 2330 9800
# JARAK      : num [1:40] 6 19.6 24.3 27.9 30.7 6.9 14.4 39.5 38.3 
# IURAN      : num [1:40] 5.82e+08 4.50e+07 4.00e+07 1.26e+07 
# BANGUNAN   : chr [1:40] "1" "1" "1" "0" ...
# J.PENGURUS : num [1:40] 14 8 12 8 13 14 7 12 30 3 ...
# CC         : chr [1:40] "0" "1" "1" "1" ...
# DAMPAK1    : chr [1:40] "0" "1" "0" "0" ...
# DAMPAK2    : chr [1:40] "0" "0" "0" "0" ...
# DAMPAK3    : chr [1:40] "0" "0" "0" "1" ...
# DAMPAK4    : chr [1:40] "0" "0" "1" "0" ...
# H.IRIGASI  : num [1:40] 21 24 8 3 20 3 1 9 4 4 ...
# JAM.IRIGASI: num [1:40] 6 4.5 4 6 3 5 5 4 3 4 ...

main_data <- read_sheet("https://docs.google.com/spreadsheets/d/1IkIw2Fc03djM0k5xJvgQ2vNEm8V6Cm72/edit#gid=1004565290")

reg_null <- glm(formula=STATUS ~ 1, family = "binomial", data=dataset)

# Model lengkap
reg_log1 <- glm(as.factor(STATUS)~PRODUKSI+JARAK+IURAN+as.factor(BANGUNAN)+J.PENGURUS+as.factor(CC)+as.factor(DAMPAK)+H.IRIGASI+JAM.IRIGASI,family = "binomial",data=dataset)

# Hilangkan dampak CC
reg_log2 <- glm(as.factor(STATUS)~PRODUKSI+JARAK+IURAN+as.factor(BANGUNAN)+J.PENGURUS+as.factor(CC)+H.IRIGASI+JAM.IRIGASI,family = "binomial",data=dataset)

# Hilangkan semua tipe data non-rasio
reg_log3 <- glm(as.factor(STATUS)~PRODUKSI+JARAK+IURAN+J.PENGURUS+H.IRIGASI+JAM.IRIGASI,family = "binomial",data=dataset)

# Hilangkan semua tipe data rasio
reg_log4 <- glm(as.factor(STATUS)~as.factor(BANGUNAN)+as.factor(CC),family = "binomial",data=dataset)

# Modifikasi 1
reg_log5 <- glm(as.factor(STATUS)~PRODUKSI_ANGGOTA+JARAK+IURAN_ANGGOTA+as.factor(BANGUNAN)+J.PENGURUS+as.factor(CC)+as.factor(DAMPAK)+H.IRIGASI+JAM.IRIGASI,family = "binomial",data=dataset)

# Modifikasi 2 (minus dampak CC)
reg_log6 <- glm(as.factor(STATUS)~PRODUKSI_ANGGOTA+JARAK+IURAN_ANGGOTA+as.factor(BANGUNAN)+J.PENGURUS+as.factor(CC)+H.IRIGASI+JAM.IRIGASI,family = "binomial",data=dataset)

# Modifikasi 3 (minus dampak CC)
reg_log7 <- glm(as.factor(STATUS)~PRODUKSI+JARAK+IURAN_ANGGOTA+as.factor(BANGUNAN)+J.PENGURUS+as.factor(CC)+H.IRIGASI+JAM.IRIGASI,family = "binomial",data=dataset)

# Modifikasi 4 (minus dampak CC)
reg_log8 <- glm(as.factor(STATUS)~PRODUKSI+JARAK+IURAN_HEKTAR+as.factor(BANGUNAN)+J.PENGURUS+as.factor(CC)+H.IRIGASI+JAM.IRIGASI,family = "binomial",data=dataset)

# Modifikasi 5 (minus dampak CC)
reg_log9 <- glm(as.factor(STATUS)~PRODUKSI_ANGGOTA+JARAK+IURAN_HEKTAR+as.factor(BANGUNAN)+J.PENGURUS+as.factor(CC)+H.IRIGASI+JAM.IRIGASI,family = "binomial",data=dataset)

# Modifikasi 6 (minus dampak CC & H.IRIGASI)
reg_log10 <- glm(as.factor(STATUS)~PRODUKSI_ANGGOTA+JARAK+IURAN_HEKTAR+as.factor(BANGUNAN)+J.PENGURUS+as.factor(CC)+JAM.IRIGASI,family = "binomial",data=dataset)

# Modifikasi 7 (tambahan air MK, minus dampak)
reg_log11 <- glm(as.factor(STATUS)~PRODUKSI+JARAK+IURAN_HEKTAR+AIR_MK+as.factor(BANGUNAN)+J.PENGURUS+as.factor(CC)+H.IRIGASI+JAM.IRIGASI,family = "binomial",data=dataset)

# Model coba-coba
coba <- glm(STATUS~PRODUKSI+BAKU+JARAK+IURAN_HEKTAR+H.IRIGASI+J.ANGGOTA+AIR_MK+as.factor(RENCANA_BANGUNAN)+as.factor(DAMPAK),family = "binomial",data=dataset)
summary(coba)


# Model coba-coba2
coba2 <- glm(STATUS~BAKU+JARAK+IURAN_HEKTAR+H.IRIGASI+J.ANGGOTA+J.PENGURUS+as.factor(AIR_MK)+as.factor(RENCANA_BANGUNAN)+as.factor(CC),family = "binomial",data=dataset)
summary(coba2)

### HASIL ANALISIS ####
summary(reg_null)
summary(reg_log1)

hoslem.test(dataset$STATUS,fitted(coba),g=10)
anova(reg_null,coba,test = "LRT")
PseudoR2(coba,which = c("McFadden","McFaddenAdj","Negelkerke","CoxSnell"))
