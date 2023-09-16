install.packages("plm")
install.packages("dplyr")
install.packages("lmtest")
install.packages("gplots")

library(plm)
library(dplyr)
library(lmtest)
library(gplots)

# Retrive data
dataset <- read.csv('https://raw.githubusercontent.com/shulum30994/rstudio_data_script/main/data_panel_workshop.csv')

# Struktur data
dataset %>%
  glimpse
# KONVERSI      : Konversi lahan (hektar)
# POP           : Populasi penduduk (ribu jiwa)
# RASIO.LAHAN   : Proporsi lahan pertanian dengan luas propinsi
# PROTAS        : Produktivitas padi (kw/ha)
# RASIO.INDU    : Rasio PDRB pertanian terhadap PDRB Industri
# GABAH         : Harga gabah (Rp/kg)
# RASIO.PDRB    : Rasio PDRB pertanian terhadap PDRB

# Statistik Deskriptif data
summary(dataset)

# Visualisasi data sederhana
plotmeans(KONVERSI~TAHUN,main="Konversi Lahan antar Tahun",data=dataset)

# Pembuatan model
# KONVERSI = POP + RASIO.LAHAN + PROTAS + RASIO.INDU + GABAH + RASIO.PDRB
panel <- log(abs(KONVERSI))~log(POP)+log(RASIO.LAHAN)+log(PROTAS)+log(RASIO.INDU)+log(GABAH)+log(RASIO.PDRB)

# Estimasi Model
## Pooled Regression (CEM)
pool1 <- plm(panel,data = dataset,index = c('PROP','TAHUN'),model = "pooling")
summary(pool1)

## Fix Effect Model (FEM)
fem1 <- plm(panel,data=dataset,index=c('PROP','TAHUN'),model='within')
summary(fem1)
fixef(fem1)

## Random Effect Model
rem1 <- plm(panel,data=dataset,index=c('PROP','TAHUN'),model='random')
summary(rem1)

# Seleksi Model
# Pool (CEM) vs REM (LM-test)
plmtest(pool1,type = "bp")
# H1 : REM lebih baik dibandingkan pool

## FEM vs CEM (Chow-test)
pooltest(pool1,fem1)
# H1 : FEM lebih baik dibandingkan pool

## FEM vs REM (Hausman-test)
phtest(fem1,rem1)
# H0 : REM
# H1 : FEM

# Uji asumsi klasik
## Homoskedastis
bptest(fem1)
# H0 : Homoskedastis

## Auto-korelasi
pbgtest(fem1)
# H0 : tidak ada autokorelasi
