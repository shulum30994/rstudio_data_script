library(plm)
library(dplyr)
library(lmtest)
library(car)

# Retrive data

# Struktur data
dataset %>%
  glimpse
# KONVERSI      : Konversi lahan (%)
# POP           : Populasi penduduk (ribu jiwa)
# RASIO.LAHAN   : Proporsi lahan pertanian dengan luas propinsi
# PROTAS        : Produktivitas padi (kw/ha)
# RASIO.INDU    : Rasio PDRB pertanian terhadap PDRB Industri
# GABAH         : Harga gabah (Rp/kg)
# RASIO.PDRB    : Rasio PDRB pertanian terhadap PDRB

# Statistik Deskriptif data
summary(dataset)

# Visualisasi boxplot
plotmeans(KONVERSI~TAHUN,main="Heterogeneitas Antar Tahun",data=dataset)

# Pembuatan model
# KONVERSI = POP + RASIO.LAHAN + PROTAS + RASIO.INDU + GABAH + RASIO.PDRB
panel <- log(abs(KONVERSI))~log(POP)+log(RASIO.LAHAN)+log(PROTAS)+log(RASIO.INDU)+log(GABAH)+log(RASIO.PDRB)

# Estimasi Model
## Pooled Regression (CEM)
pool1 <- plm(panel,data = dataset,index = c('PROP','TAHUN'),model = "pooling")
summary(pool1)

## Fix Effect Model (FEM)
fem1 <- plm(panel,data=dataset,index=c('prop','tahun'),model='within')
summary(fem1)
fixef(fem1)

## Random Effect Model
rem1 <- plm(panel,data=dataset,index=c('prop','tahun'),model='random')
summary(rem1)

# Seleksi Model
## FEM vs CEM (Chow-test)

## FEM vs REM (Hausman-test)
phtest(fem1,rem1)

# Uji asumsi klasik
## Homoskedastis
bptest(fem1)

## Auto-korelasi
pbgtest(fem1)

# Penangangan pelanggaran asumsi klasik
