library(plm)
library(tidyverse)
library(lmtest)

# Retrive data

# Struktur data
dataset %>%
  glimpse
# Y   : Konversi lahan (%)
# X1  : Populasi penduduk (ribu jiwa)
# X2  : Proporsi lahan pertanian dengan luas propinsi
# X3  : Produktivitas padi (kw/ha)
# X4  : Harga gabah (Rp/kg)
# X5  : Rasio PDRB pertanian terhadap PDRB Industri
# X6  : Rasio PDRB pertanian terhadap PDRB

# Statistik Deskriptif data
summary(dataset)

# Visualisasi boxplot
dataset %>%
  ggplot()+
  aes(x=tahun,y=log(abs(Y)))+
  geom_boxplot()

# Pembuatan model
panel <- log(abs(Y))~log(X1)+log(X2)+log(X3)+log(X4)+log(X5)+log(X6)

# Estimasi Model
## Pooled Regression (CEM)

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
