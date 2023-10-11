library(tidyverse)
library(tseries) # baru
library(lmtest)
library(olsrr)
library(patchwork) # baru
library(stargazer)

###### UJI ASUMSI GAUSS-MARKOV TERHADAP MODEL 
REGRESI LINIEAR ######
#UJI NORMALITAS (GRAFIS)
plot(regresi_berganda)

#UJI NORMALITAS (FORMAL/STATISTIK)
jarque.bera.test(regresi_berganda$residuals)
shapiro.test(regresi_berganda$residuals)
#Hipotesis yang diuji :
#H0 : Residual terdistribusi secara normal.
#H1 : Residual tidak terdistribusi secara normal.

#Aktifkan package 'lmtest'
#Lakukan uji Breusch-Pagan
lmtest::bptest(regresi_berganda)
#Hipotesis yang diuji
#H0 : Tidak terjadi heteroskedastisitas
#H1 : Terjadi heteroskedastisitas

#UJI DURBIN-WATSHON (AUTOKORELASI)
lmtest::dwtest(regresi_berganda)
#Hipotesis yang diuji:
#H0 : Tidak ada korelasi dalam disturbance terms.
#H1 : Ada autorelasi dalam disturbance terms.

#UJI Multikolinearitas
ols_vif_tol(regresi_berganda)

####### ESTIMASI PERSAMAAN REGRESI NON LINIEAR 
(FUNGSI COBB-DOUGLAS) #######

#Tambahkan dataset yang akan diolah
dataset_produksi <- read.csv('https://raw.githubusercontent.com/shulum30994/buku_ajar_kuantitatif/main/dataset_produksi.csv',header = T,sep = ',')

#Membuat diagram pencar produksi dan faktor 
#Membuat diagram pencar produksi dan faktor produksi
ggplot(dataset_produksi)+aes(x=tk,y=produksi)+geom_point()+xlab('Tenaga Kerja (HKO)')+ylab('Produksi (kg)')+geom_smooth(method = 'lm',se=F)+ggtitle('Diagram Pencar Produksi dan Faktor Produksi Tenaga Kerja')

#Menggabungkan dua diagram pencar
sebelum_transformasi <- ggplot(dataset_produksi)+aes(x=tk,y=produksi)+geom_point()+xlab('Tenaga Kerja (HKO)')+ylab('Produksi (kg)')+geom_smooth(method = 'lm',se=F)+ggtitle('SEBELUM Transformasi Logaritma')

sesudah_transformasi <- 
ggplot(dataset_produksi)+aes(x=log(tk),y=log(produksi))+geom_point()+xlab('log{Tenaga Kerja (HKO)}')+ylab('log(Produksi (kg))')+geom_smooth(method = 'lm',se=F)+ggtitle('SESUDAH Transformasi Logaritma')

diagram_pencar <- sebelum_transformasi+sesudah_transformasi

diagram_pencar

#Membuat persamaan regresi linier dengan fungsi Cobb-Douglas
regresi_produksi <- lm(log(produksi)~log(bibit)+log(tk)+log(obat),data = dataset_produksi)

summary(regresi_produksi)

stargazer(regresi_produksi,type='text',digits=2, output='estimasi_cobb_douglas.txt')
