###### INSTALL PACKAGE frontier ######
install.packages('frontier')

###### AKTIFKAN PACKAGE frontier ######
library(frontier)

###### DATASET SECARA ON LINE MELALUI URL #######
dataset_frontier <- read.csv('https://raw.githubusercontent.com/shulum30994/rstudio_exercises/main/dataset_frontier.csv', header=T, sep = ',')

###### PERIKSA DATA (STATISTIK DESKRIPTIF) #######
summary(dataset_frontier)

###### ANALISIS FRONTIER TANPA FAKTOR EFISIENSI TEKNIS ######
sfa1 <- sfa(log(produksi)~log(bibit)+log(tk)+log(obat),data = dataset_frontier)

###### MENAMPILKAN HASIL ESTIMASI ######
summary(sfa1)
coef(sfa1, which='ols') #koefiesien menggunakan estimasi OLS
efficiencies(sfa1) #output indeks efisiensi teknis masing-masing usahatani

###### ANALISIS FRONTIER DENGAN FAKTOR EFISIENSI TEKNIS ######
sfa2 <- sfa(log(produksi)~log(bibit)+log(tk)+log(obat) | pendidikan+jak+pengalaman ,data = dataset_frontier)
summary(sfa2)
efficiencies(sfa2)

##### MEMBUAT VISUALISASI INDEKS EFISIENSI TEKNIS (OPSIONAL) ######
indeks_et <- efficiencies(sfa2) #simpan indeks et menjadi sebuah vector
write.csv(indeks_et, file = 'indeks_efisiensi.csv') #konversi vektor ke format file .csv

##### MEMBUAT VISUALISASI EFISIENSI TEKNIS MENGGNAKAN ggplot2 ######
library(ggplot2)
library(dplyr)
persentase_et <- visualisasi_frontier%>%filter(kategori =='Technical Efficiency (%)' | kategori == 'Potential Output (%)')
ggplot(persentase_et)+aes(x=no, y=nilai, fill=kategori)+geom_col()+xlab('Petani')+ylab('%')+labs(fill='Keterangan :')+theme(legend.position = 'bottom')
produk_et <- visualisasi_frontier%>% filter(kategori=='Actual Output (kg)' | kategori=='Potential Output (kg)') #nilai absolut produk
ggplot(produk_et)+aes(x=no, y=nilai, fill=kategori)+geom_col()+xlab('Petani')+ylab('kg')+labs(fill='Keterangan :')+theme(legend.position = 'bottom')
#atur level produk aktual dan potensial
produk_et$kategori<-factor(produk_et$kategori,levels = c('Potential Output (kg)','Actual Output (kg)'))
