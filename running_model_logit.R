##### LINEAR PROBABIITY MODEL (LPM) & REGRESI LOGISTIK #####
data_logit <- read.csv('https://raw.githubusercontent.com/shulum30994/buku_ajar_kuantitatif/main/dataset_regresi_logistik.csv',header = T,sep = ',')

#Diagram pencar variabel dependen dan variabel independen usia petani
#Aktifkan Package
library(ggplot2)
library(patchwork)
#Diagram Pencar untuk LPM
scatter_lpm <- ggplot(data_logit)+aes(x=usia,y=kep_bermitra+geom_point()+geom_smooth(method = 'lm',se=F)+ggtitle('Linear Probability Model (LPM)')
#Diagram Pencar untuk Model Logit
scatter_logit <- ggplot(data_logit)+aes(x=usia,y=kep_bermitra+geom_point()+geom_smooth(method = 'glm',method.args = list(family = "binomial"),se=F)+ggtitle('Logit Model')
#Menampilkan kedua digram pencar dalam satu 
window
diagram_pencar <- scatter_lpm+scatter_logit
diagram_pencar
#Membuat model LPM menggunakan fungsi lm()
regresi_lpm <- lm(kep_bermitra~usia+pendidikan+pengalaman+as.factor(sumber_modal),data=data_logit)
summary(regresi_lpm)

#Membuat model regresi logistik yang berisi konstanta saja
regresi_null <- glm(kep_bermitra~1,family='binomial',data=data_logit)
summary(regresi_null)

#Membuat model regresi logistik dengan variabel independen
regresi_logistik <-glm(kep_bermitra~usia+pendidikan+pengalaman+as.factor(sumber_modal),family='binomial',data=data_logit)
summary(regresi_logistik)

#Melakukan uji kelayakan model
#aktifkan package
library(ResourceSelection)
library(DescTools)

# Likelihood Ratio Test
anova(regresi_null,regresi_logistik,test = "LRT")
#H0 :Variabel independen tidak berpengaruh nyata terhadap variabel dependen
#H1 :Paling sedikit ada satu variabel independen berpengaruh nyata terhadap variabel dependen

# Hosmer & Lameshow Test
9 hoslem.test(data_logit$kep_bermitra,fitted(regresi_logistik),g=10)
#H0 :Model sesuai (tidak ada perbedaan antara hasil observasi dengan hasil prediksi)
#H1 :Model tidak sesuai (ada perbedaan antara hasil observasi dengan hasil prediksi)

#Pseudo R-square
4 PseudoR2(regresi_logistik,which=c("McFadden","McFaddenAdj", "Nagelkerke","CoxSnell"))

#Menampilkan nilai koefisien dan Odds Ratio
cbind(Koefisien=coef(regresi_logistik),Odds_Ratio=exp(coef(regresi_logistik)))
