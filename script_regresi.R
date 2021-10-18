#------mendapatkan dataset secara online-------
dataset_regresi <- read.csv('https://raw.githubusercontent.com/shulum30994/rstudio_exercises/main/dataset_regresi.csv', header = T, sep = ',')

#install library yang diperlukan
install.packages(olsrr)
install.packages(stargazer)
install.packages(lmtest)

#load library yang telah terinstall ke dalam RStudio
library(olsrr)
library(stargazer)
library(lmtest)

#attach dan beri nama dataset
attach(dataset_regresi)
names(dataset_regresi)

#melihat statistika deskriptif dari dataset
summary(dataset_regresi)

#mencoba menggunakan persamaan analisis regresi
model1 <- lm(income~output+price+labour+transport)

#melihat hasil analisis regresi dari model1
summary(model1)

#mengubah variabel dummy menjadi factor
dummy_local <- as.factor()
dummy_edu <- as.factor()

#regresi dengan variabel dummy sebagai factor
model2 <- lm(income~output+price+local)
model3 <- lm(income~output+price+edu_cat)

#melihat hasil analisis regresi dari model2 dan model3
summary(model2)
summary(model3)

#--------uji asumsi klasik---------
#uji normalitas
jarque.bera.test()
shapiro.wilk.test()

#uji homoskedastisitas menggunakan BP-test
lmtest::bptest()

#uji autokorelasi
lmtest::dwtest()

#uji multikolinearitas
ols_vif_tol()
