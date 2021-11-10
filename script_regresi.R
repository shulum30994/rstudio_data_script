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
library(tseries)

#attach dan beri nama dataset
attach(dataset_regresi)
names(dataset_regresi)

#melihat statistika deskriptif dari dataset
summary(dataset_regresi)

#mencoba menggunakan persamaan analisis regresi
model1 <- lm(income~output+price+labour+transport)

#regresi dengan variabel dummy
model2 <- lm(income~output+price+local)
model3 <- lm(income~output+price+education)

#--------uji asumsi klasik---------
#uji normalitas
jarque.bera.test(model1$residuals)
shapiro.test(model1$residuals)

#uji homoskedastisitas menggunakan BP-test
lmtest::bptest(model1)

#uji autokorelasi
lmtest::dwtest(model1)

#uji multikolinearitas
ols_vif_tol(model1)

#melihat hasil analisis regresi dari model2 dan model3
summary(model1)
summary(model2)
summary(model3)

#mengubah output ke format txt
stargazer(model1, type = "text", digits = 2, output="fit_model1.txt")
stargazer(model2, type = "text", digits = 2, output="fit_model2.txt")
stargazer(model3, type = "text", digits = 2, output="fit_model3.txt")
