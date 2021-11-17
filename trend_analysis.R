#### AKTIFKAN PACKAGE YANG DIBUTUHKAN ####
library(forecast)

#### TAMBAHKAN DATA KE DALAM WORKSPACE ####
indonesia_urban <- read.csv("https://raw.githubusercontent.com/shulum30994/rstudio_exercises/main/urban_pop_ind.csv", header=T, sep = ',')

#### KONVERSI DATASET MENJADI TIME-SERIES OBJECT ####
indonesia_ts1 <- ts(indonesia_urban$urban_pop, start=c(1961), end=c(2020))

#### VISUALISASI DATASET YANG TELAH DI-KONVERSI ####
plot(indonesia_ts1)

#### MELAKUKAN ESTIMASI MODEL TREND ####
trend_indonesia <- tslm(indonesia_ts ~ trend)

#### MELIHAT OUTPUT HASIL ESTIMASI MODEL TREND ####
summary(trend_indonesia)

#### MELAKUKAN FORECAST DENGAN MODEL TREND ####
forecast(trend_indonesia, h=5)

###### METODE PERAMALAN LAIN (CONTOH) #########
#dataset yang akan digunakan adalah data perubahan suhu nasional dalam periode bulanan
ind_change <- read.csv('https://raw.githubusercontent.com/shulum30994/rstudio_exercises/main/ind_temp_change.csv',header = T,sep = ';')

#konversi dataset menjadi objek time series
indonesia_temp <- ts(ind_change$temp_change, start = c(1961,1), end=c(2020,12), frequency=12)

#plot objek time-series indonesia_temp yang sudah dikonversi
plot(indonesia_temp)

#visualisasi boxplot
boxplot(indonesia_temp~cycle(indonesia_temp))

#plot garis trend terhadap data time-series
plot(indonesia_temp)+abline(reg=lm(indonesia_temp~time(indonesia_temp)))

#contoh sederhana peramalan menggunakan moving average
indonesia_sm <- ma(indonesia_temp, order=12)

#contoh sederhana peramalan menggunakan exponential smoothing
es_indonesia_temp <- hw(indonesia_temp, initial = 'optimal', h=10*12, beta=NULL, gamma=NULL) #dengan 12 bulan dalam kurun waktu 10 tahun

#plot (visualisasi) hasil peramlan
plot(es_indonesia_temp)

######### PERAMALAN MENGGUNAKAN ARIMA ##########
#DEKOMPOSISI WAKTU
dindonesia_temp <-decompose(indonesia_temp,'multiplicative') #mutiplikatif
add_indonesia_temp <- decompose(indonesia_temp,'additive') #additif

#plot data time-series yang telah di-dekomposisi
plot(dindonesia_temp) #mutiplikatif
plot(add_indonesia_temp) #additif

#Membuat model ARIMA dengan fungsi auto.arima()
indonesia_arima <- auto.arima(indonesia_temp)
indonesia_arima

#Membuat model ARIMA dengan menampilkan seleksi model
auto.arima(indonesia_temp, ic='aic', trace = TRUE)

#meramalkan nilai-nilai yang akan datang berdasarkan model ARIMA yang telah terpilih
indonesia_forecast <- forecast(indonesia_arima,level = c(95), h=10*12)

#menampilkan hasil peramalan menggunakan model ARIMA yang telah terpilih
indonesia_forecast

#visualisasi hasil peramalan
plot(indonesia_forecast)
