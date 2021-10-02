#### AKTIFKAN PACKAGE YANG DIBUTUHKAN ####
library(forecast)

#### TAMBAHKAN DATA KE DALAM WORKSPACE ####
indonesia_urban <- read.csv("https://raw.githubusercontent.com/shulum30994/rstudio_exercises/main/urban_pop_ind.csv", header=T, sep = ',')

#### KONVERSI DATASET MENJADI TIME-SERIES OBJECT ####
indonesia_ts1 <- ts(indonesia_urban$urban_pop, start=c(1961), end=c(2020))

#### MELAKUKAN ESTIMASI MODEL TREND ####
trend_indonesia <- tslm(indonesia_ts ~ trend)

#### MELIHAT OUTPUT HASIL ESTIMASI MODEL TREND ####
summary(trend_indonesia)

#### MELAKUKAN FORECAST DENGAN MODEL TREND ####
forecast(trend_indonesia, h=5)
