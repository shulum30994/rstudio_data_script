library(aTSA)
library(dplyr)

raw<-read.csv('https://raw.githubusercontent.com/shulum30994/buku_ajar_kuantitatif/refs/heads/main/harga_produsen.csv')

jagung_pipilan <- jagung %>%
  filter(komoditas=='Jagung Pipilan')
kedelai <- raw %>%
  filter(komoditas=='Kedelai')

pipilan_ts <- ts(jagung_pipilan$nilai, start = c(2009,1), frequency = 12)
kedelai_ts <- ts(kedelai$nilai, start = c(2009,1), frequency = 12)

par(mfrow=c(1,2))
plot(pipilan_ts)
plot(kedelai_ts)

par(mfrow=c(1,1))

pipilan_dekommp <- decompose(pipilan_ts)
kedelai_dekomp <- decompose(kedelai_ts)

plot(pipilan_dekommp)
plot(kedelai_dekomp)

par(mfrow=c(2,1))
acf(kedelai_ts, lag=48)
pacf(kedelai_ts,lag=48)
