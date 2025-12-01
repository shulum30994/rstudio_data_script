library(mlogit)
library(dplyr)
library(googlesheets4)
library(logitr)

raw <- read_sheet('https://docs.google.com/spreadsheets/d/1pRkBXsKm2z-gf-2rjAVTmwwH6peMmPXy/edit?gid=1923821299#gid=1923821299')

raw <- read.csv('wtp_juls.csv')

short <- raw %>%
  select(Id.Responden,
         PREF,
         BENIH.MT1,
         BENIH.MT2,
         BENIH.MT3,
         PUPUK.MT1,
         PUPUK.MT2,
         PUPUK.MT3,
         OBAT.MT1,
         OBAT.MT2,
         OBAT.MT3)

data <- mlogit.data(short,
              choice = "PREF",
              shape = "wide",
              varying = c(3:11),
              sep = ".",
              id.var = "Id.Responden")

write.csv(data,"data2.csv",row.names = F)

model_logitr <- logitr(
  data=data2,
  outcome = "CHOICE",
  obsID = "chid",
  pars = c("BENIH", "PUPUK", "OBAT", "alt")
)
summary(model_logitr)

logitr_benih <-logitr(
  data=data2,
  outcome = "CHOICE",
  obsID = "chid",
  pars = c("BENIH","alt")
)
summary(logitr_benih)

logitr_pupuk <-logitr(
  data=data,
  outcome = "PREF",
  obsID = "chid",
  pars = c("PUPUK","alt")
)
