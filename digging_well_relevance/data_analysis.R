library(googlesheets4)
library(tidyverse)
library(nnet)
library(olsrr)
library(tseries)
library(lmtest)
library(corrplot)
library(stargazer)

#### Data Retrival ####
mt1 <- read_sheet('https://docs.google.com/spreadsheets/d/1I4xCteia98Jc-fwiMvfsSBAKJD9BGUsh6VAnD-TC0tg/edit?gid=1270645724#gid=1270645724',sheet='MT1_DATASET')
mt2 <- read_sheet('https://docs.google.com/spreadsheets/d/1I4xCteia98Jc-fwiMvfsSBAKJD9BGUsh6VAnD-TC0tg/edit?gid=0#gid=0',sheet='MT2_DATASET')
mt3 <- read_sheet('https://docs.google.com/spreadsheets/d/1I4xCteia98Jc-fwiMvfsSBAKJD9BGUsh6VAnD-TC0tg/edit?gid=818002766#gid=818002766',sheet = "MT3_DATASET")

#### Remove NA ####
mt1_clean <- mt1 %>% na.omit
mt2_clean <- mt2 %>% na.omit
mt3_clean <- mt3 %>% na.omit

mt3_clean %>%
  group_by(Commodities) %>%
  count(Commodities)

mt3_clean %>%
  group_by(Agglomeration) %>%
  count(Agglomeration)

mt3_clean %>%
  group_by(Crop) %>%
  count(Crop)

#### Recode Some Variables ####
mt1_clean <- mt1_clean %>%
  mutate(Commodities_select=as.factor(if_else(Commodities=="Food",0,if_else(Commodities=="Horticulture",1,2))),
Crop_select=as.factor(case_when(
  Crop == 'Padi' ~ 0,
  Crop == 'Jagung' ~ 1,
  Crop == 'Bawang Merah' ~ 2,
  Crop == 'Cabai Rawit' ~ 3,
  Crop == 'Cabai Merah' ~ 4,
  Crop == 'Pepaya' ~ 5,
  Crop == 'Jeruk' ~ 6,
  Crop == 'Kacang Tanah' ~ 7,
  Crop == 'Tomat' ~ 8,
  Crop == 'Tebu' ~ 9,
)),
Rural=as.factor(if_else(Agglomeration=="JEMBER",1,0)))

mt2_clean <- mt2_clean %>%
  mutate(Commodities_select=as.factor(if_else(Commodities=="Food",0,if_else(Commodities=="Horticulture",1,2))),
Crop_select=as.factor(case_when(
  Crop == 'Padi' ~ 0,
  Crop == 'Jagung' ~ 1,
  Crop == 'Bawang Merah' ~ 2,
  Crop == 'Cabai Rawit' ~ 3,
  Crop == 'Cabai Merah' ~ 4,
  Crop == 'Pepaya' ~ 5,
  Crop == 'Jeruk' ~ 6,
  Crop == 'Tembakau' ~ 7,
  Crop == 'Tebu' ~ 8,
)),
Rural=as.factor(if_else(Agglomeration=="JEMBER",1,0)))

mt3_clean <- mt3_clean %>%
  mutate(Commodities_select=as.factor(if_else(Commodities=="Food",0,if_else(Commodities=="Horticulture",1,2))),
Crop_select=as.factor(case_when(
  Crop == 'Padi' ~ 0,
  Crop == 'Jagung' ~ 1,
  Crop == 'Bawang Merah' ~ 2,
  Crop == 'Cabai Rawit' ~ 3,
  Crop == 'Cabai Merah' ~ 4,
  Crop == 'Kacang Tanah' ~ 5,
  Crop == 'Timun' ~ 6,
  Crop == 'Tembakau' ~ 7,
)),
Rural=as.factor(if_else(Agglomeration=="JEMBER",1,0)))

#### Model ####
mt1_comm_select <- Commodities_select ~ Water_m3 + Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural

mt2_crop_select <- Crop_select ~ Water_m3 + Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural

mt2_comm_select <- Commodities_select ~ Water_m3 + Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural

mt2_water_crop <- Water_m3 ~  Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural + Crop_select

mt2_water_comm <- Water_m3 ~  Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural + Commodities_select

mt3_comm_select <- Commodities_select ~ Water_m3 + Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural

mt3_water_crop <- Water_m3 ~  Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural + Crop_select

mt3_water_comm <- Water_m3 ~  Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural + Commodities_select

mt1_comm_lval_IDR <- Commodities_select ~ Water_m3 + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Land_val_IDR + Rural

mt1_comm_lval_USD <- Commodities_select ~ Water_m3 + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Land_val_USD + Rural

mt2_comm_lval_IDR <- Commodities_select ~ Water_m3 + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Land_val_IDR + Rural

mt2_comm_lval_USD <- Commodities_select ~ Water_m3 + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Land_val_USD + Rural

#### Model Estimate ####
mt1_comm_est <- multinom(mt1_comm_select,data = mt1_clean)
mt2_crop_est <- multinom(mt2_crop_select,data = mt2_clean)
mt2_comm_est <- multinom(mt2_comm_select,data = mt2_clean)
mt2_water_est <- lm(mt2_water_crop, data = mt2_clean)
mt2_water_comm_est <- lm(mt2_water_comm, data = mt2_clean)
mt3_water_est <- lm(mt3_water_crop, data = mt3_clean)
mt3_water_comm_est <- lm(mt3_water_comm, data = mt3_clean)
mt3_comm_est <- multinom(mt3_comm_select,data = mt3_clean)
mt1_comm_IDR <- multinom(mt1_comm_lval_IDR,data = mt1_clean)
mt2_comm_IDR <- multinom(mt2_comm_lval_IDR,data = mt2_clean)
mt1_comm_USD <- multinom(mt1_comm_lval_USD,data = mt1_clean)
mt2_comm_USD <- multinom(mt2_comm_lval_USD,data = mt2_clean)

#### Output ####
summary(mt1_comm_est)
summary(mt2_crop_est)
summary(mt2_comm_est)
summary(mt2_water_est)
summary(mt3_comm_est)
summary(mt3_water_est)
summary(mt2_water_comm_est)
summary(mt3_water_comm_est)

#### Gauss-Markov check ####
plot(mt2_water_comm_est)
plot(mt3_water_comm_est)
plot(mt2_water_est)
plot(mt3_water_est)

# Normality (tseries)
jarque.bera.test(mt2_water_comm_est$residuals)
jarque.bera.test(mt3_water_comm_est$residuals)
jarque.bera.test(mt2_water_est$residuals)
jarque.bera.test(mt3_water_est$residuals)

# Multicoliniearity (olsrr)
ols_vif_tol(mt2_water_comm_est)
ols_vif_tol(mt3_water_comm_est)
ols_vif_tol(mt2_water_est)
ols_vif_tol(mt3_water_est)

# Multicoliniearity (correlation plot)
mt1_clean %>%
  select(Water_m3,Area_sqm,Frequency_times,Hours,Fuel_consumpt,Pipe_meter,kiloWatt) %>%
  cor()%>%
  corrplot(method = 'number',type = 'upper')

# Homoskedastis (lmtest-bptest)
bp test(mt2_water_est)
bptest(mt3_water_est)

# Autokorelasi (lmtest-dwtest)
dwtest(mt2_water_comm_est)
dwtest(mt3_water_comm_est)
dwtest(mt2_water_est)
dwtest(mt3_water_est)

#### Output Print ####
stargazer(mt1_comm_USD,mt2_comm_USD,
          #poktan.remit.urban,
          #koperasi.remit.urban,
          title = "Multinomial Logistic Regression Result (Baseline Cat : Foods)",
          column.labels = c('Horticulture','Plantation','Horticulture','Plantation'),
          intercept.bottom = FALSE,
          #apply.coef = exp,
          p.auto = FALSE,
          t.auto = FALSE,
          digits = 2,
          report = ('vc*p'),
          type = "text",
          out = "mt1_mt2_comm_USD.txt")
