library(googlesheets4)
library(tidyverse)
library(nnet)
library(mclogit) # for cluster model
library(sandwich) # cluster standard error
library(olsrr)
library(tseries)
library(lmtest)
library(performance)
library(corrplot)
library(stargazer)

mt1 <- read_sheet('https://docs.google.com/spreadsheets/d/1I4xCteia98Jc-fwiMvfsSBAKJD9BGUsh6VAnD-TC0tg/edit?gid=1270645724#gid=1270645724',sheet='MT1_DATASET')
mt2 <- read_sheet('https://docs.google.com/spreadsheets/d/1I4xCteia98Jc-fwiMvfsSBAKJD9BGUsh6VAnD-TC0tg/edit?gid=0#gid=0',sheet='MT2_DATASET')
mt3 <- read_sheet('https://docs.google.com/spreadsheets/d/1I4xCteia98Jc-fwiMvfsSBAKJD9BGUsh6VAnD-TC0tg/edit?gid=818002766#gid=818002766',sheet = "MT3_DATASET")

mt1_clean <- mt1 %>% na.omit
mt2_clean <- mt2 %>% na.omit
mt3_clean <- mt3 %>% na.omit

mean_tab<-rbind(mt1_clean %>%
  group_by(Commodities,Agglomeration) %>%
  summarise(across(c('Area_sqm','Water_val_USD','Land_val_USD','Yield_kgsqm','Income_USD','TC_sqm','Fuel_USD_obs','Frequency_obs','Water_sqm','Hours_obs','Pipe_obs','kiloWatt_obs','Age_years','Family_person','Educ_years','Experience_year'),.fns = mean)) %>%
  mutate(season="MT1"), mt2_clean %>%
  group_by(Commodities,Agglomeration) %>%
  summarise(across(c('Area_sqm','Water_val_USD','Land_val_USD','Yield_kgsqm','Income_USD','TC_sqm','Fuel_USD_obs','Frequency_obs','Water_sqm','Hours_obs','Pipe_obs','kiloWatt_obs','Age_years','Family_person','Educ_years','Experience_year'),.fns = mean)) %>%
  mutate(season="MT2"))

demog_sd <- rbind(mt1_clean %>%
  group_by(Agglomeration) %>%
  summarise(across(c('Age_years','Family_person','Educ_years','Experience_year'),.fns = sd)), mt2_clean %>%
  group_by(Agglomeration) %>%
  summarise(across(c('Age_years','Family_person','Educ_years','Experience_year'),.fns = sd)))

trans_summ<-summ_tab %>%
  t() %>%
  as_tibble(rownames="var")

trans_summ <- transpose(summ_tab)
mt2_plot_overview<-mt2_clean %>%
  group_by(Index,Agglomeration) %>%
  count(Index)

mt1_plot_overview %>% 
  group_by(Agglomeration) %>%
  summarise(across(c('n'),.fns=sum))

mt2_plot_overview %>%
  group_by(Agglomeration) %>%
  count(Agglomeration)

mt1_clean <- mt1_clean %>%
  mutate(season="Wet Season (Jan - April)",
    Land_val_USD=Income_plot_USD/Area_sqm,
    Water_val_USD=Income_USD/Water_m3,
    Water_sqm = Water_m3/Area_sqm,
    TC_sqm = Total_Cost_plot_USD/Area_sqm,
    Yield_kgsqm = Product_kg/Area_sqm,
    Frequency_obs = Frequency_times/Area_sqm,
    Hours_obs = Hours/Area_sqm,
    Pipe_obs = Pipe_meter/Area_sqm,
    kiloWatt_obs = kiloWatt/Area_sqm,
    Age_obs = Age_years/Area_sqm,
    Exp_obs = Experience_year/Area_sqm,
    Family_obs = Family_person/Area_sqm,
    Fuel_USD_obs = Fuel_consumpt_USD/Area_sqm,
    Commodities_select=as.factor(if_else(Commodities=="Food",0,if_else(Commodities=="Horticulture",1,2))),
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
Male=as.factor(if_else(Gender=="LAKI-LAKI",1,0)),
Educ_years=case_when(
    Education=="SD" ~ 6,  
    Education=="SMP" ~ 9,
    Education=="SMA" ~ 12,
    Education=="D2/D3 (DIPLOMA)" ~ 15,
    Education=="D4/S1 (SARJANA)" ~ 16,
  TRUE ~ 0),
Educ_obs = Educ_years/Area_sqm,
Member=as.factor(if_else(Association=="Ya",1,0)),
Penyuluh=as.factor(if_else(Extension=="Ya",1,0)),
Mitra=as.factor(if_else(Partnership=="Ya",1,0)),
Rural=as.factor(if_else(Agglomeration=="JEMBER",1,0)))

mt2_clean <- mt2_clean %>%
  mutate(season="Dry Season (May – July/Aug)",
    Land_val_USD=Income_plot_USD/Area_sqm,Water_val_USD=Income_USD/Water_m3,
Water_sqm = Water_m3/Area_sqm,
TC_sqm = Total_Cost_plot_USD/Area_sqm,
  Yield_kgsqm = Product_kg/Area_sqm,
    Frequency_obs = Frequency_times/Area_sqm,
    Hours_obs = Hours/Area_sqm,
    Pipe_obs = Pipe_meter/Area_sqm,
    kiloWatt_obs = kiloWatt/Area_sqm,
Age_obs = Age_years/Area_sqm,
    Exp_obs = Experience_year/Area_sqm,
    Family_obs = Family_person/Area_sqm,
    Fuel_USD_obs=Fuel_consumpt_USD/Area_sqm,
Commodities_select=as.factor(if_else(Commodities=="Food",0,if_else(Commodities=="Horticulture",1,2))),
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
Male=as.factor(if_else(Gender=="LAKI-LAKI",1,0)),
Educ_years=case_when(
    Education=="SD" ~ 6,  
    Education=="SMP" ~ 9,
    Education=="SMA" ~ 12,
    Education=="D2/D3 (DIPLOMA)" ~ 15,
    Education=="D4/S1 (SARJANA)" ~ 16,
  TRUE ~ 0),
  Educ_obs = Educ_years/Area_sqm,
Member=as.factor(if_else(Association=="Ya",1,0)),
Penyuluh=as.factor(if_else(Extension=="Ya",1,0)),
Mitra=as.factor(if_else(Partnership=="Ya",1,0)),
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
#mt1_comm_select <- Commodities_select ~ Water_m3 + Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural

#mt2_crop_select <- Crop_select ~ Water_m3 + Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural

#mt2_comm_select <- Commodities_select ~ Water_m3 + Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural

#mt2_water_crop <- Water_m3 ~  Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural + Crop_select

#mt2_water_comm <- Water_m3 ~  Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural + Commodities_select

#mt3_comm_select <- Commodities_select ~ Water_m3 + Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural

#mt3_water_crop <- Water_m3 ~  Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural + Crop_select

#mt3_water_comm <- Water_m3 ~  Area_sqm + Frequency_times + Hours + Fuel_consumpt + Pipe_meter + kiloWatt + Rural + Commodities_select

#mt1_comm_lval_IDR <- Commodities_select ~ Water_m3 + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Land_val_IDR + Rural

#mt1_comm_lval_USD <- Commodities_select ~ Water_m3 + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Land_val_USD + Rural

#mt2_comm_lval_IDR <- Commodities_select ~ Water_m3 + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Land_val_IDR + Rural

#mt2_comm_lval_USD <- Commodities_select ~ Water_m3 + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Land_val_USD + Rural

#mt1_lval_USD <- Land_val_USD ~ Water_m3 + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Commodities_select + Rural

#mt2_lval_USD <- Land_val_USD ~ Water_m3 + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Commodities_select + Rural

#Comm_water_land_val_USD <- Commodities ~ Water_val_USD + Land_val_USD + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Rural + Age_years + Experience_year + Educ_years + Male + Member+ Penyuluh + Mitra

#Comm_demog_land_val_USD <- Commodities_select ~ Land_val_USD + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt + Rural + Age_years + Experience_year + Educ_years + Male + Member+ Penyuluh + Mitra

#Comm_demog_land_val_USD_ind <- Commodities_select ~ Land_val_USD + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt_USD + Age_obs + Exp_obs + Educ_obs + Rural

Comm_demog_land_water_val_USD_ind <- Commodities_select ~ Land_val_USD + Water_val_USD + Yield_kgsqm + Frequency_obs + Hours_obs + Pipe_obs + kiloWatt_obs + Fuel_USD_obs + Total_Cost_plot_USD + Exp_obs + Rural

#Comm_demog_land_water_val_TC_USD_ind <- Commodities_select ~ Land_val_USD + Fuel_USD_obs+ TC_sqm + Water_val_USD + Yield_kgsqm + Frequency_obs + Hours_obs + Pipe_obs + kiloWatt_obs + Exp_obs + Rural

#Comm_demog_land_val_USD_area <- Commodities_select ~ Land_val_USD + Frequency_obs + Hours_obs + Pipe_obs + kiloWatt_obs + kiloWatt_obs + Age_obs + Exp_obs + Educ_obs + Rural

#Comm_demog_land_water_val_USD_area <- Commodities_select ~ Land_val_USD + Water_val_USD + Frequency_obs + Hours_obs + Pipe_obs + kiloWatt_obs + Age_obs + Exp_obs + Educ_obs + Rural

#Comm_exp_land_water_val_USD_ind <- Commodities_select ~ Land_val_USD + Water_val_USD + Frequency_times + Hours + Pipe_meter + kiloWatt + Fuel_consumpt_USD + Exp_obs + Rural

#### Model Estimate ####
mod_null1 <- multinom(Commodities_select ~ 1, data = mt1_clean %>%
  select(
    Commodities_select,
    Land_val_USD,
    Water_val_USD,
    Yield_kgsqm,
    Frequency_obs,
    Hours_obs,
    Pipe_obs,
    kiloWatt_obs,
    Fuel_USD_obs,
    Total_Cost_plot_USD,
    Exp_obs,
    Rural
  ))
mod_null2 <- multinom(Commodities_select ~ 1, data = mt2_clean %>%
  select(
    Commodities_select,
    Land_val_USD,
    Water_val_USD,
    Yield_kgsqm,
    Frequency_obs,
    Hours_obs,
    Pipe_obs,
    kiloWatt_obs,
    Fuel_USD_obs,
    Total_Cost_plot_USD,
    Exp_obs,
    Rural
  ))
#mt1_comm_est <- multinom(mt1_comm_select,data = mt1_clean)
#mt2_crop_est <- multinom(mt2_crop_select,data = mt2_clean)
#mt2_comm_est <- multinom(mt2_comm_select,data = mt2_clean)
#mt2_water_est <- lm(mt2_water_crop, data = mt2_clean)
#mt2_water_comm_est <- lm(mt2_water_comm, data = mt2_clean)
#mt3_water_est <- lm(mt3_water_crop, data = mt3_clean)
#mt3_water_comm_est <- lm(mt3_water_comm, data = mt3_clean)
#mt3_comm_est <- multinom(mt3_comm_select,data = mt3_clean)
#mt1_comm_IDR <- multinom(mt1_comm_lval_IDR,data = mt1_clean)
#mt2_comm_IDR <- multinom(mt2_comm_lval_IDR,data = mt2_clean)
#mt1_comm_USD <- multinom(mt1_comm_lval_USD,data = mt1_clean)
#mt2_comm_USD <- multinom(mt2_comm_lval_USD,data = mt2_clean)
#mt1_reg_USD <- lm(mt1_lval_USD,data=mt1_clean)
#mt2_reg_USD <- lm(mt2_lval_USD,data=mt2_clean)
#mt1_comm_water_land_USD <- multinom(Comm_water_land_val_USD, data = mt1_clean)
#mt2_comm_water_land_USD <- multinom(Comm_water_land_val_USD, data = mt2_clean)
#mt1_comm_demog_land_USD <- multinom(Comm_demog_land_val_USD, data = mt1_clean)
#mt2_comm_demog_land_USD <- multinom(Comm_demog_land_val_USD, data = mt2_clean)
#mt1_comm_demog_land_USD_ind <- multinom(Comm_demog_land_val_USD_ind, data = mt1_clean)
#mt2_comm_demog_land_USD_ind <- multinom(Comm_demog_land_val_USD_ind, data = mt2_clean)
mt1_comm_demog_land_water_USD_ind <- multinom(Comm_demog_land_water_val_USD_ind, data = mt1_clean)
mt2_comm_demog_land_water_USD_ind <- multinom(Comm_demog_land_water_val_USD_ind, data = mt2_clean)
#mt1_comm_demog_land_water_TC_USD_area <- multinom(Comm_demog_land_water_val_TC_USD_ind, data = mt1_clean)
#mt2_comm_demog_land_water_TC_USD_area <- multinom(Comm_demog_land_water_val_TC_USD_ind, data = mt2_clean)
#mt1_comm_demog_land_USD_area <- multinom(Comm_demog_land_val_USD_area, data = mt1_clean)
#mt2_comm_demog_land_USD_area <- multinom(Comm_demog_land_val_USD_area, data = mt2_clean)
#mt1_comm_demog_land_water_USD_area <- multinom(Comm_demog_land_water_val_USD_area, data = mt1_clean)
#mt2_comm_demog_land_water_USD_area <- multinom(Comm_demog_land_water_val_USD_area, data = mt2_clean)
#mt1_comm_exp_land_water_USD_ind <- multinom(Comm_exp_land_water_val_USD_ind, data = mt1_clean)
#mt2_comm_exp_land_water_USD_ind <- multinom(Comm_exp_land_water_val_USD_ind, data = mt2_clean)
#mt1_comm_demog_land_USD_cluster <- mblogit(Comm_demog_land_val_USD,data=mt1_clean)

#### Cluster Standard Error correction ####
vcov_cluster <- vcovCR(mt1_comm_demog_land_USD_cluster, cluster=mt1_clean$Index,
type = "CR2")
vcov_cluster <- vcovCL(mt1_comm_demog_land_USD_cluster, cluster = ~ Index)

vcov_cluster <- vcovCR(mt2_comm_exp_land_water_USD_ind,
                       cluster = mt1_clean$Index[!is.na(mt1_comm_demog_land_USD_cluster$fitted.values[,1])],
                       type = "CR2")

#### Output ####
summary(mt1_comm_est)
summary(mt2_crop_est)
summary(mt2_comm_est)
summary(mt2_water_est)
summary(mt3_comm_est)
summary(mt3_water_est)
summary(mt2_water_comm_est)
summary(mt3_water_comm_est)
summary(mt1_reg_USD)
summary(mt2_reg_USD)

#### Gauss-Markov check ####
plot(mt2_water_comm_est)
plot(mt3_water_comm_est)
plot(mt2_water_est)
plot(mt3_water_est)
plot(mt1_reg_USD)
plot(mt2_reg_USD)

# Normality (tseries)
jarque.bera.test(mt2_water_comm_est$residuals)
jarque.bera.test(mt3_water_comm_est$residuals)
jarque.bera.test(mt2_water_est$residuals)
jarque.bera.test(mt3_water_est$residuals)
jarque.bera.test(mt1_reg_USD$residuals)
jarque.bera.test(mt2_reg_USD$residuals)

# Multicoliniearity (olsrr)
ols_vif_tol(mt2_water_comm_est)
ols_vif_tol(mt3_water_comm_est)
ols_vif_tol(mt2_water_est)
ols_vif_tol(mt3_water_est)
ols_vif_tol(mt1_reg_USD)
ols_vif_tol(mt2_reg_USD)


# Multicoliniearity (correlation plot)
mt1_clean %>%
  select(Land_val_USD,Water_val_USD,Yield_kgsqm,Frequency_obs,Hours_obs,Pipe_obs,kiloWatt_obs,Fuel_USD_obs,Total_Cost_plot_USD,Exp_obs) %>%
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

# Odds Ratio
exp(coef(mt2_comm_demog_land_water_USD_ind))

# Pseudo R-Square
model_performance(mt1_comm_demog_land_water_USD_ind, metrics = 'all')

# LR-test
LR_test_mt1<-anova(mod_null1,mt1_comm_demog_land_water_USD_ind,test = "Chisq")

#### Output Print ####
stargazer(mt1_comm_demog_land_water_USD_ind,
  mt2_comm_demog_land_water_USD_ind,
          #poktan.remit.urban,
          #koperasi.remit.urban,
          title = "Multinomial Logistic Regression Result (Baseline Cat : Foods)",
          #column.labels = c('Season 1','Season 2'),
          intercept.bottom = FALSE,
          #decimal.mark="::::",
          apply.coef = exp,
          p.auto = FALSE,
          t.auto = FALSE,
          #digits = 0,
          report = ('vc*p'),
          type = "text",
          out = "default_result.html")
