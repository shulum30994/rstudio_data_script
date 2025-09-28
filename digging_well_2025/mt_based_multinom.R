library(tidyverse)
library(nnet)
library(DescTools)
library(stargazer)
library(caret)
library(performance)
library(pscl)

MT2$crop <- relevel(as.factor(MT2$crop),ref = "Food")
mod_MT2 <- crop ~
  area_ha +
  water_m3 +
  frequency +
  hours +
  fuel_consumpt +
  pipe +
  horse_power

mod_MT2_est <- multinom(formula = mod_MT2, data = MT2 %>% na.omit())

model_performance(mod_MT2_est,metrics = "all")
summary(mod_MT2_est)
stargazer(mod_MT2_est,
          #poktan.remit.urban,
          #koperasi.remit.urban,
          title = "Reference: Food (A)",
          #column.labels = c(),
          intercept.bottom = FALSE,
          #apply.coef = exp,
          p.auto = FALSE,
          t.auto = FALSE,
          digits = 2,
          report = ('vc*p'),
          type = "text",
          out = "MT2_crop_multinom.txt")
