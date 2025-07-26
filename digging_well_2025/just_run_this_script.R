##### Package #####
library(tidyverse)
library(googlesheets4)
library(nnet)
library(ResourceSelection)
library(DescTools)
library(stargazer)
library(GGally)
library(ggeffects)
library(reshape2)
library(caret)
library(performance)
library(pscl)

raw <-read_sheet("https://docs.google.com/spreadsheets/d/1LiBvcJulFiZJ1l9sqIC489cgQfqeetGHdG1w78tiMEw/edit?gid=0#gid=0")

data <- raw %>%
  mutate(ave_AREA = (area_MT1+area_MT2+area_MT3)/3,
         dMiddle=case_when(
    educ=="SMP" ~ 1,
    educ=="SMA" ~ 1,
    TRUE ~ 0
  ),
  dHigh=case_when(
    educ=="D2/D3 (DIPLOMA)" ~ 1,
    educ=="D4/S1 (SARJANA)" ~ 1,
    TRUE ~ 0
  ),
         dS1=if_else(educ=="D4/S1 (SARJANA)",1,0),
         dD3=if_else(educ=="D2/D3 (DIPLOMA)",1,0),
         dSMA=if_else(educ=="SMA",1,0),
         dSMP=if_else(educ=="SMP",1,0),
         dSD=if_else(educ=="SD",1,0),
         location=if_else(agglomeration=="rural",1,0),
         assoc=if_else(association=="Yes",1,0),
         ext=if_else(extension=="Yes",1,0),
         part=if_else(partnership=="Yes",1,0))

data$cat2 <- relevel(as.factor(data$category),ref = "X")

mod0 <- cat2 ~ ave_AREA +
  annual_yield +
  annual_price +
  annual_cost +
  annual_volume +
  digging_well_distance +
  digging_well_depth +
  age +
  family_member +
  experience +
  as.factor(assoc) +
  as.factor(ext) +
  as.factor(part) +
  as.factor(dSD) +
  as.factor(dSMP) +
  as.factor(dSMA) +
  as.factor(dD3) +
  as.factor(dS1) +
  as.factor(location)

mod1 <- cat2 ~ ave_AREA +
  annual_yield +
  annual_price +
  annual_cost +
  annual_volume +
  digging_well_distance +
  digging_well_depth +
  age +
  family_member +
  experience +
  as.factor(assoc) +
  as.factor(ext) +
  as.factor(part) +
  as.factor(dSD) +
  as.factor(dMiddle) +
  as.factor(dHigh)+
  as.factor(location)

##### Estimate The Model #####
mod0_est <- multinom(formula = mod0, data = data)
mod1_est <- multinom(formula = mod1, data = data %>% select(
  cat2,
  ave_AREA,
  annual_yield,
  annual_price,
  annual_cost,
  annual_volume,
  digging_well_distance,
  digging_well_depth,
  age,
  family_member,
  experience,
  assoc,
  ext,
  part,
  dSD,
  dMiddle,
  dHigh,
  location
))
mod_null <- multinom(cat2 ~ 1, data = data %>% select(
  cat2,
  ave_AREA,
  annual_yield,
  annual_price,
  annual_cost,
  annual_volume,
  digging_well_distance,
  digging_well_depth,
  age,
  family_member,
  experience,
  assoc,
  ext,
  part,
  dSD,
  dMiddle,
  dHigh,
  location
))
mod1_est_all <- multinom(formula = mod1, data = data)

##### Model Chechking/Fitting #####
# p-value
z<-summary(mod1_est)$coefficients/summary(mod1_est)$standard.errors

p <- (1 - pnorm(abs(z), 0, 1)) * 2

# multicolinearity check
multicollinearity(mod1_est)

# model fitting
model_performance(mod1_est, metrics = 'all')

# goodness of fit test
chisq.test(data$cat2, predict(mod1_est))

# Chisq (LRT) test
anova(mod_null,mod1_est,test = "Chisq")
#H0 :Variabel independen tidak berpengaruh nyata terhadap variabel dependen
#H1 :Paling sedikit ada satu variabel

##### Print Output #####
stargazer(mod1_est,
          #poktan.remit.urban,
          #koperasi.remit.urban,
          title = "Reference: Mixed Pattern (X)",
          #column.labels = c(),
          intercept.bottom = FALSE,
          #apply.coef = exp,
          p.auto = FALSE,
          t.auto = FALSE,
          digits = 2,
          report = ('vc*p'),
          type = "text",
          out = "mod1.txt")

##### Vizual #####
ggcoef_multinom(mod1_est,
                variable_labels = c(
                  ave_AREA,
                  
                  annual_volume="Water volume (m3/ha)",
                  annual_yield="Urban",
                  annual_price,
                  annual_cost,
                  digging_well_distance,
                  digging_well_depth,
                  age,
                  family_member,
                ),
                show_p_values=F,
                signif_stars=F,
                y.level_label = c(
                  "I"="Full Food",
                  "II"="Full Horticulture",
                  "III"="Full Plantation",
                  "IV"="Food Dominated",
                  "V"="Horticulture Dominated",
                  "VI"="Plantation Dominated",
                  "VII"="Horticulture-Plantation Oriented",
                  "VIII"="Food-Plantation Oriented",
                  "IX"="Horticulture-Food Oriented"
                ))+
  labs(title = "Predicted Crop Pattern Identification",
       x="Multinomial Logit Coefficient")

ggcoef_multinom(mod1_est)

ggpredict(mod1_est,terms = "annual_volume") %>%
  ggplot(mapping = aes(x=x, y=predicted, colour = response.level))+
  geom_smooth(se=F,size=1)
