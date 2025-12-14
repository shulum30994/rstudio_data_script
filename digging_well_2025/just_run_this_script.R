##### Package #####
library(tidyverse)
library(googlesheets4)
library(nnet)
# library(ResourceSelection)
library(DescTools)
library(stargazer)
library(GGally)
library(ggeffects)
library(reshape2)
library(caret)
library(performance)
library(pscl)

raw <-read_sheet("https://docs.google.com/spreadsheets/d/1LiBvcJulFiZJ1l9sqIC489cgQfqeetGHdG1w78tiMEw/edit?gid=0#gid=0")

###### Desc Stats #####
desc_tabel2<-as.data.frame(raw %>%
                            mutate(location=if_else(agglomeration=="urban","LUMAJANG","JEMBER"),
                                   total_area=area_MT1+area_MT2+area_MT3,
                                   pipe_ha=pipe_length/total_area,
                                   volume_ha=annual_volume/total_area,
                                   depth_ha=digging_well_depth/total_area,
                                   productivity=annual_yield/total_area) %>%
                            group_by(new_category,location) %>%
                            summarise(across(c("productivity","volume_ha","pipe_ha","depth_ha","total_area"),mean)) %>%
                            print(width = Inf))

raw %>%
  group_by(new_category,agglomeration) %>%
  count(new_category) %>%
  mutate(perc=(n/118)*100)

#### Create New Column ####
data <- raw %>%
  mutate(ave_AREA = (area_MT1+area_MT2+area_MT3)/3,
         total_AREA = area_MT1+area_MT2+area_MT3,
         pipe_area_ratio=pipe_length/total_AREA,
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

#### Re-level crop-pattern category, Food as base ####
data$cat2 <- relevel(as.factor(data$`Dependent Variable`),ref = "A")

#### Model with Average AREA ####
mod0 <- cat2 ~ ave_AREA +
  annual_yield +
  annual_price +
  annual_cost +
  annual_volume +
  digging_well_distance +
  digging_well_depth +
  pipe_length+
  horse_power+
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

#### Model with Total AREA ####
mod2 <- cat2 ~ total_AREA +
  annual_yield +
  annual_price +
  annual_cost +
  annual_volume +
  digging_well_distance +
  digging_well_depth +
  pipe_length+
  horse_power+
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

#### Model with Average AREA 
mod2_ave <- cat2 ~ ave_AREA +
  annual_yield +
  annual_price +
  annual_cost +
  annual_volume +
  digging_well_distance +
  digging_well_depth +
  pipe_length+
  horse_power+
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

#### Model with Average AREA and Pipe length-AREA ratio ####
mod2_ave_ratio <- cat2 ~ ave_AREA +
  annual_yield +
  annual_price +
  annual_cost +
  annual_volume +
  digging_well_distance +
  digging_well_depth +
  pipe_area_ratio+
  horse_power+
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

#### Model with total AREA ####
mod3_total <- cat2 ~ total_AREA +
  annual_yield +
  annual_price +
  annual_cost +
  fuel_cost +
  annual_volume +
  digging_well_distance +
  digging_well_depth +
  pipe_length+
  horse_power+
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

#### Model with average AREA and Fuel Cost ####
mod3_ave <- cat2 ~ ave_AREA +
  annual_yield +
  annual_price +
  annual_cost +
  fuel_cost +
  annual_volume +
  digging_well_distance +
  digging_well_depth +
  pipe_length+
  horse_power+
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

#### Model with total AREA and Pipe length ratio ####
mod3_total_ratio <- cat2 ~ total_AREA +
  annual_yield +
  annual_price +
  annual_cost +
  fuel_cost +
  annual_volume +
  digging_well_distance +
  digging_well_depth +
  pipe_length/total_AREA+
  horse_power+
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

##### Correlation among variables ####
data_cor_check <-data %>%
  select(digging_well_distance,
         digging_well_depth,
         pipe_length,
         annual_volume)

rcorr(as.matrix(data_cor_check),type = "pearson")

corrplot(cor(data_cor_check),
         method = "number",
         type = "upper")

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
  total_AREA,
  annual_yield,
  annual_price,
  annual_cost,
  fuel_cost,
  annual_volume,
  digging_well_distance,
  digging_well_depth,
  pipe_length,
  horse_power,
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

mod2_est <- multinom(formula = mod2, data = data %>% select(
  cat2,
  total_AREA,
  annual_yield,
  annual_price,
  annual_cost,
  annual_volume,
  digging_well_distance,
  digging_well_depth,
  pipe_length,
  horse_power,
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

mod2_ave_est <- multinom(formula = mod2_ave, data = data %>% select(
  cat2,
  ave_AREA,
  annual_yield,
  annual_price,
  annual_cost,
  annual_volume,
  digging_well_distance,
  digging_well_depth,
  pipe_length,
  horse_power,
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

mod2_ave_ratio_est <- multinom(formula = mod2_ave_ratio, data = data %>% select(
  cat2,
  ave_AREA,
  annual_yield,
  annual_price,
  annual_cost,
  annual_volume,
  digging_well_distance,
  digging_well_depth,
  pipe_area_ratio,
  horse_power,
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

mod3_total_est <- multinom(formula = mod3_total, data = data %>% select(
  cat2,
  total_AREA,
  annual_yield,
  annual_price,
  annual_cost,
  fuel_cost,
  annual_volume,
  digging_well_distance,
  digging_well_depth,
  pipe_length,
  horse_power,
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

mod3_ave_est <- multinom(formula = mod3_ave, data = data %>% select(
  cat2,
  ave_AREA,
  annual_yield,
  annual_price,
  annual_cost,
  fuel_cost,
  annual_volume,
  digging_well_distance,
  digging_well_depth,
  pipe_length,
  horse_power,
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

mod3_total_ratio_est <- multinom(formula = mod3_total_ratio, data = data %>% select(
  cat2,
  total_AREA,
  annual_yield,
  annual_price,
  annual_cost,
  fuel_cost,
  annual_volume,
  digging_well_distance,
  digging_well_depth,
  pipe_length,
  pipe_area_ratio,
  horse_power,
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
multicollinearity(mod2_est)

# model fitting
model_performance(mod3_ave_est, metrics = 'all')

# goodness of fit test
chisq.test(data$cat2, predict(mod3_ave_est))

# Chisq (LRT) test
anova(mod_null,mod3_ave_est,test = "Chisq")
#H0 :Variabel independen tidak berpengaruh nyata terhadap variabel dependen
#H1 :Paling sedikit ada satu variabel

##### Print Output #####
stargazer(mod3_total_ratio_est,
          #poktan.remit.urban,
          #koperasi.remit.urban,
          title = "Reference: Food (A)",
          #column.labels = c(),
          intercept.bottom = FALSE,
          apply.coef = exp,
          p.auto = FALSE,
          t.auto = FALSE,
          digits = 2,
          report = ('vc*p'),
          type = "text",
          out = "mod3_total_area_fuel_cost_pipe_ratio_OR.txt")

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

prob_mod2 <- as.data.frame(predict(mod2_est, type = "probs", se = TRUE))
viz_mod2 <- as.data.frame(cbind(data$pipe_length, predict(mod2_est, type = "probs", se = TRUE)))
stacked_prob_mod2 <- stack(viz_mod2,c("V1","A","B","C"))

stacked_data_with_id <- cbind(
  Pipe = rep(data$pipe_length, 3), 
  stack(prob_mod2[, c("A", "B", "C")])
)

ggplot(stacked_data_with_id, aes(x = Pipe, y = values, colour =ind)) + geom_line() 
