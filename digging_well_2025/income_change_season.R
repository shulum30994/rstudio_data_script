library(googlesheets4)
library(tidyverse)

ori <- read_sheet('https://docs.google.com/spreadsheets/d/1LiBvcJulFiZJ1l9sqIC489cgQfqeetGHdG1w78tiMEw/edit?gid=0#gid=0')

data<-ori %>%
  select(parent_id,
  crop_pattern,
area_MT1,
area_MT2,
area_MT3,
MT1_revenue,
MT2_revenue,
MT3_revenue,
MT1_cost,
MT2_cost,
MT3_cost) 

data$area_MT2 <- replace(data$area_MT2, data$area_MT2 %in% c(0.00, 0.00), c(0.2957, 0.2301))

data<-data %>%
  separate(crop_pattern, into=c('MT1_comm', 'MT2_comm', 'MT3_comm'), sep = '-') %>%
  mutate(MT1_income=MT1_revenue-MT1_cost,
  MT2_income=MT2_revenue-MT1_cost,
MT3_income=MT3_revenue-MT3_cost) %>%
  mutate(MT1_income_acre=MT1_income/area_MT1,
  MT2_income_acre=MT2_income/area_MT2,
MT3_income_acre=MT3_income/area_MT3) %>%
mutate(delta1=MT2_income_acre-MT1_income_acre,
delta2=MT3_income_acre-MT2_income_acre) %>%
  na.omit

data %>%
  filter(MT1_comm %in% c('Padi', 'Jagung') & MT1_comm %in% c('Padi', 'Jagung')) %>%
  ggplot+
  aes(x=as.factor(parent_id), y=delta1, fill=MT2_comm)+
  geom_col()

data %>%
  filter(MT1_comm %in% c('Padi', 'Jagung') & MT1_comm %in% c('Bawang Merah', 'Cabari Rawit')) %>%
  ggplot+
  aes(x=as.factor(parent_id), y=delta1, fill=MT2_comm)+
  geom_col()

data %>%
  filter(MT1_comm %in% c('Padi', 'Jagung') & MT2_comm %in% c('Bawang Merah','Tembakau','Cabai Rawit')) %>%
  ggplot+
  aes(x=as.factor(parent_id), y=delta1, fill=MT2_comm)+
  geom_col()
