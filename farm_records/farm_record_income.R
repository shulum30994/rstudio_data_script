library(googlesheets4)
library(tidyverse)

EXPENSE <- read_sheet("https://docs.google.com/spreadsheets/d/15j-82tISFJKrGKk5sdlE3-S6ge0geWt_xog1lEY7qgE/edit?gid=0#gid=0")

LABOR <- read_sheet("https://docs.google.com/spreadsheets/d/15j-82tISFJKrGKk5sdlE3-S6ge0geWt_xog1lEY7qgE/edit?gid=1754970135#gid=1754970135", sheet = 2)

HARVEST <- read_sheet("https://docs.google.com/spreadsheets/d/15j-82tISFJKrGKk5sdlE3-S6ge0geWt_xog1lEY7qgE/edit?gid=1754970135#gid=1754970135", sheet = 3)

LABOR <- LABOR %>% select(-...13)

EXP_COMM<-EXPENSE %>%
  group_by(Commodity) %>%
  summarise(across(c('Total_Rp'), .fns = sum, na.rm=TRUE))

LABOR_COMM<-LABOR %>%
  select(Commodities, Fee_Rp) %>%
  group_by(Commodities) %>%
  summarise(across(c('Fee_Rp'), .fns = sum, na.rm=TRUE))

HARV_COMM<-HARVEST %>%
  select(Commodity, REVENUE_Rp) %>%
  group_by(Commodity) %>%
  summarise(across(c('REVENUE_Rp'), .fns = sum, na.rm=TRUE))

EXP_LABOR<-left_join(EXP_COMM,LABOR_COMM,by=c('Commodity'='Commodities'))

INCOME <- left_join(EXP_LABOR,HARV_COMM, by=c("Commodity"="Commodity"))

INCOME %>%
  rename(EXPENSE_Rp=Total_Rp, LABOR_Rp=Fee_Rp) %>%
  mutate(INCOME_Rp=REVENUE_Rp-(EXPENSE_Rp+LABOR_Rp))

INCOME %>%
  rename(EXPENSE_YEN=Total_Rp, LABOR_YEN=Fee_Rp, REVENUE_YEN=REVENUE_Rp) %>%
  mutate(INCOME_YEN=REVENUE_YEN-(EXPENSE_YEN+LABOR_YEN)) %>%
  mutate(across(where(is.numeric), ~ ./110))
