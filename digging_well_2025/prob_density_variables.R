library(tidyverse)
library(googlesheets4)

raw <-read_sheet("https://docs.google.com/spreadsheets/d/1LiBvcJulFiZJ1l9sqIC489cgQfqeetGHdG1w78tiMEw/edit?gid=0#gid=0")

# re-categorize
raw <- raw %>% mutate(crop_pattern=case_when(`Dependent Variable`=='A' ~ 'Food',
`Dependent Variable`=='B' ~ 'Food-others',
`Dependent Variable`=='C' ~ 'Mixed',
TRUE ~ 'Und'))

# double/numerical variables only
# Cropping-pattern
pattern_summ<-raw %>%
  select(-parent_id) %>%
  group_by(crop_pattern) %>%
  summarise(across(where(is.numeric),
    list(mean = ~mean(.x, na.rm = TRUE),
  sd = ~sd(.x, na.rm = TRUE)), .names="{.col}_{.fn}"))

# Rural and Urban
agglo_summ<-raw %>%
  select(-parent_id) %>%
  group_by(agglomeration) %>%
  summarise(across(where(is.numeric),
    list(mean = ~mean(.x, na.rm = TRUE),
  sd = ~sd(.x, na.rm = TRUE)), .names="{.col}_{.fn}"))

# simple histogram
raw %>%
  ggplot()+
  aes(x=experience)+
  geom_histogram()

# density plot with one variable and function
raw %>%
  na.omit %>%
  ggplot()+
  aes(x=area_MT1)+
  geom_histogram(aes(y=..density..), bindwith=1)+
  geom_function(
    fun=dnorm,
    args=list(
      mean=mean(raw$area_MT1),
      sd=sd(raw$area_MT1)
    ),
    color="blue"
  )

#### Cropping Pattern ####
# MT1 Acreage
raw %>%
  ggplot()+
  aes()+
  geom_histogram(aes(y=after_stat(density), x=area_MT1, fill=crop_pattern), alpha=0.2)+
  facet_grid(crop_pattern~.)+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Food'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Food')$area_MT1_mean,
sd=filter(pattern_summ, crop_pattern=='Food')$area_MT1_sd))+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Food-others'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Food-others')$area_MT1_mean,
sd=filter(pattern_summ, crop_pattern=='Food-others')$area_MT1_sd))+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Mixed'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Mixed')$area_MT1_mean,
sd=filter(pattern_summ, crop_pattern=='Mixed')$area_MT1_sd))+
  xlab("Luas lahan (ha)")+
  labs(title = "Densitas luas lahan di MT1 pada setiap kategori pola tanam",
fill="Pola tanam:")+
  theme_bw()

# MT2 Acreage
raw %>%
  ggplot()+
  aes()+
  geom_histogram(aes(y=after_stat(density), x=area_MT2, fill=crop_pattern), alpha=0.2)+
  facet_grid(crop_pattern~.)+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Food'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Food')$area_MT2_mean,
sd=filter(pattern_summ, crop_pattern=='Food')$area_MT2_sd))+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Food-others'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Food-others')$area_MT2_mean,
sd=filter(pattern_summ, crop_pattern=='Food-others')$area_MT2_sd))+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Mixed'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Mixed')$area_MT2_mean,
sd=filter(pattern_summ, crop_pattern=='Mixed')$area_MT2_sd))+
  xlab("Luas lahan (ha)")+
  labs(title = "Densitas luas lahan di MT2 pada setiap kategori pola tanam",
fill="Pola tanam:")+
  theme_bw()

# MT3 Acreage
raw %>%
  ggplot()+
  aes()+
  geom_histogram(aes(y=after_stat(density), x=area_MT3, fill=crop_pattern), alpha=0.2)+
  facet_grid(crop_pattern~.)+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Food'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Food')$area_MT3_mean,
sd=filter(pattern_summ, crop_pattern=='Food')$area_MT3_sd))+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Food-others'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Food-others')$area_MT3_mean,
sd=filter(pattern_summ, crop_pattern=='Food-others')$area_MT3_sd))+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Mixed'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Mixed')$area_MT3_mean,
sd=filter(pattern_summ, crop_pattern=='Mixed')$area_MT3_sd))+
  xlab("Luas lahan (ha)")+
  labs(title = "Densitas luas lahan di MT3 pada setiap kategori pola tanam",
fill="Pola tanam:")+
  theme_bw()

# Annual Volume
raw %>%
  ggplot()+
  aes()+
  geom_histogram(aes(y=after_stat(density), x=annual_volume, fill=crop_pattern), alpha=0.2)+
  facet_grid(crop_pattern~.)+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Food'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Food')$annual_volume_mean,
sd=filter(pattern_summ, crop_pattern=='Food')$annual_volume_sd))+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Food-others'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Food-others')$annual_volume_mean,
sd=filter(pattern_summ, crop_pattern=='Food-others')$annual_volume_sd))+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Mixed'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Mixed')$annual_volume_mean,
sd=filter(pattern_summ, crop_pattern=='Mixed')$annual_volume_sd))+
  xlab("Volume (m3)")+
  labs(title = "Densitas penggunaan air sumur tahunan pada setiap kategori pola tanam",
fill="Pola tanam:")+
  theme_bw()

# Annual Cost
raw %>%
  ggplot()+
  aes()+
  geom_histogram(aes(y=after_stat(density), x=annual_cost, fill=crop_pattern), alpha=0.2)+
  facet_grid(crop_pattern~.)+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Food'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Food')$annual_cost_mean,
sd=filter(pattern_summ, crop_pattern=='Food')$annual_cost_sd))+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Food-others'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Food-others')$annual_cost_mean,
sd=filter(pattern_summ, crop_pattern=='Food-others')$annual_cost_sd))+
  stat_function(data=pattern_summ %>% filter(crop_pattern=='Mixed'),
fun=dnorm,
args = list(mean=filter(pattern_summ, crop_pattern=='Mixed')$annual_cost_mean,
sd=filter(pattern_summ, crop_pattern=='Mixed')$annual_cost_sd))+
  xlab("Total Biaya (Rp)")+
  labs(title = "Densitas total biaya tahunan pada setiap kategori pola tanam",
fill="Pola tanam:")+
  theme_bw()

#### Rural vs Urban ####
# Annual Cost
raw %>%
  ggplot()+
  aes()+
  geom_histogram(aes(y=after_stat(density), x=annual_cost, fill=agglomeration), alpha=0.2)+
  facet_grid(agglomeration~.)+
  stat_function(data=agglo_summ %>% filter(agglomeration=='rural'),
fun=dnorm,
args = list(mean=filter(agglo_summ, agglomeration=='rural')$annual_cost_mean,
sd=filter(agglo_summ, agglomeration=='rural')$annual_cost_sd))+
  stat_function(data=agglo_summ %>% filter(agglomeration=='urban'),
fun=dnorm,
args = list(mean=filter(agglo_summ, agglomeration=='urban')$annual_cost_mean,
sd=filter(agglo_summ, agglomeration=='urban')$annual_cost_sd))+
  xlab("Total Biaya (Rp)")+
  labs(title = "Densitas total biaya tahunan berdasarkan agglomeras",
fill="Agglomerasi:")+
  theme_bw()
