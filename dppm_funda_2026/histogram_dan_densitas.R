library(tidyverse)
library(googlesheets4)

raw <- read_sheet('https://docs.google.com/spreadsheets/d/1aT_iDh_Y83ZIlW2RSrJE5pUT-wBvrZPwliokOn6Gub0/edit?gid=1265269007#gid=1265269007', sheet='unstacked')

data<-raw %>%
  group_by(samp_div) %>%
  summarise(across(c('age','experience','family','CSSA_score'), list(mean=~mean(.x, na.rm = T), sd=~sd(.x, na.rm = T)), .names="{.col}_{.fn}"))

# Age
raw %>%
  ggplot()+
  aes()+
  geom_histogram(aes(x=age, y=after_stat(density), fill=samp_div), alpha=2)+
  facet_grid(samp_div ~ .)+
  stat_function(data = data %>% filter(samp_div=="Upstream"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Upstream")$age_mean, sd=filter(data, samp_div=="Upstream")$age_sd))+
  stat_function(data = data %>% filter(samp_div=="Mid"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Mid")$age_mean, sd=filter(data, samp_div=="Mid")$age_sd))+
  stat_function(data = data %>% filter(samp_div=="Downstream"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Downstream")$age_mean, sd=filter(data, samp_div=="Downstream")$age_sd))+
  xlab("Usia (tahun)")+
  labs(title="Sebaran densitas usia responden")+
  theme(legend.position = "none",
        text=element_text(size=10, face="bold", family = "mono"))

# Experience
raw %>%
  ggplot()+
  aes()+
  geom_histogram(aes(x=experience, y=after_stat(density), fill=samp_div), alpha=2)+
  facet_grid(samp_div ~ .)+
  stat_function(data = data %>% filter(samp_div=="Upstream"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Upstream")$experience_mean, sd=filter(data, samp_div=="Upstream")$experience_sd))+
  stat_function(data = data %>% filter(samp_div=="Mid"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Mid")$experience_mean, sd=filter(data, samp_div=="Mid")$experience_sd))+
  stat_function(data = data %>% filter(samp_div=="Downstream"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Downstream")$experience_mean, sd=filter(data, samp_div=="Downstream")$experience_sd))+
  xlab("Pengalaman (tahun)")+
  labs(title="Sebaran densitas pengalaman usahatani responden")+
  theme(legend.position = "none",
        text=element_text(size=10, face="bold", family = "mono"))

# Family
raw %>%
  ggplot()+
  aes()+
  geom_histogram(aes(x=family, y=after_stat(density), fill=samp_div), alpha=2)+
  facet_grid(samp_div ~ .)+
  stat_function(data = data %>% filter(samp_div=="Upstream"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Upstream")$family_mean, sd=filter(data, samp_div=="Upstream")$family_sd))+
  stat_function(data = data %>% filter(samp_div=="Mid"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Mid")$family_mean, sd=filter(data, samp_div=="Mid")$family_sd))+
  stat_function(data = data %>% filter(samp_div=="Downstream"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Downstream")$family_mean, sd=filter(data, samp_div=="Downstream")$family_sd))+
  xlab("JAK (orang)")+
  labs(title="Sebaran densitas jumlah anggota keluarga responden")+
  theme(legend.position = "none",
        text=element_text(size=10, face="bold", family = "mono"))

# Skor CSA
raw %>%
  ggplot()+
  aes()+
  geom_histogram(aes(x=CSSA_score, y=after_stat(density), fill=samp_div), alpha=2)+
  facet_grid(samp_div ~ .)+
  stat_function(data = data %>% filter(samp_div=="Upstream"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Upstream")$CSSA_score_mean, sd=filter(data, samp_div=="Upstream")$CSSA_score_sd))+
  stat_function(data = data %>% filter(samp_div=="Mid"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Mid")$CSSA_score_mean, sd=filter(data, samp_div=="Mid")$CSSA_score_sd))+
  stat_function(data = data %>% filter(samp_div=="Downstream"),
fun=dnorm,
args = list(mean=filter(data, samp_div=="Downstream")$CSSA_score_mean, sd=filter(data, samp_div=="Downstream")$CSSA_score_sd))+
  xlab("Skor")+
  labs(title="Sebaran densitas skor CSA responden")+
  theme(legend.position = "none",
        text=element_text(size=14, face="bold", family = "mono"))
