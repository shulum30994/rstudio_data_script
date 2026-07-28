##### DISTRIBUSI DATA DENGAN FUNGSI DENSITAS SEDERHANA #####
library(tidyverse)

# Hitung rata-rata (mean) dan deviasi standar (sd) lalu grouping data berdasarkan kategori (DESA atau KOTA)
urb_rural <- data %>%
  group_by(URBAN_STAT) %>%
summarise(across(where(is.numeric),
list(mean=~mean(.x, na.rm = T),
sd=~sd(.x, na.rm = T)), .names = "{.col}_{.fn}"))

# Plot fungsi densitas (data group+data utama) berdasarkan kategori (DESA atau KOTA)
data %>%
  ggplot()+
  aes()+
  geom_histogram(aes(x=AGE,y=after_stat(density), fill=URBAN_STAT), alpha=2)+
  facet_grid(URBAN_STAT ~ .)+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="DESA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='DESA')$AGE_mean,
sd=filter(urb_rural, URBAN_STAT=='DESA')$AGE_sd))+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="KOTA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='KOTA')$AGE_mean,
sd=filter(urb_rural, URBAN_STAT=='KOTA')$AGE_sd))+
  xlab("Usia (tahun)")+
  labs(title = "Sebaran densitas usia responden")+
  theme(legend.position = "none")
