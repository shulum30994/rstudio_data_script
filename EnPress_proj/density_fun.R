library(tidyverse)

raw <- read.csv('data14.csv')

data <- raw %>%
  na.omit %>%
  select(KAB,
  URBAN_STAT,
  jumlah_anggota_kel,
  B3_K5, # usia
R503B_III1, # luas bukan sawah
R503B_III2, # luas lahan sawah
R501D_K6, # total luas lahan
R501E1_K6, # luas pertanian dikuasai-diusahakan
B16F_K2, # total income
B16A_K2 # revenue pertanian
) %>%
  rename(AGE=B3_K5,
  LUAS.BUKAN.SAWAH=R503B_III1,
LUAS.SAWAH=R503B_III2,
TOTAL.LUAS=R501D_K6,
LUAS.AGR.DIUSAHAKAN=R501E1_K6,
TOTAL.INCOME=B16F_K2,
REVENUE.PERTANIAN=B16A_K2
)

data$KAB <- as.factor(data$KAB)

urb_rural <- data %>%
  group_by(URBAN_STAT) %>%
summarise(across(where(is.numeric),
list(mean=~mean(.x, na.rm = T),
sd=~sd(.x, na.rm = T)), .names = "{.col}_{.fn}"))

#### USIA ####
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

#### JAK ####
data %>%
  ggplot()+
  aes()+
  geom_histogram(aes(x=jumlah_anggota_kel,y=after_stat(density), fill=URBAN_STAT), alpha=2)+
  facet_grid(URBAN_STAT ~ .)+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="DESA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='DESA')$jumlah_anggota_kel_mean,
sd=filter(urb_rural, URBAN_STAT=='DESA')$jumlah_anggota_kel_sd))+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="KOTA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='KOTA')$jumlah_anggota_kel_mean,
sd=filter(urb_rural, URBAN_STAT=='KOTA')$jumlah_anggota_kel_sd))+
  xlab("JAK (orang)")+
  labs(title = "Sebaran densitas Jumlah Anggota Keluarga")+
  theme(legend.position = "none")

#### LUAS BUKAN SAWAH #####
data %>%
  ggplot()+
  aes()+
  geom_histogram(aes(x=LUAS.BUKAN.SAWAH,y=after_stat(density), fill=URBAN_STAT), alpha=2)+
  facet_grid(URBAN_STAT ~ .)+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="DESA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='DESA')$LUAS.BUKAN.SAWAH_mean,
sd=filter(urb_rural, URBAN_STAT=='DESA')$LUAS.BUKAN.SAWAH_sd))+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="KOTA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='KOTA')$LUAS.BUKAN.SAWAH_mean,
sd=filter(urb_rural, URBAN_STAT=='KOTA')$LUAS.BUKAN.SAWAH_sd))+
  xlab("Luas (m2)")+
  labs(title = "Sebaran densitas luas bukan sawah")+
  theme(legend.position = "none")

#### LUAS SAWAH #####
data %>%
  ggplot()+
  aes()+
  geom_histogram(aes(x=LUAS.SAWAH,y=after_stat(density), fill=URBAN_STAT), alpha=2)+
  facet_grid(URBAN_STAT ~ .)+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="DESA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='DESA')$LUAS.SAWAH_mean,
sd=filter(urb_rural, URBAN_STAT=='DESA')$LUAS.SAWAH_sd))+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="KOTA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='KOTA')$LUAS.SAWAH_mean,
sd=filter(urb_rural, URBAN_STAT=='KOTA')$LUAS.SAWAH_sd))+
  xlab("Luas (m2)")+
  labs(title = "Sebaran densitas luas sawah")+
  theme(legend.position = "none")

#### LUAS PERTANIAN DIUSAHAKAN #####
data %>%
  ggplot()+
  aes()+
  geom_histogram(aes(x=LUAS.AGR.DIUSAHAKAN,y=after_stat(density), fill=URBAN_STAT), alpha=2)+
  facet_grid(URBAN_STAT ~ .)+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="DESA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='DESA')$LUAS.AGR.DIUSAHAKAN_mean,
sd=filter(urb_rural, URBAN_STAT=='DESA')$LUAS.AGR.DIUSAHAKAN_sd))+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="KOTA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='KOTA')$LUAS.AGR.DIUSAHAKAN_mean,
sd=filter(urb_rural, URBAN_STAT=='KOTA')$LUAS.AGR.DIUSAHAKAN_sd))+
  xlab("Luas (m2)")+
  labs(title = "Sebaran densitas luas sawah")+
  theme(legend.position = "none")

#### LUAS PERTANIAN DIUSAHAKAN #####
data %>%
  ggplot()+
  aes()+
  geom_histogram(aes(x=TOTAL.INCOME,y=after_stat(density), fill=URBAN_STAT), alpha=2)+
  facet_grid(URBAN_STAT ~ .)+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="DESA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='DESA')$TOTAL.INCOME_mean,
sd=filter(urb_rural, URBAN_STAT=='DESA')$TOTAL.INCOME_sd))+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="KOTA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='KOTA')$TOTAL.INCOME_mean,
sd=filter(urb_rural, URBAN_STAT=='KOTA')$TOTAL.INCOME_sd))+
  xlab("Income (Rp)")+
  labs(title = "Sebaran densitas total pendapatan")+
  theme(legend.position = "none")

#### REVENUE DIUSAHAKAN #####
data %>%
  ggplot()+
  aes()+
  geom_histogram(aes(x=REVENUE.PERTANIAN,y=after_stat(density), fill=URBAN_STAT), alpha=2)+
  facet_grid(URBAN_STAT ~ .)+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="DESA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='DESA')$REVENUE.PERTANIAN_mean,
sd=filter(urb_rural, URBAN_STAT=='DESA')$REVENUE.PERTANIAN_sd))+
  stat_function(data = urb_rural %>% filter(URBAN_STAT=="KOTA"),
fun = dnorm,
args = list(mean=filter(urb_rural, URBAN_STAT=='KOTA')$REVENUE.PERTANIAN_mean,
sd=filter(urb_rural, URBAN_STAT=='KOTA')$REVENUE.PERTANIAN_sd))+
  xlab("Revenue (Rp)")+
  labs(title = "Sebaran densitas penerimaan pertanian")+
  theme(legend.position = "none")
