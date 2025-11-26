library(mlogit)
library(dplyr)
library(logitr)

##### Save the output #####
stargazer(production_logitr,
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
          out = "prod_cost_logitr.txt")


data("Heating", package = "mlogit")
H <- dfidx(Heating, choice = "depvar", varying = c(3:12))

long <- long %>%
  mutate(y_logic=as.logical(y))

data_long <- long %>%
  select(id_plot,
         y_logic,
         cost,
         fuel_cost,
         tanaman_lain)

sumur <- dfidx(short,choice = "y",varying = c(3:10))
sumur <- mlogit.data(short,
                     choice = "y",
                     shape = "wide",
                     varying = c(3:10),
                     sep = ".",
                     id.var = "idresponden")

coba <- mlogit(y ~ co + fc | 0, sumur)

m <- mlogit(depvar ~ ic + oc | 0, H)
summary(m)

##### LogitR #####
# Skala ulang biar stabil
data_long[,3:10] <- data_long[,3:10] / 1e6

# Ubah ke long format manual
library(tidyr)
data_long_1000 <- pivot_longer(
  data_long,
  cols = starts_with("co.") | starts_with("fc."),
  names_to = c(".value", "alt"),
  names_sep = "\\."
)

data_long_1000$outcome <- ifelse(data_long_1000$y == data_long_1000$alt, 1, 0)

df <- data_long_1000[, c("idcases", "alt", "outcome", "co", "fc")]

names(df)[names(df) == "idcases"] <- "obsID"

model_logitr <- logitr(
  data = sumur,
  outcome = "y",
  obsID = "idresponden",
  pars = c("co", "fc","idx$id2")
)

wtp_est<-wtp(model_logitr,scalePar = "fc")

barplot(
  wtp_est$Estimate,
  names.arg = wtp_est$Par,
  main = "Willingness To Pay",
  ylab = "Unit Harga",
  col = "skyblue"
)

##### Volume WTP #####
volume_logitr <- logitr(
  data = volume_long,
  outcome = "plot",
  obsID = "idplot",
  pars = c("cost","fuel_cost", "volume","crops")
)

wtp_volume <- wtp(volume_logitr,scalePar = "fuel_cost")
wtp_volume

##### Volume-Cost WTP #####
volcost_logitr <- logitr(
  data = volume_long,
  outcome = "plot",
  obsID = "idplot",
  pars = c("cost", "volume","crops")
)

wtp_volcost <- wtp(volcost_logitr,scalePar = "cost")
wtp_volume

##### Harga Jual WTP #####
outprice_logitr <- logitr(
  data = output_logitr,
  outcome = "plot",
  obsID = "idplot",
  pars = c("fuel_cost", "output_price","crops")
)

wtp_outprice <- wtp(outprice_logitr,scalePar = "output_price")
wtp_outprice

##### Biaya-Harga Jual WTP #####
outcost_logitr <- logitr(
  data = output_logitr,
  outcome = "plot",
  obsID = "idplot",
  pars = c("cost", "output_price","crops")
)

wtp_outcost <- wtp(outcost_logitr,scalePar = "output_price")
wtp_outcost

##### Produksi WTP #####
production_logitr <- logitr(
  data = production_long,
  outcome = "plot",
  obsID = "idplot",
  pars = c("production","fuel_cost","crops")
)

wtp_production <- wtp(production_logitr,scalePar = "fuel_cost")
wtp_production

##### Produksi-Biaya WTP #####
prod_cost_logitr <- logitr(
  data = production_long,
  outcome = "plot",
  obsID = "idplot",
  pars = c("production","fuel_cost","volume","crops")
)
summary(prod_cost_logitr)

wtp_prod_cost <- wtp(prod_cost_logitr,scalePar = "volume")
wtp_prod_cost
