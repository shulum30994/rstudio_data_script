library(dplyr)
library(ggplot2)
library(ragg)
library(rfm)

kopi <- read.csv("https://raw.githubusercontent.com/shulum30994/rstudio_data_script/refs/heads/main/rfm_pwt/sudah_gabung_semua.csv")

metadata<-read.csv("https://raw.githubusercontent.com/shulum30994/rstudio_data_script/refs/heads/main/rfm_pwt/pedoman_kode_HS_kolom.csv")

# Coffee, roasted, not decaffeinated, unground
kopi_901212000 <- kopi %>% filter(HSCODE==901212000)

# set the date format through new column
kopi_901212000 <- kopi_901212000 %>% mutate(YEAR_DATE=paste("1",PROCMTH,PROCYEAR,sep = "/"))

# convert new column into date type
kopi_901212000 <- kopi_901212000 %>% mutate(YEAR_DATE=as.Date(strptime(YEAR_DATE,"%d/%m/%Y")))
kopi_901212000<- kopi_901212000 %>% mutate(customer_id=DESTCTRY_L)

# check last transaction date
kopi_901212000 %>% arrange(desc(YEAR_DATE)) %>% head(5)

# perform RFM analysis
rfm_kopi_901212000 <- rfm_table_order(
  data = kopi_901212000,
  customer_id = customer_id,
  revenue = VAL,
  order_date = YEAR_DATE,
  analysis_date = as.Date("2016-12-01")
)

# convert into dataframe
rfm_kopi_901212000_df <- as.data.frame(rfm_kopi_901212000$rfm)

# save into .csv format
write.csv(rfm_kopi_901212000_df,"rfm_result.csv")
