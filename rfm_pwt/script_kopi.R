library(dplyr)
library(ggplot2)
library(tidyr)
library(ragg)
library(rfm)

kopi <- read.csv("https://raw.githubusercontent.com/shulum30994/rstudio_data_script/refs/heads/main/rfm_pwt/sudah_gabung_semua.csv")

metadata<-read.csv("https://raw.githubusercontent.com/shulum30994/rstudio_data_script/refs/heads/main/rfm_pwt/pedoman_kode_HS_kolom.csv")

# Coffee, roasted, not decaffeinated, unground
kopi_901212000 <- kopi %>% filter(HSCODE==901212000)

# set the date format through new column
kopi_901212000 <- kopi_901212000 %>% mutate(YEAR_DATE=paste("1",PROCMTH,PROCYEAR,sep = "/"))

# visualize quantity against date
kopi_901212000 %>%
  ggplot()+
  aes(x=YEAR_DATE,y=VAL/1000)+
  geom_point(aes(colour =REG))+
  theme(legend.position = "none")

# convert new column into date type
kopi_901212000 <- kopi_901212000 %>% mutate(YEAR_DATE=as.Date(strptime(YEAR_DATE,"%d/%m/%Y")),customer_id=DESTCTRY,revenue=VAL*NETWT)

# check last transaction date
kopi_901212000 %>% arrange(desc(YEAR_DATE)) %>% head(5)

# perform RFM analysis
rfm_kopi_901212000 <- rfm_table_order(
  data=kopi_901212000,
  customer_id = DESTCTRY_L,
  revenue = REVENUE,
  order_date = YEAR_DATE,
  analysis_date = as.Date("2016-12-02")
)

# convert into dataframe
rfm_kopi_901212000_df <- as.data.frame(rfm_kopi_901212000$rfm)

# calculate weighted RFM score
rfm_kopi_901212000_df<-rfm_kopi_901212000_df %>% mutate(W.SCORE=5*recency_score+2*frequency_score+monetary_score)

# rfm score and weighted score
rfm_kopi_901212000_df %>%
  select(customer_id,rfm_score,W.SCORE) %>%
  group_by(customer_id) %>%
  summarise(across(c("rfm_score","W.SCORE"),.fns=mean))

# heatmap
rfm_plot_heatmap(rfm_kopi_901212000)

# bar chart
rfm_plot_bar_chart(rfm_kopi_901212000)

# scatter plot
rfm_plot_segment_scatter(rfm_kopi_901212000)

# segment names
segment_names <- c("Champions", "Potential Loyalist", "Loyal Customers",
                   "Promising", "New Customers", "Can't Lose Them",
                   "At Risk", "Need Attention", "About To Sleep", "Lost")

# segment intervals
recency_lower <- c(5, 3, 2, 3, 4, 1, 1, 1, 2, 1)
recency_upper <- c(5, 5, 4, 4, 5, 2, 2, 3, 3, 1)
frequency_lower <- c(5, 3, 2, 1, 1, 3, 2, 3, 1, 1)
frequency_upper <- c(5, 5, 4, 3, 3, 4, 5, 5, 3, 5)
monetary_lower <- c(5, 2, 2, 3, 1, 4, 4, 3, 1, 1)
monetary_upper <- c(5, 5, 4, 5, 5, 5, 5, 5, 4, 5)

# generate segments
segments <- rfm_segment(rfm_kopi_901212000, segment_names, recency_lower,
                        recency_upper, frequency_lower, frequency_upper, monetary_lower,
                        monetary_upper)
# rfm report
rfm_create_report(rfm_kopi_901212000,  segments, FALSE, "Customer Segmentation Report")

# save into .csv format
write.csv(rfm_kopi_901212000_df,"rfm_result1.csv")
write.csv(dist_901212000,"dist_rfm.csv")
