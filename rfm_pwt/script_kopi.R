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
  data = kopi_901212000 %>% na.omit(),
  customer_id = DESTCTRY,
  revenue = revenue,
  order_date = YEAR_DATE,
  analysis_date = as.Date("2016-12-02")
)

# heatmap
rfm_plot_heatmap(rfm_kopi_901212000)

# bar chart
rfm_plot_bar_chart(rfm_kopi_901212000)

# scatter plot
rfm_plot_segment_scatter(rfm_kopi_901212000)

# convert into dataframe
rfm_kopi_901212000_df <- as.data.frame(rfm_kopi_901212000$rfm)

rfm_kopi_901212000_df %>%
 # group_by(recency_score) %>%
  summarise(across(c('recency_days',
                     'transaction_count',
                     'amount'),mean))

rfm_kopi_901212000_df %>%
  group_by(recency_score) %>%
  summarise(Recency=count(as.factor(recen)))

dist_901212000<-cbind(rfm_kopi_901212000_df %>%
        group_by(recency_score) %>%
        count(recency_score),
      rfm_kopi_901212000_df %>%
        group_by(frequency_score) %>%
        count(frequency_score),
      rfm_kopi_901212000_df %>%
        group_by(monetary_score) %>%
        count(monetary_score)) %>%
  select(recency_score,n...2,n...4,n...6) %>%
  rename(SKOR=recency_score,
         Recency=n...2,
         Frequency=n...4,
         Monetary=n...6)

dist_901212000<-dist_901212000 %>%
  group_by(SKOR) %>%
  mutate(sub_total=sum(c(Recency,Frequency,Monetary)),
         Recency_perc=round((Recency/sub_total)*100,1),
         Frequency_perc=round((Frequency/sub_total)*100,1),
         Monetary_perc=round((Monetary/sub_total)*100,1))

# visualisasi distribusi RFM berdasarkan negara
rfm_kopi_901212000_df %>%
  select(customer_id,recency_score,frequency_score,monetary_score)%>%
  mutate(sum_RFM=recency_score+frequency_score+monetary_score,
         Recency=(recency_score/sum_RFM)*100,
         Frequency=(frequency_score/sum_RFM)*100,
         Monetary=(monetary_score/sum_RFM)*100) %>%
  #stack(c("Recency","Frequency","Monetary"))
  pivot_longer(cols = c("Recency","Frequency","Monetary"),
               names_to = "kategori",
               values_to = "nilai") %>%
  mutate(new_cat=factor(kategori, levels=
                          c("Monetary",
                            "Frequency",
                            "Recency"), ordered = TRUE)) %>%
  ggplot()+
  aes(x=customer_id,y=nilai,fill = new_cat)+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("Recency"="#084C61","Frequency"="#E3B505","Monetary"="#DB504A"))+
  labs(fill="Keterangan",
       title = "Proporsi skor RFM berdasarkan Negara",
       y="Proporsi Skor",
       x="Negara")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom")+
  coord_flip()

# calculate weighted RFM score
rfm_kopi_901212000_df<-rfm_kopi_901212000_df %>% mutate(W.SCORE=5*recency_score+2*frequency_score+monetary_score)

# save into .csv format
write.csv(rfm_kopi_901212000_df,"rfm_result1.csv")
write.csv(dist_901212000,"dist_rfm.csv")
