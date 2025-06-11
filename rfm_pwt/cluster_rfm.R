library(factoextra)
library(cluster)
library(dplyr)

clus_rfm <- rfm_kopi_901212000_df %>%
  select(customer_id,
         recency_days,
         transaction_count,
         amount,
         recency_score,
         frequency_score,
         monetary_score,
         rfm_score)

rownames(clus_rfm)<-clus_rfm$customer_id

numeric_df <- clus_rfm %>% select(recency_days,
                                  transaction_count,
                                  amount)

scale_df <- clus_rfm %>% select(recency_score,
                                monetary_score,
                                rfm_score)
numeric_df <- scale(numeric_df)
scale_df <- scale(scale_df)

#### Numeric Cluster ####
fviz_nbclust(numeric_df,
             kmeans,
             method = "wss")+
  geom_vline(xintercept = 4,linetype=2)
numeric_kmeans <- kmeans(numeric_df,4,nstart = 25)

fviz_cluster(numeric_kmeans,data=numeric_df,
             palette=c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid",
             star.plot=TRUE,
             repel = TRUE,
             ggtheme = theme_minimal())

#### Scale Cluster ####
fviz_nbclust(scale_df,
             kmeans,
             method = "wss")+
  geom_vline(xintercept = 4,linetype=2)
scale_kmeans <- kmeans(scale_df,4,nstart = 25)

fviz_cluster(scale_kmeans,data=scale_df,
             palette=c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid",
             star.plot=TRUE,
             repel = TRUE,
             ggtheme = theme_minimal())
joined_scale_cluster_df <- cbind(scale_df,cluster=scale_kmeans$cluster)

aggregate(clus_rfm %>% select(recency_days,
                              transaction_count,
                              amount),by=list(cluster=scale_kmeans$cluster),mean)
