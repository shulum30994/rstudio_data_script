library(factoextra)

# Penggabungan data
rfm_segment <- left_join(
  segments %>%
    select(customer_id,
           transaction_count,
           recency_days,
           amount,
           rfm_score,
           segment),
  kopi_901212000 %>%
    select(DESTCTRY,
           DESTCTRY_L,
           VAL,
           NETWT) %>%
    group_by(DESTCTRY,
             DESTCTRY_L) %>%
    summarise(ave_VAL=mean(VAL),
              sum_NETWT=sum(NETWT)),
  by=c("customer_id"="DESTCTRY")
)

# Scaling (normalisasi) data
rfm_segment_scaled <- scale(rfm_segment %>%
                              select(transaction_count,
                                     recency_days,
                                     amount))

# Konversi data frame menjadi objek matrix
rfm_segment_matrix<- as.matrix(rfm_segment_scaled)
rownames(rfm_segment_matrix)=rfm_segment$DESTCTRY_L

# Jumlah cluster maksimal yang dapat dibuat dengan metode WSS
fviz_nbclust(rfm_segment_matrix,
             kmeans,
             method = "wss")+
  geom_vline(xintercept=4,
             linetype=2)
# Jumlah cluster maksimal yang dapat dibuat dengan metode Silhoutte
fviz_nbclust(rfm_segment_matrix,
             kmeans,
             method = "silhouette")+
  labs(subtitle = "Jumlah cluster berdasarkan metode Silhoutte")

# Mengitung pengelompokan negara ke dalam kelompok atau cluster
set.seed(123)
rfm_cluster <- kmeans(rfm_segment_matrix,
                        4,
                        nstart = 25)

# Menggabungkan data hasil cluster ke data asal
world_rfm_cluster <-cbind(
  rfm_segment,
  cluster=rfm_cluster$cluster
)

# Visualisasi hasil identifikasi masing-masing negara ke dalam cluster
fviz_cluster(rfm_cluster,
             data = rfm_segment_scaled,
             palette=c("#70d6ff","#ff70a6","#ff9770","#ffd670"),
             ellipse.type = "euclid",
             star.plot=TRUE,
             repel = TRUE,
             ggtheme = theme_minimal())

# Menyimpan hasil cluster ke dalam format .csv
write.csv(world_rfm_cluster, "rfm_cluster_result.csv", row.names = FALSE)

# Dimensionality PCA (Principal Component Analysis)
fviz_pca_ind(prcomp(rfm_segment_scaled),
             title="PCA - RFM",
             #habillage = rfm_segment$customer_id,
             palette = "jco",
             geom="point",
             ggtheme=theme_classic(),
             legend="bottom")

# Visualisasi hasil cluster dalam bentuk peta
dunia <- ne_countries(scale = "small",
                      returnclass = "sf")

# Standarisasi nama negara sesuai iso3c
world_rfm_iso <- world_rfm_cluster %>%
  mutate(iso3=countrycode::countrycode(
    sourcevar = DESTCTRY_L,
    origin = "country.name",
    destination = "iso3c"
  ))

# Penggabungan data dunia dengan data cluster
cluster_rfm_exporter <-
  dunia %>%
  select(geometry,name,iso_a3) %>%
  left_join(world_rfm_iso, by=c("iso_a3"="iso3")) %>%
  filter(ave_VAL!=0)

# Plotting peta
dunia %>%
  st_transform(crs="+proj=robin") %>%
  ggplot()+
  geom_sf(color="gray")+
  geom_sf(data=cluster_rfm_exporter,
          aes(fill=as.factor(cluster)))+
  geom_label_repel(data=cluster_rfm_exporter,
                   aes(label = segment,
                       geometry=geometry),
                   stat = "sf_coordinates",
                   min.segment.length = 0)+
  scale_fill_manual(values = c("#70d6ff","#ff70a6","#ff9770","#ffd670"))+
  theme_minimal()+
  theme(axis.text = element_blank())+
  labs(fill="Cluster :")

# Jumlah masing-masing negara tiap cluster
world_rfm_cluster %>%
  group_by(as.factor(cluster)) %>%
  count(as.factor(cluster))
