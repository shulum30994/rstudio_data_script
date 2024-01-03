library(factoextra)
library(sf)
library(ggrepel)
library(RColorBrewer)
library(ggthemes)

#### Cluster Analysis ####
kab.kalimantan <-read.csv('/home/shohibul/R Wokspace/cluster_kalimantan/contoh1.csv')
caption = as.list(kab.kalimantan$kab)
kalimantan.kmeans <- scale(kab.kalimantan[,c(2,3,5,6,7,8,9,10)])
rownames(kalimantan.kmeans)=c(caption)

fviz_nbclust(kalimantan.kmeans,kmeans,method = "wss")+geom_vline(xintercept = 5,linetype=2)

kmeans.result <-kmeans(kalimantan.kmeans,6,nstart = 30)
print(kmeans.result)

aggregate(kalimantan.kmeans, by=list(cluster=kmeans.result$cluster),mean)

dd <- cbind(kalimantan.kmeans, cluster=kmeans.result$cluster)
head(dd)

fviz_cluster(kmeans.result,data = kalimantan.kmeans,
             palette="Dark2",
             ellipse.type = "euclid",
             star.plot=TRUE,
             repel = TRUE,
             ggtheme = theme_minimal())

#### Thematic Mapping ####
kalimantan_sf <- read_sf("/home/shohibul/KERJA/DATA/MASTER DATASET/SPASIAL/Pulau Kalimantan/Kecuali Tarakan/kecuali_tarakan_2022.shp")

# data frame cluster_kab
cluster_kab <- data.frame(dd)
cluster_kab <- tibble::rownames_to_column(cluster_kab,"kabupaten")

# join dataframe
kalimantan_sf <- left_join(kalimantan_sf,cluster_kab,by=c("NAME_2"="kabupaten"))

# Preview ggplot
ggplot(kalimantan_sf)+geom_sf()

# tematik klaster
ggplot(kalimantan_sf)+geom_sf(aes(fill=as.factor(cluster)))+scale_fill_brewer(palette="Blues",direction = -1)+labs(fill="Cluster :")+theme_minimal()
