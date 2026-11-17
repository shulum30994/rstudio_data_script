library(tidyverse)
library(factoextra)
library(cluster)
library(tibble)
library(sf)

# Load the data
thai_data <- read.csv('https://raw.githubusercontent.com/shulum30994/rstudio_data_script/refs/heads/main/cluster_analysis/thailand_cluster_indicator.csv')

# Data preparation
thai_clus <- thai_data %>%
  select(-prov_code,province_name) %>%
  column_to_rownames(var="province_name")

# Scale or standarisation on data value
numeric_df <- scale(thai_clus)

# Determine the number of cluster
fviz_nbclust(
  numeric_df,
  kmeans,
  method = 'wss'
)+
  geom_vline(xintercept = 5, linetype=2)

# Cluster Analysis using k-means
numeric_kmeans <- kmeans(numeric_df,5,nstart = 25)

# Cluster visualization
fviz_cluster(
  numeric_kmeans,
  data=numeric_df,
  palette=c('#233d4d','#197278','#edddd4','#c44536','#772e25'),
  ellipse.type = 'euclid',
  star.plot=TRUE,
  repel = TRUE,
  ggtheme = theme_minimal()
)

# Joined the analytical result into main data
thai_data$cluster <- as.factor(numeric_kmeans$cluster)

# Making thematic map based on cluster analysis
thai_province <- read_sf("thai_provinces.geojson")

thai_province_cluster <- left_join(thai_province, thai_data %>% select(prov_code,cluster), by=c("code"="prov_code"))
thai_province_cluster %>%
  ggplot()+
  aes(fill=cluster)+
  geom_sf()+
  scale_fill_brewer(palette = "Accent", direction = -1)
