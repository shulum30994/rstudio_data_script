library(readxl)
library(sf)
library(tmap)
library(dplyr)

#### DATA TALANG ####
mt3<-read_excel('/home/shohibul/KERJA/DATA/DPPM dikti 2026/mt3_talang_crop_category.xlsx')

talang <- read_sf('/home/shohibul/KERJA/DATA/Irigasi Talang 2023/TALANG MAP/GRAND DATA/ADM_AREA.shp')

canal <- read_sf('/home/shohibul/KERJA/DATA/Irigasi Talang 2023/TALANG MAP/GRAND DATA/IRRIGATION.shp')

talang_mt3 <- left_join(talang, mt3, by=c("GID_4"="kode"))

#### DATA WILALUNG ####
nama_petani<-read.csv('/home/shohibul/KERJA/DATA/DPPM dikti 2026/referensi_divisi_wilalung.csv')

vill <- read_sf("/home/shohibul/KERJA/Pemetaan/Klambu Wilalung/Village ADM shp/VILL_ADM_BORDER.shp")

samp_div <- read.csv('/home/shohibul/KERJA/DATA/DPPM dikti 2026/wilalung_sampling_division.csv')

write.csv(st_drop_geometry(vill),
'nama_kode_desa.csv',
row.names=FALSE)

#### TALANG ####
mt3_crop<-talang_mt3 %>%
  tm_shape()+
  tm_polygons("Category",
fill.scale = tm_scale_categorical(
  values = c("Food"="#CCF8BF", "Food-others"="#8BEE6D", "Mixed"="#3CBB16")
))+
  tm_text("NAME_4")+
  tm_shape(canal)+
  tm_lines(lwd = 2, col = "blue")+
  tm_compass(position = c("right","top"))+
  tm_scalebar(position = c("left","top"))+
  tm_layout(
    title="MT3 Crop Distribution",
    title.fontface="bold"
  )

division<-talang_mt3 %>%
  tm_shape()+
  tm_polygons("Division",
fill.scale = tm_scale_categorical(
  values = c("Upper"="#CCF8BF", "Middle"="#8BEE6D", "Lower"="#3CBB16")
))+
  tm_text("NAME_4")+
  tm_shape(canal)+
  tm_lines(lwd = 2, col = "blue")+
  tm_compass(position = c("right","top"))+
  tm_scalebar(position = c("left","top"))+
  tm_layout(
    title="Sampling Area Division",
    title.fontface="bold"
  )

#### Wilalung ####
# Convert to sf features
koor<- st_as_sf(nama_petani,
coords = c("LONG","LAT"),
crs=4326)

vill_div <- left_join(vill, samp_div, by=c("GID_4"="kode"))

vill_div %>%
  tm_shape()+
  tm_polygons("division",fill.scale = tm_scale_categorical(
  values = c("Upper"="#CCF8BF", "Middle"="#8BEE6D", "Lower"="#3CBB16")
))+
  #tm_text("NAME_4")+
  tm_shape(koor)+
  tm_dots(col="black")+
  tm_text("name")+
  tm_compass(position = c("right","bottom"))+
  tm_scalebar(position = c("right","bottom"))+
  tm_layout(
    title="Sampling Area Division",
    title.fontface="bold"
  )

tmap_arrange(mt3_crop,division)
