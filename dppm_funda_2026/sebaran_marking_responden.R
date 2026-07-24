library(googlesheets4)
library(sf)
library(tmap)
library(dplyr)

#### DATA TALANG ####
resp_csa<-read_sheet('https://docs.google.com/spreadsheets/d/1EvmIAri2-BDz43-BhR1tSwzd94xS2jF78LGYHnlm1Tc/edit?gid=0#gid=0')

resp_gender<-read_sheet('https://docs.google.com/spreadsheets/d/1M762oiKlf32lAQKrDZoc0KCvkgjJOr_rYhFSN2Ijiz0/edit?gid=950375195#gid=950375195')

resp_csa_sf <- st_as_sf(resp_csa,
                         coords=c('LONG', 'LAT'),
                         crs=4326)

resp_gender_sf <- st_as_sf(resp_gender,
                        coords=c('LONG', 'LAT'),
                        crs=4326)

division <- read_sheet('https://docs.google.com/spreadsheets/d/1TL19mnRCUviuxIq_sDRjgWGO_d15UcIvCoStMDZJKUk/edit?gid=0#gid=0', sheet = 'IDENTITY')

talang <- read_sf('G:\\2023\\DIGITASI TALANG\\TALANG MAP\\GRAND DATA\\ADM_AREA.shp')

canal <- read_sf('G:\\2023\\DIGITASI TALANG\\TALANG MAP\\GRAND DATA\\IRRIGATION.shp')

canal$GID_4 <- replace(canal$GID_4, canal$GID_4 %in% c("IDN.11.8.29.7_1"), c("IDN.11.8.29.6_1")) # replace data GID_4 Desa Sukodadi dengan IDN.11.8.29.6_1

talang_division <- left_join(talang, division %>% select(ID_VILL, AREA_DIVISION), by=c("GID_4"="ID_VILL"))

canal_division <- left_join(canal, division %>% select(ID_VILL, AREA_DIVISION), by=c("GID_4"="ID_VILL"))

#### DATA WILALUNG ####
nama_petani<-read.csv('/home/shohibul/KERJA/DATA/DPPM dikti 2026/referensi_divisi_wilalung.csv')

vill <- read_sf("/home/shohibul/KERJA/Pemetaan/Klambu Wilalung/Village ADM shp/VILL_ADM_BORDER.shp")

samp_div <- read.csv('/home/shohibul/KERJA/DATA/DPPM dikti 2026/wilalung_sampling_division.csv')

write.csv(st_drop_geometry(vill),
          'nama_kode_desa.csv',
          row.names=FALSE)

#### TALANG ####
talang_division %>%
  #filter(GID_4=='IDN.11.8.2.2_1') %>%
  tm_shape()+
  tm_polygons("AREA_DIVISION",
              lwd = 0.5,
              fill.scale = tm_scale_categorical(
                values = c("Upper"="#CCF8BF", "Mid"="#8BEE6D", "Lower"="#3CBB16")
              ),
              fill.legend = tm_legend(
                title = "Keterangan :",
                title.fontfamily = "Times New Roman",
                title.fontface = "bold",
                text.fontfamily = "Times New Roman",
                text.fontface = "plain"
              ))+
  tm_text(
    text='NAME_4',
    fontface='italic',
    fontfamily = 'Times New Roman'
  )+
  tm_facets(by='AREA_DIVISION')+
  tm_shape(canal_division)+
  tm_lines(lwd = 0.8, col = "blue")+
  tm_facets(by='AREA_DIVISION')+
  #tm_shape(resp_csa_sf)+
  tm_shape(resp_gender_sf)+
  tm_dots(fill = 'black', size = 0.4)+
  tm_compass(position = c("right","top"))+
  tm_scalebar(position = c("left","top"))+
  tm_title("Sebaran Responden CSA Gender")+
  tm_add_legend(
    type = "line",
    labels = "Saluran Irigasi",
    col = "blue",
    lwd=2
  )+
  tm_add_legend(
    type = "symbols",
    labels = "Responden",
    fill="black",
    shape=16,
    size=0.4
  )+
  tm_layout(
    legend.title.size=1.1,
    legend.text.size=0.8,
    fontfamily = "Times New Roman"
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
