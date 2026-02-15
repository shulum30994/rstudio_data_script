library(sf)
library(ggspatial)

jember <- read_sf("https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_JEMBER/JEMBER_TANJUNG.geojson")
lumajang <- read_sf('https://raw.githubusercontent.com/shulum30994/spatial_collection/refs/heads/main/DIGGING_WELL_LUMAJANG/LUMAJANG_BLUKON.geojson')

mt1_plot_jbr <- left_join(jember %>% select(descriptio,geometry), mt1_clean %>% select(Code,Commodities), by=c('descriptio'='Code'))
mt1_plot_lmj <- left_join(lumajang %>% select(descriptio,geometry), mt1_clean %>% select(Code,Commodities), by=c('descriptio'='Code'))

mt2_plot_jbr <- left_join(jember %>% select(descriptio,geometry), mt2_clean %>% select(Code,Commodities), by=c('descriptio'='Code'))
mt2_plot_lmj <- left_join(lumajang %>% select(descriptio,geometry), mt2_clean %>% select(Code,Commodities), by=c('descriptio'='Code'))

mt1_plot_jbr<-mt1_plot_jbr %>%
  na.omit %>%
  mutate(season='Crop Season 1', location='JEMBER')

mt1_plot_lmj<-mt1_plot_lmj%>%
  na.omit %>%
  mutate(season='Crop Season 1', location='LUMAJANG')

mt2_plot_jbr<-mt2_plot_jbr %>%
  na.omit %>%
  mutate(season='Crop Season 2', location='JEMBER')

mt2_plot_lmj<-mt2_plot_lmj %>%
  na.omit %>%
  mutate(season='Crop Season 2', location='LUMAJANG')

all_map_seasons <- bind_rows(mt1_plot_jbr,mt1_plot_lmj,mt2_plot_jbr,mt2_plot_lmj)

mt1_plot %>%
  na.omit %>%
  nrow()

mt2_plot %>%
  na.omit %>%
  nrow()

mt1_plot_lmj %>%
  na.omit %>%
  ggplot()+
  aes()+
  geom_sf()

mt2_plot_lmj %>%
  na.omit %>%
  ggplot()+
  aes()+
  geom_sf()

  jember_comm_change<-ggplot()+
  aes()+
  geom_sf(data = jember)+
    geom_sf(data = all_map_seasons %>% filter(location=='JEMBER'), aes(fill=Commodities))+
    scale_fill_manual(values = c("Food"="Red","Horticulture"="Yellow","Plantation"="Blue"))+
    annotation_north_arrow(location="tl",which_north="true")+
    annotation_scale(location="br")+
  facet_wrap(location~season)+
    theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

lumajang_comm_change<-ggplot()+
  aes()+
  geom_sf(data = lumajang)+
    geom_sf(data = all_map_seasons %>% filter(location=='LUMAJANG'), aes(fill=Commodities))+
    scale_fill_manual(values = c("Food"="Red","Horticulture"="Yellow","Plantation"="Blue"))+
    annotation_north_arrow(location="tl",which_north="true")+
    annotation_scale(location="bl")+
  facet_wrap(location~season)+
    theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

(jember_comm_change/lumajang_comm_change)+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
