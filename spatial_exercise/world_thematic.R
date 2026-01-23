library(rnaturalearth)
library(countrycode)
library(ggplot2)
library(sf)
library(dplyr)

kopi <- read.csv("FAOSTAT_data_en_1-23-2026_KOPI_1984_1994_2024.csv")
glimpse(kopi)
kopi$Year.Code <- as.character(kopi$Year.Code)

world_small <- ne_countries(scale = "small", returnclass = "sf")

world_small %>%
  st_transform(crs="+proj=robin") %>%
  ggplot()+
  geom_sf()

world_coffe <-
  world_small %>% select(geometry,name,iso_a3) %>%
  left_join(kopi,by=c("iso_a3"="ISO3"))

world_small %>%
  filter(admin!="Antartica") %>%
  st_transform(crs="+proj=robin") %>%
  ggplot()+
  geom_sf(color="darkgrey")+
  geom_sf(data=world_coffe %>%
  na.omit() , aes(fill=Value/1000))+
  scale_fill_distiller(palette = "Blues",direction = 1)+
  facet_wrap(vars(Year.Code), ncol = 1)+
  labs(title = "World Countries Coffe Exporter 1984, 1994, 2024",
subtitle = "Unit measure in 000 ton",
caption = "Source : FAOSTAT",
fill="Quantity (000 ton):")+
  theme_minimal()+
  theme(plot.title = element_text(family = "Roboto", face = "bold"),
plot.subtitle = element_text(family = "Roboto",face = "italic"),
plot.caption = element_text(family = "Roboto",face = "bold.italic"))
