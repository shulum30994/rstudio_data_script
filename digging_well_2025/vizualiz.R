library(tidyverse)

#### Distribution ####
ggplot(MT2 %>% na.omit)+
  aes(x=water_m3)+
  geom_histogram()

ggplot(MT2 %>% na.omit)+
  aes(x=id,y=horse_power)+
  geom_col()+
  facet_grid(vars(crop))+
  coord_flip()

ggplot(data)+
  aes(x=parent_id,y=annual_volume)+
  geom_col()+
  facet_wrap(vars(new_category))

ggplot(data)+
  aes(x=new_category,y=annual_volume)+
  geom_boxplot()

##### Scatter #####
ggplot(MT2 %>% na.omit)+
  aes(x=log(horse_power), y=log(water_m3))+
  geom_point(aes(colour = crop))+
  geom_smooth(method = lm)

ggplot(MT2 %>% na.omit)+
  aes(x=log(area_ha), y=log(water_m3))+
  geom_point()+
  geom_smooth(method = lm)+
  facet_grid(vars(crop))
