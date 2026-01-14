library(tidyverse)
library(ggrepel)

old_price <- read.csv("used_car_prices.csv")

old_price %>%
  count(Car.Model, sort = T) %>%
  slice_head(n=5)

#1  Chevrolet Aveo 2009
#2 Chevrolet Cruze 2010
#3 Chevrolet Optra 2008
#4    Daewoo Lanos 1999
#5    Daewoo Lanos 2000

old_price %>%
  count(Car.Model, sort = T) %>%
  slice_tail(n=5)

#1     Toyota Rumion 2022
#2      Toyota Yaris 2018
#3      Toyota Yaris 2019
#4 Volkswagen Golf 4 2000
#5       Volvo XC 40 2021

# Price extraction
old_price<-old_price %>%
  na.omit %>%
  mutate(new_date=paste(Month.Year,"-","01",sep = ""),
    max_price=str_extract(Maximum.price,".*(?= EGP)") %>% str_replace_all(",",""),
    min_price=str_extract(Minimum.price,".*(?= EGP)") %>% str_replace_all(",",""),
    avg_price=str_extract(Average.price,".*(?= EGP)") %>% str_replace_all(",",""))

# Fav cars
fav_cars<-old_price %>%
  select(Car.Model,new_date,min_price,avg_price,max_price) %>%
  filter(Car.Model=="Chevrolet Aveo 2009"|Car.Model=="Chevrolet Cruze 2010"|Car.Model=="Chevrolet Optra 2008"|Car.Model=="Daewoo Lanos 1999"|Car.Model=="Daewoo Lanos 2000") %>%
  mutate(across(c('new_date'),as.Date),
across(c('min_price','avg_price','max_price'),as.numeric))

unfav_cars<-old_price %>%
  select(Car.Model,new_date,min_price,avg_price,max_price) %>%
  filter(Car.Model=="Toyota Rumion 2022"|Car.Model=="Toyota Yaris 2018"|Car.Model=="Toyota Yaris 2019"|Car.Model=="Volkswagen Golf 4 2000"|Car.Model=="Volvo XC 40 2021") %>%
  mutate(across(c('new_date'),as.Date),
across(c('min_price','avg_price','max_price'),as.numeric))

end_fav <- fav_cars %>%
  group_by(Car.Model) %>%
  slice_max(order_by = new_date,n=1) %>%
  ungroup()

end_unfav <- unfav_cars %>%
  na.omit %>%
  group_by(Car.Model) %>%
  slice_max(order_by = new_date,n=1) %>%
  ungroup()

old_price %>%
  count(Car.Model, sort = T) %>%
  slice_head(n=5) %>%
  ggplot()+
  aes(x=Car.Model,y=n)+
  geom_col()+
  coord_flip()+
  theme(legend.position = "none")

fav_cars %>%
  ggplot()+
  aes(x=new_date,y=avg_price)+
  geom_line(aes(col=Car.Model))+
  geom_label(data = end_fav, aes(label=Car.Model), hjust=-0.1)+
  scale_x_date(expand = expansion(mult = c(0.02, 0.15)))+
  xlab('Year')+
  ylab('Price (EGP)')+
  labs(title = "Price of Favorite Car Models in Eqypt (EGP)",
caption = "Source : Kaggle (2025)")+
  theme(plot.title = element_text(size = 14, face = "bold", family = "Monospace"),
        plot.caption = element_text(size = 10, face = "italic", family = "serif"),
        axis.title.x = element_text(size = 12, face = "bold",family = "Monospace"),
        axis.title.y = element_text(size = 12, face = "bold",family = "Monospace"),
        axis.text.x = element_text(size = 11, face = "bold",family = "Monospace"),
        axis.text.y = element_text(size = 11, face = "bold",family = "Monospace"),
    legend.position = "none")

unfav_cars %>%
  na.omit() %>%
  ggplot()+
  aes(x=new_date,y=avg_price)+
  geom_line(aes(col=Car.Model))+
  geom_label(data = end_unfav, aes(label=Car.Model), hjust=-0.1)+
  scale_x_date(expand = expansion(mult = c(0.02, 0.15)))+
  xlab('Year')+
  ylab('Price (EGP)')+labs(title = "Price of Favorite Car Models in Eqypt (EGP)",
caption = "Source : Kaggle (2025)")+
  theme(plot.title = element_text(size = 14, face = "bold", family = "Monospace"),
        plot.caption = element_text(size = 10, face = "italic", family = "serif"),
        axis.title.x = element_text(size = 12, face = "bold",family = "Monospace"),
        axis.title.y = element_text(size = 12, face = "bold",family = "Monospace"),
        axis.text.x = element_text(size = 11, face = "bold",family = "Monospace"),
        axis.text.y = element_text(size = 11, face = "bold",family = "Monospace"),
    legend.position = "none")
