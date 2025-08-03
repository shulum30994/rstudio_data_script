library(googlesheets4)
library(tidyverse)
library(ggrepel)

raw <- read_sheet("https://docs.google.com/spreadsheets/d/15j-82tISFJKrGKk5sdlE3-S6ge0geWt_xog1lEY7qgE/edit?gid=1754970135#gid=1754970135", sheet = "FARM_WORKERS")

# Fee each activities
raw %>%
  select(Date,
         Commodities,
         Start_dttm,
         End_dttm,
         Activity,
         `Fee (Rp)`) %>%
  filter(Commodities=="Tobacco") %>%
  group_by(Date,Activity) %>%
  summarise(across(c("Fee (Rp)"),sum)) %>%
  ggplot()+
  aes(x=Date,y=`Fee (Rp)`)+
  geom_line()

# Working hours viz
raw %>%
  filter(Commodities=="Maize") %>%
  select(Date,
         Commodities,
         Start_dttm,
         End_dttm,
         Activity,
         `Fee (Rp)`) %>%
  group_by(Date, Activity) %>%
  mutate(Working_hours=difftime(End_dttm, Start_dttm, units = "hours")) %>%
  summarise(across(c("Working_hours"),mean)) %>%
  ggplot()+
  aes(x=Date,y=Working_hours,fill = Activity)+
  geom_col()

red_pepper<-raw %>%
  filter(Commodities=="Red Pepper") %>%
  select(Date,
         Commodities,
         Activity,
         `Fee (Rp)`) %>%
  mutate(relative_cumsum=cumsum(`Fee (Rp)`)) %>%
  group_by(Date)

tobacco %>%
  group_by(Activity) %>%
  summarise(across(c('Fee (Rp)'),sum))

tobacco<-raw %>%
  filter(Commodities=="Tobacco") %>%
  select(Date,
         Commodities,
         Start_dttm,
         End_dttm,
         Activity,
         `Fee (Rp)`) %>%
  mutate(relative_cumsum=cumsum(`Fee (Rp)`))

ggplot()+
  geom_line(data=red_pepper %>%
              group_by(Date) %>%
              summarise(across(c('relative_cumsum'),sum)),
            aes(
              x=Date,
              y=relative_cumsum
            ))

raw %>%
  group_by(Commodities) %>%
  filter(Commodities=="Tobacco"|Commodities=="Red Pepper") %>%
  select(Date,
         Commodities,
         Start_dttm,
         End_dttm,
         Activity,
         `Fee (Rp)`) %>%
  mutate(relative_cumsum=cumsum(`Fee (Rp)`))%>%
ggplot()+
  aes(x=Date,y=relative_cumsum,color=Commodities)+
  #geom_rect(aes(ymin = 0,
   #             ymax = Inf,
    #            xmin = Start_dttm,
     #           xmax = End_dttm))+
  geom_line()+
  #geom_label_repel(aes(label=Commodities),
                   #nudge_x = 1,
                  # na.rm = TRUE)+
  theme(legend.position = "bottom")

##### Tobacco and Red Pepper ####
toba_red<-raw %>% 
  group_by(Commodities) %>%
  filter(Date==max(Date)|Commodities=="Tobacco"|Commodities=="Red Pepper") %>%
  select(Date,
         Commodities,
         `Fee (Rp)`) %>%
  mutate(relative_cumsum = cumsum(`Fee (Rp)`),
         label=NA)

toba_red$label[which(toba_red$relative_cumsum == max(toba_red$relative_cumsum))] <- toba_red$Commodities[which(toba_red$relative_cumsum == max(toba_red$relative_cumsum))]

ggplot(toba_red)+
  aes(x=Date,y=relative_cumsum/9062.73,col=Commodities)+
  geom_line()+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm=T)+
  ylab("Cumulative Cost (¥)")+
  theme(legend.position = "none")

##### Tobacco and Paddy #####
tobacco_red_maize<-raw %>% 
  group_by(Commodities) %>%
  filter(Date==max(Date)|Commodities=="Tobacco"|Commodities=="Red Pepper"|Commodities=="Maize") %>%
  select(Date,
         Commodities,
         Activity,
         `Fee (Rp)`) %>%
  mutate(relative_cumsum = cumsum(`Fee (Rp)`),
         label=NA)

label_max <- tobacco_red_maize %>%
  group_by(Commodities) %>%
  summarise(across(c("relative_cumsum"),.fns=max))

label_max <-label_max %>%
  mutate(Tanggal=c("2024-08-13 00:00:00",
                "2024-09-22 00:00:00",
                NA,
                "2024-09-26 00:00:00"))

tobacco_red_maize$label[which(tobacco_red_maize$relative_cumsum == max(tobacco_red_maize$relative_cumsum))] <- tobacco_red_maize$Commodities[which(tobacco_red_maize$relative_cumsum == max(tobacco_red_maize$relative_cumsum))]

ggplot(tobacco_red_maize)+
  aes(x=Date,y=relative_cumsum*0.0091,col=Commodities)+
  geom_point(aes(col=Activity,size = `Fee (Rp)`))+
  geom_line()+
  geom_label_repel(aes(x=Tanggal,
                       y=relative_cumsum*0.0091,
                       label = Commodities),
                   data = label_max)+
  labs(title = "Cumulative Worker's Expense in Tobacco, Red Pepper and Maize farming",
       subtitle = "Each dot represent the amount of worker's fee for its respective activities",
       y="Cumulative Cost (¥)")+
  theme(legend.position = "none",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11,face = "italic"),
        axis.title.x = element_text(size = 12, face = "bold.italic"),
        axis.title.y = element_text(size = 12, face = "bold.italic")
  )
