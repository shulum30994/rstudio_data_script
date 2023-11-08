library(tidyverse)

data <- read.csv("/home/shohibul/KERJA/DATA/Efisiensi Teknis Jessica-Billy/faktor_produksi_efisiensi.csv")

# PROP        <fct> 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, # YIELD       <dbl> 1256, 81, 1249, 806, 1003, 980, 321, 78, 80, 
# CHEM_MANURE <dbl> 9, 2, 9, 7, 7, 8, 3, 2, 2, 9, 37, 26, 86, 66, 
# SEED        <dbl> 33, 4, 33, 26, 28, 29, 9, 3, 4, 41, 21, 6, 26, 
# LABOUR      <dbl> 33.7, 5.8, 33.7, 19.4, 25.8, 26.2, 8.5, 3.8, 5
# DISTRICT    <chr> "PERDESAAN", "PERDESAAN", "PERDESAAN",
# IRRIGATION  <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
# CATEGORY    <chr> "NON-IRRIGATION", "NON-IRRIGATION",
# LABEL       <chr> "VILLAGE", "VILLAGE", "VILLAGE", "VILLAGE"

data %>%
  group_by(LABEL) %>%
  count(PROP)

area_prop<-data %>%
  group_by(PROP,CATEGORY) %>%
  summarise(across(c("AREA"),.fns=mean))

faprod<-data %>% 
  group_by(CATEGORY) %>%
  summarise(across(c("YIELD","CHEM_MANURE","SEED","LABOUR"),.fns=sum))

stack_faprod <- stack(faprod,select = -CATEGORY)

faprod2<-stack_faprod %>% 
  add_column(KATEGORI=c("IRRIGATION","NON-IRRIGATION",
                        "IRRIGATION","NON-IRRIGATION",
                        "IRRIGATION","NON-IRRIGATION",
                        "IRRIGATION","NON-IRRIGATION"),
                          TOTAL=c(137264532,137264532,
                                  11294309,11294309,
                                  1763537,1763537,
                                  2744244,2744244)) %>%
  mutate(PERCENTAGE=round((values/TOTAL)*100,1))

ggplot(faprod2,aes(x=KATEGORI,y=PERCENTAGE,fill=KATEGORI))+
  scale_fill_manual(values = c("IRRIGATION"="gray71","NON-IRRIGATION"="gray31"))+
  geom_col()+
  geom_text(faprod2 %>%
              group_by(ind) %>%
              slice(1) %>%
              mutate(label=paste0(LETTERS[as.numeric(ind)],".")),
            mapping=aes(label=label,x=0.5,y=55),color="black",fontface="bold")+
  facet_wrap(.~ind)+
  labs(fill="Category :",x="",y="Percentage (%)")+
  theme(legend.position = "none",
        axis.text.x = element_text(family = "monospace",
                                   size = 10,
                                   face = "bold"),
        axis.text.y = element_text(family = "monospace",
                                   size = 10,
                                   face = "bold"),
        axis.title.y = element_text(family = "monospace",
                                    size = 11,
                                    face = "bold"),
        legend.title = element_text(face = "bold",
                                    size = 12,
                                    family = "serif"),
        legend.text = element_text(family = "serif",
                                   size = 11,
                                   face="italic"),
        strip.text.x = element_blank())
#element_text(family = "monospace",
#             size = 11,
#             face = "bold")
                  