library(googlesheets4)
library(tidyverse)

chem_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1ApGjPu5hZuKqBw3k107kAimv0BX53nDiSycXJWmb7t0/edit?gid=258546240#gid=258546240",sheet="b4.1_chemical")

chem_raw %>%
  select(manf) %>%
  count(manf, sort = TRUE, name = "total") %>%
  na.omit() %>%
  mutate(perc=(total/sum(total))*100) %>%
  filter(perc<=5) %>%
  ggplot()+
  aes(x=reorder(manf,perc),y=perc,fill=manf)+
  geom_col()+
  geom_text(
    aes(label=paste0(round(perc, digits = 1),"%")),
    vjust = -0.3,
    size = 4
  )+
  labs(
    title = "Least Popular Chemical Manufacturer",
    subtitle = "The least used chemical in Growing Season 1 (GS1)",
    caption = "Source = Survey Data (2026)",
    x = "Chemical Manufacturer",
    y = ""
  )+
  coord_flip()+
  theme_minimal()+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10, face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold.italic")
  ) -> chem_manf

chem_raw %>%
  select(manf,type) %>%
  count(manf,type, sort = TRUE, name = "total") %>%
  na.omit() %>%
  mutate(perc=(total/sum(total))*100) %>%
  filter(perc>=5) %>%
  ggplot()+
  aes(x=reorder(manf,perc),y=perc,fill=type)+
  geom_col(position = position_dodge(width = 0.9),
    width = 0.8)+
  geom_text(
    aes(label=paste0(round(perc, digits = 1),"%")),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 4
  )+
  labs(
    title = "Top 5 Chemical Manufacturer",
    subtitle = "The most used chemical by types in Growing Season 1 (GS1)",
    caption = "Source = Survey Data (2026)",
    fill = "Type :",
    x = "Chemical Manufacturer",
    y = ""
  )+
  coord_flip()+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10, face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold.italic")
  ) -> chem_type

chem_raw %>%
  select(manf,bahan_aktif) %>%
  count(manf,bahan_aktif, sort = TRUE, name = "total") %>%
  na.omit() %>%
  mutate(perc=(total/sum(total))*100)%>%
  filter(perc>=5) %>%
  ggplot()+
  aes(x=reorder(manf,perc),y=perc,fill=bahan_aktif)+
  geom_col(position = position_dodge(width = 0.9),
    width = 0.8)+
  geom_text(
    aes(label=paste0(round(perc, digits = 1),"%")),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 4
  )+
  labs(
    title = "Top 5 Chemical Manufacturer",
    subtitle = "The most used chemical by active ingredients in Growing Season 1 (GS1)",
    caption = "Source = Survey Data (2026)",
    fill = "Active Inggredients :",
    x = "Chemical Manufacturer",
    y = ""
  )+
  coord_flip()+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10, face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold.italic")
  ) -> chem_ingg

ggsave(
    "least_pop_manf.png",
    plot = chem_manf,
    width = 10,
    height = 7,
    dpi = 300
  )
