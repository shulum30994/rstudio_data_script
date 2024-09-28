library(tidyverse)
library(directlabels)

prayer <- prayer %>%
  mutate(sunset=format(as.POSIXct(maghrib,format="%Y-%m-%d %H:%M:%S"), "%H:%M"))

# All Cities
ggplot(prayer)+
  aes(x=as.Date(day),y=as.POSIXct(sunrise,format="%H:%M"),colour=cities)+
  geom_line()+
  labs(titles="Sunrise Time 2024")+
  xlab("Date")+
  ylab("Hour")+
  geom_vline(xintercept = c(as.Date("2024-03-20"),
                            as.Date("2024-06-20"),
                            as.Date("2024-09-22"),
                            as.Date("2024-12-21")),
             linetype="dashed",
             color="navy")+
  annotate("text",
           x=as.Date("2024-03-20"),
           y=as.POSIXct("05:38",format="%H:%M"),
           label="\nMarch 20",
           color="red",
           angle=90)+
  annotate("text",
           x=as.Date("2024-06-20"),
           y=as.POSIXct("05:38",format="%H:%M"),
           label="\nJune 20",
           color="red",
           angle=90)+
  annotate("text",
           x=as.Date("2024-09-22"),
           y=as.POSIXct("05:38",format="%H:%M"),
           label="\nSept 22",
           color="red",
           angle=90)+
  annotate("text",
           x=as.Date("2024-12-21"),
           y=as.POSIXct("05:38",format="%H:%M"),
           label="\nDec 20",
           color="red",
           angle=90)+
  scale_color_manual(values = c("Banda Aceh"="#1984c5",
                                "Palembang"="#22a7f0",
                                "Banyuwangi"="#63bff0",
                                "Johor Baharu"="#c23728",
                                "Kuala Lumpur"="#e14b31",
                                "Ipoh"="#de6e56"))+
  scale_x_date(limits = c(as.Date("2024-01-01"),as.Date("2025-02-15")))+
  geom_dl(aes(label=cities),method = list("last.points"),cex=0.8)+
  theme(legend.position = "none")
