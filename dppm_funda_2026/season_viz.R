library(googlesheets4)
library(tidyverse)


season_start_end <- read_sheet('https://docs.google.com/spreadsheets/d/1aT_iDh_Y83ZIlW2RSrJE5pUT-wBvrZPwliokOn6Gub0/edit?gid=0#gid=0')

season_start_end %>%
  ggplot()+
  geom_segment(
    aes(
      x=start, xend=end,
      y=farmers, yend=farmers,
      color=season
    ), linewidth=3, lineend="round"
  )+
  scale_color_manual(
    values = c(
      "GS3"="#f6511d",
      "GS1"="#ffb400",
      "GS2"="#00a6ed"
    )
  )+
  geom_vline(
    xintercept = as.Date(c("2025-12-01","2026-04-01","2026-08-01")),
    linetype="dashed",
    color="black",
    linewidth=1.5
  )+
  annotate(
      "text",
      x=as.Date(c("2026-02-01", "2026-06-09", "2026-10-01")),
      y=114,
      label = c("MT/GS 1", "MT/GS 2", "MT/GS 3"),
      family="mono",
  size=5)+
  xlab("Waktu")+
  ylab("Petani/Responden")+
  labs(title="Waktu Musim Tanam Responden & Anjuran Kementerian Pertanian 2026 di Area Survey Jember",
subtitle = "Hasil survey telah disesuaikan dengan ASEP 2026",
color="Keterangan :",
caption="Sumber : Hasil Survey")+
  theme_linedraw()+
  theme(text=element_text(family = "Times New Roman", size = 12, fontface="italic"),
plot.subtitle=element_text(fontface="italic"),
axis.text.y = element_blank())

### Scatter plot ###
ggplot(var_cor %>% mutate(eko_score=eko_19.cssa_efisiensi+eko_20.css_kualitas+eko_21.jaringan_pemasaran+eko_22.hubungan_pemasar+eko_23.harga_gabah+eko_24.produksi_gabah,
adapt_score=adapt_1.penyesuaian_waktu+adapt_2.lebih_satu_tanaman+adapt_3.varietas_tahan,
mitigasi_score=miti_6.sisa_tanaman+miti_7.irigasi+miti_8.pupuk_kimia+miti_9.pestisida_kimia+miti_10.pestisida_nabati))+
  aes(x=age, y=CSSA_score, colour=samp_div)+
  geom_point(size=3)+
  geom_smooth(method = 'lm')
