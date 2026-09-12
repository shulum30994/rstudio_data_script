library(googlesheets4)
library(dplyr)
library(seminr)

raw <- read_sheet('https://docs.google.com/spreadsheets/d/1ApGjPu5hZuKqBw3k107kAimv0BX53nDiSycXJWmb7t0/edit?gid=59088606#gid=59088606', sheet='qualitative_var')

profile <- read_sheet('https://docs.google.com/spreadsheets/d/1ApGjPu5hZuKqBw3k107kAimv0BX53nDiSycXJWmb7t0/edit?gid=59088606#gid=59088606', sheet='DRPM CSA dan SA Padi 2026')

socio_var <- read_sheet('https://docs.google.com/spreadsheets/d/1ApGjPu5hZuKqBw3k107kAimv0BX53nDiSycXJWmb7t0/edit?gid=59088606#gid=59088606', sheet='socio__var')

farming <- read_sheet('https://docs.google.com/spreadsheets/d/1ApGjPu5hZuKqBw3k107kAimv0BX53nDiSycXJWmb7t0/edit?gid=59088606#gid=59088606', sheet='farming_var')

###### Cleaning Data ######
profile %>%
  select(kabupaten_responen, sampling_div) %>%
  group_by(kabupaten_responen) %>%
  count(sampling_div)

sampling_div_code <-
  profile %>%
  select(`_index`,sampling_div) %>%
  mutate(
    socio3_1 = if_else(sampling_div=='Downstream',1,0),
    socio3_2 = if_else(sampling_div=='Middle',1,0))
  
socio_dummny <- socio_var %>%
  select(-a18_status_mitra) %>%
  rename(socio5_1=a5_usia_responden,
         socio9_1=a9_keluarga_responden,
         socio12_1=a12_pengalaman_tani) %>%
  mutate(
    socio3_1 = if_else(sampling_div=='Downstream',1,0),
    socio3_2 = if_else(sampling_div=='Middle',1,0),
    socio6_1 = if_else(a6_gender_responden==2,1,0),
    socio7_1 = if_else(a7_pendidikan_responden==2,1,0),
    socio7_2 = if_else(a7_pendidikan_responden==3,1,0),
    socio7_3 = if_else(a7_pendidikan_responden==4,1,0),
    socio7_4 = if_else(a7_pendidikan_responden==5,1,0),
    socio10_1 = if_else(a10_pekerjaan_responden=='petani',1,0),
    socio17_1 = if_else(a17_poktan_responden=='ya',1,0),
    socio22_1 = if_else(a22_akses_internet==1,1,0),
    socio23_1 = if_else(a23_akses_smartphone==1,1,0))
# socio5_1 : Usia responden (tahun)
# socio9_1 : Jumlah anggota keluarga (orang)
# socio12_1 : Pengalaman usahatani (tahun)
# socio3_1 : Dummy Hilir
# socio3_2 : Dummy Tengah
# socio6_1 : Dummy gender (Perempuan = 1)
# socio7_1 : Dummy SD (Tidak sekolah = 0)
# socio7_2 : Dummy SMP
# socio7_3 : Dummy SMA
# socio7_4 : Dummy PT
# socio10_1 : Dummy pekerjaan utama (Petani = 1)
# socio17_1 : Dummy anggota poktan (Anggota = 1)
# socio22_1 : Dummy akses internet (Ada = 1)
# socio23_1 : Dummy akses smartphone (Ada = 1)

farming_fix <-
  farming %>%
  select(id_farming,
         b1.1_luas_ha_mt1,
         b1.1_luas_ha_mt2,
         b1.1_luas_ha_mt3,
         yield_kg,
         total_revenue_hitung_Rp,
         total_biaya_benih_Rp,
         total_biaya_obat_Rp,
         biaya_tk_upahan,
         total_biaya_tk,
         total_biaya_pupuk) %>%
  mutate(farm1_1=rowSums(cbind(b1.1_luas_ha_mt1, b1.1_luas_ha_mt2, b1.1_luas_ha_mt3), na.rm = TRUE)) %>%
  rename(
    farm2_1 = yield_kg,
    farm3_1 = total_revenue_hitung_Rp,
    farm4_1 = total_biaya_benih_Rp,
    farm5_1 = total_biaya_obat_Rp,
    farm6_1 = biaya_tk_upahan,
    farm6_2 = total_biaya_tk,
    farm7_1 = total_biaya_pupuk
  )
# farm1_1 : Luas lahan (ha)
# farm2_1 : Produksi (kg/tahun)
# farm3_1 : Penerimaan (Rp/tahun)
# farm4_1 : Biaya benih (Rp/tahun)
# farm5_1 : Biaya obat (Rp/tahun)
# farm6_1 : Biaya Pekerja Upahan (Rp/tahun)
# farm6_2 : Biaya Pekerja Keseluruhan (Rp/tahun)
# farm7_1 : Biaya Pupuk (Rp/tahun)

socio_farming <-
  left_join(
    farming_fix %>% select(
      id_farming,
      farm1_1,
      farm2_1,
      farm3_1,
      farm4_1,
      farm5_1,
      farm6_1,
      farm6_2,
      farm7_1
    ),
    socio_dummny %>% select(
      id_socio,
      socio3_1,
      socio3_2,
      socio5_1,
      socio6_1,
      socio7_1,
      socio7_2,
      socio7_3,
      socio7_4,
      socio9_1,
      socio10_1,
      socio12_1,
      socio17_1,
      socio22_1,
      socio23_1
    ),
    by=c('id_farming'='id_socio')
  )
sum(is.na(socio_farming))
###########################

pre_MSI <- raw %>% select(-id)
pre_MSI <- raw %>% select(-id,-d4_1,-d4_2,-d4_3,-d4_4)

##### Fungsi MSIB satu KOLOM beserta keterangan #####
MSI <- function(x) {
  # hilangkan data NA
  x_valid <- x[!is.na(x)]
  
  # data yang dikategorikan
  kategori <- sort(unique(x_valid))
  
  # frekuensi
  frekuensi <- table(factor(x_valid, levels = kategori))
  
  # proporsi
  proporsi <- as.numeric(frekuensi)/sum(frekuensi)
  
  # proporsi kumulatif
  prop_kumulatif <- cumsum(proporsi)
  
  # batas z
  z_bawah <- c(-Inf, qnorm(prop_kumulatif[-length(prop_kumulatif)]))
  z_atas <- c(qnorm(prop_kumulatif[-length(prop_kumulatif)]), Inf)
  
  # density normal
  dens_bawah <- dnorm(z_bawah)
  dens_atas <- dnorm(z_atas)
  
  # scale value MSI
  scale_value <- (dens_bawah-dens_atas)/(pnorm(z_atas) - pnorm(z_bawah))
  
  # normalisasi agar nilai terendah adalah 1
  scale_value<-scale_value - min(scale_value) + 1
  
  # tabel hasil
  tabel <- data.frame(
    kategori = kategori,
    frekuensi = as.numeric(frekuensi),
    proporsi = proporsi,
    prop_kumulatif=prop_kumulatif,
    z_bawah = z_bawah,
    z_atas = z_atas,
    density_bawah = dens_bawah,
    density_atas = dens_atas,
    scale_value = scale_value
  )
  # transfromasi data
  hasil <- scale_value[match(x, kategori)]
  
  return(list(
    tabel_MSI = tabel,
    data_transformasi = hasil
  ))
}

##### Fungsi MSIB satu KOLOM langsung transform #####
MSIX <- function(x) {
  # Kategori dan frekuensi
  kategori <- sort(unique(x[!is.na(x)]))
  f <- table(factor(x, levels = kategori))
  
  # Proporsi
  p <- as.numeric(f) / sum(f)
  
  # Proporsi kumulatif
  pk <- cumsum(p)
  
  # Batas distribusi normal
  z_bawah <- c(-Inf, qnorm(pk[-length(pk)]))
  z_atas  <- c(qnorm(pk[-length(pk)]), Inf)
  
  # Scale value MSI
  sv <- (dnorm(z_bawah) - dnorm(z_atas)) /
    (pnorm(z_atas) - pnorm(z_bawah))
  
  # Transformasi sehingga nilai minimum = 1
  sv <- sv - min(sv) + 1
  
  # Kembalikan ke data asli
  hasil <- ifelse(is.na(x), NA, sv[match(x, kategori)])
  
  hasil
}

##### Fungsi MSIB satu DATAFRAME sekaligus #####
MSI_multi <- function(data) {
  
  MSI_one <- function(x) {
    
    if (is.factor(x)) {
      x <- as.numeric(as.character(x))
    }
    
    kategori <- sort(unique(x[!is.na(x)]))
    f <- table(factor(x, levels = kategori))
    
    p <- as.numeric(f) / sum(f)
    pk <- cumsum(p)
    
    zb <- c(-Inf, qnorm(pk[-length(pk)]))
    za <- c(qnorm(pk[-length(pk)]), Inf)
    
    sv <- (dnorm(zb) - dnorm(za)) /
      (pnorm(za) - pnorm(zb))
    
    sv <- sv - min(sv, na.rm = TRUE) + 1
    
    hasil <- rep(NA_real_, length(x))
    idx <- !is.na(x)
    hasil[idx] <- sv[match(x[idx], kategori)]
    
    hasil
  }
  
  as.data.frame(lapply(data, MSI_one))
}


hasil_MSI <- MSI(pre_MSI$d1_1)
hasil_MSI$tabel_MSI
data_MSI<-MSI_multi(pre_MSI)
SEM_dataset <- bind_cols(socio_farming,data_MSI)

# Rename column SEM_dataset
SEM_dataset_col<-SEM_dataset %>%
  rename(
    socio1_1 = socio5_1,
    socio1_2 = socio9_1,
    socio1_3 = socio12_1,
    socio1_4 = socio3_1,
    socio1_5 = socio3_2,
    socio1_6 = socio6_1,
    socio1_7 = socio7_1,
    socio1_8 = socio7_2,
    socio1_9 = socio7_3,
    socio1_10 = socio7_4,
    socio1_11 = socio10_1,
    socio1_12 = socio17_1,
    socio1_13 = socio22_1,
    socio1_14 = socio23_1,
    farming1_1 = farm1_1,
    farming1_2 = farm2_1,
    farming1_3 = farm3_1,
    farming1_4 = farm4_1,
    farming1_5 = farm5_1,
    farming1_6 = farm6_1,
    farming1_7 = farm6_2,
    farming1_8 = farm7_1
  )

##### SEM-PLS analysis #####
# Measurement model
CSA_meas_model <- constructs(
  composite("D1", multi_items("d1_",1:4)),
  composite("D2", multi_items("d2_",1:5)),
  composite("D3", multi_items("d3_",1:4)),
  composite("E1", multi_items("e1_",1:4)),
  composite("E2", multi_items("e2_",1:5)),
  composite("E3", multi_items("e3_",1:4)),
  composite("F1", multi_items("f1_",1:4)),
  composite("F2", multi_items("f2_",1:4)),
  composite("F3", multi_items("f3_",1:5)),
  composite("G2", multi_items("g2_",1:4)),
  composite("G1", multi_items("g1_",1:5)),
  composite("socio", multi_items("socio1_",1:14)),
  composite("farming", multi_items("farming1_",1:8))
)

# Structural model
CSA_stru_model <- relationships(
  paths(from=c("D1","D2", "D3", "E1","E2","E3","F1","F2","F3","socio","farming"), to=c("G1","G2")),
  paths(from=c("G2"), to=c("G1"))
)

# Model estimation
CSA_SEM_model <- estimate_pls(
  data = SEM_dataset_col,
  measurement_model = CSA_meas_model,
  structural_model = CSA_stru_model,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

# Model Summary
summ_CSA_SEM_model <- summary(CSA_SEM_model)

# Inspect the constructs reliability metrics
summ_CSA_SEM_model$reliability

# Bootstrapping model
boot_CSA_SEM_model <- bootstrap_model(
  seminr_model = CSA_SEM_model,
  nboot = 1000,
  cores = NULL,
  sed = 123
)

# Summary of Bootsrapping
sum_boot_CSA_SEM_model <- summary(boot_CSA_SEM_model)

# Indicator boot loadings
sum_boot_CSA_SEM_model$bootstrapped_loadings
write.csv(sum_boot_CSA_SEM_model$bootstrapped_loadings, file="boot_SEM_CSA.csv")

# Model Colinearty (VIF < 3)
summ_CSA_SEM_model$vif_antecedents

# Bootstrapped structural paths
sum_boot_CSA_SEM_model$bootstrapped_paths

# Total effects
sum_boot_CSA_SEM_model$bootstrapped_total_paths

# Path coefficients and R-square values
summ_CSA_SEM_model$path

# Model fSquare
summ_CSA_SEM_model$fSquare

##### Output Visualization ####
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Simple graph
graph_csa_sem <- plot(CSA_SEM_model, title = "Bootsrapped CSA Model")

# Export to SVG format
svg_graph_csa_sem <- export_svg(graph_csa_sem)

# render to png format
rsvg_png(
  svg = charToRaw(svg_graph_csa_sem),
  file = "bootsrap__csa_model_hd.png",
  width = 1080,
  height = 1920
)
