library(googlesheets4)
library(dplyr)
library(seminr)

raw <- read_sheet('https://docs.google.com/spreadsheets/d/1ApGjPu5hZuKqBw3k107kAimv0BX53nDiSycXJWmb7t0/edit?gid=59088606#gid=59088606', sheet='qualitative_var')

pre_MSI <- raw %>% select(-id)

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
  composite("G1", multi_items("g1_",1:5))
)

# Structural model
CSA_stru_model <- relationships(
  paths(from=c("D1","D2", "D3", "E1","E2","E3","F1","F2","F3"), to=c("G1","G2")),
  paths(from=c("G2"), to=c("G1"))
)

# Model estimation
CSA_SEM_model <- estimate_pls(
  data = data_MSI,
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
