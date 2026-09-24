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
SEM_dataset <- bind_cols(socio_farming,data_MSI)
csa_practice_MSI <- MSI_multi(csa_practice)
