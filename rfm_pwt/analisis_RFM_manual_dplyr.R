# Data harus di-cleaning terlebih dahulu e.g : transaksi harus bertipe date, nomor invoice sudah harus, dibuat dst
# Tanggal acuan analisis
snapshot_date <- max(df$InvoiceDate, na.rm = TRUE) + 1

# Menghitung RFM per pelanggan
rfm <- df %>%
  filter(
    !is.na(CustomerID),
    !is.na(InvoiceNo),
    !is.na(InvoiceDate)
  ) %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.integer(
      snapshot_date - max(InvoiceDate)
    ),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  )

# Membuat skor RFM (1-5
rfm <- rfm %>%
  mutate(
    R_Score = ntile(-Recency, 5),
    F_Score = ntile(Frequency, 5),
    M_Score = ntile(Monetary, 5)
  )

# Menggabungkan ketiga skor menjadi satu
rfm <- rfm %>%
  mutate(
    RFM_Score = paste0(
      R_Score, F_Score, M_Score
    ),
    RFM_Total = R_Score + F_Score + M_Score
  )
