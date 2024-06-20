# Mengambil dataset secara online
ret1 <- read.csv(“https://raw.githubusercontent.com/shulum30994/rstudio_data_script/main/rfm_pwt/customer_shopping_data.csv”

# Pasang packages yang dibutuhkan
Install.packages(dplyr)
Install.packages(ggplot)
Install.packages(rfm)

# Aktifkan packages yang sudah terpasang
library(dplyr)
library(ggplot2)
library(ragg)
library(rfm)

# Periksa data yang sudah diambil secara online
ret1 %>% glimpse()

# Menambahkan kolom baru ‘revenue’
ret1 <- ret1 %>%
              mutate(revenue=quantity * price)

# Menambahkan kolom baru untuk data transaksi
ret1 <- ret1 %>%
              mutate(date=as.Date(strptime(invoice_date,"%d/%m/%Y"))) %>%
              arrange(desc(date))
ret1 %>% head(5)

# Melakukan analisis RFM
rfm_ret1 <- rfm_table_order(
            data=ret1,
            customer_id = customer_id,
            revenue = revenue,
            order_date = date,
            analysis_date = as.Date(“2023-03-08”))

# Menampilkan hasil analisis RFM
rfm_ret1

# Visualisasi heatmap hasil penghitungan RFM
rfm_plot_heatmap(rfm_ret1)

# Visualisasi heatmap hasil penghitungan RFM
rfm_plot_bar_chart(rfm_ret1)

# Menyimpan hasil penghitungan (analisis) RFM ke objek data frame
rfm_df <- as.data.frame(rfm_ret1$rfm)

# Kriteria masing-masing segmen :
segment_names = c("Champions",
                  “Potential Loyalist",
                  "Loyal Customers",
                  "Promising",
                  "New Customers",
                  "Can't Lose Them",
                  "At Risk",
                  "Need Attention",
                  "About To Sleep",
                  "Lost")

# Tentukan batas atas-bawah masing-masing kriteria:
recency_lower <-   c(5, 3, 2, 3, 4, 1, 1, 1, 2, 1)
recency_upper <-   c(5, 5, 4, 4, 5, 2, 2, 3, 3, 1)
frequency_lower <- c(5, 3, 2, 1, 1, 3, 2, 3, 1, 1)
frequency_upper <- c(5, 5, 4, 3, 3, 4, 5, 5, 3, 5)
monetary_lower <-  c(5, 2, 2, 3, 1, 4, 4, 3, 1, 1)
monetary_upper <-  c(5, 5, 4, 5, 5, 5, 5, 5, 4, 5)

# Membuat segmentasi customer berdasarkan nama segment dan batas segment
segment <- rfm_segment(rfm_ret1,
                      segment_names,
                      recency_lower,
                      recency_upper,
                      frequency_lower,
                      frequency_upper,
                      monetary_lower,  
                      monetary_upper)

segment %>% head(10)

# Membbuat segmentasi customer berdasarkan bobot skor
segment <- segment %>%
                    mutate(bobot_skor = (recency_score*5)+(frequency_score*3)+(monetary_score*1),
                    segmentasi = cut(score_weight,breaks = 5,labels = c("Low Customer","Low Value Customer","Medium Value Customer","High Customer","Top Customer"))) %>%
                    select(score_weight,score_segment) %>%
                    head(15)
