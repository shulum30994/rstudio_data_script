# Memeriksa persamaan atau keberadaan dari dua kolom yang berbeda
data1$kolom1[!(data1$kolom1 %in% data2$kolom2)] # menampilkan output yang ADA di data2 tetapi TIDAK ADA di data1

# melakukan REPLACE satu kolom dengan nilai/values tertentu
data1$kolom1 <- replace(data1$kolom1, data1$kolom1 %in% c("A","B"), c("A1","B1")) # replace data "A" dan "B" dengan "A1" dan "B1"

# menambahkan kolom baru dengan nilai yang konstan setiap baris
data1 %>%
  mutate(kolom_baru="Nilai")

# menggambung dua data secara vertikal (tersusun) dengan syarat jumlah dan nama kolom harus SAMA
data3 <- bind_rows(data1, data2)

# mengubah dataframe dari baris menjadi kolom (data dengan dua kolom) :
data1 %>%
  select(kolom1,kolom2) %>% # opsional
  gather(key="kolom_baru",value="nilai_baru",-kolom1,-kolom[n])
# "kolom_baru" adalah nama kolom baru yang berisi nama-nama kolom yang sudah menjadi baris
# "nilai_baru" adalah nama kolom baru yang berisi nilai dari kolom yang sudah menjadi baris
# argumen "-kolom1" berarti diurutkan berdasarkan kolom1 hingga kolom ke-n

# melakukan index-match (MS Excel) di RStudio
df1$col <- df2$kol1[match(df1$kol_id, df2$kol_id)]
# col : dapat langsung diganti dengan nama kolom yang akan di-replace
# kol1 : nilai kolom dari df2 yang akan di-replace ke col di df1
# kol_id : kolom yang memiliki nilai sama antara df1 dan df2, bisa juga merupakan "primary key"

# melakukan filter (package dplyr) berdasarkan sebagian karakter/nilai yang ada di kolom
df %>% filter(grepl("KEYWORD",kolom))

