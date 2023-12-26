# Memeriksa persamaan atau keberadaan dari dua kolom yang berbeda
data1$kolom1[!(data1$kolom1 %in% data2$kolom2)] # menampilkan output yang ADA di data2 tetapi TIDAK ADA di data1

# melakukan REPLACE satu kolom dengan nilai/values tertentu
data1$kolom1 <- replace(data1$kolom1, data1$kolom1 %in% c("A","B"), c("A1","B1")) # replace data "A" dan "B" dengan "A1" dan "B1"

# mengubah dataframe dari baris menjadi kolom (data dengan dua kolom) :
data1 %>%
  select(kolom1,kolom2) %>% # opsional
  gather(key="kolom_baru",value="nilai_baru,-kolom1)
# "kolom_baru" adalah nama kolom baru yang berisi nama-nama kolom yang sudah menjadi baris
# "nilai_baru" adalah nama kolom baru yang berisi nilai dari kolom yang sudah menjadi baris
# argumen "-kolom1" berarti diurutkan berdasarkan kolom1
