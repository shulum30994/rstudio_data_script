library(tidyverse)

#### Ini adalah bookmark ####

###### MENYUNTING TABEL #####
#### Preview data ####
View(dataset1)
print(dataset1,n=10)

#### mengetahui properties data dari data ####
glimpse(dataset1) #atau
dataset1 %>% glimpse
summary(dataset1)

#### Mengubah nama kolom yang ada di dalama dataset ####
dataset1 %>% rename(Nama_Kolom_Lama=`Nama Kolom Baru`)

dataset1<-dataset1 %>% rename(Element.Code=`Element Code`,
                              Item.Code=`Item Code`,
                              Year.Code=`Year Code`,
                              Flag.Description=`Flag Description`)

#### Memilih kolom sebagai dataset baru ####
dataset2 <- dataset1 %>% select(Area,Element,Item,Year,Unit,Value)

#### Menambahkan kolom baru ####
dataset2 <- dataset2 %>% mutate(Credit_in_mil=Value*1000000,percentage_total=(Value/max(Value))*100)

#### menambahkan kolom baru dengan nilai yang konstan setiap baris ###
data1 %>%
  mutate(kolom_baru="Nilai")

#### menggambung dua data secara vertikal (tersusun) dengan syarat jumlah dan nama kolom harus SAMA
data3 <- bind_rows(data1, data2)

#### mengubah dataframe dari baris menjadi kolom (data dengan dua kolom) :
data1 %>%
  select(kolom1,kolom2) %>% # opsional
  gather(key="kolom_baru",value="nilai_baru",-kolom1,-kolom[n])
# "kolom_baru" adalah nama kolom baru yang berisi nama-nama kolom yang sudah menjadi baris
# "nilai_baru" adalah nama kolom baru yang berisi nilai dari kolom yang sudah menjadi baris
# argumen "-kolom1" berarti diurutkan berdasarkan kolom1 hingga kolom ke-n

############################################

##### MENYUNTING DATA #####
#### melakukan REPLACE satu kolom dengan nilai/values tertentu
data1$kolom1 <- replace(data1$kolom1, data1$kolom1 %in% c("A","B"), c("A1","B1")) # replace data "A" dan "B" dengan "A1" dan "B1"

#### melakukan index-match (MS Excel) di RStudio
df1$col <- df2$kol1[match(df1$kol_id, df2$kol_id)]
# col : dapat langsung diganti dengan nama kolom yang akan di-replace
# kol1 : nilai kolom dari df2 yang akan di-replace ke col di df1
# kol_id : kolom yang memiliki nilai sama antara df1 dan df2, bisa juga merupakan "primary key"
############################################

##### SORTASI DATA #####
#### Count and grouping
dataset1 %>% group_by(Year) %>% count(Year)

#### Menampilkan data unique dari sebuah kolom
dataset1 %>% distinct(Year)

#### Memilih (filter) data berdasarkan kolom
dataset1 %>% filter(Area=="Indonesia"|Year=='2008'|Value>=6133.63)

### melakukan filter (package dplyr) berdasarkan sebagian karakter/nilai yang ada di kolom
df %>% filter(grepl("KEYWORD",kolom))

### Memeriksa persamaan atau keberadaan dari dua kolom yang berbeda
data1$kolom1[!(data1$kolom1 %in% data2$kolom2)] # menampilkan output yang ADA di data2 tetapi TIDAK ADA di data1

#### Nilai rata-rata tiap negara ####
dataset1 %>% group_by(Area) %>% summarise(across(c("Value"),.fns=sum)) %>% print(n=131)

####################################################
