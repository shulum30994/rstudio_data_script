library(tidyverse)

#### Ini adalah bookmark ####

#### Preview data ####
View(dataset1)
print(dataset1,n=10)

#### mengetahui properties data dari data ####
glimpse(dataset1) #atau
dataset1 %>% glimpse
summary(dataset1)

#### Mengubah nama kolom yang ada di dalama dataset ####
dataset1 %>% rename(Area_Code_(M49)=`Area Code (M49)`)

dataset1<-dataset1 %>% rename(Element.Code=`Element Code`,
                              Item.Code=`Item Code`,
                              Year.Code=`Year Code`,
                              Flag.Description=`Flag Description`)

#### Memilih kolom sebagai dataset baru ####
dataset2 <- dataset1 %>% select(Area,Element,Item,Year,Unit,Value)

#### Menampilkan data unique dari sebuah kolom ####
dataset1 %>% distinct(Year)

#### Memilih (filter) data berdasarkan kolom ####
dataset1 %>% filter(Area=="Indonesia"|Year=='2008'|Value>=6133.63)

#### Menambahkan kolom baru ####
dataset2 <- dataset2 %>% mutate(Credit_in_mil=Value*1000000,percentage_total=(Value/max(Value))*100)

#### Count and grouping ####
dataset1 %>% group_by(Year) %>% count(Year)

#### Nilai rata-rata tiap negara ####
dataset1 %>% group_by(Area) %>% summarise(across(c("Value"),.fns=sum)) %>% print(n=131)
                                          
