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
 
csa_practice <- profile %>%
  select(
    adapt_1.penyesuaian_waktu,
    adapt_2.lebih_satu_tanaman,
    adapt_3.varietas_tahan,
    lingk_4.rotasi_tanaman,
    lingk_5.penambahan_organik,
    miti_6.sisa_tanaman,
    miti_7.irigasi,
    miti_8.pupuk_kimia,
    miti_9.pestisida_kimia,
    miti_10.pestisida_nabati,
    prod_11.monev_hpt,
    prod_12.peningkatan_income,
    prod_13.persen_rotasi,
    prod_14.intensitas_rotasi,
    prod_15.rotasi_organik,
    sos_16.k3_pertanian,
    sos_17.k3_terlibat,
    sos_18.dukungan_stake,
    eko_19.cssa_efisiensi,
    eko_20.css_kualitas,
    eko_21.jaringan_pemasaran,
    eko_22.hubungan_pemasar,
    eko_23.harga_gabah,
    efff_25.effisiensi_cssa_saprodi
  ) %>%
  rename(
    adaptasi1_1 = adapt_1.penyesuaian_waktu,
    adaptasi1_2 = adapt_2.lebih_satu_tanaman,
    adaptasi1_3 = adapt_3.varietas_tahan,
    lingkungan2_1 = lingk_4.rotasi_tanaman,
    lingkungan2_2 = lingk_5.penambahan_organik,
    mitigasi3_1 = miti_6.sisa_tanaman,
    mitigasi3_2 = miti_7.irigasi,
    mitigasi3_3 = miti_8.pupuk_kimia,
    mitigasi3_4 = miti_9.pestisida_kimia,
    mitigasi3_5 = miti_10.pestisida_nabati,
    produksi4_1 = prod_11.monev_hpt,
    produksi4_2 = prod_12.peningkatan_income,
    produksi4_3 = prod_13.persen_rotasi,
    produksi4_4 = prod_14.intensitas_rotasi,
    produksi4_5 = prod_15.rotasi_organik,
    sosial5_1 = sos_16.k3_pertanian,
    sosial5_2 = sos_17.k3_terlibat,
    sosial5_3 = sos_18.dukungan_stake,
    ekonomi6_1 = eko_19.cssa_efisiensi,
    ekonomi6_2 = eko_20.css_kualitas,
    ekonomi6_3 = eko_21.jaringan_pemasaran,
    ekonomi6_4 = eko_22.hubungan_pemasar,
    ekonomi6_5 = eko_23.harga_gabah,
    ekonomi6_6 = efff_25.effisiensi_cssa_saprodi
  )

socio_dummny <- socio_var %>%
  select(-a18_status_mitra) %>%
  rename(socio1_1=a5_usia_responden,
         socio1_2=a9_keluarga_responden,
         socio1_3=a12_pengalaman_tani) %>%
  mutate(
    socio1_4 = if_else(sampling_div=='Downstream',1,0),
    socio1_5 = if_else(sampling_div=='Middle',1,0),
    socio1_6 = if_else(a6_gender_responden==2,1,0),
    socio1_7 = if_else(a7_pendidikan_responden==2,1,0),
    socio1_8 = if_else(a7_pendidikan_responden==3,1,0),
    socio1_9 = if_else(a7_pendidikan_responden==4,1,0),
    socio1_10 = if_else(a7_pendidikan_responden==5,1,0),
    socio1_11 = if_else(a10_pekerjaan_responden=='petani',1,0),
    socio1_12 = if_else(a17_poktan_responden=='ya',1,0),
    socio1_13 = if_else(a22_akses_internet==1,1,0),
    socio1_14 = if_else(a23_akses_smartphone==1,1,0))

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
         total_tk,
         biaya_tk_upahan,
         total_biaya_tk,
         total_biaya_pupuk,
        total_biaya_petani_ver) %>%
  mutate(farming1_1=rowSums(cbind(b1.1_luas_ha_mt1, b1.1_luas_ha_mt2, b1.1_luas_ha_mt3), na.rm = TRUE)) %>%
  rename(
    farming1_2 = yield_kg,
    farming1_3 = total_revenue_hitung_Rp,
    farming1_4 = total_biaya_benih_Rp,
    farming1_5 = total_biaya_obat_Rp,
    farming1_6 = total_tk,
    farming1_7 = biaya_tk_upahan,
    farming1_8 = total_biaya_tk,
    farming1_9 = total_biaya_pupuk,
    farming1_10 = total_biaya_petani_ver
  )

socio_farming <-
  left_join(
    farming_fix %>% select(
      id_farming,
      farming1_1,
      farming1_2,
      farming1_3,
      farming1_4,
      farming1_5,
      farming1_6,
      farming1_7,
      farming1_8,
      farming1_9,
      farming1_10
    ),
    socio_dummny %>% select(
      id_socio,
      socio1_1,
      socio1_2,
      socio1_3,
      socio1_4,
      socio1_5,
      socio1_6,
      socio1_7,
      socio1_8,
      socio1_9,
      socio1_10,
      socio1_11,
      socio1_12,
      socio1_13,
      socio1_14
    ),
    by=c('id_farming'='id_socio')
  )

sum(is.na(socio_farming))

##### Korelasi antar varibel #####
korelasi_qual <- rcorr(
  as.matrix(raw %>% select(-id),
  type="spearman")
)
corrplot(
  cor(raw %>% select(-id), method = "spearman"),
  method = "circle",
  type = "upper",
  diag = FALSE,
  title = "Korelasi Antar Items"
)
