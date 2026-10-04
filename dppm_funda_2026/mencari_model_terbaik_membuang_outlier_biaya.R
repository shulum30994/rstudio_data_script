# ubah tipe data menjadi faktor
csa_out_free <- csa_out_free %>%
  mutate(
    across(c("dgender","mid","down","dSD","dhigh_school","duniv","dinternet"),as.factor)
  )

###### CSA-OUT0 ######
CSA_meas_out0 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CSA_Adopt", c("g1_1","g1_2","g1_3","g1_4","g1_5","g2_1","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("a5_usia_responden","a9_keluarga_responden","a12_pengalaman_tani","dgender","mid","down","dSD","dhigh_school","duniv","dinternet","total_revenue_hitung_Rp","total_biaya_benih_Rp","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk","total_biaya_pupuk"))
)

CSA_stru_out0 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROF"))
)

CSA_SEM_out0<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out0,
  structural_model = CSA_stru_out0,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out0<- summary(CSA_SEM_out0)
summ_CSA_SEM_out0$reliability
summ_CSA_SEM_out0$loadings
write.csv(summ_CSA_SEM_out0$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out0_rel.csv")
write.csv(summ_CSA_SEM_out0$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out0_loading.csv")

###### CSA-OUT1 ######
CSA_meas_out1 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CSA_Adopt", c("g1_2","g1_3","g1_4","g1_5","g2_1","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("a5_usia_responden","a12_pengalaman_tani","mid","dSD","dinternet","total_revenue_hitung_Rp","total_biaya_benih_Rp","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk","total_biaya_pupuk"))
)

CSA_stru_out1 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROF"))
)

CSA_SEM_out1<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out1,
  structural_model = CSA_stru_out1,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out1<- summary(CSA_SEM_out1)
summ_CSA_SEM_out1$reliability
summ_CSA_SEM_out1$loadings
write.csv(summ_CSA_SEM_out1$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out1_rel.csv")
write.csv(summ_CSA_SEM_out1$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out1_loading.csv")

###### CSA-OUT2 ######
CSA_meas_out2 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CSA_Adopt", c("g1_2","g1_3","g1_4","g1_5","g2_1","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("mid","dSD","dinternet","total_revenue_hitung_Rp","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk","total_biaya_pupuk"))
)

CSA_stru_out2 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROF"))
)

CSA_SEM_out2<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out2,
  structural_model = CSA_stru_out2,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out2<- summary(CSA_SEM_out2)
summ_CSA_SEM_out2$reliability
summ_CSA_SEM_out2$loadings
write.csv(summ_CSA_SEM_out2$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out2_rel.csv")
write.csv(summ_CSA_SEM_out2$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out2_loading.csv")

###### CSA-OUT3 ######
CSA_meas_out3 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4")),
  composite("CSA_Adopt", c("g1_2","g1_3","g1_4","g1_5","g2_1","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("mid","dSD","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk","total_biaya_pupuk"))
)

CSA_stru_out3 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROF"))
)

CSA_SEM_out3<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out3,
  structural_model = CSA_stru_out3,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out3<- summary(CSA_SEM_out3)
summ_CSA_SEM_out3$reliability
summ_CSA_SEM_out3$loadings
write.csv(summ_CSA_SEM_out3$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out3_rel.csv")
write.csv(summ_CSA_SEM_out3$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out3_loading.csv")

###### CSA-OUT4 ######
CSA_meas_out4 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_2","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_3","f3_1","f3_3","f3_4")),
  composite("CSA_Adopt", c("g1_2","g1_3","g1_4","g1_5","g2_1","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("mid","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk"))
)

CSA_stru_out4 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROF"))
)

CSA_SEM_out4<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out4,
  structural_model = CSA_stru_out4,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out4<- summary(CSA_SEM_out4)
summ_CSA_SEM_out4$reliability
summ_CSA_SEM_out4$loadings
write.csv(summ_CSA_SEM_out4$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out4_rel.csv")
write.csv(summ_CSA_SEM_out4$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out4_loading.csv")

###### CSA-OUT5 ######
CSA_meas_out5 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_2","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f3_1","f3_3","f3_4")),
  composite("CSA_Adopt", c("g1_2","g1_3","g1_4","g1_5","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("mid","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk"))
)

CSA_stru_out5 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROF"))
)

CSA_SEM_out5<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out5,
  structural_model = CSA_stru_out5,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out5<- summary(CSA_SEM_out5)
summ_CSA_SEM_out5$reliability
summ_CSA_SEM_out5$loadings
write.csv(summ_CSA_SEM_out5$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out5_rel.csv")
write.csv(summ_CSA_SEM_out5$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out5_loading.csv")

###### CSA-OUT6 ######
CSA_meas_out6 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f3_1","f3_3","f3_4")),
  composite("CSA_Adopt", c("g1_2","g1_3","g1_4","g1_5","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("mid","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk"))
)

CSA_stru_out6 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROF"))
)

CSA_SEM_out6<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out6,
  structural_model = CSA_stru_out6,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out6<- summary(CSA_SEM_out6)
summ_CSA_SEM_out6$reliability
summ_CSA_SEM_out6$loadings
write.csv(summ_CSA_SEM_out6$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out6_rel.csv")
write.csv(summ_CSA_SEM_out6$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out6_loading.csv")

###### CSA-OUT7 ######
CSA_meas_out7 <- constructs(
  composite("ROG", c("d1_2","d2_1","d2_2","d2_3","d2_4","d2_5","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_2","e2_3","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f3_1","f3_3","f3_4")),
  composite("CSA_Adopt", c("g1_2","g1_3","g1_4","g1_5","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("mid","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk"))
)

CSA_stru_out7 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROF"))
)

CSA_SEM_out7<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out7,
  structural_model = CSA_stru_out7,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out7<- summary(CSA_SEM_out7)
summ_CSA_SEM_out7$reliability
summ_CSA_SEM_out7$loadings
write.csv(summ_CSA_SEM_out7$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out7_rel.csv")
write.csv(summ_CSA_SEM_out7$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out7_loading.csv")


###### CSA-OUT8 ######
CSA_meas_out8 <- constructs(
  composite("ROG", c("d2_1","d2_2","d2_3","d2_4","d2_5","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_2","e2_3","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f3_1")),
  composite("CSA_Adopt", c("g1_2","g1_3","g1_4","g1_5","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("mid","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk"))
)

CSA_stru_out8 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROF"))
)

CSA_SEM_out8<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out8,
  structural_model = CSA_stru_out8,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out8<- summary(CSA_SEM_out8)
summ_CSA_SEM_out8$reliability
summ_CSA_SEM_out8$loadings
write.csv(summ_CSA_SEM_out8$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out8_rel.csv")
write.csv(summ_CSA_SEM_out8$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out8_loading.csv")

###### CSA-OUT9 ######
CSA_meas_out9 <- constructs(
  composite("ROG", c("d2_1","d2_2","d2_3","d2_4","d2_5","d4_1","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_2","e2_3","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f3_1")),
  composite("CSA_Adopt", c("g1_2","g1_3","g1_4","g1_5","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("mid","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk"))
)

CSA_stru_out9 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROF"))
)

CSA_SEM_out9<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out9,
  structural_model = CSA_stru_out9,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out9<- summary(CSA_SEM_out9)
summ_CSA_SEM_out9$reliability
summ_CSA_SEM_out9$loadings
write.csv(summ_CSA_SEM_out9$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out9_rel.csv")
write.csv(summ_CSA_SEM_out9$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out9_loading.csv")

###### CSA-OUT9A [SOFAR -> ROF] ######
CSA_meas_out9A <- constructs(
  composite("ROG", c("d2_1","d2_2","d2_3","d2_4","d2_5","d4_1","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_2","e2_3","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f3_1")),
  composite("CSA_Adopt", c("g1_2","g1_3","g1_4","g1_5","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("mid","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk"))
)

CSA_stru_out9A <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROF"))
)

CSA_SEM_out9A<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out9A,
  structural_model = CSA_stru_out9A,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out9A<- summary(CSA_SEM_out9A)
summ_CSA_SEM_out9A$reliability
summ_CSA_SEM_out9A$loadings
write.csv(summ_CSA_SEM_out9A$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out9A_rel.csv")
write.csv(summ_CSA_SEM_out9A$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out9A_loading.csv")
boot_CSA_SEM9A <- bootstrap_model(seminr_model = CSA_SEM_out9A, nboot = 1000)
sum_CSA_SEM_out9A <- summary(CSA_SEM_out9A, alpha = 0.10)
plot(boot_CSA_SEM9A, title = "Hasil Bootstrap Model 9A")
graph <- plot(boot_CSA_SEM9A, title = "Hasil Bootstrap Model 9A")
svg_graph <- export_svg(graph)
bg_css <- "svg { background-color: #ffcc00; }"
rsvg_png(
  svg = charToRaw(svg_graph),
  file = "bootstrap_SEM9_model.png",
  width = 1920,
  height = 1080
)

###### CSA-OUT9B [SOFAR -> ROG] ######
CSA_meas_out9B <- constructs(
  composite("ROG", c("d2_1","d2_2","d2_3","d2_4","d2_5","d4_1","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_2","e2_3","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f3_1")),
  composite("CSA_Adopt", c("g1_2","g1_3","g1_4","g1_5","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("mid","total_biaya_obat_Rp","biaya_tk_upahan","total_biaya_tk"))
)

CSA_stru_out9B <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","SOFAR"),to=c("CSA_Adopt")),
  paths(from=c("SOFAR"), to=c("ROG"))
)

CSA_SEM_out9B<- estimate_pls(
  data = csa_out_free,
  measurement_model = CSA_meas_out9B,
  structural_model = CSA_stru_out9B,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM_out9B<- summary(CSA_SEM_out9B)
summ_CSA_SEM_out9B$reliability
summ_CSA_SEM_out9B$loadings
write.csv(summ_CSA_SEM_out9B$reliability,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out9B_rel.csv")
write.csv(summ_CSA_SEM_out9B$loadings,"G:\\R_Workspace\\SEM_R_csa\\csa_sem_free_out_result\\CSA_SEM_out9B_loading.csv")
