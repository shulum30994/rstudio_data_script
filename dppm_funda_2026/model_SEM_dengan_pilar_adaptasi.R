###### CSA3 ######
CSA_meas3 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_4")),
  composite("ROF", c("e1_1","e2_3","e2_4","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CSA", c("g1_1","g1_2","g1_3","g1_4","g2_1","g2_2","g2_4")),
  composite("farming", c("mid" ,"downstream","labour_org","seed_kg","manure_kg")),
  composite("adaptasi", c("adaptasi1_1","adaptasi1_2","adaptasi1_3")),
  composite("lingkungan", c("lingkungan2_1", "lingkungan2_2")),
  composite("mitigasi", c("mitigasi3_1","mitigasi3_2","mitigasi3_3","mitigasi3_4","mitigasi3_5")),
  composite("produksi", c("produksi4_1", "produksi4_2", "produksi4_3", "produksi4_4")),
  composite("sosial", c("sosial5_1", "sosial5_2", "sosial5_3")),
  composite("ekonomi", c("ekonomi6_1", "ekonomi6_2", "ekonomi6_3", "ekonomi6_4", "ekonomi6_5", "ekonomi6_6"))
)

CSA_stru3 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("adaptasi","lingkungan","mitigasi","produksi","sosial","ekonomi")),
  paths(from=c("adaptasi","lingkungan","mitigasi","produksi","sosial","ekonomi"),to=c("farming")),
  paths(from = c("farming"), to=c("ROF")),
  paths(from=c("GFP","farming"), to=c("CSA"))
)

CSA_SEM3<- estimate_pls(
  data = fisik_sem,
  measurement_model = CSA_meas3,
  structural_model = CSA_stru3,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM3<- summary(CSA_SEM3)

###### CSA4 (rm : adaptasi, mitigasi)) #####
CSA_meas4 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_4")),
  composite("ROF", c("e1_1","e2_3","e2_4","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CSA", c("g1_1","g1_2","g1_3","g1_4","g2_1","g2_2","g2_4")),
  composite("farming", c("mid" ,"downstream","labour_org","seed_kg","manure_kg")),
  composite("lingkungan", c("lingkungan2_1", "lingkungan2_2")),
  composite("produksi", c("produksi4_1", "produksi4_2", "produksi4_3", "produksi4_4")),
  composite("sosial", c("sosial5_1", "sosial5_2", "sosial5_3")),
  composite("ekonomi", c("ekonomi6_1", "ekonomi6_2", "ekonomi6_3", "ekonomi6_4", "ekonomi6_5", "ekonomi6_6"))
)

CSA_stru4 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("lingkungan","produksi","sosial","ekonomi")),
  paths(from=c("lingkungan","produksi","sosial","ekonomi"),to=c("farming")),
  paths(from = c("farming"), to=c("ROF")),
  paths(from=c("GFP","farming"), to=c("CSA"))
)

CSA_SEM4<- estimate_pls(
  data = fisik_sem,
  measurement_model = CSA_meas4,
  structural_model = CSA_stru4,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM3<- summary(CSA_SEM4)
summ_CSA_SEM3$reliability

###### CSA5 (rm : farming, lingkungan)) #####
CSA_meas5 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_4")),
  composite("ROF", c("e1_1","e2_3","e2_4","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CSA", c("g1_1","g1_2","g1_3","g1_4","g2_1","g2_2","g2_4")),
  composite("sosek", c("sosial5_1", "sosial5_2", "sosial5_3","ekonomi6_1", "ekonomi6_2", "ekonomi6_3", "ekonomi6_4", "ekonomi6_5", "ekonomi6_6","produksi4_1", "produksi4_2", "produksi4_3", "produksi4_4"))
)

CSA_stru5 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","ROF","ROG"),to=c("CSA","sosek")),
  paths(from=c("CSA"), to = ("sosek"))
)

CSA_SEM5<- estimate_pls(
  data = fisik_sem,
  measurement_model = CSA_meas5,
  structural_model = CSA_stru5,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM5<- summary(CSA_SEM5)
summ_CSA_SEM5$reliability

###### CSA6 ######
CSA_meas6 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_4")),
  composite("ROF", c("e1_1","e2_3","e2_4","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CSA", c("g1_1","g1_2","g1_3","g1_4","g2_1","g2_2","g2_4")),
  composite("farming", c("family_org","mid" ,"downstream","log_acreage","log_yield","log_labour")),
  composite("CSA_Practice",c("lingkungan2_1", "lingkungan2_2","produksi4_1", "produksi4_2", "produksi4_3", "sosial5_1", "sosial5_2", "sosial5_3"))
)

CSA_stru6 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("CSA")),
  paths(from=c("farming"), to=c("ROF")),
  paths(from=c("CSA","farming","ROF"), to=c("CSA_Practice"))
)

CSA_SEM6<- estimate_pls(
  data = fisik_sem ,
  measurement_model = CSA_meas6,
  structural_model = CSA_stru6,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM6<- summary(CSA_SEM6)
summ_CSA_SEM6$reliability
summ_CSA_SEM6$loadings
summ_CSA_SEM6$vif_antecedents

###### CSA7 ######
CSA_compact_model <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CPP", c()),
  composite("G2", multi_items("g2_",1:4)),
  composite("G1", multi_items("g1_",1:5)),
  composite("socio-farming", c("socio1_1","socio1_2","socio1_3","socio1_4","socio1_5","socio1_6","socio1_12")),
  composite("csa-practice", c("farming1_1","farming1_2","farming1_3","farming1_4","farming1_5","farming1_7","farming1_8"))
)

CSA_stru6 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("CSA")),
  paths(from=c("farming"), to=c("ROF")),
  paths(from=c("CSA","farming","ROF"), to=c("CSA_Practice"))
)

CSA_SEM6<- estimate_pls(
  data = fisik_sem ,
  measurement_model = CSA_meas6,
  structural_model = CSA_stru6,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM6<- summary(CSA_SEM6)
summ_CSA_SEM6$reliability
summ_CSA_SEM6$loadings
summ_CSA_SEM6$vif_antecedents
