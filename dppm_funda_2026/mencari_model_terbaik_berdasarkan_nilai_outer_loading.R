###### CSA0 ######
CSA_meas0 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CS_Practice", c("adaptasi1_1","adaptasi1_2","adaptasi1_3","lingkungan2_1", "lingkungan2_2","mitigasi3_1","mitigasi3_2","mitigasi3_3","mitigasi3_4","mitigasi3_5","produksi4_1", "produksi4_2", "produksi4_3", "produksi4_4","produksi4_5","sosial5_1", "sosial5_2", "sosial5_3","ekonomi6_1", "ekonomi6_2", "ekonomi6_3", "ekonomi6_4", "ekonomi6_5", "ekonomi6_6")),
  composite("CS_Adopt_Perc", c("g1_1","g1_2","g1_3","g1_4","g1_5","g2_1","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("age_yr","exp_yr","family_org","downstream","mid","log_acreage","log_yield","log_labour","log_seed","log_manure"))
)

CSA_stru0 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("CS_Adopt_Perc")),
  paths(from=c("SOFAR"), to=c("ROF")),
  paths(from=c("CS_Adopt_Perc","SOFAR","ROF"), to=c("CS_Practice"))
)

CSA_SEM0<- estimate_pls(
  data = fisik_sem ,
  measurement_model = CSA_meas0,
  structural_model = CSA_stru0,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM0<- summary(CSA_SEM0)
summ_CSA_SEM0$reliability
summ_CSA_SEM0$loadings
summ_CSA_SEM0$vif_antecedents

##### CSA1 ######
CSA_meas1 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CS_Practice", c("adaptasi1_2","adaptasi1_3","lingkungan2_1", "lingkungan2_2","mitigasi3_1","mitigasi3_3","mitigasi3_4","mitigasi3_5","produksi4_1", "produksi4_2", "produksi4_3", "produksi4_4","produksi4_5","sosial5_1", "sosial5_2", "sosial5_3", "ekonomi6_2", "ekonomi6_3", "ekonomi6_5", "ekonomi6_6")),
  composite("CS_Adopt_Perc", c("g1_2","g1_3","g1_4","g1_5","g2_1","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("exp_yr","family_org","downstream","mid","log_acreage","log_yield","log_labour"))
)

CSA_stru1 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("CS_Adopt_Perc")),
  paths(from=c("SOFAR"), to=c("ROF")),
  paths(from=c("CS_Adopt_Perc","SOFAR","ROF"), to=c("CS_Practice"))
)

CSA_SEM1<- estimate_pls(
  data = fisik_sem ,
  measurement_model = CSA_meas1,
  structural_model = CSA_stru1,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM1<- summary(CSA_SEM1)
summ_CSA_SEM1$reliability
summ_CSA_SEM1$loadings
summ_CSA_SEM1$vif_antecedents

##### CSA2 ######
CSA_meas2 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CS_Practice", c("lingkungan2_1", "lingkungan2_2","mitigasi3_3","mitigasi3_4","mitigasi3_5","produksi4_4","produksi4_5","sosial5_1", "sosial5_2", "sosial5_3","ekonomi6_2","ekonomi6_3","ekonomi6_6")),
  composite("CS_Adopt_Perc", c("g1_2","g1_3","g1_4","g1_5","g2_1","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("family_org","downstream","log_acreage","log_yield","log_labour"))
)

CSA_stru2 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("CS_Adopt_Perc")),
  paths(from=c("SOFAR"), to=c("ROF")),
  paths(from=c("CS_Adopt_Perc","SOFAR","ROF"), to=c("CS_Practice"))
)

CSA_SEM2<- estimate_pls(
  data = fisik_sem ,
  measurement_model = CSA_meas2,
  structural_model = CSA_stru2,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM2<- summary(CSA_SEM2)
summ_CSA_SEM2$reliability
summ_CSA_SEM2$loadings
summ_CSA_SEM2$vif_antecedents

##### CSA3 ######
CSA_meas3 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CS_Practice", c("lingkungan2_1", "lingkungan2_2","mitigasi3_3","mitigasi3_4","mitigasi3_5","produksi4_4","produksi4_5","sosial5_1", "sosial5_2", "sosial5_3","ekonomi6_2","ekonomi6_3","ekonomi6_6")),
  composite("CS_Adopt_Perc", c("g1_2","g1_3","g1_4","g1_5","g2_1","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("family_org","downstream","log_labour"))
)

CSA_stru3 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("CS_Adopt_Perc")),
  paths(from=c("SOFAR"), to=c("ROF")),
  paths(from=c("CS_Adopt_Perc","SOFAR","ROF"), to=c("CS_Practice"))
)

CSA_SEM3<- estimate_pls(
  data = fisik_sem ,
  measurement_model = CSA_meas3,
  structural_model = CSA_stru3,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM3<- summary(CSA_SEM3)
summ_CSA_SEM3$reliability
summ_CSA_SEM3$loadings
summ_CSA_SEM3$vif_antecedents

##### CSA4 ######
CSA_meas4 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_3","f3_4","f3_5")),
  composite("CS_Practice", c("lingkungan2_1", "lingkungan2_2","mitigasi3_3","mitigasi3_4","mitigasi3_5","produksi4_4","produksi4_5","sosial5_2", "sosial5_3","ekonomi6_2")),
  composite("CS_Adopt_Perc", c("g1_2","g1_3","g1_4","g1_5","g2_1","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("family_org","downstream","log_labour"))
)

CSA_stru4 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("CS_Adopt_Perc")),
  paths(from=c("SOFAR"), to=c("ROF")),
  paths(from=c("CS_Adopt_Perc","SOFAR","ROF"), to=c("CS_Practice"))
)

CSA_SEM4<- estimate_pls(
  data = fisik_sem ,
  measurement_model = CSA_meas4,
  structural_model = CSA_stru4,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM4<- summary(CSA_SEM4)
summ_CSA_SEM4$reliability
summ_CSA_SEM4$loadings
summ_CSA_SEM4$vif_antecedents

##### CSA5 ######
CSA_meas5 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_3","f3_4")),
  composite("CS_Practice", c("lingkungan2_1", "lingkungan2_2","mitigasi3_4","mitigasi3_5","produksi4_4","produksi4_5","sosial5_2", "sosial5_3","ekonomi6_2")),
  composite("CS_Adopt_Perc", c("g1_2","g1_3","g1_4","g1_5","g2_1","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("family_org","downstream","log_labour"))
)

CSA_stru5 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("CS_Adopt_Perc")),
  paths(from=c("SOFAR"), to=c("ROF")),
  paths(from=c("CS_Adopt_Perc","SOFAR","ROF"), to=c("CS_Practice"))
)

CSA_SEM5<- estimate_pls(
  data = fisik_sem ,
  measurement_model = CSA_meas5,
  structural_model = CSA_stru5,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM5<- summary(CSA_SEM5)
summ_CSA_SEM5$reliability
summ_CSA_SEM5$loadings
summ_CSA_SEM5$vif_antecedents

##### CSA6 ######
CSA_meas6 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_2","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_3","f3_1","f3_3","f3_4")),
  composite("CS_Practice", c("lingkungan2_2","produksi4_4","produksi4_5","sosial5_2", "sosial5_3")),
  composite("CS_Adopt_Perc", c("g1_2","g1_3","g1_4","g1_5","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("downstream","log_labour"))
)

CSA_stru6 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("CS_Adopt_Perc")),
  paths(from=c("SOFAR"), to=c("ROF")),
  paths(from=c("CS_Adopt_Perc","SOFAR","ROF"), to=c("CS_Practice"))
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

##### CSA7 ######
CSA_meas7 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_2","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f3_1","f3_3","f3_4")),
  composite("CS_Practice", c("lingkungan2_2","produksi4_5","sosial5_2", "sosial5_3")),
  composite("CS_Adopt_Perc", c("g1_2","g1_3","g1_4","g1_5","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("downstream","log_labour"))
)

CSA_stru7 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("CS_Adopt_Perc")),
  paths(from=c("SOFAR"), to=c("ROF")),
  paths(from=c("CS_Adopt_Perc","SOFAR","ROF"), to=c("CS_Practice"))
)

CSA_SEM7<- estimate_pls(
  data = fisik_sem ,
  measurement_model = CSA_meas7,
  structural_model = CSA_stru7,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM7<- summary(CSA_SEM7)
summ_CSA_SEM7$reliability
summ_CSA_SEM7$loadings
summ_CSA_SEM7$vif_antecedents

##### CSA8 ######
CSA_meas8 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d4_1","d4_2","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f3_1","f3_3","f3_4")),
  composite("CS_Practice", c("lingkungan2_2","produksi4_5","sosial5_2", "sosial5_3")),
  composite("CS_Adopt_Perc", c("g1_2","g1_3","g1_4","g1_5","g2_2","g2_3","g2_4")),
  composite("SOFAR", c("downstream","log_labour"))
)

CSA_stru8 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"),to=c("CS_Adopt_Perc")),
  paths(from=c("SOFAR"), to=c("ROF")),
  paths(from=c("CS_Adopt_Perc","SOFAR","ROF"), to=c("CS_Practice"))
)

CSA_SEM8<- estimate_pls(
  data = fisik_sem ,
  measurement_model = CSA_meas8,
  structural_model = CSA_stru8,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

summ_CSA_SEM8<- summary(CSA_SEM8)
summ_CSA_SEM8$reliability
summ_CSA_SEM8$loadings
summ_CSA_SEM8$vif_antecedents
