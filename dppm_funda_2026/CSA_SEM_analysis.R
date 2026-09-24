library(googlesheets4)
library(dplyr)
library(seminr)

##### SEM-PLS analysis #####
# Measurement model
CSA_meas_model <- constructs(
  composite("D1", multi_items("d1_",1:4)),
  composite("D2", multi_items("d2_",1:5)),
  composite("D3", multi_items("d3_",1:4)),
  composite("D4", multi_items("d4_",1:4)),
  composite("E1", multi_items("e1_",1:4)),
  composite("E2", multi_items("e2_",1:5)),
  composite("E3", multi_items("e3_",1:4)),
  composite("F1", multi_items("f1_",1:4)),
  composite("F2", multi_items("f2_",1:4)),
  composite("F3", multi_items("f3_",1:5)),
  composite("G2", multi_items("g2_",1:4)),
  composite("G1", multi_items("g1_",1:5)),
  composite("socio", multi_items("socio1_",1:14)),
  composite("farming", multi_items("farming1_",1:8))
)

CSA_compact_model <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("G2", multi_items("g2_",1:4)),
  composite("G1", multi_items("g1_",1:5)),
  composite("socio", c("socio1_1","socio1_2","socio1_3","socio1_4","socio1_5","socio1_6","socio1_12")),
  composite("farming", c("farming1_1","farming1_2","farming1_3","farming1_4","farming1_5","farming1_7","farming1_8"))
)

CSA_compact_model2 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("G2", multi_items("g2_",1:4)),
  composite("G1", multi_items("g1_",1:5)),
  composite("socio", c("socio1_1","socio1_2","socio1_3","socio1_4","socio1_5","socio1_6","socio1_12")),
  composite("farming", c("farming1_1","farming1_2","farming1_3","farming1_4","farming1_5","farming1_6","farming1_10"))
)

CSA_compact_model3 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CSA", c("g1_1","g1_2","g1_3","g1_4","g2_1","g2_2","g2_3","g2_4")),
  composite("socio", c("socio1_1","socio1_2","socio1_3")),
  composite("farming", c("farming1_1","farming1_3","farming1_4","farming1_5","farming1_6","farming1_10"))
)

CSA_compact_exc_socio <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d2_4","d2_5","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_3","d4_4")),
  composite("ROF", c("e1_1","e1_2","e1_3","e1_4","e2_1","e2_2","e2_3","e2_4","e2_5","e3_1","e3_2","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CSA", c("g1_1","g1_2","g1_3","g1_4","g2_1","g2_2","g2_3","g2_4")),
  composite("farming", c("farming1_1","farming1_3","farming1_4","farming1_5","farming1_6","farming1_10"))
)

CSA_meas3 <- constructs(
  composite("ROG", c("d1_1","d1_2","d1_3","d1_4","d2_1","d2_2","d2_3","d3_1","d3_2","d3_3","d3_4","d4_1","d4_2","d4_4")),
  composite("ROF", c("e1_1","e2_3","e2_4","e3_3","e3_4")),
  composite("GFP", c("f1_1","f1_2","f1_3","f1_4","f2_1","f2_2","f2_3","f2_4","f3_1","f3_2","f3_3","f3_4","f3_5")),
  composite("CSA", c("g1_1","g1_2","g1_3","g1_4","g2_1","g2_2","g2_4")),
  composite("farming", c("socio1_4" ,"socio1_5","farming1_2","farming1_4","farming1_5"))
)
# Structural model
CSA_stru_model <- relationships(
  paths(from=c("D1","D2", "D3", "D4", "E1","E2","E3","F1","F2","F3","socio","farming"), to=c("G1","G2")),
  paths(from=c("G2"), to=c("G1"))
)

CSA_stru_compact<- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","socio","farming"), to=c("G2","G1")),
  paths(from=c("G2"), to=c("G1"))
)

CSA_stru_compact3<- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","socio","farming"), to=c("CSA"))
)

CSA_stru_exc_socio<- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP","farming"), to=c("CSA"))
)

CSA_stru3 <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from = c("farming"), to=c("ROF")),
  paths(from=c("GFP","farming"), to=c("CSA"))
)

CSA_stru_natural <- relationships(
  paths(from=c("ROF","ROG"), to=c("GFP")),
  paths(from=c("GFP"), to=c("CSA"))
)

# Model estimation
CSA_SEM_model <- estimate_pls(
  data = SEM_dataset_col,
  measurement_model = CSA_meas_model,
  structural_model = CSA_stru_model,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

CSA_SEM_compact <- estimate_pls(
  data = SEM_dataset,
  measurement_model = CSA_compact_model3,
  structural_model = CSA_stru_compact3,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

CSA_SEM_compact_exc_socio<- estimate_pls(
  data = SEM_dataset,
  measurement_model = CSA_meas3,
  structural_model = CSA_stru_natural,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

CSA_SEM3<- estimate_pls(
  data = fisik_sem,
  measurement_model = CSA_meas3,
  structural_model = CSA_stru3,
  inner_weight = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

# Model Summary
summ_CSA_SEM_model <- summary(CSA_SEM_model)
summ_CSA_SEM_compact <- summary(CSA_SEM_compact)
summ_CSA_SEM_compact_exc_socio <- summary(CSA_SEM_compact_exc_socio)

# Inspect the constructs reliability metrics
summ_CSA_SEM_model$reliability
summ_CSA_SEM_compact$reliability
summ_CSA_SEM_compact_exc_socio$reliability

# Bootstrapping model
boot_CSA_SEM_model <- bootstrap_model(
  seminr_model = CSA_SEM_model,
  nboot = 1000,
  cores = NULL,
  sed = 123
)

# Summary of Bootsrapping
sum_boot_CSA_SEM_model <- summary(boot_CSA_SEM_model)

# Indicator boot loadings
sum_boot_CSA_SEM_model$bootstrapped_loadings
write.csv(sum_boot_CSA_SEM_model$bootstrapped_loadings, file="boot_SEM_CSA.csv")

# Model Colinearty (VIF < 3)
summ_CSA_SEM_model$vif_antecedents

# Bootstrapped structural paths
sum_boot_CSA_SEM_model$bootstrapped_paths

# Total effects
sum_boot_CSA_SEM_model$bootstrapped_total_paths

# Path coefficients and R-square values
summ_CSA_SEM_model$path

# Model fSquare
summ_CSA_SEM_model$fSquare

##### Output Visualization ####
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Simple graph
graph_csa_sem <- plot(CSA_SEM_model, title = "Bootsrapped CSA Model")

# Export to SVG format
svg_graph_csa_sem <- export_svg(graph_csa_sem)

# render to png format
rsvg_png(
  svg = charToRaw(svg_graph_csa_sem),
  file = "bootsrap__csa_model_hd.png",
  width = 1080,
  height = 1920
)
