
corp_rep_data <- read.csv("https://raw.githubusercontent.com/srk7774/data/refs/heads/master/Corporate_Reputation_Data.csv", header = TRUE, sep = ";")

head(corp_rep_data)

##### Specifying Measurement Models ####
# Create measurement model
simple_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

# Create structural model
simple_sm <- relationships(
  paths(from=c("COMP", "LIKE"), to=c("CUSA", "CUSL")),
  paths(from=c("CUSA"), to=c("CUSL"))
)

##### Estimating Models ####
# Estimate models
corp_rep_simple_model <- estimate_pls(
  data = corp_rep_data,
  measurement_model = simple_mm,
  structural_model = simple_sm,
  inner_weights = path_weighting,
  missing = mean_replacement,
  missing_value = "-99"
)

##### Summarizng (Estimation result) Models ####
# Summarize the model result
summary_simple_corp_rep <- summary(corp_rep_simple_model)

# Inspect the construct reliability metrics
summary_simple_corp_rep$reliability

##### Bootsrapping Models ####
# Bootstrap the models
boot_simpe_corp_rep <- bootstrap_model(
  seminr_model = corp_rep_simple_model,
  nboot = 1000,
  cores = NULL,
  sed = 123
)

# Store the summary of the bootsrapped model
sum_boot_simpe_corp_rep <- summary(boot_simpe_corp_rep)

# Inspect the bootsrapped indicator loadings
sum_boot_simpe_corp_rep$bootsrapped_loadings

##### Structural Model Assesment ####
# Colinearity (VIF < 3)
summary_simple_corp_rep$vif_antecedents

# Inspect bootsrapped structural paths
sum_boot_simpe_corp_rep$bootstrapped_paths

# Inspect total effects
sum_boot_simpe_corp_rep$bootstrapped_total_paths

# Inspect the model's path coefficients and the R-square values
summary_simple_corp_rep$path

# Inspect the model's f-square
summary_simple_corp_rep$fSquare

##### Output Visualization ####
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Simple graph
graph <- plot(boot_simpe_corp_rep, title = "Bootsrapped Model")

# Export to SVG format
svg_graph <- export_svg(graph)

# render to png format
rsvg_png(
  svg = charToRaw(svg_graph),
  file = "bootsrap_model_hd.png",
  width = 1920,
  height = 1080
)
