library(tidyverse)
library(caret) # data wrangling

examples <- read.csv("examples_meteo+radiation+cluster.csv")

# CONFIGURING DATASET FOR REGRESSION
# examples are defined by:
# gd_previo,
# kd_previo,
# t9_12_diaprevio,
# t13_15_diaprevio,
# h8_diaprevio,
# h14_diaprevio,
# t_dia_diaprevio,
# preciptac_dia_diaprevio,
# Predicc_temp_day_C,
# Predicc_Relative_humidity_day_0_1,
# Predicc_cloudy_sky_day_0_1,
# Predicc_temp_10_11_12_C,
# Predicc_temp_13_14_15_C,
# Predicc_R_hum_10_11_12,
# Predicc_R_hum_13_14_15,
# Predicc_cloudy_10_11_12,
# Predicc_cloudy_13_14_15,
# kd,
# Today_cluster14_experts

# Today_cluster14_experts is not available in the prediction of kd. Is used in a classification step (1 step forward)
# previous extraterrial gd was used in the definition of the system,
# but it is not available in the original dataset. It must be calculated
# kd is going to be learned (class)
dataset <- examples %>%
  select (-Today_cluster14_experts) %>% 
  mutate(gdext_previo = gd_previo/kd_previo)  %>%
  select(1,gdext_previo,everything()) %>% 
  rename(class=kd)

# DEFINING 100% TO TRAIN AND GET PREDICTION FOR k_d using previous day
# model selected: random forest
# dataset selected: basic (without prediction of AEMET for the k_d)

# define training control
full_dataset_control <- trainControl(
  ## 100% for training
  method = "none"
)

# INDUCING MODEL
file <- "random_forest_regression_model.rds"
if (file.exists(file)) {
  model_rf <- readRDS(file)
} else {
  model_rf <- train(class ~., 
                    data=dataset,
                    trControl=full_dataset_control,
                    ## Specify method to build the model
                    method="rf")
  saveRDS(model_rf, file=file)
}


# TESTING
TESTING <- FALSE
if(TESTING)
{
  # Calculating error between real k_d and predicted k_d' with random forest
  kd_real <- examples %>% select(kd)
  kd_real <- kd_real$kd
  kd_predicted_rf <- predict(model_rf)
  
  errors_kd_real_vs_kd_rf <- kd_real - kd_predicted_rf
  mae_kd_real_vs_kd_rf <- mean(abs(errors_kd_real_vs_kd_rf))
  rmae_kd_real_vs_kd_rf <- mae_kd_real_vs_kd_rf / mean(kd_real)
  squared_sums_kd_real_vs_kd_rf <- sum((errors_kd_real_vs_kd_rf)^2)
  mse_kd_real_vs_kd_rf <- squared_sums_kd_real_vs_kd_rf/length(kd_real)
  rmse_kd_real_vs_kd_rf <- sqrt(mse_kd_real_vs_kd_rf)
  nrmse_kd_real_vs_kd_rf <- rmse_kd_real_vs_kd_rf/mean(kd_real)
  
  rmae_kd_real_vs_kd_rf
  nrmse_kd_real_vs_kd_rf
  
  # results
  # rmae_kd_real_vs_kd_rf = 0.06009241
  # nrmse_kd_real_vs_kd_rf = 0.08331453
  
  # Calculating error between real k_d and predicted k_d' with AEMET (national agency) prediction
  full_dataset <- read.csv("full_dataset.csv")
  kd_predicted_AEMET <- full_dataset %>% select(Predic_Kd)
  kd_predicted_AEMET <- kd_predicted_AEMET$Predic_Kd
  
  errors_kd_real_vs_kd_AEMET <- kd_real - kd_predicted_AEMET
  mae_kd_real_vs_kd_AEMET <- mean(abs(errors_kd_real_vs_kd_AEMET))
  rmae_kd_real_vs_kd_AEMET <- mae_kd_real_vs_kd_AEMET / mean(kd_real)
  squared_sums_kd_real_vs_kd_AEMET <- sum((errors_kd_real_vs_kd_AEMET)^2)
  mse_kd_real_vs_kd_AEMET <- squared_sums_kd_real_vs_kd_AEMET/length(kd_real)
  rmse_kd_real_vs_kd_AEMET <- sqrt(mse_kd_real_vs_kd_AEMET)
  nrmse_kd_real_vs_kd_AEMET <- rmse_kd_real_vs_kd_AEMET/mean(kd_real)
  
  rmae_kd_real_vs_kd_AEMET
  nrmse_kd_real_vs_kd_AEMET
  
  # results
  # rmae_kd_real_vs_kd_AEMET = 0.1992758
  # nrmse_kd_real_vs_kd_AEMET = 0.2728151
}