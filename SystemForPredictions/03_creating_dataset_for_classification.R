library(tidyverse)

examples <- read.csv("examples_meteo+radiation+cluster.csv")

# BUILDING NEW DATASET FOR CLASSIFICATION

# CREATING DATASET TO USE REGRESSION MODEL TO PREDICT kd
# previous extraterrial gd was used in the definition of the system,
# but it is not available in the original dataset. It must be calculated
# we must remove real kd (because we are calculating the prediction of kd using information from previous day)
dataset <- examples %>%
  mutate(gdext_previo = gd_previo/kd_previo)  %>%
  select(1,gdext_previo,everything()) %>% 
  select(-kd)

# LOADING MODEL: random forest in "model_rf"
file <- "random_forest_regression_model.rds"
if (file.exists(file)) {
  model_rf <- readRDS(file)
  
  # Prediction of kd
  kd_predicted<-predict.train(object=model_rf,dataset,type="raw")
 
  # new dataset for classification purposes
  # include new predicted kd
  # remove real data about radiation
  # and reorder some attributes
  dataset_with_prediction <- cbind(dataset,kd_predicted)
  dataset_with_prediction <- dataset_with_prediction %>% 
                                select(-gd_previo, -gdext_previo, -kd_previo) 
  dataset_with_prediction <- dataset_with_prediction %>% 
                                select(1:(length(dataset_with_prediction)-2),
                                          length(dataset_with_prediction),
                                          length(dataset_with_prediction)-1)
  
  write.csv(dataset_with_prediction, "examples_meteo+predicted_kd+cluster.csv", row.names=FALSE)
  
} else {
  stop("Model is not available at ", file, " SOURCE: 1_inducing_RF_for_regression.R")
}
