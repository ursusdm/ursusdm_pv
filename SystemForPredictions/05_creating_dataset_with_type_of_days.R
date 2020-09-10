library(tidyverse)

# loading centroid description
description_centroid <- read.csv("centroids_14_types_of_days.csv")

# BUILDING NEW DATASET FOR CLASSIFICATION

# examples
examples <- read.csv("examples_meteo+predicted_kd+cluster.csv")

# CREATING DATASET TO USE REGRESSION MODEL TO PREDICT kd
# previous extraterrial gd was used in the definition of the system,
# but it is not available in the original dataset. It must be calculated
# we must remove real kd (because we are calculating the prediction of kd using information from previous day)
dataset <- examples %>% select(-Today_cluster14_experts)

# LOADING MODEL: LMT in "model_lmt"
file <- "lmt_classification_model.rds"
if (file.exists(file)) {
  model_lmt <- readRDS(file)
  
  # Prediction of type of day
  CLUSTER <- predict(model_lmt, newdata = dataset)
  CLUSTER <- as.data.frame(CLUSTER)
 
  # getting kh info for every type of day (and removing type of day)
  CLUSTER_with_kh_prediction <- left_join(CLUSTER,description_centroid,by="CLUSTER") %>% 
    select(-CLUSTER)
  
  # CREATING FINAL DATASET
  # remove attributes created in the process
  dataset_with_kh_prediction <- dataset %>% select(-kd_predicted)
  
  dataset_with_kh_prediction <- cbind(dataset_with_kh_prediction,CLUSTER_with_kh_prediction)
  
  write.csv(dataset_with_kh_prediction, "examples_meteo+radiation+kh_prediction.csv", row.names=FALSE)
  
} else {
  stop("Model is not available at ", file, " SOURCE: 3_inducing_LMT_for_classification.R")
}
