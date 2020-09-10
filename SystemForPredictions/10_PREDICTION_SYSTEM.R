library(caret) # data wrangling
library(rJava)
library(RWeka)
library(tidyverse)

# CHECKING FOR MODELS
file_regression <- "random_forest_regression_model.rds"
file_classification <- "lmt_classification_model.rds"

if (!(file.exists(file_regression) &&
      file.exists(file_classification))) {
  stop("Models are not available")
} else {
  # LOADING MODELS
  model_rf <- readRDS(file_regression)
  model_lmt <- readRDS(file_classification)
  
  description_centroid <- read.csv("centroids_14_types_of_days.csv")
  
  observations <- read.csv("../aemet/observations.csv")
  
  # calculate gdext_previo needed in the regression model
  observations <- observations %>% 
                              mutate(gdext_previo = gd_previo/kd_previo)  %>%
                              select(1,gdext_previo,everything())
  
  # Prediction of kd
  kd_predicted<-predict.train(object=model_rf,observations,type="raw")
  
  # new dataset for classification purposes
  # include new predicted kd
  # remove real data about radiation
  observations_with_kd_prediction <- cbind(observations,kd_predicted)
  observations_with_kd_prediction <- observations_with_kd_prediction %>% 
                                    select(-gd_previo, -gdext_previo, -kd_previo) 

  # Prediction of type of day
  CLUSTER <- predict(model_lmt, newdata = observations_with_kd_prediction)
  CLUSTER <- as.data.frame(CLUSTER)
  
  # getting kh info for every type of day (and removing type of day)
  CLUSTER_with_kh_prediction <- left_join(CLUSTER,description_centroid,by="CLUSTER") %>% 
                                  select(-CLUSTER)
  
  # CREATING FINAL DATASET
  # including new attributes to original observations
  observations_with_kh_prediction <- cbind(observations,CLUSTER_with_kh_prediction)
  
  write.csv(observations_with_kh_prediction, "observations+kh_prediction.csv", row.names=FALSE)
}



