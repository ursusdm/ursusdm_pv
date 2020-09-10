library(tidyverse)
library(caret) # data wrangling
library(rJava)
library(RWeka)


examples_predicted_kd <- read.csv("examples_meteo+predicted_kd+cluster.csv")

# CONFIGURING DATASET FOR CLASSIFICATION
dataset <- examples_predicted_kd %>% rename(class=Today_cluster14_experts)

# INDUCING MODEL
file <- "lmt_classification_model.rds"

if (file.exists(file)) {
  model_lmt <- readRDS(file)
} else {
  path_learner <- "weka/classifiers/trees/LMT"
  
  # induce model with training to classify considering the cluster (named class in the dataset)
  # previous code: model <- do.call("LMT", list(class ~ ., data = dataset))
  learner <- make_Weka_classifier(path_learner)
  model_lmt <- learner(class~ ., data = dataset)
  
  # needed to sava a java object
  .jcache(model_lmt$classifier)
  saveRDS(model_lmt, file=file)
}

# predict complete dataset
prediction <- predict(model_lmt, newdata = dataset)





# TESTING (results are similar to those presented in the paper)
# There is a very little improvement (because of the overfitting)
TESTING <- FALSE
if(TESTING)
{
  confusion_matrix <- table(dataset$class, prediction)
  # calculate global accuracy x 100
  accuracy <- (sum(diag(confusion_matrix))/sum(confusion_matrix)) * 100
  accuracy
  
  #RESULTS
  # accuracy = 68.34564
  
  # FUNCTION FOR TESTING: CALCULATE ERROR IN ENERGY
  # prediction (cluster predicted)
  calculate_errors_given_centroids <- function(predicted_centroids)
  {
    # information from other files is needed
    
    # we assume datasets keep internal ordering of examples
    full_dataset <- read.csv("full_dataset.csv")
    # select ghext and gh information
    ghext <- full_dataset %>% select(ghext9,ghext10,ghext11,ghext12,ghext13,ghext14,ghext15,ghext16)
    gh    <- full_dataset %>% select(gh9,gh10,gh11,gh12,gh13,gh14,gh15,gh16)
    
    # loading centroid description
    description_centroid <- read.csv("centroids_14_types_of_days.csv")
    
    # for debugging purpose
    # total  <-  0
    
    # initialize variables that will accumulate values for the entire dataset
    sum_error_abs <-  0
    sum_error_sq  <-  0
    sum_gh  <-  0
    
    # for every predicted centroid in dataset
    # calculate the error and accumulate it
    # (with all other errors from this cluster and any other cluster)
    for (j in 1:length(predicted_centroids)) {
      # select the centroid
      predicted_centroid  <-  predicted_centroids[j]
      descrip_predicted_centroid  <-  description_centroid %>% 
        filter(CLUSTER == predicted_centroid) %>%
        select(-CLUSTER)
      
      # errors will be estimated by
      # gh_i - (kh'_i · ghext_i)
      estimated  <-  descrip_predicted_centroid * ghext[j,]
      abs_errors  <-  sum(abs(estimated - gh[j,]))
      sq_errors  <-  sum((estimated - gh[j,])^2)
      
      sum_error_abs <- sum_error_abs + abs_errors
      sum_error_sq   <-  sum_error_sq + sq_errors
      sum_gh  <-  sum_gh + sum(gh[j,])
      
      # for debugging purpose
      # total = total + 1
      # if (total%%1000==0){
      #   cat("progress: ", 100*(total/length(predicted_centroids)), "size = ", length(predicted_centroids), "\n")}
    }
    
    # m = number of measures, as many values in one curve * number of curves
    m  <-  (ncol(descrip_predicted_centroid))*(length(predicted_centroids))
    MAE   <-  sum_error_abs / m
    rMAE  <-  (sum_error_abs / sum_gh) * 100
    RMSE  <-  sqrt(sum_error_sq / m)
    rRMSE  <-  (RMSE / (sum_gh / m)) * 100
    
    errors  <-  list()
    errors$MAE  <-  MAE
    errors$rMAE  <-  rMAE
    errors$RMSE  <-  RMSE
    errors$rRMSE <-  rRMSE
    
    return(errors)
  }
  
  errors <-  calculate_errors_given_centroids(prediction)
  errors
  # RESULTS
  # $MAE   = 62.7106
  # $rMAE  = 12.73358
  # $RMSE  = 96.11814
  # $rRMSE = 19.51709
}
