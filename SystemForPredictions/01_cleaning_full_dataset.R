library(tidyverse)

full_dataset <- read.csv("full_dataset.csv")

# GETTING ONLY ATTRIBUTES NEEDED FOR THE PREDICTION SYSTEM
examples <- full_dataset %>% select(gd_previo,
                                    kd_previo,
                                    t9_12_diaprevio,
                                    t13_15_diaprevio,
                                    h8_diaprevio,
                                    h14_diaprevio,
                                    t_dia_diaprevio,
                                    preciptac_dia_diaprevio,
                                    Predicc_temp_day_C,
                                    Predicc_Relative_humidity_day_0_1,
                                    Predicc_cloudy_sky_day_0_1,
                                    Predicc_temp_10_11_12_C,
                                    Predicc_temp_13_14_15_C,
                                    Predicc_R_hum_10_11_12,
                                    Predicc_R_hum_13_14_15,
                                    Predicc_cloudy_10_11_12,
                                    Predicc_cloudy_13_14_15,
                                    kd, # will be predicted with a regression model
                                    Today_cluster14_experts) # will be predicted with a classification model

# CREATING NEW FILE FOR DATASET
write.csv(examples,"examples_meteo+radiation+cluster.csv", row.names=FALSE)
