library(tidyverse)

examples <- read.csv("examples_meteo+radiation+cluster.csv")

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

# remove information not available in the input of the system
observations <- examples %>% select(-kd, -Today_cluster14_experts)

write.csv(observations, "observations_meteo+radiation.csv", row.names=FALSE)
