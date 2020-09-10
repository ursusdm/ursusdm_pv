

################################################################## Libraries #########################################################################


require("httr")
require("jsonlite")
library(urltools)
library("tidyverse")
library(tidyjson)
library(leaflet)
library(Rcpp)
library(kableExtra)
library(lubridate)


##Add funciones_prod_fv.R para los cálculos de energía solar a corto plazo

source("funciones_prod_fv.R")




############### Abrir con las estaciones y municipios ##########################################

estaciones <- as.data.frame(read_csv("estaciones_meteorologicas.csv"))
municipios <- as.data.frame(read_csv("municipios.csv"))
estaciones_rad <- as.data.frame(read_csv("estaciones_radiacion.csv"))

########################## Función que calcula la distancia euclidea o de Manhattan entre dos puntos #################################################
###### Nos servirá para conocer la estación más cercana a cada tejado que se esté procesando

distancia <- function(lat1, lon1, lat2, lon2, distancia = "euclidea"){
  
  if(distancia == "euclidea"){
    
    dist <- sqrt((lat1 - lat2)**2+ (lon1 -lon2)**2)
    
  }else if(distancia == "manhattan"){
    
    dist <- abs(lat1 - lat2)+ abs(lon1 -lon2)
  }
  
  dist 
}

######################################################## Radiación extraterrestre diaria ##############################################################################

## E0 Excentricidad solar diaria
## ws Ángulo de la salida del sol diario (wsr)
## decl Ángulo de declinación solar diaria
## lat latitud

gh_diaria <- function(E0, ws, decl, lat){
  
  k_solar <- 1361 #W/m^2
  
  gh <- 24/pi * k_solar * E0 * (ws* sin(decl) * sin(lat) + cos(decl) * cos(lat) * sin(ws))
}





########## Script que obtiene las estaciones que ofrecen datos de radiación y genera un csv en "estaciones_radiacion.csv" ########################## 

radiacion_url <- "red/especial/radiacion"

get_radiacion <- get_response(url_base, radiacion_url, api_key)

radiacion_text <- content(get_radiacion, "text")

datos_rad <- substring(radiacion_text, 32)

csv_rad <- read_delim(datos_rad, delim =  ";")

malaga <- which(csv_rad$Estación == "Málaga")

csv_rad$Indicativo[malaga] <- paste0(csv_rad$Indicativo[malaga], "X")

estaciones_radiacion <- csv_rad %>% 
  select(Estación, indicativo = Indicativo) %>%
  left_join(datos_estaciones_df, by = "indicativo")

# Datos completados con:
# http://www.aemet.es/es/eltiempo/observacion/radiacion/ozono?l=zaragoza&f=ozono
# http://www.aemet.es/es/eltiempo/observacion/radiacion/radiacion?l=maspalomas
# http://www.aemet.es/es/eltiempo/observacion/radiacion/radiacion?l=badajoz

zaragoza <- which(estaciones_radiacion$Estación == "Zaragoza")
maspalomas <- which(estaciones_radiacion$Estación == "Maspalomas")
badajoz <- which(estaciones_radiacion$Estación == "Badajoz")

columnas_na <- c(zaragoza, maspalomas, badajoz)

estaciones_radiacion$altitud[columnas_na] <- c( 258, 45, 175 )
estaciones_radiacion$latitud[columnas_na] <- c("413800N", "274529N", "385360N")

estaciones_radiacion$grados_lat[columnas_na] <- c(41, 27, 38)
estaciones_radiacion$minutos_lat[columnas_na] <- c(38, 45, 53)
estaciones_radiacion$segundos_lat[columnas_na] <- c(00, 29, 60)
estaciones_radiacion$orient_lat[columnas_na] <- c("N", "N", "N")
estaciones_radiacion$latitud_dec[columnas_na] <- c(latitud_longitud_decimal(41, 38, 00),
                                                   latitud_longitud_decimal(27, 45, 53),
                                                   latitud_longitud_decimal(38, 53, 60))

estaciones_radiacion$longitud[columnas_na] <- c("005256W", "153432W", "070046W")

estaciones_radiacion$grados_lon[columnas_na] <- c(00, 15, 07)
estaciones_radiacion$minutos_lon[columnas_na] <- c(52, 34, 00)
estaciones_radiacion$segundos_lon[columnas_na] <- c(56, 32, 46)
estaciones_radiacion$orient_lon[columnas_na] <- c("W", "W", "W")
estaciones_radiacion$longitud_dec[columnas_na] <- c(-latitud_longitud_decimal(00, 52, 56),
                                                    -latitud_longitud_decimal(15, 34 ,32),
                                                    -latitud_longitud_decimal(07, 00, 46))


write.csv(estaciones_radiacion, "estaciones_radiacion.csv")


####################################### Consulta de observación convecional de un día completo ############## 

#fecha dia de hoy, hora1, hora2 (horas a las que se ha programado la descarga automatica 00:00 y 10:00)

#idEstacion estación más cercana al tejado que se está procesando

# Devuelve un dataframe con las observaciones convencionales del día anterior

observacion_conv_df <- function(id_estacion, fecha, hora1, hora2) {
  
  ayer <- as.character(as.Date(fecha) - 1)
  
  obs_conv_hora1 <- paste("observacion_convencional","id", id_estacion, fecha, hora1, sep = "_")
  obs_conv_hora1 <- paste0(obs_conv_hora1,".csv")
  
  # Se abre el csv de observaciones convencionales de la estación más cercana al CSV de la 1ª hora programada de descarga
  obs_conv_hora1_df <-  read_csv(file = paste ( "aemet/",obs_conv_hora1))
  
  obs_conv_hora2 <- paste("observacion_convencional","id", id_estacion, fecha, hora2, sep = "_")
  obs_conv_hora2 <- paste0(obs_conv_hora2,".csv")
  
  # Se abre el csv de observaciones convencionales de la estación más cercana al CSV de la 2º hora programada de descarga
  obs_conv_hora2_df <- read_csv(file = paste ( "aemet/",obs_conv_hora2))
  
  obs_conv_hora1_df <- obs_conv_hora1_df %>% 
    mutate(dia = as.Date(fint)) %>%
    filter(dia == ayer)

  obs_conv_hora2_df <- obs_conv_hora2_df %>% 
    mutate(dia = as.Date(fint)) %>%
    filter(dia == ayer)
  
  obs_dia_df <- merge.data.frame(obs_conv_hora1_df, obs_conv_hora2_df, by = colnames(obs_conv_hora1_df), all =  TRUE) %>%
    mutate(hora = hour(fint))
  
  obs_dia_df
  
}






############################################ Datos de radiación de una estación el día de ayer #######################################################################################

# Leer el fichero csv  descargado automáticamente

## ayer Será el día de ayer (un día menos a cuando se llame el script)

## Sid_estacion de la que se quiere consultar

rglobal_df <- function(id_estacion, ayer) {
  
  rad <- paste("radiacion_solar", ayer, sep = "_")
  rad <- paste0("AEMET/", rad, ".csv")
  
  rad_df <- read_csv(file = paste ( "aemet/",rad))
  
  
  rad_df <- rad_df %>% filter(Indicativo == id_estacion)
  
  horas <- c("Tipo", as.character(5:20), "SUMA")
  rad_gl <- rad_df %>% select(all_of(horas))
  
  # 1 wh = 3.6 kJ
  
  rad_gl[1, 2:ncol(rad_gl)] <- rad_gl[1, 2:ncol(rad_gl)] *10/3.6 #Pasar los datos a Wh
  
  rad_gl
  
}

######################################### Extracción de los datos de predicción ##############################################################

prediccion_horas_df <- function(id_municipio, fecha, hora) {
  
  ## fecha, hora. Valores correspondientes a la fecha y hora de descarga automática del script. 
  ## id_municipio id del municipio del que se quieren conocer las predicciones para el día siguiente
  
  pred_horas <- paste("prediccion_horaria","id", id_municipio, fecha, hora, sep = "_")
  pred_horas <- paste0(pred_horas,".csv")
  
  pred_horas_df <- read_csv(file = paste ( "aemet/",pred_horas))
  
  pred_horas_df <- pred_horas_df %>%
    select(starts_with("estadoCielo"), 
           starts_with("temperatura."), 
           starts_with("humedadRelativa."),
           starts_with("viento."))
  
  pred_horas_df
  
}

########## Función que calcula la energía a corto plazo para un tejado ###############################

# input lat,long,orient,incl,area parametros de cada tejado que se esté procesando

calcularEnergiaDiaSiguiente <- function (lat, lon, orient, incl, area) {
  
  #Obtener municipio y estación convencional y de radiación más cercana
  
  municipios2 <- municipios %>%                
    mutate(distancia = distancia(latitud_dec, longitud_dec, lat, lon, distancia = "manhattan")) %>%
    filter(distancia == min(distancia))
  
  estaciones2 <- estaciones %>% 
    mutate(distancia = distancia(latitud_dec, longitud_dec, lat, lon)) %>%
    filter(distancia == min(distancia))
  
  estaciones_rad2 <- estaciones_rad %>%  
    mutate(distancia = distancia(latitud_dec, longitud_dec, lat, lon)) %>% 
    filter(distancia == min(distancia))
  
  ## Accedemos a las observaciones de los CSVS del aemet (observaciones convencionales, predicciones y observaciones de radiacion)
  
  hoy <-  as.character(Sys.Date())
  
  ## Obtenemos un dataframe los datos del csv de observaciones diarias (fusionamos el que se descarga a las 10:00 con el que se descarga a las 00:00)
  
  df_observacion <- observacion_conv_df(id_estacion = estaciones2$indicativo, fecha = hoy, hora1 = "00h", hora2 = "10h")
  
  #Preparamos un dataframe
  
  t_dia_previo <- mean(df_observacion$ta)
  
  t_9_12_diaprevio <- df_observacion %>%
    filter(hora %in% c(10:12))%>%
    select(ta) %>% 
    summarise(t_9_12_diaprevio = mean(ta, na.rm = T))%>%
    unlist()
  
  
  t_13_15_diaprevio <- df_observacion %>%
    filter(hora %in% c(13:15))%>%
    select(ta) %>% 
    summarise(t_13_15_diaprevio = mean(ta, na.rm = T))%>%
    unlist()
  
  h8_diaprevio <- df_observacion %>%
    filter(hora %in% c(10:12))%>%
    select(hr) %>% 
    summarise(hr_media = mean(hr, na.rm = T))%>%
    unlist() / 100
  
  h14_diaprevio <- df_observacion %>%
    filter(hora %in% c(13:15))%>%
    select(hr) %>% 
    summarise(hr_media = mean(hr, na.rm = T))%>%
    unlist() / 100
  
  precipitac_dia_previo <- mean(df_observacion$prec)
  
  humedad_diaria <- mean(df_observacion$hr) / 100
  
  
  ## Obtenemos un dataframe del csv de predicciones para el municipio más cercano al tejado y obtenemos los valores necesarios a partir del dataframe
  
  
  df_prediccion_horaria <- prediccion_horas_df(id_municipio = municipios2$id, hoy, "10h")
  df_prediccion_horaria$temperatura.periodo <- as.numeric(df_prediccion_horaria$temperatura.periodo)
  
  Predicc_temp_day_C <- mean(df_prediccion_horaria$temperatura.value)
  
  Predicc_Relative_humidity_day_0_1 <- mean(df_prediccion_horaria$humedadRelativa.value) / 100
  
  Predicc_temp_10_11_12_C <- df_prediccion_horaria %>%
    filter(temperatura.periodo %in% c(10:12))%>%
    select(temperatura.value) %>% 
    summarise(Predicc_temp_10_11_12_C = mean(temperatura.value, na.rm = T))%>%
    unlist()
  
  Predicc_temp_13_14_15_C <- df_prediccion_horaria %>%
    filter(temperatura.periodo %in% c(13:15))%>%
    select(temperatura.value) %>% 
    summarise(Predicc_temp_10_11_12_C = mean(temperatura.value, na.rm = T))%>%
    unlist()
  
  Predicc_R_hum_10_11_12 <- df_prediccion_horaria %>%
    filter(temperatura.periodo %in% c(10:12))%>%
    select(humedadRelativa.value) %>% 
    summarise(Predicc_R_hum_10_11_12 = mean(humedadRelativa.value, na.rm = T))%>%
    unlist() / 100
  
  Predicc_R_hum_13_14_15 <- df_prediccion_horaria %>%
    filter(temperatura.periodo  %in% c(13:15))%>%
    select(humedadRelativa.value) %>% 
    summarise(Predicc_R_hum_13_14_15 = mean(humedadRelativa.value, na.rm = T))%>%
    unlist() / 100
  
  Predicc_cloudy_sky_day_0_1 <- NA
  
  Predicc_cloudy_10_11_12 <- NA
  Predicc_cloudy_13_14_15 <- NA
  
  Predicc_temperatura_horaria <- df_prediccion_horaria$temperatura.value
  
  Predicc_vviento_horario <- df_prediccion_horaria$viento.velocidad * 1000 # La velocidad del viento viene en Km/h, pasar a m/s para la entrada de la prod_fotovoltaica
  
  # Parte de radiación solar. Obtenemos de la estación de radiación más cercana el dataframe del csv de datos de radiación, y a partir del mismo, obtenemos los valores necesarios
  
  ayer <- as.character(as.Date(hoy) - 1)
  
  df_r_global <- rglobal_df(estaciones_rad2$indicativo, ayer)
  
  gd_previo <- df_r_global %>% 
    select(SUMA) %>% 
    unlist() 
  
  # Radiación extraterrestre Gh----------------
  
  # Calculamos la radiación incidente  para la lat, lng, orient, iclinacion del tejado que se está procesando
  
  df_solar_energy <- df_energia_solar (lat, lon, orient, incl, area) 
  
  #mes, dia del que se quiera extraer la declinacion_solar, excentricidad_diaria, w_sr (entiendo que del siguiente que es cuando se quiera predecir)
  
  nextDay <- day (as.character(Sys.Date()+1))
  nextDayMonth <- month (as.character(Sys.Date()+1))
  
  gh_datos <- df_solar_energy %>%
    filter(mes == nextDayMonth, dia == nextDay) %>%
    select(declinacion_solar, excentricidad_diaria, w_sr)
  
  # Radiación extraterrestre Gh----------------
  
  gh <- gh_diaria(E0 = gh_datos$excentricidad_diaria,
                  ws = gh_datos$w_sr,
                  decl = gh_datos$declinacion_solar,
                  lat = lat)
  
  k_d_previo <- gd_previo/ gh #indice de transparencia (Kd) calculado
  
  # Componemos la observación con la que trabajará el sistema predictor
  
  observacion <- data.frame(gd_previo = gd_previo,
                            k_d_previo = k_d_previo,
                            t_9_12_diaprevio,
                            t_13_15_diaprevio,
                            h8_diaprevio,
                            h14_diaprevio,
                            t_dia_previo,
                            precipitac_dia_previo,
                            Predicc_temp_day_C,
                            Predicc_Relative_humidity_day_0_1,
                            Predicc_cloudy_sky_day_0_1,
                            Predicc_temp_10_11_12_C,
                            Predicc_cloudy_13_14_15,
                            Predicc_R_hum_10_11_12,
                            Predicc_R_hum_13_14_15,
                            Predicc_cloudy_10_11_12,
                            Predicc_cloudy_13_14_15)
  
  
  #Faltarían los datos de radiación para el cálculo de energía fotovoltaica
  
  prod_fv <- data.frame(Predicc_temperatura_horaria,
                        Predicc_vviento_horario)
  
  
}
  
#################################################################################################
