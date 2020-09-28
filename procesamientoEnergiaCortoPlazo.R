
###########Script que se utilizará para obtener la energía a corto plazo para el día siguiente de cada tejado ##############

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


############### Obtiene dataframes con las estaciones y municipios a partir de los CSVS del AEMET prev. descargados ###############################

estaciones <- read_csv("estaciones_meteorologicas.csv")
municipios <- read_csv("municipios.csv")
estaciones_rad <- read.csv("estaciones_radiacion.csv",stringsAsFactors=FALSE, fileEncoding="latin1")

########################## Función que calcula la distancia euclidea o de Manhattan entre dos puntos #################################################
############## Nos servirá para conocer la estación o municipio más cercana/o a cada tejado que se esté procesando ###################################

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

######################## Función que transforma las observaciones del estado del cielo a valor numérico ##############

sustitucion <- function(df){
  
  descripcion <- c("Despejado", "Poco nuboso", "Intervalos nubosos", "Nuboso", "Muy nuboso", "Cubierto", "Nubes altas", "Intervalos nubosos con lluvia", "Nuboso con lluvia", "Muy nuboso con lluvia", "Cubierto con lluvia", "Intervalos nubosos con nieve", "Nuboso con nieve", "Muy nuboso con nieve",  "Cubierto con nieve", "Chubascos", "Tormenta", "Granizo", "Bruma", "Niebla", "Calma", "Intervalos nubosos con lluvia escasa", "Muy nuboso con lluvia escasa",  "Nuboso con lluvia escasa", "Cubierto con lluvia escasa")
  
  valor <- c(1, 0.8, 0.7, 0.6, 0.4, 0.2, 0.8, 0.6, 0.4, 0.2, 0.2, 0.6, 0.4, 0.2, 0.2, 0.4, 0.2, 0.4, 0.5, 0.4, 0.8, 0.65, 0.25, 0.45, 0.25)
  
  for (d in 1:length(descripcion)){
    
    df$estadoCielo.descripcion[which(df$estadoCielo.descripcion == descripcion[d])] <- valor[d]
    
  }
  
  df
  
}

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
  
  rad <- paste0(rad,".csv")
  
  rad_df <- read_csv(file = paste ("aemet/",rad) )


  rad_df <- rad_df %>%
    filter(Indicativo == id_estacion)
  
  horas <- c("Tipo", as.character(5:20), "SUMA")
  rad_gl <- rad_df %>% select(all_of(horas))
  
  # 1 wh = 3.6 kJ
  
  rad_gl[1, 2:ncol(rad_gl)] <- rad_gl[1, 2:ncol(rad_gl)] *10/3.6 #Pasar los datos a Wh
  
  rad_gl
  
}

######################################### Extracción de los datos de predicción ##############################################################

prediccion_horas_df <- function(id_municipio, fecha, hora) {
  
  ## fecha, hora. Valores correspondientes a la fecha (hoy) y hora de descarga automática del script.  (10)
  ## id_municipio id del municipio del que se quieren conocer las predicciones para el día siguiente
  
  pred_horas <- paste("prediccion_horaria","id", id_municipio, fecha, hora, sep = "_")
  pred_horas <- paste0(pred_horas,".csv")
  
  pred_horas_df <- read_csv(file = paste ( "aemet/",pred_horas))
  
  pred_horas_df <- pred_horas_df %>%
    select(starts_with("estadoCielo"), 
           starts_with("temperatura."), 
           starts_with("humedadRelativa."),
           fecha,
           starts_with("viento."))
  
  #Filtramos las predicciones que se elaboraron ayer para el día de hoy
  hoy <-  as.character(Sys.Date())
  
  pred_horas_df <- pred_horas_df%>%
    filter(fecha == as.Date(hoy))
  
  pred_horas_df
  
}

########## Función que calcula la energía a corto plazo para un tejado ###############################

# input lat,long,orient,incl,area parametros de cada tejado que se esté procesando

calcularEnergiaDiaSiguiente <- function (lat, lon, orient, incl, area) {
  
  #Obtener municipio y estación convencional y de radiación más cercana
  
  municipios2 <- municipios %>%                
    mutate(distancia = distancia(latitud_dec, longitud_dec, lat, lon)) %>%
    filter(distancia == min(distancia))
  
  print ("mun2")
  print (municipios2)
  
  estaciones2 <- estaciones %>% 
    mutate(distancia = distancia(latitud_dec, longitud_dec, lat, lon)) %>%
    filter(distancia == min(distancia))
  
  print ("estaciones2")
  print (estaciones2)
  
  estaciones_rad2 <- estaciones_rad %>%  
    mutate(distancia = distancia(latitud_dec, longitud_dec, lat, lon)) %>% 
    filter(distancia == min(distancia))
  
  print ("estaciones_rad2")
  print (estaciones_rad2)
  
  ## Accedemos a las observaciones de los CSVS del aemet (observaciones convencionales, predicciones y observaciones de radiacion)
  
  hoy <-  as.character(Sys.Date())
  
  ## Obtenemos un dataframe los datos del csv de observaciones diarias (fusionamos el que se descarga a las 10:00 con el que se descarga a las 00:00)
  
  df_observacion <- observacion_conv_df(id_estacion = estaciones2$indicativo, fecha = hoy, hora1 = "00h", hora2 = "10h")
  
  print ("df_observacion")
  print (df_observacion)
  
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
    summarise(Predicc_temp_13_14_15_C = mean(temperatura.value, na.rm = T))%>%
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
  
  df_valores_cielo <- sustitucion(df_prediccion_horaria)

  Predicc_cloudy_13_14_15 <- df_valores_cielo %>%
    filter(estadoCielo.periodo %in% c(13:15))%>%
    select(estadoCielo.descripcion) %>% 
    summarise(Predicc_cloudy_13_14_15 = mean(as.numeric(estadoCielo.descripcion), na.rm = T))%>%
    unlist() 
  
  
  Predicc_cloudy_sky_day_0_1 <- df_valores_cielo %>%
    summarise(Predicc_cloudy_sky_day_0_1 = mean(as.numeric(estadoCielo.descripcion), na.rm = T))%>%
    unlist() 
  
  Predicc_cloudy_10_11_12 <- df_valores_cielo %>%
    filter(estadoCielo.periodo %in% c(10:12))%>%
    select(estadoCielo.descripcion) %>% 
    summarise(Predicc_cloudy_10_11_12 = mean(as.numeric(estadoCielo.descripcion), na.rm = T))%>%
    unlist() 
  

  
  Predicc_temperatura_horaria <- df_prediccion_horaria$temperatura.value
  
  Predicc_vviento_horario <- df_prediccion_horaria$viento.velocidad * 1000 # La velocidad del viento viene en Km/h, pasar a m/s para la entrada de la prod_fotovoltaica
  
  # Parte de radiación solar. Obtenemos de la estación de radiación más cercana el dataframe del csv de datos de radiación, y a partir del mismo, obtenemos los valores necesarios
  
  ayer <- as.character(as.Date(hoy) - 1)
  
  df_r_global <- rglobal_df(estaciones_rad2$indicativo, ayer)
  
  #para Málaga
  #df_r_global <- rglobal_df("6156X", ayer)
  
  #Nos quedamos con la radiación total global del día anterior
  gd_previo <- df_r_global %>% 
    select(SUMA) %>% 
    unlist() 
  
  #Valores horarios de radiacion global del día anterior
  gh_previo  <- df_r_global %>% 
    select(-SUMA,-Tipo) 
  
  #dia y mes actual
  actualDay <- day (as.character(Sys.Date()))
  actualMonth <- month (as.character(Sys.Date()))
  
  # Calculamos el dataframe con la información de radiación (excentricidad, wsr, ...)  para la lat, lng, orient, iclinacion del tejado que se está procesando
  
  df_solar_energy <- df_energia_solar (lat, lon, orient, incl, area) 
  
  #Nos quedamos con los datos de radiación para hoy
  calculos_dia <- df_solar_energy%>%filter(mes==actualMonth, dia == actualDay)
  
  acimut_h_dia <- paste("Acimut", as.character(4:19), as.character(5:20), sep = "_")
  altura_h_dia <- paste("Altura", as.character(4:19), as.character(5:20), sep = "_")
  angulo_h_dia <- paste("W", as.character(4:19), as.character(5:20), sep = "_")
  
  
  dia_Acimut_h <- calculos_dia[1, acimut_h_dia]
  dia_Altura_h <- calculos_dia[1, altura_h_dia]
  dia_Angulo_h <- calculos_dia[1, angulo_h_dia]
  dia_declinacion <- calculos_dia$declinacion_solar[1]
  
  inclinacion <- incl * pi/180
  orientacion <- orient * pi/180
  lat <- lat * pi/180
  
  dia_Gh <- gh_previo
  

  
  # Radiación extraterrestre Gh----------------
  
  #Para el mes, dia actual, extraemos la declinacion_solar, excentricidad_diaria, w_sr (es decir dia y mes de hoy ya que la prediccion es para un dia a vista)
  
  gh_datos <- df_solar_energy %>%
    filter(mes == actualMonth, dia == actualDay) %>%
    select(declinacion_solar, excentricidad_diaria, w_sr)
  
  # Radiación extraterrestre diaria Gd0----------------
  
  gd0 <- gh_diaria(E0 = gh_datos$excentricidad_diaria,
                  ws = gh_datos$w_sr,
                  decl = gh_datos$declinacion_solar,
                  lat = lat)
  
  k_d_previo <- gd_previo/ gd0 #indice de transparencia (Kd) calculado
  
  # Componemos la observación con la que trabajará el sistema predictor
  
  observacion <- data.frame(gd_previo = gd_previo,
                            kd_previo = k_d_previo,
                            t9_12_diaprevio = t_9_12_diaprevio,
                            t13_15_diaprevio = t_13_15_diaprevio,
                            h8_diaprevio,
                            h14_diaprevio,
                            t_dia_diaprevio = t_dia_previo,
                            preciptac_dia_diaprevio = precipitac_dia_previo,
                            Predicc_temp_day_C = Predicc_temp_day_C,
                            Predicc_Relative_humidity_day_0_1 = Predicc_Relative_humidity_day_0_1,
                            Predicc_cloudy_sky_day_0_1 =  Predicc_cloudy_sky_day_0_1,
                            Predicc_temp_10_11_12_C = Predicc_temp_10_11_12_C,
                            Predicc_cloudy_13_14_15 = Predicc_cloudy_13_14_15,
                            Predicc_R_hum_10_11_12 = Predicc_R_hum_10_11_12,
                            Predicc_R_hum_13_14_15 = Predicc_R_hum_13_14_15,
                            Predicc_temp_13_14_15_C = Predicc_temp_13_14_15_C,
                            Predicc_cloudy_10_11_12 = Predicc_cloudy_10_11_12)
  
  # Almacenamos el dataframe observationa en el servidor
  
  nombre_csv <- paste0("observations",".csv")
  write.csv(observacion, file = paste ( "aemet/",nombre_csv), row.names = FALSE)
  
  # Use system for prediction
  uri <- "SystemForPredictions/10_PREDICTION_SYSTEM.R"
  
  # Se generará un CSV con las predicciones hechas por el modelo
  source(uri)
  
  #Abrimos elcsv generado por el sistema predictor para tomar los valores de transparencia horaria predicha para hoy
  
  obs_predichas <- read_csv(file = paste ( "aemet/","observations+kh_prediction.csv"))
  


  dia_Kh <- obs_predichas%>%
    select(kh9,kh10,kh11,kh12,kh13,kh14,kh15,kh16)
  
  kh5 <- dia_Kh$kh9
  kh6 <- dia_Kh$kh9
  kh7 <- dia_Kh$kh9
  kh8 <- dia_Kh$kh9
  
  kh17<- dia_Kh$kh16
  kh18<- dia_Kh$kh16
  kh19<- dia_Kh$kh16
  kh20<- dia_Kh$kh16
 

  kh_array <- cbind(kh8,dia_Kh)
  kh_array <- cbind(kh7,kh_array)
  kh_array <- cbind(kh6,kh_array)
  kh_array <- cbind(kh5,kh_array)
  
  kh_array <- cbind(kh_array,kh17)
  kh_array <- cbind(kh_array,kh18)
  kh_array <- cbind(kh_array,kh19)
  kh_array <- cbind(kh_array,kh20)

  
  
#Obtener dataframe de radiación para el día actual 
  
    df_radiacion_horaria <- calculo_radiacion_horaria(as.data.frame(dia_Gh) ,
                                                      kh_array, 
                                                      dia_Acimut_h, 
                                                      dia_Altura_h, 
                                                      dia_Angulo_h,
                                                      dia_declinacion,
                                                      lat,
                                                      inclinacion,
                                                      orientacion)
    


  ###########preparar DF de radiación horaria necesario para la estimación de la energía a corto plazo ##############
  
  #Componer el df para el cálculo de la energía a 1 día vista
  
  #prod_fv <- data.frame(Predicc_temperatura_horaria,
                        #Predicc_vviento_horario, df_radiacion_horaria  )
    
    #Calcular la energía
    
    potencia_dia_h <- potencia_generada(t_ambiente = Predicc_temperatura_horaria, 
                                        irr_solar = df_radiacion_horaria$r_global_h, 
                                        v_viento = Predicc_vviento_horario)
    
    potencia_silicio_dia_h <- potencia_paneles_silicio(area = area, 
                                                       potencia = potencia_dia_h)


    as.data.frame(potencia_silicio_dia_h)

}
  
#################################################################################################
