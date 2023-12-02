
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

#setwd("~/Programas/estima_FV_media")
source("funciones_prod_fv.R")


############### Obtiene dataframes con las estaciones y municipios a partir de los CSVS del AEMET prev. descargados ###############################

estaciones <- read_csv("estaciones_meteorologicas.csv",locale=locale(encoding="latin1"))
municipios <- read_csv("municipios.csv",locale=locale(encoding="latin1"))
estaciones_rad <- read_csv("estaciones_radiacion.csv",locale=locale(encoding="latin1"))
#estaciones_rad <- read.csv ("estaciones_radiacion.csv",sep=";",encoding="UTF-8")
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

radiacion_extraterrestre_diaria <- function(excentricidad, angulo_salida_sol, declinacion, latitud){
  
  k_solar <- 1367 #W/m^2
  
  gd <- 24/pi * k_solar * excentricidad * (angulo_salida_sol* sin(declinacion) * sin(latitud) + cos(declinacion) * cos(latitud) * sin(angulo_salida_sol))
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
  obs_conv_hora1_df <-  read_csv(file = paste ( "aemet/",obs_conv_hora1,sep = " "),locale=locale(encoding="latin1"))
  obs_conv_hora2 <- paste("observacion_convencional","id", id_estacion, fecha, hora2, sep = "_")
  obs_conv_hora2 <- paste0(obs_conv_hora2,".csv")
  
  # Se abre el csv de observaciones convencionales de la estación más cercana al CSV de la 2º hora programada de descarga
  obs_conv_hora2_df <- read_csv(file = paste ("aemet/",obs_conv_hora2,sep=" "),locale=locale(encoding="latin1"))
  
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
  
  #rad_df <- read_csv(file = paste ("aemet/",rad,sep=" "))

  #names(rad_df) <- gsub("\\..*", "", names(rad_df))
  
  rad_df <- read.csv(file = paste ("aemet/",rad,sep=" "))
  #names(rad_df) <- gsub("\\..*", "", names(rad_df))
  #names(rad_df) <- gsub("X", "", names(rad_df))

  rad_df <- rad_df %>%
    filter(Indicativo == id_estacion)
  
  names(rad_df) <- gsub("\\..*", "", names(rad_df))
  names(rad_df) <- gsub("X", "", names(rad_df))
  

  
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
  
  pred_horas_df <- read_csv(file = paste ( "aemet/",pred_horas,sep=" "),locale=locale(encoding="latin1"))
  pred_horas_df <- data.frame(pred_horas_df)
  #view (pred_horas_df)
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


prepareCSVS <- function (coords) {
  
  lat <- coords[1]
  lon <- coords[2]
  
  #Obtener municipio y estación convencional y de radiación más cercana
  
  municipios2 <<- municipios %>%                
    mutate(distancia = distancia(latitud_dec, longitud_dec, lat, lon)) %>%
    filter(distancia == min(distancia))
  
  #print ("municipio:")
  #print (municipios2)
  
  estaciones2 <<- estaciones %>% 
    mutate(distancia = distancia(latitud_dec, longitud_dec, lat, lon)) %>%
    filter(distancia == min(distancia))
  
  #print ("estaciones convencional cercana:")
  #print (estaciones2)
  
  estaciones_rad2 <<- estaciones_rad %>%  
    mutate(distancia = distancia(latitud_dec, longitud_dec, lat, lon)) %>% 
    filter(distancia == min(distancia))
  
  hoy <-  as.character(Sys.Date())
  ayer <-  as.character(Sys.Date()-1)
  
  ## Obtenemos un dataframe los datos del csv de observaciones diarias (fusionamos el que se descarga a las 10:00 con el que se descarga a las 00:00)
  
  # Tomamos la hora actul
  hora <- hour(Sys.time())
  
  if (hora>=10) {
    df_observacion <<- observacion_conv_df(id_estacion = estaciones2$indicativo, fecha = hoy, hora1 = "00h", hora2 = "10h")
  } 
  else {
    df_observacion <<- observacion_conv_df(id_estacion = estaciones2$indicativo, fecha = ayer, hora1 = "00h", hora2 = "10h")
  }
  
  #predicciones
  if (hora>=10) {
    
    df_prediccion_horaria <<- prediccion_horas_df(id_municipio = municipios2$id, hoy, "10h")
    
  } 
  
  else {
    
    df_prediccion_horaria <<- prediccion_horas_df(id_municipio = municipios2$id, ayer, "10h")
  }
  
  # Obtenemos de la estación de radiación más cercana el dataframe del csv de datos de radiación, y a partir del mismo, obtenemos los valores necesarios
  
  ayer <- as.character(as.Date(hoy) - 1)
  antesDeAyer <- as.character(as.Date(hoy) - 2)
  
  if (hora>=10) {  	
    df_r_global <<- rglobal_df(estaciones_rad2$indicativo, ayer)
  }
  else {
    df_r_global <<- rglobal_df(estaciones_rad2$indicativo, antesDeAyer)
  } 
  
 
  
}

########## Función que calcula la energía a corto plazo para un tejado ###############################

# input lat,long,orient,incl,area parametros de cada tejado que se esté procesando

calcularEnergiaDiaSiguiente <- function (latitud, longitud, orientacion, inclinacion, area,municipio) {
  
  t_dia_previo <- mean(df_observacion$ta)
  #print ("***************************************")
  #print (dim(df_observacion))
  
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
  
  df_prediccion_horaria$temperatura.periodo <- as.numeric(df_prediccion_horaria$temperatura.periodo)
  
  precipitac_dia_previo <- mean(df_observacion$prec)
  
  humedad_diaria <- mean(df_observacion$hr) / 100
 
  ##  predicciones para el municipio más cercano al tejado y obtenemos los valores necesarios a partir del dataframe

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
  #dia_juliano actual
  dias_previos <- c(0,31,59,90,120,151,181,212,243,273,304,334)
  dia_juliano<-actualDay+dias_previos[actualMonth]
  #Solo interesa calcular la energía del día siguiente, eliminada la llamada a df_energia_solar que calculaba para todo el año 

  inclinacion <- inclinacion * pi/180
  orientacion <- orientacion * pi/180
  latitud <- latitud * pi/180
  dia_Gh <- gh_previo
  #print ("dia_Gh")
  #print (dim(dia_Gh))
  #view (dia_Gh)
  
 
  # Radiación extraterrestre diaria Gd0----------------
  #Cálculos posición relativa sol-tierra diaria y rad extraterrestre diaria
  
  declinacion<-angulo_declinacion_solar(dia_juliano)
  excentricidad<-excentricidad(dia_juliano)
  angulo_salida_sol<-angulo_salida_sol(latitud,declinacion)
 
  gd0_previo <- radiacion_extraterrestre_diaria(excentricidad,angulo_salida_sol,declinacion,latitud)
  
  k_d_previo <- gd_previo/ gd0_previo #indice de transparencia (Kd) calculado
  
  # Componemos la observación con la que trabajará el sistema predictor
  
  observacionAPredecir <<- data.frame(gd_previo = gd_previo,
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
  
  #print ("*******DATOS MODELO JOSE **********")
  #view (observacionAPredecir)
  # Almacenamos el dataframe observationa en el servidor
  
  #nombre_csv <- paste0("observations",".csv")
  #write.csv(observacionAPredecir, file = paste ( "aemet/",nombre_csv), row.names = FALSE)
  
  # Use system for prediction
  uri <- "SystemForPredictions/10_PREDICTION_SYSTEM.R"
  
  # Se generará un CSV con las predicciones hechas por el modelo
  source(uri)
  
  #view (observations_with_kh_prediction)
  #Obtenemos la predicción del modelo
  dia_Kh <- observations_with_kh_prediction%>%
    select(kh9,kh10,kh11,kh12,kh13,kh14,kh15,kh16)
  
  kh9 <- dia_Kh$kh9
  kh10 <- dia_Kh$kh10
  kh11 <- dia_Kh$kh11
  kh12 <- dia_Kh$kh12
  
  kh13<- dia_Kh$kh13
  kh14<- dia_Kh$kh14
  kh15<- dia_Kh$kh15
  kh16<- dia_Kh$kh16
 
  #kh_array <- cbind(0,0,0,0,kh9,kh10,kh11,kh12,kh13,kh14,kh15,kh16,0,0,0,0)
  #kh_array <- data.frame(kh_array)
  ## se construye un vector unidimensional
  #kh_array<- c(0,0,0,0,kh9,kh10,kh11,kh12,kh13,kh14,kh15,kh16,0,0,0,0)
  #kh_array<-c(kh_array)
  #print("BLAaaaaa")
  #print(dim(kh_array))
  #view (kh_array)
  #para pruebas, 
  kh_array<- c(0,0,0,0,0.56,0.6,0.7,0.7,0.7,0.65,0.6,0.66,0,0,0,0)
  view (kh_array)
  
  #Cálculos de coordenadas solares y rad extraterrestre para dia actual
  
  horas_mediodia<-horas_mediodia(angulo_salida_sol)
  duracion_dia<-horas_mediodia*2
  h_ini<-hora_inicial(horas_mediodia)
  h_fin<-hora_final(horas_mediodia)
  segundos<-segundos(horas_mediodia)
  
  # 
  print ("Temperaturas")
  print (Predicc_temperatura_horaria)
  view (Predicc_temperatura_horaria)
  
  energia_h <- c()
#Obtener dataframe de radiación para el día actual 
 for(h in 1:16){
   hh=h+4
   angulo_s<-angulo_solar(h=hh,h_ini=h_ini,h_fin=h_fin,segundos,angulo_salida_sol)
   altura_s<-altura_solar(hh,h_ini,h_fin,latitud,declinacion,angulo_s)
   acimut_s<-acimut_solar(hh,h_ini,h_fin,altura_s,latitud,declinacion,angulo_s)
   angulo_puesta_sol <- angulo_salida_sol - ((segundos/3600)/2 * pi/12)
   altura_solar_salida_puesta_sol <- asin(sin(declinacion) * sin (latitud) + cos(declinacion) * cos(latitud) *
                                            cos(angulo_puesta_sol))
   r_extraterrestre_h <- radiacion_extraterrestre_horaria(altura_s,excentricidad,segundos,altura_solar_salida_puesta_sol)
   ## esto sig comentado estaba mal, hay que usar la del día que se ha estimado, no la del día anterior
   #r_global_h<-dia_Gh[h]
   r_global_h <- kh_array[h]*r_extraterrestre_h
   kh<-kh_array[h]
   r_global_i <- calculo_radiacion_horaria_inclinada (r_global_h,kh,acimut_s,altura_s,angulo_s,angulo_salida_sol,
                                                      declinacion, latitud, inclinacion, orientacion)
  
   
   #Calcular la energía
  energia_producida <- energia_generada(t_ambiente = Predicc_temperatura_horaria, 
                                        r_global_i = r_global_i, 
                                        v_viento = Predicc_vviento_horario)
  energia_producida_silicio <- energia_paneles_silicio(area = area, energia = energia_producida)
  energia_h <- c(energia_h,energia_producida_silicio)
 
  
 } 
  view(energia_h)
  as.data.frame(energia_h)
}
  
#################################################################################################
