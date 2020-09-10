

###################################### SCRIPT DE DESCARGA AUTOMÁTICA DE FICHEROS NECESARIOS DEL AEMET ##############################


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
library(cronR)

############################################################# API parameters ########################################################################

api_key <-"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYXJ0YWZlY3VAZ21haWwuY29tIiwianRpIjoiYzlmMWRkMmUtNjE2YS00ZGNmLWE5YzItNzc1ODg3YzlkZTdmIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE1ODUzMDUzNDUsInVzZXJJZCI6ImM5ZjFkZDJlLTYxNmEtNGRjZi1hOWMyLTc3NTg4N2M5ZGU3ZiIsInJvbGUiOiIifQ.p2Ka71le5XedoJE8iIYIKVh-wyOvAOYEszH9OcwoQxc"

url_base <- "https://opendata.aemet.es/opendata/api"

prediccion_horaria_url <- "prediccion/especifica/municipio/horaria"

################################################################# Función GENÉRICA de consulta a la api #############################################

##  url_base: url base para todas las urls de descarga.
##  url: Fragmento de la url para la descarga.
##  api_key: Clave necesaria para la descarga de los datos (se crea en [AEMET](https://opendata.aemet.es/centrodedescargas/altaUsuario?))
##  id: parámetros para realizar la consulta (puede no ser necesario)

#####################################################################################################################################################

get_response <- function(url_base, url = "", api_key, id = ""){
  
  call <- paste(url_base, url, id, sep = "/")
  call <- param_set(call, key = "api_key", value = api_key)
  
  response <- GET(call)
  
  # Si obtenemos un json, en el caso de AEMET, significa que en el json devuelto está la siguiente url que debemos consultar para obtener los datos
  
  if(http_type(response) == "application/json"){
    
    resp_text <- content(response, "text")
    body <- fromJSON(resp_text, flatten = TRUE)
    response <- get_response(body$datos, api_key = api_key)
    
  }
  
  # Comprobar si la petición es errónea y en caso afirmativo devolver un mensaje 
  
  if(http_error(response)){
    
    resp_text <- content(response, "text")
    body <- fromJSON(resp_text, flatten = TRUE)
    
    sprintf(
      "GitHub API request failed [%s]\n%s\n", 
      status_code(response),
      body$message
    )
  }
  
  response
}



#################### LECTURA DE LOS CSV NECESARIOS ######################

#Importante que ya existan en el servidor dichos ficheros que solo son necesarios descargar una vez


municipios <- as.data.frame(read_csv("municipios.csv"))
estacionesMeteorologicas <- as.data.frame(read_csv("estaciones_meteorologicas.csv"))


############### Función para obtener los datos de las estaciones meteorológicas de obs. convencionales de un municipio ####################

## prov provincia

# Se llamará a esta función para cada provincia con la que se trabaje (MALAGA, SEVILLA, ...)

obtenerMunicipios<- function (cap = "Málaga") {
  municipios %>% 
    filter(capital==cap) 
  
}


#################################################### SCRIPT ###############################################

#Para cada municipio de interés, descargamos las predicciones horarias

municipiosDeInteres <- obtenerMunicipios("Málaga")

# munSevilla <- obtenerMunicipios("Sevilla")

# municipiosDeInteres <- rbind (municipiosDeInteres,munSevilla)

descargarPrediccionesHorarias ()

### Se descargará a las 10:00 cada día un csv con las predicciones horarias
### Se programa con R-CRON cron_rstudioaddin() ############################

descargarPrediccionesHorarias <- function() {
  
  for (idmun in municipiosDeInteres$id) {
    prediccion_horaria (url_base, prediccion_horaria_url ,api_key, idmun )
  }

  
}

################################################## Función que realiza una consulta de predicciones horarias de un municipio al AEMET #####################################

## idmun id del municipio del que se quiere realizar la consulta
## apiKey globar var with api key
## base url base global var
## prediccion_horaria_url url de la API para las predicciones horarias de un municipio

## Debería automatizarse a una hora de cada día (una vez para cada municipio)

prediccion_horaria <- function(base, prediccion_horaria_url, api_key, idmun) {
  
  get_pred <- get_response(base, prediccion_horaria_url, api_key, id = idmun)
  pred_text <- content(get_pred, "text")
  
  prediccion_horaria_df <- fromJSON(pred_text, flatten = TRUE)
  
  # devuelve una lista con  dos dataframe (uno con valores horarios y otro con predicciones de probabilidades de lluvia, nieve, ... en intervalos horarios)
  prediccion <- desanidamiento(prediccion_horaria_df)
  
  fecha <- as.character(Sys.Date())
  hora <- paste0(format(Sys.time(), "%H"),"h")
  
  # Primer df
  nombre_csv1 <- paste("prediccion_horaria","id", idmun, fecha, hora, sep = "_")
  nombre_csv1 <- paste0(nombre_csv1,".csv")
  
  write.csv(prediccion$columnas_horas, file = paste ( "aemet/",nombre_csv1), row.names = FALSE)
  
  # Segundo df
  nombre_csv2 <- paste("prediccion_intervalo","id", idmun, fecha, hora, sep = "_")
  nombre_csv2 <- paste0(nombre_csv2,".csv")
  
  write.csv(prediccion$columnas_intervalo, file = paste ( "aemet/",nombre_csv2), row.names = FALSE)
  
  #obs_convencional_df
  
  prediccion
  
}



######## Función que devuelve  una lista con dos dataframe con el resultado de la consulta de predicciones horaria de un municipio ################

desanidamiento <- function(prediccion_horaria_df) {
  
  prediccion_dia <- prediccion_horaria_df$prediccion.dia[[1]]
  
  prediccion_horaria_df <- prediccion_horaria_df %>% unnest(prediccion.dia)
  
  col_horarias <- c("estadoCielo", "precipitacion", "nieve", "temperatura", "sensTermica", "humedadRelativa", "vientoAndRachaMax")
  col_prob <- c("probPrecipitacion", "probTormenta", "probNieve","vientoAndRachaMax")
  
  prediccion_cols_horarias <- prediccion_horaria_df %>% select(!col_prob)
  prediccion_cols_prob <- prediccion_horaria_df %>% select(!col_horarias)
  
  
  prediccion_cols_horarias <- prediccion_cols_horarias %>% 
    
    unnest(estadoCielo, 
           precipitacion, 
           nieve, 
           temperatura, 
           sensTermica, 
           humedadRelativa, 
           names_sep = ".")
  
  prediccion_cols_prob <- prediccion_cols_prob %>% 
    
    unnest(probPrecipitacion, 
           probNieve, 
           probTormenta, 
           names_sep = ".")
  
  df_viento <- rbind(prediccion_dia$vientoAndRachaMax[[1]], 
                     prediccion_dia$vientoAndRachaMax[[2]],
                     prediccion_dia$vientoAndRachaMax[[3]])
  
  filas_viento <- seq(2, 96, by = 2)
  value <- df_viento$value[filas_viento]
  
  filas <- seq(1,96, by = 2)
  
  df_viento <- cbind(df_viento[filas, 1:3], value)
  
  colnames(df_viento) <- paste("viento", colnames(df_viento), sep = ".")
  df_viento$viento.velocidad <- unlist(df_viento$viento.velocidad)
  df_viento$viento.direccion <- unlist(df_viento$viento.direccion)
  
  prediccion_cols_horarias <- cbind(prediccion_cols_horarias, df_viento)
  
  list ( columnas_intervalo = prediccion_cols_prob, 
         columnas_horas     = prediccion_cols_horarias)
}

