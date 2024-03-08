

###################################### SCRIPT DE DESCARGA AUTOMÁTICA DE LAS OBSERVACIONES CONVENCIONALES DE LAS ESTACIONES METEREOlOGICAS##############################

### Descargar cada día las 00 y a las 10 de forma automática #########

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

observaciones_convencionales_url <- "observacion/convencional/datos/estacion"

#estacionesMeteorologicas <- as.data.frame(read_csv("/srv/shiny-server/ursus/ursusdm_pv/scriptAEMET/estaciones_meteorologicas.csv"))

################################################################# Función GENÉRICA de consulta a la api #############################################
#LEER DATASET CON LAS ESTACIONES METEOR.
#estacionesMeteorologicas <- as.data.frame(read_csv("estaciones_meteorologicas.csv",locale=locale(encoding="latin1")))
estacionesMeteorologicas <- as.data.frame(read_csv("/srv/shiny-server/ursus/ursusdm_pv/estaciones_meteorologicas.csv",locale=locale(encoding="latin1")))

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


# Se llamará a esta función para cada provincia con la que se trabaje (MALAGA, SEVILLA, ...)

obtenerEstaciones<- function (prov = "MALAGA") {
  estacionesMeteorologicas %>% 
    filter(provincia==prov) 
  
}

estaciones <- obtenerEstaciones("MALAGA")


#Para cada provincia con la que trabaje nuestro sistema, obtenemos las estaciones meteorológicas de observaciones convencionales

#estSevilla <- obtenerEstaciones("SEVILLA")
#estMadrid <- obtenerEstaciones("MADRID")

#estaciones <- rbind (estaciones,estSevilla)
#estaciones <- rbind (estaciones,estMadrid)


########################## Se descargará a las 00:00 cada día las observaciones convencionales de cada estación. Se programa con R-CRON cron_rstudioaddin() ############################

descargarObservacionesConvencionales  <- function() {
  
  # Para cada una de las estaciones con las que trabaja nuestro sistema, realizamos una consulta de observaciones convencionales
  
  for (idEst in estaciones$indicativo) {
    observacion_convencional (url_base, observaciones_convencionales_url ,api_key, idEst )
  }
  
}

################# Descarga de observaciones convencionales del AEMET de una estación meteorológica############################################# 

## idema  indicativo de la estación que se quiere consultar
## observacion_convencional URL observación convencional

observacion_convencional <- function(base, observacion_convencional, api_key, idema) {
  
  get_observacion_convencional <- get_response(base, observacion_convencional, api_key, id = idema)
  observacion_text <- content(get_observacion_convencional, "text")
  
  obs_convencional_df <- fromJSON(observacion_text, flatten = TRUE)
  
  fecha <- as.character(Sys.Date())
  hora <- paste0(format(Sys.time(), "%H"),"h")
  
  nombre_csv <- paste("observacion_convencional","id", idema, fecha, hora, sep = "_")
  nombre_csv <- paste0(nombre_csv,".csv")
  write.csv(obs_convencional_df, file = paste ( "/srv/shiny-server/ursus/ursusdm_pv/aemet/",nombre_csv), row.names = FALSE)
  #write.csv(obs_convencional_df, file = paste ( "/Users/franciscorodriguezgomez/Documents/Developer/R/URSUS_PV/ursusdm_pv/aemet/",nombre_csv), row.names = FALSE)
  
  obs_convencional_df
  
}


descargarObservacionesConvencionales ()
