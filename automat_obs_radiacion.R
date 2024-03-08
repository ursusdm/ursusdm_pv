

###################################### SCRIPT DE DESCARGA AUTOMÁTICA DE DATOS DE RADIACIÓN ##############################
###################################### CADA dÍA A LAS 10h

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

observaciones_radiacion_url <- "red/especial/radiacion"

#################### LECTURA DE LOS CSV NECESARIOS ######################

#estacionesRadiacion <- as.data.frame(read_csv("/srv/shiny-server/ursus/ursusdm_pv/scriptAEMET/estaciones_radiacion.csv",locale=locale(encoding="latin1")))

#estacionesRadiacion <- as.data.frame(read_csv("estaciones_radiacion.csv",locale=locale(encoding="latin1")))
estacionesRadiacion <- as.data.frame(read_csv("/srv/shiny-server/ursus/ursusdm_pv/estaciones_radiacion.csv",locale=locale(encoding="latin1")))

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


obtenerEstaciones<- function (prov = "MALAGA") {
  estacionesRadiacion %>% 
    filter(provincia==prov) 
  
}

estaciones <- obtenerEstaciones("MALAGA")

#Para cada provincia con la que trabaje nuestro sistema, obtenemos las estaciones meteorológicas de observaciones convencionales

#estMadrid <- obtenerEstaciones("MADRID")

#estaciones <- rbind (estaciones,estMadrid)

#################################################### SCRIPT ###############################################


### Se descargará a las 10:00 cada día un csv con las observaciones de radiación del día anterior de cada estación. 
### Se programa con R-CRON cron_rstudioaddin() ############################

descargarObservacionesRadiacion <- function() {
  for (idEst in estaciones$indicativo) {
    radiacion (url_base, observaciones_radiacion_url ,api_key )
  }
}

############################### Descarga de datos de radiación solar del día anterior de todas las estaciones ############################ 


radiacion <- function(base, radiacion, api_key) {
  
  get_radiacion <- get_response(base, radiacion, api_key)
  
  radiacion_text <- content(get_radiacion, "text")
  
  datos_rad <- substring(radiacion_text, 32)
  
  csv_rad <- read_delim(datos_rad, delim =  ";")
  
  malaga <- which(csv_rad$Indicativo == "6156")
  
  csv_rad$Indicativo[malaga] <- paste0(csv_rad$Indicativo[malaga], "X")
  
  fecha <- as.character(Sys.Date()-1)
 
  nombre_csv <- paste("radiacion_solar", fecha, sep = "_")
  nombre_csv <- paste0(nombre_csv,".csv")
  names(csv_rad) <- gsub("\\..*", "", names(csv_rad))
  write.csv(csv_rad, file = paste ("/srv/shiny-server/ursus/ursusdm_pv/aemet/",nombre_csv), row.names = FALSE)
  #write.csv(csv_rad, file = paste ("/Users/franciscorodriguezgomez/Documents/Developer/R/URSUS_PV/ursusdm_pv/aemet/",nombre_csv), row.names = FALSE)
  
  csv_rad
  
}

descargarObservacionesRadiacion ()

