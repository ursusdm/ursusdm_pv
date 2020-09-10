

################## Utilizar solo una vez para descargar los csvs de estaciones meteorológicas y de municipios del AEMET ************************
####3 El resto de scripts de descarga automática trabajarán con los csvs que se descargan con este script


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

################################################### Función que convierte Lat y long en formato (º, Min, Seg) a decimal###############################

latitud_longitud_decimal <- function(grados, minutos, segundos){
  
  lat_lon <- grados + (1/60)*minutos + (1/3600)*segundos
  
}


######################################################## Radiación extraterrestre diaria ##############################################################################


##### El siguiente script sólo será necesario ejecutarlo una vez. Una vez se tengan los csv en el servidor no es necesario ejecutarlo más ######

################################################## Consulta municipios para los que el AEMET elabora predicciones y genera un csv en "municipios.csv" ################################


municipios_url <- "maestro/municipios"

get_municipios <- get_response(url_base, municipios_url, api_key)

municipios_text <- content(get_municipios, "text")

datos_municipios <- fromJSON(municipios_text, flatten = TRUE)

datos_municipios$id <- substr(datos_municipios$id, 3, 7) # Las consultas que requieren el id, requieren de la eliminación de la palabra "id"

write.csv(datos_municipios, "municipios.csv")


###### SCRIPT que obtiene las estaciones meteorológicas disponibles y genera un csv en "estaciones_meteorologicas.csv" #################

estaciones_url <- "valores/climatologicos/inventarioestaciones/todasestaciones"

get_estaciones <- get_response(url_base, estaciones_url, api_key = api_key)

estaciones_text <- content(get_estaciones, "text")

datos_estaciones_df <- fromJSON(estaciones_text, flatten = TRUE)

datos_estaciones_df <- datos_estaciones_df %>% 
  mutate(grados_lat = as.numeric(substr(latitud, 1, 2)),
         minutos_lat = as.numeric(substr(latitud, 3, 4)),
         segundos_lat = as.numeric(substr(latitud, 5, 6)),
         orient_lat = substr(latitud,7,7),
         latitud_dec = latitud_longitud_decimal(grados_lat, 
                                                minutos_lat, 
                                                segundos_lat),
         
         grados_lon =  as.numeric(substr(longitud, 1, 2)),
         minutos_lon = as.numeric(substr(longitud, 3, 4)),
         segundos_lon = as.numeric(substr(longitud, 5, 6)),
         orient_lon = substr(longitud,7,7),
         longitud_dec = latitud_longitud_decimal(grados_lon, 
                                                 minutos_lon, 
                                                 segundos_lon))

estaciones_lat_W <- which(datos_estaciones_df$orient_lon == "W")
estaciones_lon_S <- which(datos_estaciones_df$orient_lon == "S")

datos_estaciones_df$longitud_dec[estaciones_lat_W] <- - datos_estaciones_df$longitud_dec[estaciones_lat_W] 
datos_estaciones_df$latitud_dec[estaciones_lon_S] <- - datos_estaciones_df$latitud_dec[estaciones_lon_S] 

# Corregir indicativo incorrecto
est <- which(datos_estaciones_df$indicativo == "4121C")

datos_estaciones_df$indicativo[est] <- "4121"

write.csv(datos_estaciones_df, "estaciones_meteorologicas.csv")


################# ESTACIONES RADIACIÓN SOLAR ##########################

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



