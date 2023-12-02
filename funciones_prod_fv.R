# Script con todas las funciones necesarias para la estimación de la producción fotovoltaica

library(tidyverse)
library(readr)

## Posición relativa sol-superficie horizontal
##Excentricidad
excentricidad <- function(dia){
  1 + 0.033 * cos((2*pi*dia)/365)
}

### Declinación de la tierra
gamma_declinacion <- function(dia){
  2.0 * pi *(dia-1)/365.0
}

angulo_declinacion_solar <- function(dia){
  gamma <- gamma_declinacion(dia)
  0.006918 - 0.399912 * cos(gamma) + 0.070257 * sin(gamma) - 0.006758 * cos(2*gamma) + 0.000907 * sin(2* gamma) - 0.002697* cos(3*gamma) + 0.001480 * sin(3*gamma)
  #(radianes)
}

### Ángulo de salida del sol $\omega_{sr}$
angulo_salida_sol <- function(latitud, declinacion){
  acos(- tan(latitud) * tan(declinacion))
}

### Horas hasta el mediodía solar
horas_mediodia <- function(angulo_salida_sol){
  angulo_salida_sol * 12 / pi
}

segundos <- function(horas_mediodia){
  (horas_mediodia - trunc(horas_mediodia)) * 3600
}

hora_inicial <- function(horas_mediodia){
  12 - trunc(horas_mediodia) 
}

hora_final <- function(horas_mediodia){
  12 + trunc(horas_mediodia)
}


### Ángulo solar horario
angulo_solar <- function(h, h_ini, h_fin, segundos, angulo_salida_sol) {
  case_when(
    h == (h_ini - 1)        ~ -(angulo_salida_sol - ((segundos/3600)/2 * pi/12)),
    h == h_fin              ~   angulo_salida_sol - ((segundos/3600)/2 * pi/12),
    h >= h_ini & h < h_fin  ~  ((h+1) - 12 - 0.5) * pi/12,
    TRUE                    ~  0
  )
}


### Altura solar horaria
altura_solar <- function(h, h_ini, h_fin, latitud, declinacion, angulo_s) {
  case_when(
    h < (h_ini-1) | h > h_fin ~ 0,
    TRUE ~
      asin(sin(declinacion) * sin (latitud) + cos(declinacion) * cos(latitud) *
             cos(angulo_s))
  )
}


#### Acimut solar horario
acimut_solar <- function(h, h_ini, h_fin, altura_s, latitud, declinacion, angulo_s){
  a<-case_when(
    latitud>=0   ~ acos((sin(altura_s)*sin(latitud)- sin(declinacion))/(cos(altura_s)*cos(latitud))),
    TRUE   ~  acos((sin(declinacion)*sin(latitud) - cos(declinacion)* sin(latitud)* cos(angulo_s))/cos(declinacion)) , 
  )
  acim<-case_when(
    h >= (h_ini - 1) & h < 12 ~ -a,
    h >= 12 & h <= h_fin      ~ a,
    TRUE ~ 0
  )  
  acim
}
### Rad extraterrestre
radiacion_extraterrestre_horaria <- function(altura_s, excentricidad, segundos,altura_solar_salida_sol) {
  K_solar <- 1367 #W/m^2
  r <- K_solar * excentricidad * sin(altura_s)
  case_when(
    altura_s>0 & altura_s<=altura_solar_salida_sol  ~ r * segundos / 3600,
    altura_s>altura_solar_salida_sol       ~ r,
    TRUE                         ~ 0
  )
}

#datos_ciudad<-cargaHistorico("MA")
cargaHistorico <- function(city){
  #setwd("Programas/estima_FV_media")
  url <- paste ('AEMETHistoricData/',city,'/',sep="")
  #url <- paste (city,'/',sep="")
  city_R <- read_table2(paste(url,'city_R.sal',sep=""), 
                        col_names = FALSE)
  city_R_colnames <- c("año", "mes", "dia")
  gh <- paste("Gh", as.character(5:20), sep = "_")
  city_R_colnames <- c(city_R_colnames, gh)
  colnames(city_R) <- city_R_colnames
  ## Temperatura
  city_T <- read_table2(paste(url,'city_T.sal',sep=""), 
                        col_names = FALSE)
  city_T_colnames <- c("año", "mes", "dia")
  th <- paste("t", as.character(1:24), sep = "_")
  city_T_colnames <- c(city_T_colnames, th)
  colnames(city_T) <- city_T_colnames
  city_T[, th] <- city_T[, th]/10 #Temperatura en uds decimales
  ## Unión
  cityData <- left_join(city_R, city_T, by = c("año", "mes", "dia"))
  cityData <- as.data.frame(cityData)
  #Eliminar dia 29, se han eliminado, los datos de entrada no tienen día 29 para años bisiestos en febrero
#  dias_29 <-which(cityData$dia == 29)
#  mes_2 <- which(cityData$mes == 2)
#  dia_29_2 <- dias_29[which(dias_29 %in% mes_2)]
#  cityData <- cityData[-dia_29_2, ]
  # Eliminar valores negativos
  cityData <- cityData %>% 
    filter(Gh_5 >= 0, Gh_6 >= 0, Gh_7 >= 0, Gh_8 >= 0, Gh_9 >= 0, Gh_10 >= 0, Gh_11 >= 0, Gh_12 >= 0, 
           Gh_13 >= 0, Gh_14 >= 0, Gh_15 >= 0, Gh_16 >= 0, Gh_17 >= 0, Gh_18 >= 0, Gh_19 >= 0, Gh_20 >= 0) %>% 
    filter(t_5 >= 0, t_6 >= 0, t_7 >= 0, t_8 >= 0, t_9 >= 0, t_10 >= 0, t_11 >= 0, t_12 >= 0, 
           t_13 >= 0, t_14 >= 0, t_15 >= 0, t_16 >= 0, t_17 >= 0, t_18 >= 0, t_19 >= 0, t_20 >= 0)
  #Como los valores de radiación de AEMET están en 10*KJ/M^2 se pasan a Wh 1kj  = 1/3.6 Wh
  cityData[,gh] <- cityData[,gh]*10/3.6
  cityData
}

#Estimación de las componentes de la rad sobre superficie horizontal e inclinada y/o orientada

##Radiación difusa
radiacion_difusa_horizontal <- function(kh, r_horizontal){
  case_when(
    kh >= 0 & kh <= 0.22    ~ r_horizontal * (1 - 0.09 * kh),
    0.22 < kh & kh <= 0.8   ~ r_horizontal * (0.9511 - 0.16 * kh + 4.388 * kh^2 - 16.638 * kh^3 + 12.336 * kh^4),
    TRUE                    ~ r_horizontal * 0.165
  )
}

#difusa inclinada
radiacion_difusa_inclinada <- function(r_difusa, inclinacion){
  r_difusa * (1 + cos(inclinacion)) / 2
}


### Radiación reflejada
radiacion_reflejada <- function(r_global_h, inclinacion = 0){
  albedo <- 0.2
  r_global_h * albedo * (1- cos(inclinacion))/2
}

## Radiación directa, 

### Cálculo de ángulos "visibles"
## angulo solar "visible" por la mañana
calculo_wsrm <- function(angulo_salida_sol,orientacion, inclinacion, latitud, declinacion,signo){
 # if(orientacion<0){ signo=-1}
  #else {signo=1}
  A <- sin(latitud)/tan(orientacion) + cos(latitud)/(sin(orientacion) * tan(inclinacion))
  B <- -tan(declinacion)*(sin(latitud)/(sin(orientacion)*tan(inclinacion)) - cos(latitud)/tan(orientacion))
  wsrm <- acos((A*B+signo*sqrt(A*A-B*B+1))/(A*A+1))
   # wsrm <- -min(wsrm_v,angulo_sol) ####CAMBIADO aquí he añadido el signo -, la función min no sirve para mapply
  case_when(
    wsrm<angulo_salida_sol    ~ -wsrm,
    TRUE            ~  -angulo_salida_sol
  )
}

### Ángulo solar visible por la tarde
calculo_wsrt <- function(angulo_salida_sol,orientacion, inclinacion, latitud, declinacion,signo){
  A <- sin(latitud)/tan(orientacion) + cos(latitud)/(sin(orientacion) * tan(inclinacion))
  B <- -tan(declinacion)*( sin(latitud)/(sin(orientacion)*tan(inclinacion)) - cos(latitud)/tan(orientacion))
  wsrt <- acos((A*B-signo*sqrt(A*A-B*B+1))/(A*A+1))
  case_when(
    wsrt<angulo_salida_sol    ~ wsrt,
    TRUE            ~ angulo_salida_sol
  )
}


# beta = inclinacion radiacion_directa
##angulo solar para superficie inclinada y/o orientada
angulo_solar_inclinado <- function(angulo_s,angulo_salida_sol,acimut_s,declinacion,latitud,orientacion,inclinacion,signo){
  
  wsrt <- calculo_wsrt(angulo_salida_sol,orientacion, inclinacion, latitud, declinacion,signo)
  wsrm <- calculo_wsrm(angulo_salida_sol,orientacion, inclinacion, latitud, declinacion,signo)
  
  angulo_inclinado <- case_when(
    orientacion < 0 & acimut_s > 0  & angulo_s > wsrt ~ wsrt,
    orientacion > 0 & acimut_s <0 & angulo_s < wsrm ~ wsrm,
    TRUE ~ angulo_s
  )
  angulo_inclinado
}

radiacion_directa_inclinada <- function(r_directa_h, declinacion,
                              angulo_solar_i, latitud,
                              orientacion, inclinacion){

  costhita <- sin(declinacion) * sin(latitud) * cos(inclinacion) - sin(declinacion) *
    cos(latitud) * sin(inclinacion) * cos(orientacion) + cos(declinacion) *
    cos(latitud) * cos(inclinacion) * cos(angulo_solar_i) + cos(declinacion) *
    sin(latitud) * sin(inclinacion) * cos(orientacion) * cos(angulo_solar_i) +
    cos(declinacion) * sin(inclinacion) * sin(orientacion) * sin(angulo_solar_i)
  
  costhita <- case_when(
    costhita < 0    ~ 0,
    TRUE  ~ costhita
  )
  costhitaz <- sin(declinacion) * sin(latitud) + cos(declinacion) * cos(latitud) * cos(angulo_solar_i)
  rb<-case_when(
    costhitaz > 0 && !near(costhitaz, 0)  ~ costhita/costhitaz,
    TRUE  ~ 1
  )
  r_directa_i <- r_directa_h * rb
}

## Componentes de la radiación a partir de la radiación global

radiacion_global_inclinada <- function(r_directa_i, r_difusa_i, r_reflejada_i){
  r_global_i <- r_directa_i + r_difusa_i + r_reflejada_i
  r_global_i
}



radiacion_directa_inclinada_dia <- function(r_global_h, r_difusa_h, inclinacion = 0, orientacion = 0, acimut_s, altura_s, declinacion, latitud, angulo_s,angulo_salida_sol){
  
  r_directa_h <- r_global_h - r_difusa_h
  if (inclinacion != 0){
    if(orientacion<0) {signo<- -1} else {signo<- 1}

    angulo_solar_i <- angulo_solar_inclinado(angulo_s,angulo_salida_sol,acimut_s,declinacion,
                                     latitud,orientacion,inclinacion,signo)
    
    r_directa_i <- radiacion_directa_inclinada(r_directa_h, declinacion,angulo_solar_i,
                                                       latitud,orientacion,inclinacion)
  } else {
    r_directa_i <- r_directa_h
  }
  
  r_directa_i
}

#índice de transparencia
indice_transparencia_horario <- function(r_global_h,r_extraterrestre){
case_when(
  r_extraterrestre==0       ~ 0,
  r_extraterrestre!=0       ~ r_global_h/r_extraterrestre,
  TRUE            ~  0
)
}

## Estimación de la producción de energía fotovoltaica
### Obtención de la energía generada por los paneles fotovoltaicos
energia_generada <- function(t_ambiente, r_global_i,v_viento=0){
   g_ref <- 1000 #Wh/m^2
  coef_y <- -0.0048
  t_ref <- 25
  P_ref <- 1000
  coefCC <- 0.97 * 0.98 *0.98 * 0.98
  efinversor <- 0.95
  perdCA <- 0.99
  m <- -3.56
  n <- -0.079
  T_m <- t_ambiente + r_global_i * exp(m + n * v_viento)
  P_ref * r_global_i/g_ref * (1+ coef_y *(T_m - t_ref))* coefCC * efinversor * perdCA
}

energia_paneles_silicio <- function(area, energia){
  energia <- energia * area/6
}


### Energía generada media diaria para el área, en Wh
energia_generada_media<-function(latitud, longitud, inclinacion, orientacion, area, datos){
# Cálculos comunes para todos los años y todos los emplazamientos de un área  
  ####quitar luego
 # latitud <- 37
#  longitud <- -4
#  inclinacion <- 30
#  orientacion <- -40
#  area <- 6
#  day <- 55
  #Y COMENTAR LECTURA DE DATOS
  
  ###hasta aquí quitar
  
  latitud <- latitud * pi/180
  longitud <- longitud * pi/180
  lat<-as.data.frame(matrix(latitud,nrow=365,ncol=24))
  day <- as.data.frame(matrix(1:365, nrow = 365, ncol=24))
  h <- matrix(0:23, nrow = 24, ncol=365)
  h <- as.data.frame(t(h))
  # Ángulos solares y rad solar extraterrestre
  declina<-angulo_declinacion_solar(day)
  excentricity<-excentricidad(day)
  w_sr<-angulo_salida_sol(lat,declina)
  horas_mediod<-horas_mediodia(w_sr)
  duracion_dia<-horas_mediod*2
  h_ini<-hora_inicial(horas_mediod)
  h_fin<-hora_final(horas_mediod)
  seg<-segundos(horas_mediod)
  angulo<-as.data.frame(mapply(angulo_solar,h=h,h_ini=h_ini,h_fin=h_fin,seg,w_sr))
  altura<-as.data.frame(mapply(altura_solar,h,h_ini,h_fin,lat,declina,angulo))
  acimut<-as.data.frame(mapply(acimut_solar,h,h_ini,h_fin,altura,lat,declina,angulo))
  angulo_puesta_sol <- w_sr - ((seg/3600)/2 * pi/12)
  altura_solar_salida_puesta_sol <- asin(sin(declina) * sin (lat) + cos(declina) * cos(lat) *
                                           cos(angulo_puesta_sol))
  rad_i_e <- as.data.frame(mapply(radiacion_extraterrestre_horaria,altura,excentricity,seg,altura_solar_salida_puesta_sol))
  
  # Lectura de datos, rutina de Marta 
  # datos<-cargaHistorico("MA")
  # Los datos de rad tienen solo 16 columnas, si en datos tienen 24, cambiar esto, se añaden 4 por delante y 4 al final  
  columnas_cero<- matrix(0, nrow = 365, ncol=4)
  
  # Inclinación y orientación de la superficie
  inclinacion <- inclinacion * pi/180
  orientacion <- orientacion * pi/180
  inclina<-as.data.frame(matrix(inclinacion,nrow=365,ncol=24))
  orienta<-as.data.frame(matrix(orientacion,nrow=365,ncol=24))
  if(orientacion<0) {sig_orientacion<- -1} else {sig_orientacion<- 1}
  signo <- as.data.frame(matrix(sig_orientacion,nrow=365,ncol=24))
  
    # para cada año se calcula la energía diaria producida y luego la media de todos los años
  media_diaria <- 0 
  years <- nrow(datos)/365
  
  for(y in 1:years){ 
    ini<-(y-1)*365+1
    fin<-y*365
    ghi<-datos[ini:fin,4:19] ##esto depende de cómo estén los datos
    tem<-datos[ini:fin,20:43] ###idem 
    ghi<-cbind(columnas_cero,ghi,columnas_cero)  ##idem
    ghi<-as.data.frame(ghi)
    tem<-as.data.frame(tem)
    # Índice de transparencia
    kh<- as.data.frame(mapply(indice_transparencia_horario,ghi,rad_i_e))

    # Componentes de la rad, primero horizontal y luego inclinada
    r_difusa<-as.data.frame(mapply(radiacion_difusa_horizontal, kh,ghi))
    r_difusa_inclinada<- as.data.frame(mapply(radiacion_difusa_inclinada,r_difusa, inclina))
    r_directa_horizontal <- ghi - r_difusa
  
    if (inclinacion != 0) {
      angulo_s_inc <- as.data.frame(mapply(angulo_solar_inclinado,angulo,w_sr,acimut,declina,lat,orienta,inclina,signo))
      r_directa_inclinada <- as.data.frame(mapply(radiacion_directa_inclinada,r_directa_horizontal,
                                   declina,angulo_s_inc,lat,orienta,inclina))
    } else {
      r_directa_inclinada <- r_directa_horizontal
    }
    r_reflejada <- as.data.frame(mapply(radiacion_reflejada,ghi,inclina))
  
    r_global_inclinada <- r_directa_inclinada + r_difusa_inclinada + r_reflejada

    # Energía producida
    velocidad_viento <- 0 #si hubiera datos de veloc de viento se cargarían ahí
    energia_fv<-energia_generada(tem,r_global_inclinada,velocidad_viento) 
    energia_fv <- energia_paneles_silicio(area, energia_fv)
    energia_dia <- as.data.frame(rowSums (energia_fv[ , 5:20]))
    energia_dia <- energia_dia %>% filter(energia_dia!=0)
    media=mean(energia_dia[,1])
    media_diaria<-media_diaria+media
  } 
  media_diaria <- media_diaria/years
}
  ##------------------------------------------------------------------------------------------------
  

# Función auxiliar para calcular la radiación global horaria 
# necesaria para obtener la producción fotovoltaica horaria.

calculo_radiacion_horaria_inclinada <- function(r_global_h, kh, acimut_s, altura_s, angulo_s, angulo_salida_sol,declinacion, 
                                      latitud, inclinacion, orientacion){
  
  #if(orientacion<0) {signo<- -1} else {signo<- 1}
 
  r_difusa_h <- radiacion_difusa_horizontal(kh,r_global_h)
    
  r_difusa_i <- radiacion_difusa_inclinada(r_difusa_h,inclinacion)
  
  r_directa_i <-radiacion_directa_inclinada_dia(r_global_h,r_difusa_h,inclinacion,
                                                orientacion, acimut_s,altura_s,declinacion,
                                                latitud, angulo_s,angulo_salida_sol)
      
    #r_directa_h <- c(r_directa_h, r_directa)
    
    r_reflejada_i <- radiacion_reflejada(r_global_h,inclinacion)
    #r_reflejada_h <- c(r_reflejada_h, r_reflejada)
    
    r_global_i <- r_directa_i + r_difusa_i + r_reflejada_i
   # r_global_h <- c(r_global_h, r_global_plano)
  
  #df_radiaciones <- data.frame(r_difusa_h, r_reflejada_h, r_directa_h, r_global_h)
  #df_radiaciones <- data.frame(r_global_h)
  #df_radiaciones
    r_global_i
}
  
extraccion_valores_medios <- function(latitud = 36.72016, longitud = -4.42034, inclinacion=0, 
                                        orientacion=0, area=6, datos) {
    
      
    #PEtición de Llanos
    if (inclinacion <= 10) {
      inclinacion <- 30
    }
    
    print ("orientacion")
    print (orientacion)
    print ("inclinacion")
    print (inclinacion)
    print ("area")
    print (area)
    
    #PEtición de Llanos
    orientacion <- orientacion -180
    prod_fv_diaria_media<-energia_generada_media(latitud, longitud, inclinacion, orientacion, area, datos)
}

  
