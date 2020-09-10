
# Script con todas las funciones necesarias para la estimación de la producción fotovoltaica

library(tidyverse)
library(readr)

## Posición relativa sol-superficie horizontal

### Excentricidad de la tierra ($E_0$)
excentricidad <- function(dia){
  
  e_0 <- 1 + 0.033 * cos((2*pi*dia)/365)
  e_0
}

### Declinación de la tierra
gamma_declinacion <- function(dia){
  gamma = 2.0 * pi *(dia-1)/365.0
  
  gamma
}

angulo_declinacion_solar <- function(dia){
  
  gamma = gamma_declinacion(dia)
  delta = 0.006918 - 0.399912 * cos(gamma) + 0.070257 * sin(gamma) - 0.006758 * cos(2*gamma) + 0.000907 * sin(2* gamma) - 0.002697* cos(3*gamma) + 0.001480 * sin(3*gamma)
  
  delta #(radianes)
}


### Ángulo de salida del sol $\omega_{sr}$
angulo_salida_sol <- function(latitud, declinacion){
  
  w <- acos(- tan(latitud) * tan(declinacion))
  w
}

### Horas hasta el mediodía solar
horas_mediodia <- function(wsr){
  horas <- wsr * 12 / pi
}

segundos <- function(horas_mediodia){
  segundos <-  (horas_mediodia - trunc(horas_mediodia)) * 3600
  segundos
}

hora_inicial <- function(horas_mediodia){
  hora.inicial <- 12 - trunc(horas_mediodia)
  hora.inicial
}

hora_final <- function(horas_mediodia){
  hora.final <- 12 + trunc(horas_mediodia)
  hora.final
}

### Ángulo solar horario

angulo_solar_primera_ultima_hora <- function(segundos, w_sr){
  w = w_sr - ((segundos/3600)/2 * pi/12)
  w
}

angulo_solar <- function(hora){
  w = (hora - 12 - 0.5) * pi/12
  w
}

### Altura solar horaria
altura_solar <- function(declinacion, latitud, angulo_solar_hora){
  
  altura <- asin(sin(declinacion) * sin (latitud) + cos(declinacion) * cos(latitud) * cos(angulo_solar_hora))
  altura
  
}

#### Acimut solar horario

acimut_solar <- function(altura_solar, latitud, declinacion, angulo_horario){
  
  
  if (latitud > 0) {
    
    
    acimut <- acos((sin(altura_solar)*sin(latitud)- sin(declinacion))/(cos(altura_solar)*cos(latitud)))
    
  }else{
    
    
    acimut <- acos((sin(declinacion)*sin(latitud) - cos(declinacion)* sin(latitud)* cos(angulo_horario))/cos(declinacion))
  }
  
  acimut
}

### Radiación extraterrestre incidente

K_solar <- 1361 #W/m^2

radiacion_incidente_extraterrestre <- function(excentricidad, declinacion, angulo_solar_hora, altura_solar_hora, latitud, beta = 0){
  
  r_incidente <- K_solar * excentricidad * sin(altura_solar_hora)
  
  r_incidente
}

radiacion_incidente_extraterrestre_primera_ultima_hora <- function(excentricidad, declinacion, angulo_solar_hora, altura_solar_hora, latitud, beta=0, segundos){
  
  radiacion <- radiacion_incidente_extraterrestre(excentricidad, declinacion, angulo_solar_hora, altura_solar_hora, latitud, beta)
  
  radiacion <- radiacion * segundos / 3600
  radiacion
}

#### Obtener dataframe de medidas horarias para una inclinación, latitud, longitud, altura y acimut (orientación)

df_energia_solar <- function(lat, lon, alt = NULL, inclinacion = 0, orientacion = 0){
  
  lat <- lat * pi/180
  lon <- lon * pi/180
  inclinacion <- inclinacion * pi/180
  orientacion <- orientacion * pi/180
  
  mes = c(rep(1, 31), rep(2, 28), rep(3, 31), rep(4, 30), rep(5, 31), rep(6, 30), rep(7, 31),     rep(8, 31), rep(9, 30), rep(10, 31), rep(11, 30), rep(12, 31))
  dia = c(1:31, 1:28, 1:31, 1:30, 1:31, 1:30, 1:31, 1:31, 1:30, 1:31, 1:30, 1:31)
  dia_juliano = c(1:365)
  
  df = data.frame(mes, dia, dia_juliano)
  
  
  df <- df %>% mutate(declinacion_solar = angulo_declinacion_solar(dia_juliano))
  
  df <- df %>% mutate(excentricidad_diaria = excentricidad(dia_juliano))
  
  df <- df %>% mutate(w_sr = angulo_salida_sol(lat, declinacion = declinacion_solar))
  
  df <- df %>% mutate(horas_mediodia = horas_mediodia(w_sr))
  
  df <- df %>% mutate(duracion_dia = horas_mediodia*2)
  
  df <- df %>% mutate(hora_inicial = hora_inicial(horas_mediodia), 
                      hora_final = hora_final(horas_mediodia))
  
  df <- df %>% mutate(segundos = segundos(horas_mediodia))
  
  df_radiacion <- data.frame(dia_juliano = 1:365)
  df_altura <- data.frame(dia_juliano = 1:365)
  df_acimut <- data.frame(dia_juliano = 1:365)
  df_angulo_solar <- data.frame(dia_juliano = 1:365)
  
  for(h in 0:23){
    
    w <- c()
    altura <- c()
    acimut <- c()
    radiacion <- c()
    
    for(d in 1:365){
      
      hora_inicial <- df$hora_inicial[d]
      hora_final <- df$hora_final[d]
      segundos <- df$segundos[d]
      w_sr <- df$w_sr[d]
      delta <- df$declinacion_solar[d]
      E <- df$excentricidad_diaria[d]
      
      
      if(h == hora_inicial-1 || h == hora_final){
        
        w_d <- angulo_solar_primera_ultima_hora(segundos, w_sr)
        
        altura_d <- altura_solar(delta, lat, w_d)
        
        acimut_d <- acimut_solar(altura_d, lat, delta, w_d)
        
        if (h == hora_inicial -1){
          acimut_d <- - acimut_d
          w_d = -w_d ####CAMBIADO esta línea es nueva
        }
        
        rad_d <- radiacion_incidente_extraterrestre_primera_ultima_hora(E, delta, w_d, altura_d, lat, inclinacion, segundos)
        
        
      }else if(h >= hora_inicial && h < hora_final){
        
        w_d <- angulo_solar(h)
        
        altura_d <- altura_solar(delta, lat, w_d)
        
        acimut_d <- acimut_solar(altura_d, lat, delta, w_d)
        
        if(h < 12){
          acimut_d <- -acimut_d
        }
        
        rad_d <- radiacion_incidente_extraterrestre(E, delta, w_d,  altura_d, lat, inclinacion)
        
      }else{
        w_d <- 0
        altura_d <- 0
        acimut_d <- 0
        rad_d <- 0
        
      }
      
      w <- c(w, w_d)
      altura <- c(altura, altura_d)
      acimut <- c(acimut, acimut_d)
      radiacion <- c(radiacion, rad_d)
      
      
    }
    
    # angulo solar
    df_angulo_solar <- cbind(df_angulo_solar, w)
    names(df_angulo_solar)[length(names(df_angulo_solar))] <- paste("W_",h,"_",h+1,sep="")
    
    # altura solar
    df_altura <- cbind(df_altura, altura)
    names(df_altura)[length(names(df_altura))] <- paste("Altura_",h,"_",h+1,sep="")
    
    # acimut solar
    df_acimut <- cbind(df_acimut, acimut)
    names(df_acimut)[length(names(df_acimut))] <- paste("Acimut_",h,"_",h+1,sep="")
    
    # Radiacion extraterrestre
    df_radiacion <- cbind(df_radiacion, radiacion)
    names(df_radiacion)[length(names(df_radiacion))] <- paste("Gh0_",h,"_",h+1,sep="")
    
  }
  
  df <- merge(x=df, y=df_angulo_solar, by="dia_juliano", all=TRUE)
  df <- merge(x=df, y=df_altura, by="dia_juliano", all=TRUE)
  df <- merge(x=df, y=df_acimut, by="dia_juliano", all=TRUE)
  df <- merge(x=df, y=df_radiacion, by="dia_juliano", all=TRUE)
  
  #write_csv(df, paste("calculos_radiacion_incidente_extraterrestre_lat_",lat * 180/pi,"_lon_", lon * 180/pi,"_incl_",inclinacion * 180/pi, "_orient_", orientacion*180/pi,".csv", sep = ""))
  
  df
}

#######################################################################################################################

## Componentes de la radiación a partir de la radiación global

radiacion_global <- function(r_directa, r_difusa, r_reflejada){
  r_global <- r_directa + r_difusa + r_reflejada
  r_global
}

### Índice de transparencia atmosférico

indice_transparencia <- function(r_global, r_extraterrestre){
  i_transparencia <- r_global/r_extraterrestre
  i_transparencia
}

r_global_kh <- function(kh, I_0h){
  
  rg <- kh * I_0h
  rg
  
}

### Radiación difusa:

radiacion_difusa <- function(indice_transparencia, r_global, beta = 0){
  
  if(indice_transparencia >= 0 && indice_transparencia <= 0.22){
    
    r_difusa <- r_global *(1 - 0.09 * indice_transparencia)
    
    
  }else if (0.22 < indice_transparencia && indice_transparencia <= 0.8){
    
    r_difusa <- r_global * (0.9511 - 0.16 * indice_transparencia + 4.388 * indice_transparencia^2 - 16.638 * indice_transparencia^3 + 12.336 * indice_transparencia^4)
    
  }else{
    
    r_difusa <- r_global * 0.165
  }
  
  
  if(beta != 0){
    
    r_difusa <- (1 + cos(beta))/2 * r_difusa
    
  }
  
  r_difusa
}

### Radiación reflejada:
radiacion_reflejada <- function(radiacion_global, albedo = 0.2, beta = 0){
  
  radiacion_reflejada <- radiacion_global * albedo * (1- cos(beta))/2
  radiacion_reflejada
}

### Radiación directa:

wsrm <- function(orientacion, beta, latitud, declinacion, angulo_solar){
  
  if (orientacion < 0){
    
    signo <- -1
    
  }else{
    
    signo <- 1
  }
  
  A <- sin(latitud)/tan(orientacion) + cos(latitud)/(sin(orientacion) * tan(beta))
  
  B <- -tan(declinacion)*(sin(latitud)/(sin(orientacion)*tan(beta)) - cos(latitud)/tan(orientacion))
  
  wsrm <- acos((A*B+signo*sqrt(A*A-B*B+1))/(A*A+1))
  
  wsrm <- - min(wsrm,angulo_solar) ####CAMBIADO aquí he añadido el signo -
  
  wsrm
  
}

wsrt <- function(orientacion, beta, latitud, declinacion, angulo_solar){
  if (orientacion < 0){
    
    signo <- -1
    
  }else{
    
    signo <- 1
  }
  
  A <- sin(latitud)/tan(orientacion) + cos(latitud)/(sin(orientacion) * tan(beta))
  
  B <- -tan(declinacion)*( sin(latitud)/(sin(orientacion)*tan(beta)) - cos(latitud)/tan(orientacion))
  
  wsrt <- acos((A*B-signo*sqrt(A*A-B*B+1))/(A*A+1))
  
  wsrt <- min(wsrt, angulo_solar)
}

radiacion_directa <- function(r_global, r_difusa, beta = 0, orientacion = 0, acimut_solar_h, altura_solar_h, declinacion, latitud, angulo_solar_h){
  
  r_directa <- r_global - r_difusa
  
  if (beta != 0){
    
    if(orientacion <0 && acimut_solar_h > 0){
      wsrt <- wsrt(orientacion, beta, latitud, declinacion, angulo_solar_h)
      
      if(angulo_solar_h > wsrt){
        angulo_solar_h <- wsrt
      }
    }
    
    if(orientacion >0 && acimut_solar_h <0){
      wsrm <- wsrm(orientacion, beta, latitud, declinacion, angulo_solar_h)
      
      if(angulo_solar_h < wsrm){ ####CAMBIADO Aquí ponía > pero es <, porque ahora los dos son negativos
        angulo_solar_h <- wsrm
      }
      
    }
    
    costhita <- sin(declinacion) * sin(latitud) * cos(beta) - sin(declinacion) * cos(latitud) * sin(beta) * cos(orientacion) + cos(declinacion) * cos(latitud) * cos(beta) * cos(angulo_solar_h) + cos(declinacion) * sin(latitud) * sin(beta) * cos(orientacion) * cos(angulo_solar_h) + cos(declinacion) * sin(beta) * sin(orientacion) * sin(angulo_solar_h)
    
    if(costhita < 0){
      costhita <- 0
    }
    
    costhitaz <- sin(declinacion) * sin(latitud) + cos(declinacion) * cos(latitud) * cos(angulo_solar_h)
    
    if(costhitaz > 0 && !near(costhitaz, 0)){
      
      rb <- costhita/costhitaz
      
    }else{
      rb <- 1
    }
    
    r_directa <- r_directa * rb
  }
  
  
  r_directa
}

## Estimación de la producción de energía fotovoltaica

### Obtención de la potencia generada por los paneles fotovoltaicos

temperatura_instalacion <- function (temperatura_ambiente, irradiacion_solar, velocidad_viento, m = -3.56, n = -0.079){
  
  temperatura <- temperatura_ambiente + irradiacion_solar * exp(m + n * velocidad_viento)
  temperatura
}

potencia_generada <- function(t_ambiente, irr_solar, v_viento, P_ref = 1){
  
  g_ref <- 1000 #Wh/m^2
  coef_y <- -0.0048
  t_ref <- 25
  P_ref <- 1000
  
  coefCC <- 0.97 * 0.98 *0.98 * 0.98
  efinversor <- 0.95
  perdCA <- 0.99
  
  T_m <- temperatura_instalacion(temperatura_ambiente = t_ambiente,irradiacion_solar = irr_solar, velocidad_viento = v_viento)
  
  potencia <- P_ref * irr_solar/g_ref * (1+ coef_y *(T_m - t_ref))
  
  potencia <- potencia * coefCC * efinversor * perdCA
  
  potencia
}

potencia_paneles_silicio <- function(area, potencia){
  potencia <- potencia * area/6
}

potencia_lamina_delgada <- function(area, potencia){
  potencia <- potencia * area/10
}

########################################################################################################

#### Datos de Málaga (Radiación y temperatura)

# Con los datos del fichero MA_R.Sal y MA_T.Sal.


datos_malaga <- function(){
  
  ## Radiación
  MA_R <- read_table2("Datos Malaga/MA_R.sal", 
                      col_names = FALSE)
  
  MA_R_colnames <- c("año", "mes", "dia")
  
  gh <- paste("Gh", as.character(5:20), sep = "_")
  
  MA_R_colnames <- c(MA_R_colnames, gh)
  
  colnames(MA_R) <- MA_R_colnames
  
  
  ## Temperatura
  MA_T <- read_table2("Datos Malaga/MA_T.sal")
  
  MA_T_colnames <- c("año", "mes", "dia")
  
  th <- paste("t", as.character(1:24), sep = "_")
  
  MA_T_colnames <- c(MA_T_colnames, th)
  
  colnames(MA_T) <- MA_T_colnames
  
  MA_T[, th] <- MA_T[, th]/10 #Temperatura en uds decimales
  
  ## Unión
  MA <- left_join(MA_R, MA_T, by = c("año", "mes", "dia"))
  
  MA <- as.data.frame(MA)
  #Eliminar dia 29
  dias_29 <-which(MA$dia == 29)
  mes_2 <- which(MA$mes == 2)
  dia_29_2 <- dias_29[which(dias_29 %in% mes_2)]
  
  MA <- MA[-dia_29_2, ]
  
  # Eliminar valores negativos
  MA <- MA %>% 
    filter(Gh_5 >= 0, Gh_6 >= 0, Gh_7 >= 0, Gh_8 >= 0, Gh_9 >= 0, Gh_10 >= 0, Gh_11 >= 0, Gh_12 >= 0, 
           Gh_13 >= 0, Gh_14 >= 0, Gh_15 >= 0, Gh_16 >= 0, Gh_17 >= 0, Gh_18 >= 0, Gh_19 >= 0, Gh_20 >= 0) %>% 
    filter(t_5 >= 0, t_6 >= 0, t_7 >= 0, t_8 >= 0, t_9 >= 0, t_10 >= 0, t_11 >= 0, t_12 >= 0, 
           t_13 >= 0, t_14 >= 0, t_15 >= 0, t_16 >= 0, t_17 >= 0, t_18 >= 0, t_19 >= 0, t_20 >= 0)
  
  #Como los valores de radiación de AEMET están en 10*KJ/M^2 se pasan a Wh 1kj  = 1/3.6 Wh
  
  MA[,gh] <- MA[,gh]*10/3.6
  MA
  
}

#----------------------------------------------------------------------------------------

# A continuación se muestra una función auxiliar para calcular la radiación global horaria 
# necesaria para obtener la producción fotovoltaica horaria.


calculo_radiacion_horaria <- function(r_global, K, acimut_sol, altura_sol, angulo_sol, decl, 
                                      latitud, inclinacion, orientacion){
  r_difusa_h <- c()
  r_directa_h <- c()
  r_reflejada_h <- c()
  r_global_h <- c()
  for (h in 1:ncol(r_global)){
    k <- K[1, h]
    rg <- r_global[1, h]
    acimut_h <- acimut_sol[1, h]
    altura_h <- altura_sol[1, h]
    angulo_h <- angulo_sol[1, h]
    declinacion <- decl
    
    r_difusa <- radiacion_difusa(indice_transparencia = k, r_global = rg, beta = inclinacion)
    r_difusa_h <- c(r_difusa_h, r_difusa)
    
    r_directa <- radiacion_directa(r_global = rg, 
                                   r_difusa = r_difusa, 
                                   beta = inclinacion, 
                                   orientacion = orientacion, 
                                   acimut_solar_h = acimut_h, 
                                   altura_solar_h = altura_h, 
                                   declinacion = declinacion, 
                                   latitud = latitud, 
                                   angulo_solar_h = angulo_h)
    
    r_directa_h <- c(r_directa_h, r_directa)
    
    r_reflejada <- radiacion_reflejada(radiacion_global = rg, beta = 30)
    r_reflejada_h <- c(r_reflejada_h, r_reflejada)
    
    r_global_plano <- radiacion_global(r_directa = r_directa, r_difusa = r_difusa, r_reflejada = r_reflejada)
    r_global_h <- c(r_global_h, r_global_plano)
  }
  
  df_radiaciones <- data.frame(r_difusa_h, r_reflejada_h, r_directa_h, r_global_h)
  df_radiaciones
}

# Función principal

calculos_dias <- function(lat, lon, inclinacion, orientacion, area, datos){
  
  ## Paso 1: Calcular radiación extraterrestre incidente horaria para la latitud y longitud de Málaga durante un año.
  calculos_gh0_MA <- df_energia_solar(lat = lat, lon = lon,  inclinacion = inclinacion, orientacion = orientacion)
  
  # Variables auxiliares para seleccionar columnas
  gh <- paste("Gh", as.character(5:20), sep = "_")
  gh0 <- paste("Gh0", as.character(4:19), as.character(5:20), sep = "_")
  
  acimut_h_dia <- paste("Acimut", as.character(4:19), as.character(5:20), sep = "_")
  altura_h_dia <- paste("Altura", as.character(4:19), as.character(5:20), sep = "_")
  angulo_h_dia <- paste("W", as.character(4:19), as.character(5:20), sep = "_")
  
  th_dia <- paste("t", as.character(5:20), sep = "_") 
  
  inclinacion <- inclinacion * pi/180
  orientacion <- orientacion * pi/180
  lat <- lat * pi/180
  
  # Vectores para almacenar las potencias diarias
  potencia_diaria <- c()
  potencia_silicio_diaria <- c()
  potencia_lamina_delgada_diaria <- c()
  
  for(d in 1:nrow(datos)){
    
    calculos_dia <- calculos_gh0_MA%>%filter(mes==datos[d,]$mes, dia == datos[d,]$dia)
    ## Paso 2: Indice de transparencia
    
    dia_Gh <- datos[d, gh]                 #Valores horarios de Gh para el día d
    dia_Gh0 <- calculos_dia[1, gh0]  #Valores horarios de Gh0 para el día d
    
    dia_Kh <- indice_transparencia(r_global = dia_Gh, r_extraterrestre = dia_Gh0)
    # Eliminar valores NaN para aquellas horas en las que la radiación global Gh es 0
    dia_Kh <- replace(dia_Kh, is.na(dia_Kh), 0)
    
    dia_Acimut_h <- calculos_dia[1, acimut_h_dia]
    dia_Altura_h <- calculos_dia[1, altura_h_dia]
    dia_Angulo_h <- calculos_dia[1, angulo_h_dia]
    dia_declinacion <- calculos_dia$declinacion_solar[1]
    
    ## Paso 3: Calcular radiación directa, difusa y reflejada sobre un plano inclinado para cada día
    df_radiacion_horaria <- calculo_radiacion_horaria(dia_Gh, 
                                                      dia_Kh, 
                                                      dia_Acimut_h, 
                                                      dia_Altura_h, 
                                                      dia_Angulo_h,
                                                      dia_declinacion,
                                                      lat,
                                                      inclinacion,
                                                      orientacion)
    
    
    ## Paso 4: Estimación de la producción fotovoltaica
    dia_Temp <- as.numeric(datos[d, th_dia])
    
    potencia_dia_h <- potencia_generada(t_ambiente = dia_Temp, 
                                        irr_solar = df_radiacion_horaria$r_global_h, 
                                        v_viento = 0)
    
    potencia_silicio_dia_h <- potencia_paneles_silicio(area = area, 
                                                       potencia = potencia_dia_h)
    potencia_lamina_delgada_dia_h <- potencia_lamina_delgada(area = area, 
                                                             potencia = potencia_dia_h)
    
    potencia_diaria <- c(potencia_diaria, sum(potencia_dia_h))
    potencia_silicio_diaria <- c(potencia_silicio_diaria, sum(potencia_silicio_dia_h))
    potencia_lamina_delgada_diaria <- c(potencia_lamina_delgada_diaria, sum(potencia_lamina_delgada_dia_h))
    
  }
  
  datos <- cbind(datos, Energia_diaria_producida_Wh = potencia_diaria, 
                 Energía_diaria_producida_Wh_silicio = potencia_silicio_diaria,
                 Energía_diaria_producida_Wh_lámina_delgada = potencia_lamina_delgada_diaria)
  
  datos
  
}

##------------------------------------------------------------------------------------------------

# Esta función tiene por defecto los valores de la latitud y la longitud del municipio de Málaga. 
# Ha sido creada para facilitar el uso de los cálculos ya que realiza la media de los valores de 
# producción para todos los días de los que se tienen datos.
# Otra operación que realiza es restar 180 a la orientación proporcionada por los cálculos de Fran 
# para la aplicación.

# OJO! Que lo que devuelve, prod_fv_diaria_media es de tipo data.frame


extraccion_valores_medios <- function(latitud = 36.72016, longitud = -4.42034, inclinacion, 
                                      orientacion, area, datos) {
  
  
  
  if (inclinacion <= 10) {
    inclinacion <- 30
  }
  
  
  
  print ("orientacion")
  print (orientacion)
  print ("inclinacion")
  print (inclinacion)
  print ("area")
  print (area)

  
  orientacion <- orientacion -180
  
  
  MA_prod_fv <- calculos_dias(lat = latitud, lon = longitud, inclinacion = inclinacion, 
                              orientacion = orientacion, area = area, datos = datos)
  
  prod_fv_diaria_media <- MA_prod_fv %>%
    summarise(
      Energía_media_diaria_producida_Wh_silicio = mean(Energía_diaria_producida_Wh_silicio, na.rm = TRUE),
      Energía_media_diaria_producida_Wh_lámina_delgada = mean(Energía_diaria_producida_Wh_lámina_delgada, na.rm = TRUE))
  
  prod_fv_diaria_media
}

  


