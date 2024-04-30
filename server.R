#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

###########################Libraries####################################################################### 

library(igraph)
library(raster)
#library (rgdal)
library(RStoolbox)
#library(RGISTools)
library(zoom)
library(excelR)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(widgetframe)
library (lidR)
library(rgl)
#library(rasterVis)
library(shiny)
library(shinythemes)
library(ggmap)
library(leaflet.extras)
library(sp)
library (cronR)

###########################Libraries####################################################################### 


options(shiny.sanitize.errors = FALSE)
pdf(NULL)

#Add energy production calculation functions script
source("funciones_prod_FV.R")

#Add script para la predicción de energía a corto plazo
source("procesamientoEnergiaCortoPlazo.R")

#Autonatizamos la descarga diaria automática de datos del AEMET



automatizacionScripts <- function () {
  
  #observaciones
  automatizarDescargaObservacionesConvencionales <- "automat_obs_conv.R"
  cmd <- cron_rscript(automatizarDescargaObservacionesConvencionales)
  cron_add(command = cmd, frequency = 'daily', at = '00:00', 
           id = 'outomat_obs_00h', description = 'Automatización de descarga de observaciones a las 00:00')
  cron_add(command = cmd, frequency = 'daily', at = '10:00', 
           id = 'outomat_obs_10h', description = 'Automatización de descarga de observaciones a las 10:00')
  
  #predicciones
  automatizarDescargaPredicciones <- "automat_pred_horarias.R"
  cmd <- cron_rscript(automatizarDescargaPredicciones)
  cron_add(command = cmd, frequency = 'daily', at = '10:00', 
           id = 'outomat_pred_10h', description = 'Automatización de descarga de predicciones a las 10:00')
 
  #datos de radiacion
  automatizarDescargaObservacionesRadiacion<- "automat_obs_radiacion.R"
  cmd <- cron_rscript(automatizarDescargaObservacionesRadiacion)
  cron_add(command = cmd, frequency = 'daily', at = '10:00', 
           id = 'outomat_rad_10h', description = 'Automatización de descarga de radiaciones a las 10:00')
  
}

#automatizacionScripts ()

############################getExtent()#####################################################################

## Para cada imagen Lidar del catálogo, obtenemos su extensión, y devolvemos un df con las extensiones ##

############################################################################################################

getExtent <- function(catalogo) {
  extentList <- c ()

  for (i in 1:nrow(catalogo) ) {
    im <- catalogo [i,]
    print (sf::st_bbox(im))
    # cada imagen lidar se reprojecta a latitud y longitud y cogemos la extensión
    e <- sf::st_bbox(im)
    #x lng y lat sW (abajo izq)   NE (arriba derecha)
    lng1 <- xmin (extent (e))
    lat1 <- ymin (extent (e))
    lng2 <- xmax (extent (e))
    lat2 <- ymax (extent (e))
    minDF <- cbind (lng1,lat1)
    maxDF <- cbind (lng2,lat2)
    fDF <- as.data.frame(rbind(minDF,maxDF))
    colnames (fDF) <- c("lng","lat")
    cord.UTM = SpatialPoints(cbind(fDF$lng, fDF$lat),
                             proj4string=crs(im))
    
    cord.DEC<- spTransform(cord.UTM, CRS("+proj=longlat"))
    lng1 <- xmin(extent(cord.DEC))
    lat1 <- ymin(extent(cord.DEC))
    lng2 <- xmax(extent(cord.DEC))
    lat2 <- ymax(extent(cord.DEC))
    extentList[[i]] <- c (lng1,lat1,lng2,lat2)
  }
  
  
  #dataFrame with img extents
  dfExtents = t(as.data.frame(extentList, optional = TRUE))
  colnames(dfExtents) = c("lng1", "lat1", "lng2", "lat2" )
  rownames(dfExtents) = 1:length (extentList)
  dfExtents <- as.data.frame (dfExtents)
  
  #print (dfExtents)
  
  #closeAlert(num = 0, id = NULL)
  
  return(as.data.frame(dfExtents))
  
}




###################################### segmentRoofs () ##########################################################

## Función que segmenta los tejados del área de interés 

segmentRoofs<- function (aoi) {
  
  segmentedRoof <- filter_poi(aoi,Classification == 6L | Classification == 2L )
  
  return (segmentedRoof)
  
}

#################################################################################################################

###################################### chmRoofs () ##########################################################

## Función que devuelve el modelo normalizado de alturas de los edificios segmentados en el AOI 

chmRoofs<- function (aoiSegmented) {
  
  thr <- c(0,2,5,10,15)
  edg <- c(0, 1.5)
  
  # Eliminamos el suelo de la nube de puntos. Normalizamos cada imagen o nube de puntos LIDAR
  lasNormalized <- normalize_height (aoiSegmented,tin())

  #print ('**********Alturas Lidar normalizadas')
  
  chm  <- grid_canopy(lasNormalized, 1, pitfree(thr, edg))
  
  plot(chm,main="Modelo de alturas normalizado. CHM")
  
  return (chm)
  
}



## Función que muestra las áreas de los tejados que cumplen las condiciones de filtrado de los criterios de inclinación y pendiente 

showRoofAreas2 <- function () {
  
  formask <- setValues(raster(segmentRoofCHM), NA)
  
  aspectValues <-  c()
  orientationValues <-  c()
  
  #Tomamos los valores de las discretizaciones en los que el usuario está interesado (los que representan los intervalos de orientación y pendientes en los que está interesado)
  aspectValues <- unique(na.omit(values(aspectDiscretization)))
  slopeValues <- unique(na.omit(values(slopeDiscretization)))
  
  aspectValues <- na.omit(aspectValues)
  slopeValues <- na.omit(slopeValues)
  
  # print ("aspectValues")
  # print (aspectValues)
  # print ("aspectDisc")
  # print (aspectDiscretization)
  # 
  # print ("slopeValues")
  # print (slopeValues)
  # print ("slopeDisc")
  # print (slopeDiscretization)
  
  formask[ (aspectDiscretization %in% aspectValues ) & (slopeDiscretization %in% slopeValues)  ] <- 1
  
  plot (formask)
  
  return (formask)
 
}


#####################################addnewDataToDataset ############################################################################

## Return a daset with roof slopes, orientation, lat, lng para el cálculo a largo plazo

addnewDataToDataset <- function (ds) {
  
  val_ <- unique(cC)
  
  #print (values)
  
  
  # Array con las latitudes para cada tejado
  latsArray <- c()
  # Array con las lngitudes para cada tejado
  lngsArray <- c()
  # Array con las pendientes para cada tejado
  slopesArray <- c()
  # Array con las orientaciones para cada tejado
  aspectssArray <- c()
  # Array con las producciones para placas de silicio de cada tejado
  silicePVProductionArray <- c()
  
  tejadosAProcesar <- ds$idTejado
  
  tejadoEnProceso <- 1
  
  # para cada tejado (para cada valor distinto de componente conexa) que cumpla las condiciones de pendientes, orientaciones, y area > a 4m2
  totales <- length(tejadosAProcesar)
  
  for (v in tejadosAProcesar) {
    
    #Obtenemos en un array las posiciones (indices) donde los valores de las componentes conexas son = al valor del tejado v que se está procesando
    indexes <- which (values (cC==v) )
    
      #Calculamos la media de los valores de inclinaciones para las coordenadas del tejado
      avgSlope <- mean(slopeDiscretization [indexes])
      avgSlope<- format(round(avgSlope, 2), nsmall = 2)
      #Calculamos la media de los valores de orientaciones para las coordenadas del tejado
      avgAspect <- mean(aspectDiscretization [indexes])
      avgAspect<- format(round(avgAspect, 2), nsmall = 2)
      #Calculamos las coordenadas del tejado que se está procesando (latitudes y longitudes) y tomamos la primera latitud y longitud
      lats<- coordinates (slopeDiscretization) [indexes,1]
      lngs <- coordinates (slopeDiscretization) [indexes,2]
      
     
      # Convert lat lng -UTM- to lat lng 
  
      coords <- coordinates (slopeDiscretization) [indexes,]
      

        dataFrameCoords <- as.data.frame(coords)
        
        colnames(dataFrameCoords) <- c("x_coord","y_coord")
        
        
        
        # Creamos un SpatialPoints object con las coordenadas del polígono en UTM
        cord.UTM = SpatialPoints(cbind(dataFrameCoords$x_coord, dataFrameCoords$y_coord), proj4string=CRS("+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" ))
        
        # Convertimos las coordenadas del polígono  UTM a lat lng 
        cord.dec <- spTransform(cord.UTM, CRS("+proj=longlat"))
        
        #Obtenemos un dataframe con las coordenadas del sistema Lat Lng
        dfT <- as.data.frame(cord.dec)
        
        #Area que se está procesando
        procesedArea <- ds %>%
          filter(ds$idTejado == v) 
        
        
        proc_area <- procesedArea$area_m2
        
        #Calculamos la producción si la orientación está entre 90 y 270
        
        ####### POR AQUÍIIIII
        
      # if ( (abs (as.numeric(avgAspect))>90) & ( abs(as.numeric(avgAspect))<270) ) { 
      #   
        #print (paste ("***********PROCESANDO TEJADO:*********",tejadoEnProceso))
        
        showModal(modalDialog( title = paste("Calculando producción del tejado ", tejadoEnProceso, "de", totales, "tejados"), footer=NULL))
        
        
        # print ("Valores que paso a funciones_prov.R: para cada tejado")
        # 
        # print (paste("orientacion:",as.numeric(avgAspect)) )
        # print (paste("innclinación:",as.numeric(avgSlope)) )
        # print (paste("area:",proc_area) )
        # print (paste("latitud:",dfT [1,2]) )
        # print (paste("longitur:",dfT [1,1]) )
        # 
        
        print ("***********DATOS_AEMET*********")
        #view(datos_aemet)
        produccion <- extraccion_valores_medios (latitud = dfT [1,2], longitud = dfT [1,1], as.numeric(avgSlope), as.numeric(avgAspect), proc_area, datos_aemet)
        
        print ("***********TEJADO CALCULADO*********")
        print ("***********produccion devuelta*********")
        print (produccion)
        prodProcesed <- round((produccion/1000),0)
        
        #OLD prodProcesed <- round(produccion$Energia_media_diaria_producida_Wh_silicio/1000,1)
        #print (produccion)
        
        silicePVProductionArray <- append (silicePVProductionArray,prodProcesed)
        
        removeModal()
        
        #tejadoEnProceso <- tejadoEnProceso+1
        
      
        
      
      # else { ##Cuando no podamos calcular la produción por no tener orientacion entre 90 y 270, el valor de prod. será -1 (luego se elminian del dataframe)
      #   silicePVProductionArray <- append (silicePVProductionArray,-1)
      # }
      #   
      #Tomamos la primera coordenada de lat y lng del tejado que se está procesando
      latsArray <- append (latsArray,dfT [1,2]) 
      lngsArray <- append (lngsArray,dfT [1,1]) 
        
      slopesArray <- append (slopesArray,avgSlope)
      aspectssArray <- append (aspectssArray,avgAspect)
      
      
      
    #}
    
    tejadoEnProceso <- tejadoEnProceso +1
 
  }
  
  #add columns to ds
  
  ds$lat <- latsArray
  ds$lng <- lngsArray
  ds$slope <- slopesArray
  ds$orientation <- aspectssArray
  ds$produccion <- silicePVProductionArray
  
  #view (ds)
  
  #Eliminamos del ds los que tienen prod. -1; es decir las filas (tejados) para los que no se pudo calcular la prod. debido a que su orientacion no estaba entre 90 y 270
  #ds <- ds %>%
    #filter(as.numeric(ds$produccion) >0) 
  #view (ds)
  
  return (ds)
  
}


#####################################

#####################################addnewDataToDataset ############################################################################

## Return a daset with roof slopes, orientation, lat, lng para el cálculo a un día vista

addnewDataToDataset2 <- function (ds) {
  
  val_ <- unique(cC)
  
  #print (values)
  
  # Array con las latitudes para cada tejado
  latsArray <- c()
  # Array con las lngitudes para cada tejado
  lngsArray <- c()
  # Array con las latitudes para cada tejado
  slopesArray <- c()
  # Array con las lngitudes para cada tejado
  aspectssArray <- c()
  # Array con las producciones para placas de silicio de cada tejado
  silicePVProductionArray <- c()
  
  silicePVProductionArrayFiltered <- c()
  
  tejadosAProcesar <- ds$idTejado
  
  tejadoEnProceso <- 1
  
  # para cada tejado (para cada valor distinto de componente conexa) que cumpla las condiciones de pendientes, orientaciones, y area > a 4m2

  for (v in tejadosAProcesar) {
    

    #Obtenemos en un array las posiciones (indices) donde los valores de las componentes conexas son = al valor del tejado v que se está procesando
    indexes <- which (values (cC==v) )
    
    #if (length(indexes)>=4) { # cc (tejados) con área >=4m2. Procesando una componente conexa con menos de 4m2 "casca" la aplicacion en el bloque de cálculo
      
      #Calculamos la media de los valores de inclinaciones para las coordenadas del tejado
      avgSlope <- mean(slopeDiscretization [indexes])
      avgSlope<- format(round(avgSlope, 2), nsmall = 2)
      #Calculamos la media de los valores de orientaciones para las coordenadas del tejado
      avgAspect <- mean(aspectDiscretization [indexes])
      avgAspect<- format(round(avgAspect, 2), nsmall = 2)
      #Calculamos las coordenadas del tejado que se está procesando (latitudes y longitudes) y tomamos la primera latitud y longitud
      lats<- coordinates (slopeDiscretization) [indexes,1]
      lngs <- coordinates (slopeDiscretization) [indexes,2]
      
      # Convert lat lng -UTM- to lat lng 
      
      coords <- coordinates (slopeDiscretization) [indexes,]
      
      #print (length(coords))
      
      #print (paste("**********orientación tejado:",avgSlope))
      
      
      
      dataFrameCoords <- as.data.frame(coords)
      
      #print ("dataFrameCoords")
      #print (dataFrameCoords)
      
      colnames(dataFrameCoords) <- c("x_coord","y_coord")
      
      
      
      # Creamos un SpatialPoints object con las coordenadas del polígono en UTM
      cord.UTM = SpatialPoints(cbind(dataFrameCoords$x_coord, dataFrameCoords$y_coord), proj4string=CRS("+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" ))
      
      # Convertimos las coordenadas del polígono  UTM a lat lng 
      cord.dec <- spTransform(cord.UTM, CRS("+proj=longlat"))
      
      #Obtenemos un dataframe con las coordenadas del sistema Lat Lng
      dfT <- as.data.frame(cord.dec)
      
      #Area que se está procesando
      procesedArea <- ds %>%
        filter(ds$idTejado == v) 
      
      
      proc_area <- procesedArea$area_m2
      
      #Calculamos la producción si la orientación está entre 90 y 270
      
      #if ( (abs (as.numeric(avgAspect))>90) & ( abs(as.numeric(avgAspect))<270) ) {  
        
        showModal(modalDialog( title = paste("Calculando producción del tejado ", tejadoEnProceso, "de", nrow(ds), "tejados"), footer=NULL))
        
        print ("***************************")
        print ("lat")
        print (dfT [1,2])
        print ("lng")
        print (dfT [1,1])
        print ("aspect")
        print(as.numeric(avgAspect))
        print ("inc")
        print(as.numeric(avgSlope))
        print ("area")
        print (proc_area)
        
        prod_un_dia_vista <- calcularEnergiaDiaSiguiente (lat = dfT [1,2], lon = dfT [1,1], as.numeric(avgAspect), as.numeric(avgSlope), proc_area)
        
        #producción horaria en kw/h
        #print ("**************Prod próx. día***************")
       
        #producción horaria para ese tejado (5 y las 20)
        #HERE
        prod_un_dia_vista_filtrada <- prod_un_dia_vista #%>% 
          #filter(potencia_silicio_dia_h>0)
        
        print ("Producción un dia vista filtrada")
        print (prod_un_dia_vista_filtrada)
        
        #print ("Producción un dia vista sin filtrar")
        print (prod_un_dia_vista)
        
        prodProcesed <- round((prod_un_dia_vista)/1000,1)
        prodProcesedFiltered <- round((prod_un_dia_vista_filtrada)/1000,1)
        
        #producción total del tejado en kw/h

        silicePVProductionArray <- append (silicePVProductionArray,prodProcesed)
        silicePVProductionArrayFiltered <- append (silicePVProductionArrayFiltered,prodProcesedFiltered)
        
        #print (paste ("Producción con silicio 24",silicePVProductionArray))
        #print (paste ("Producción con silicio 16",silicePVProductionArrayFiltered))
        
        
        removeModal()
  
      #Tomamos la primera coordenada de lat y lng del tejado que se está procesando
      latsArray <- append (latsArray,round(dfT [1,2],2)) 
      lngsArray <- append (lngsArray,round(dfT [1,1],2) )
      
      slopesArray <- append (slopesArray,round(as.numeric(avgSlope),0))
      aspectssArray <- append (aspectssArray,round(as.numeric(avgAspect),0))
      
    # }
    
    tejadoEnProceso <- tejadoEnProceso +1
    
  }
  
  #add columns to ds
  
  ds$lat <- latsArray
  ds$lng <- lngsArray
  ds$slope <- slopesArray
  ds$orientation <- aspectssArray
  ds$produccion <- silicePVProductionArrayFiltered 
  
  #ds <- ds %>% select (-c('idTejado'))
  
  #names (ds) <- c("id","area","lat","lng","slope","orient.","kWh[5:00-20:00")
  
  #print ("Datos tejados")
  print (ds)
  
  #Eliminamos del ds los que tienen prod. -1; es decir las filas (tejados) para los que no se pudo calcular la prod. debido a que su orientacion no estaba entre 90 y 270
  # ds <- ds %>%
  #   filter(as.numeric(ds$produccion) >0) 
  # 
  return (ds)
  
}

#####################################CREATE connected component raster roof areas with ori and slope criteria############################################################################

createRoofAreaConnectedComponents <- function (formask) {
  comp_conexas <- clump(formask, directions=8)
  return (comp_conexas)
  
}


################################################CREATE CONNECTED COMPONENT AREAS DATASET####################################################

createRoofAreaDataset <- function (cc) {
  
  clumpFreq <- na.omit(freq(cc))
  
  #Creamos un dataframe
  df <-  as.data.frame(clumpFreq)
  
  names_ <- c("idTejado", "area_m2")
  colnames(df) <- names_
  
  return (df)
  
  
}

#######################################################
##################START PROCESSING CODE#################
########################################################

################Actualizar Municipios ####################

coordinatesMunicipios <- data.frame(ciudad = c('Malaga', 'Casares','Gaucin','Tolox'),
                         Latitude = c(36.719444, 36.43999824,36.5166646,36.6851814 ),
                         Longitude = c(-4.420000, -5.270332252,-5.3166654,-4.9093267))

 

################################################LOAD LIDAR IMAGES CATALOG####################################################


# Define server logic
shinyServer(function(input, output) {
   #Ocultar controles de pendientes, orientaciones y cálculos pv
   shinyjs::hide("aspectMulti2")
   shinyjs::hide("slopeMulti2")
   shinyjs::hide("showSlopes2")
   shinyjs::hide("showOrientation2")
   shinyjs::hide("estimar")

    #AOI
    feature <- NULL

    #Cuando cambiamos de ciudad, se lanza este código
    cityData=reactive({
      
      filteredCity <- coordinatesMunicipios %>% filter(ciudad==input$ciudad)
      #print (filteredCity)
      lat <- filteredCity$Latitude
      lng <- filteredCity$Longitude
      coords <- c(lat,lng)
      print (coords)
      
      #######################################################
      #load Lidar catalog from selected municipio
      dataFiles <- paste("LidarData","/",input$ciudad,sep="")
      print(paste("dataFiles",dataFiles))
      files<-list.files(path=dataFiles, full.names = TRUE)
      #cat(file=stderr(),files)
    
      print (paste("files:",files))
      #files <- list.files(path="LidarData/Malaga", full.names = TRUE)
      ctg <<- catalog(files)
     
      # getExtents for every lidar image in catalog
      extents <<- getExtent (ctg)
      
      ###############################################CARGAMOS histórico de 10 años DE TEMP Y RAD PARA CÁLCULOS L/P###########################################################################
      ciudadSeleccionada <- input$ciudad
      datos_aemet <<- cargaHistorico (ciudadSeleccionada)
      print (paste("Datos históricos cargados de:",input$ciudad))
      print (datos_aemet)
      #datos_aemet_Casares <- cargaHistorico ('Casares')
      
      #################
      #Preparamos los CSVS para los cálculos a c/p a partir de las coords del munic. select.
      
      response = prepareCSVS (coords)
      
      if (is_null(response)){ ## datos aemet no disponibles
       
          showModal(modalDialog(
            title = "Short-term forecasts not available today",
            "Error downloading data from AEMET weather service",
            easyClose = TRUE,
            footer = NULL
          ))
        
      }
        
      #######################################
      return(coords)
    })
    
  
    # Mapa Leaflet
    output$mymap <- renderLeaflet({
      
        data=cityData()
        
        #print ("data")
        
        #print (data)

        leaflet() %>% 
          
          setView(data[2], data[1], zoom = 13) %>%
        
          addProviderTiles('Esri.WorldImagery') %>%
            
          # Mostramos una cuadrícula por imagen LIDAR del catálogo
          addRectangles(
            lng1=extents$lng1, lat1=extents$lat1,
            lng2=extents$lng2, lat2=extents$lat2,
            fillColor = "transparent"
          ) %>%
      
          # añadimos la barra con herramientas gráficas (sólo rectángulo por ahora)
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions = FALSE,
            polygonOptions = FALSE,
            circleOptions = FALSE,
            rectangleOptions = drawRectangleOptions(),
            markerOptions = FALSE,
            circleMarkerOptions = FALSE, 
            singleFeature = FALSE,
            editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
          ) 
        
        
    })
    
    # Cuando pulsemos mostrar tejados (boton)
    observeEvent(input$showRoof, {
      
      #print ("inside showRoof")      
      
      # leemos las coordenadas del polígono trazado
      coords_ <- feature()$geometry$coordinates[[1]]
      #print ("coordenadas rectángulo leídas")
      #print (coords_)
      
      #Creamos un df con las coordenadas (coords) del polígono
      df <- data.frame(matrix(unlist(coords_), nrow=length(coords_), byrow=T))
      colnames(df) <- c("x_coord","y_coord")
      #print (df)
      
      # Creamos un SpatialPoints object con las coordenadas del polígono en (LAt,LNG)
      cord.dec = SpatialPoints(cbind(df$x_coord, df$y_coord), proj4string=CRS("+proj=longlat"))
      #print ("coord dec")
      #print (cord.dec)
      
      # Convertimos las coordenadas del polígono (lat,lng) a UTM que es como tenemos el catalogo y las imagenes LIDAR 
      cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "))
      
      #print ("UTM COOORDS")
      #print (cord.UTM)
      
      #Procedemos con el CROPPING (Corte del polígono trazado sobre el catalogo LIDAR)
      
      x_min <- xmin (extent (cord.UTM))
      x_max <- xmax (extent (cord.UTM))
      y_min <- ymin (extent (cord.UTM))
      y_max <- ymax (extent (cord.UTM))
      
      # print (x_min)
      # print (x_max)
      # print (y_min)
      # print (y_max)
      
      clipped <- clip_rectangle(ctg, x_min, y_min, x_max, y_max)
      #print ("Imagen Lidar del AOI clipeada")
      
      # Extraemos los tejados de la imagen
      segmentRoof <- segmentRoofs (clipped)
      #print ("Extracción de tejados realizada correctamente")
      
      #Mostramos la imagen 3D de los tejados (Lidar)
      plot (segmentRoof, backend="rgl")
      
      
      # Obtenemos el CHM modelo de alturas normalizado de los tejados del AOI
      #Make var accesible inside server 
      
      #print ("Calculando modelo NDSM")
      segmentRoofCHM <<- chmRoofs (segmentRoof)
      
      #print("Ploteando modelo de alturas normalizado en 2D")
      filteredHeight <- segmentRoofCHM
      filteredHeight [filteredHeight<2] <-  NA
      plot (filteredHeight)
      
      #Salida en el panel lateral del área disponible
      output$graficPlotTejados <- renderPlot({
        
        plot (filteredHeight, col=rainbow(10) , main = "Tejados disponibles y alturas")
        
      })
      
  
  
      #Activar para el concurso. Desactivar para app normal
      shinyjs::show("slopeMulti2")
      shinyjs::show("showSlopes2")
      shinyjs::show("aspectMulti2")
      shinyjs::show("showOrientation2")
      
    })
    
    # Cuando pulsemos mostrar pendientes (boton), mostramos el CHM con las pendientes de los tejados
    observeEvent(input$showSlopes2, {
      #print ("*********Inside slopes")
      filteredHeight <- segmentRoofCHM
      
      #Remove roofs with <2m
      filteredHeight [filteredHeight<2] <-  NA
      
      #Pendiente. Calculamos las pendientes de todos los tejados a partir del CHM
      slope <<- terrain(filteredHeight, opt=c( 'slope'), unit='degrees')
      
      # Filtramos los tejados que satisfacen las pendientes los criterios
      
      #slopeDiscretization <<- slope [slope %in% input$slopeMulti2]
      
      slopeDiscretization <<- slope
      
      #print ("Valores de pendiente seleccionados")
      #print (input$slopeMulti2[1])
      #print (input$slopeMulti2[2])
      
      slopeDiscretization[slopeDiscretization < input$slopeMulti2[1]  ] <<-  NA
      slopeDiscretization[slopeDiscretization > input$slopeMulti2[2] ] <<-  NA
      
      
      #slopeDiscretization[slopeDiscretization[] < input$slopeMulti2[1] || slopeDiscretization[] > input$slopeMulti2[2]  ] <<-  NA
      plot (slopeDiscretization)
      
      #Obtenemos los valores del select multiple de pendientes para ver que valores tiene como interés el usuario (10-20,20-30, ...)
      
     
      output$graficPlotSlopesAllRoofs <- renderPlot({
        
        plot (slope,  col=rainbow(10), main ="Pendiente en º de todos los tejados" )
        
      })
      
      output$graficPlot9 <- renderPlot({
        
        plot (slopeDiscretization, col = rainbow(10), main ="Tejados con pendientes de interés")
        
      })
      
      
    })
    
    # Cuando pulsemos mostrar pendientes (boton), mostramos el CHM con las orientaciones de los tejados
    observeEvent(input$showOrientation2, {
      #print ("*********Inside orientation")
      filteredHeight <- segmentRoofCHM
      
      #tejados menores de 2 m no son tejados
      filteredHeight [filteredHeight<2] <-  NA
      
     
      #Orientaciones Calculamos las orientaciones a partir del CHM
      aspect <<- terrain(filteredHeight, opt=c( 'aspect'), unit='degrees')
      
      
      # (Petición de Llanos) asignamos cero a los valores de orientación en las coordenadas donde la pendiente está [0-10)
      aspect [(slope>=0 & slope<10)] <<- 0
      
      # Discretizamos las orientaciones en intervalos de 10
      
      aspectDiscretization <<- aspect
      
      #Obtenemos los valores del select multiple de pendientes para ver que valores tiene como interés el usuario (10-20,20-30, ...)
      #print ("Valores del rango de orientaciones")
      #print (input$aspectMulti2[1])
      #print (input$aspectMulti2[2])
      
     
      aspectDiscretization[aspectDiscretization < input$aspectMulti2[1]  ] <<-  NA
      aspectDiscretization[aspectDiscretization > input$aspectMulti2[2] ] <<-  NA
      
      #aspectDiscretization <- aspect [aspect %in% input$aspectMulti2]
      
      plot (aspectDiscretization)
      
    
     
      #Salida en el panel lateral del área disponible
      output$graficPlotOrientationAllRoofs <- renderPlot({
        
        plot (aspect, col=rainbow(10), main ="Orientación en º de todos los tejados" )
        
      })
      
      output$graficPlotRoofFilteredOrientation <- renderPlot({
        
        plot (aspectDiscretization, col = rainbow(10), main ="Tejados con orientaciones de interés")
        
      })
      
      
      #Activar para concurso. Desactivar para app normal
      shinyjs::show("estimar") 
      
    })
    
   
    
   
    # Obseravmos el evento de estimación energética solar a l/p
    observeEvent(input$estimar, {
      
      areasRaster <- showRoofAreas2()

      #Generamos los tejados que cumplen las condiciones en base a las condiciones conexas de los píxeles que satisfacen los criterios de orientaciones y pendientes
      cC <<- createRoofAreaConnectedComponents(areasRaster)
      
      output$roofMatchtCriteria <- renderPlot({
        
        plot(na.omit(cC), col= rev(rainbow(max(na.omit(values(cC))))), main=paste("Tejados  disponibles:",""))
        
      })
      
      filterAream2 <- 4
      
      # Filtramos los tejados con un área menor a 4m2
      areaDS <- createRoofAreaDataset(cC)
      
      areaDS <- areaDS %>%
        filter(areaDS$area_m2>filterAream2)
      
      #print ("$$$$Tejados que pasan el filtro de area$$$$$")
      #print (areaDS)
      
      val_ <- unique(cC)
      
 
      #Obtener Dataset con las producciones de energía media diaria y demás características de los tejados
      fullDS <- addnewDataToDataset (areaDS)
      
      output$areatable = DT::renderDataTable({
        fullDS
      })
      
      output$totalRoofAreaBox <- renderInfoBox({
        infoBox(
          "Tejados válidos", nrow(fullDS), icon = icon("check"),
          color = "purple"
        )
      })
      
      #print ()
      #Obtenemos la producción total
      prodTotal <- fullDS %>%
        filter(as.numeric(fullDS$produccion)>0) %>%
        summarise(sum(fullDS$produccion))
      
      prodTotal <- round (prodTotal,0)
      
      #Renderizamos
      output$totalEnergyAreaBox <- renderInfoBox({
        infoBox(
          "Producción total diaria (kWh)", prodTotal, icon = icon("solar-panel"),
          color = "orange"
        )
      })
    
      
      
    } )
    
    
    
    ## Boton de estimación a un día vista
    
    observeEvent(input$estimarCP, {
      
      # capa raster con un 1 para cada pixel de los tejados que satisfacen las condiciones de ori y pend
      areasRaster <- showRoofAreas2()
     
      #Generamos el dataset de las áreas de los tejados que cumplen las condiciones
      cC <<- createRoofAreaConnectedComponents(areasRaster)
      
      output$roofMatchtCriteriaCP <- renderPlot({
        
        plot(na.omit(cC), col= rev(rainbow(max(na.omit(values(cC))))), main=paste("Tejados disponibles:",""))
        
      })
      
      areaDS <- createRoofAreaDataset(cC)
      
      #Nos quedamos con los tejados con área >4m2
      
      filterAream2 <- 4
      
      areaDS <- areaDS %>%
        filter(areaDS$area_m2>filterAream2)
      
      #id de cada componente conexa (de cada tejado a procesar)
      val_ <- unique(cC)
      
      #print ("areas DS")
      #print (areaDS)
      
      #ADataset completo con los cálculos de producción a un día vista
      fullDS <- addnewDataToDataset2 (areaDS)
      #view(fullDS)
     
     # fullDSParse <- fullDS %>% unnest_wider (produccion)
     
      fullDSParse <- unnest_wider(fullDS, produccion,names_sep="")
      names (fullDSParse) <- c("id","m2","lat","lng","inc.","orient.","5",
                                "6","7","8","9","10","11","12","13","14","15"
                                ,"16","17","18","19","20")
      
       #names (fullDSParse) <- c("id","m2","lat","lng","inc.","orient.",c(0,23))
      
      fullDSParse <-  fullDSParse %>% select(-c(lat, lng,id))
      
      #view (fullDSParse)
      
      # Muestra todos los tejados con la prod. horaria en cada tejado
      output$tejadosProcesados = DT::renderDataTable({
        fullDSParse
      })
      
      output$totalRoofAreaBox2 <- renderInfoBox({
        infoBox(
          "Tejados válidos", nrow(fullDS), icon = icon("check"),
          color = "purple"
        )
      })
      
      
      #print ("*******prodTotal*******")
      #print (fullDS$produccion)
      
      dfProd <- data.frame (fullDS$produccion)
      #print ("dfProd")
      #print (dfProd)
      
      #print ("mutatedDF")
      mutatted_df <- as.data.frame(t(dfProd))
      #print (mutatted_df)
      
      #print ("mutatedDF[1]")
      mutatted_df <- as.data.frame(t(dfProd))
      #print (mutatted_df[1,])

      #get number of roofs
      #nRoofs <- ncol(dfProd)
      
      #namesArray <- c(1:nRoofs)
      
     
      names (mutatted_df) <- c("kWh_5","kWh_6","kWh_7","kWh_8","kWh_9","kWh_10",
                                  "kWh_11","kWh_12","kWh13","kWh_14","kWh_15","kWh_16",
                                  "kWh_17","kWh_18","kWh19","kWh_20") 
      
      #print ("finalDFHorario")
      #print (mutatted_df)
      
      #Obtenemos la producción total
      # prodTotal <- fullDS %>%
      #   summarise(sum(fullDS$produccion))
      # 
      
      sumaFila <- colSums(Filter(is.numeric, mutatted_df))

      #print ("*******DF CON PROD TOTAL HORARIA DE CADA TEJADO******")
      #print (sumaFila)
      
      sumaFila <- sumaFila[1:16]
      
      #print ("Suma fila:")
      #print (sumaFila)
      
      #print ("Suma fila df:")
      kWh <- sumaFila
      #print (data.frame(kWh))
      
      dfFila <- data.frame (sumaFila)
      output$produccionHorariaTotalTejados <- DT::renderDataTable(DT::datatable({
        data <- data.frame(kWh)
        
        #sumaFila
      }))
      
      
      #Renderizamos
      # output$totalEnergyAreaBox2 <- renderInfoBox({
      #   infoBox(
      #     "Producción total horaria", prodTotal, icon = icon("solar-panel"),
      #     color = "orange"
      #   )
      # })
      
   
      output$prediccionHoraria = DT::renderDataTable({
        prodHoraria
      })
      
      
      
    } )
    
    
    # Observamos los eventos de selección gráfica (trazar polígono, ..) de forma reactiva en la variable función feature
    feature <- eventReactive( input$mymap_draw_new_feature, {
      input$mymap_draw_new_feature
    })
    
    #Salida en el panel lateral del área disponible
    output$distPlot <- renderPlot({
        
      plot(ctg)
  
    })
    
    #Combo aspect
    output$aspect <- renderPrint({
      input$aspectMulti
    })
    
    #Combo slope
    output$slope <- renderPrint({
      input$slopeMulti
    })

})
