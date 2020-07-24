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
library (rgdal)
library(RStoolbox)
library(RGISTools)
library(zoom)
library(excelR)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(widgetframe)
library (lidR)
library(rgl)
library(rasterVis)
library(shiny)
library(shinythemes)
library(ggmap)
library(leaflet.extras)
library(sp)


#Add energy production calculation functions script
source("funciones_prod_fv.R")


############################getExtent()#####################################################################

## Para cada imagen Lidar del catálogo, obtenemos su extensión, y devolvemos un df con las extensiones ##

############################################################################################################

getExtent <- function(catalogo) {
  
  extentList <- c ()
  
  for (i in 1:length(catalogo) ) {
    
    im <- catalogo [i,]
  
    #convert UTM coordinates to lat lng
    longlats <- spTransform(im, CRS("+proj=longlat")) 
    
    #Obtenemos el rectangulo que abarca todas las imágenes LIDAR
    e <- extent (longlats)

    #x lng y lat sW (abajo izq)   NE (arriba derecha)
    lng1 <- xmin (extent (e))
    lat1 <- ymin (extent (e))
    lng2 <- xmax (extent (e))
    lat2 <- ymax (extent (e))
    
    extentList[[i]] <- c (lng1,lat1,lng2,lat2)
    
  }
  
  #dataFrame with img extents
  dfExtents = t(as.data.frame(extentList, optional = TRUE))
  colnames(dfExtents) = c("lng1", "lat1", "lng2", "lat2" )
  rownames(dfExtents) = 1:length (extentList)
  
  return(as.data.frame(dfExtents))
  
}



###################################### segmentRoofs () ##########################################################

## Función que segmenta los tejados del área de interés 

segmentRoofs<- function (aoi) {
  
  segmentedRoof <- lasfilter(aoi,Classification == 6L | Classification == 2L )
  
  return (segmentedRoof)
  
}

#################################################################################################################

###################################### chmRoofs () ##########################################################

## Función que rasteriza los tejados segmentados del área de interés 

chmRoofs<- function (aoiSegmented) {
  
  thr <- c(0,2,5,10,15)
  edg <- c(0, 1.5)
  
  # Eliminamos el suelo de la nube de puntos. Normalizamos cada imagen o nube de puntos LIDAR
  lasNormalized <- lasnormalize (aoiSegmented,tin())
  
  chm <- grid_canopy(lasNormalized, 1, pitfree(thr, edg))
  
  plot(chm,main="Modelo de alturas normalizado. CHM")
  
  return (chm)
  
}

################################################SHOW ROOF AREAS####################################################

## Función que muestra las áreas de los tejados que cumplen las condiciones de filtrado de los criterios de inclinación y pendiente 

showRoofAreas <- function () {
  
  formask <- setValues(raster(segmentRoofCHM), NA)
  
  if (norte) {
    if (planos) {
      formask[ (aspectDiscretization==1) & (slopeDiscretization==1)  ] <- 1
    }
    if (ligeramenteInclinados) {
      formask[ (aspectDiscretization==1) & (slopeDiscretization==2)  ] <- 1
    }
    if (inclinados) {
      formask[ (aspectDiscretization==1) & (slopeDiscretization==3)  ] <- 1
    }
    if (muyInclinados) {
      formask[ (aspectDiscretization==1) & (slopeDiscretization==4)  ] <- 1
    }
  }
  
  if (este) {
    if (planos) {
      formask[ (aspectDiscretization==2) & (slopeDiscretization==1)  ] <- 1
    }
    if (ligeramenteInclinados) {
      formask[ (aspectDiscretization==2) & (slopeDiscretization==2)  ] <- 1
    }
    if (inclinados) {
      formask[ (aspectDiscretization==2) & (slopeDiscretization==3)  ] <- 1
    }
    if (muyInclinados) {
      formask[ (aspectDiscretization==2) & (slopeDiscretization==4)  ] <- 1
    }
  }
  
  if (sur) {
    if (planos) {
      formask[ (aspectDiscretization==3) & (slopeDiscretization==1)  ] <- 1
    }
    if (ligeramenteInclinados) {
      formask[ (aspectDiscretization==3) & (slopeDiscretization==2)  ] <- 1
    }
    if (inclinados) {
      formask[ (aspectDiscretization==3) & (slopeDiscretization==3)  ] <- 1
    }
    if (muyInclinados) {
      formask[ (aspectDiscretization==3) & (slopeDiscretization==4)  ] <- 1
    }
  }
  
  if (oeste) {
    if (planos) {
      formask[ (aspectDiscretization==4) & (slopeDiscretization==1)  ] <- 1
    }
    if (ligeramenteInclinados) {
      formask[ (aspectDiscretization==4) & (slopeDiscretization==2)  ] <- 1
    }
    if (inclinados) {
      formask[ (aspectDiscretization==4) & (slopeDiscretization==3)  ] <- 1
    }
    if (muyInclinados) {
      formask[ (aspectDiscretization==4) & (slopeDiscretization==4)  ] <- 1
    }
  }
  
  plot (formask)
  
  return (formask)
  
  
  

}



################################################SHOW ROOF AREAS2 (Concurso)####################################################

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
  
  print ("aspectValues")
  print (aspectValues)
  print ("aspectDisc")
  print (aspectDiscretization)
  
  print ("slopeValues")
  print (slopeValues)
  print ("slopeDisc")
  print (slopeDiscretization)
  
  formask[ (aspectDiscretization %in% aspectValues ) & (slopeDiscretization %in% slopeValues)  ] <- 1
  
  plot (formask)
  
  return (formask)
 
}


#####################################addnewDataToDataset ############################################################################

## Return a daset with roof slopes, orientation, lat, lng

addnewDataToDataset <- function (ds) {
  
  val_ <- unique(cC)
  
  #print (values)
  
  
  
  # Array con las latitudes para cada edificio
  latsArray <- c()
  # Array con las lngitudes para cada edificio
  lngsArray <- c()
  # Array con las latitudes para cada edificio
  slopesArray <- c()
  # Array con las lngitudes para cada edificio
  aspectssArray <- c()
  # Array con las producciones para placas de silicio de cada edificio
  silicePVProductionArray <- c()
  
  tejadosAProcesar <- ds$idArea
  
  tejadoEnProceso <- 1
  
  # para cada tejado (para cada valor distinto de componente conexa) que cumpla las condiciones de pendientes, orientaciones, y superficie
  
  for (v in tejadosAProcesar) {
    
   
    #Obtenemos en un array las posiciones (indices) donde los valores de las componentes conexas son = al valor del tejado v que se está procesando
    indexes <- which (values (cC==v) )
    
    if (length(indexes)>=4) { # cc (tejados) con área >=4m2. Procesando una componente conexa con menos de 4m2 "casca" la aplicacion
      
      #Calculamos la media de los valores de inclinaciones para las coordenadas del tejado
      avgSlope <- mean(slope [indexes])
      avgSlope<- format(round(avgSlope, 2), nsmall = 2)
      #Calculamos la media de los valores de orientaciones para las coordenadas del tejado
      avgAspect <- mean(aspect [indexes])
      avgAspect<- format(round(avgAspect, 2), nsmall = 2)
      #Calculamos las coordenadas del tejado que se está procesando (latitudes y longitudes) y tomamos la primera latitud y longitud
      lats<- coordinates (slope) [indexes,1]
      lngs <- coordinates (slope) [indexes,2]
      
      # Convert lat lng -UTM- to lat lng 
  
      coords <- coordinates (slope) [indexes,]
      
      #print (length(coords))
      
      
      
        
        showModal(modalDialog( title = paste("Calculando producción del tejado ", tejadoEnProceso, "de", nrow(ds), "tejados"), footer=NULL))
        
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
          filter(ds$idArea == v) 
        
        
        proc_area <- procesedArea$area_m2
        
        #Calculamos la producción si la orientación está entre 90 y 270
        
      if ( (abs (as.numeric(avgAspect))>90) & ( abs(as.numeric(avgAspect))<270) ) {  
        
        produccion <- extraccion_valores_medios (latitud = dfT [1,2], longitud = dfT [1,1], as.numeric(avgSlope), as.numeric(avgAspect), proc_area, datos_aemet)
        
        prodProcesed <- round(produccion$Energía_media_diaria_producida_Wh_silicio/1000,1)
        #print (produccion)
        
        silicePVProductionArray <- append (silicePVProductionArray,prodProcesed)
        
      } 
        
      
      else { ##Cuando no podamos calcular la produción por no tener orientacion entre 90 y 270, el valor de prod. será -1 (luego se elminian del dataframe)
        silicePVProductionArray <- append (silicePVProductionArray,-1)
      }
        
      #Tomamos la primera coordenada de lat y lng del tejado que se está procesando
      latsArray <- append (latsArray,dfT [1,2]) 
      lngsArray <- append (lngsArray,dfT [1,1]) 
        
      slopesArray <- append (slopesArray,avgSlope)
      aspectssArray <- append (aspectssArray,avgAspect)
      
      removeModal()
      
    }
    
    tejadoEnProceso <- tejadoEnProceso +1
 
  }
  
  #add columns to ds
  
  ds$lat <- latsArray
  ds$lng <- lngsArray
  ds$slope <- slopesArray
  ds$orientation <- aspectssArray
  ds$produccion <- silicePVProductionArray
  
  #Eliminamos del ds los que tienen prod. -1; es decir las filas (tejados) para los que no se pudo calcular la prod. debido a que su orientacion no estaba entre 90 y 270
  ds <- ds %>%
    filter(as.numeric(ds$produccion) >0) 
  
  return (ds)
  
}


#####################################

#####################################CREATE connected component raster roof areas############################################################################

createRoofAreaConnectedComponents <- function (formask) {
  
  comp_conexas <- clump(formask, directions=8)
  return (comp_conexas)
  
}


################################################CREATE CONNECTED COMPONENT AREAS DATASET####################################################

createRoofAreaDataset <- function (cc) {
  
  clumpFreq <- na.omit(freq(cc))
  
  #Creamos un dataframe
  df <-  as.data.frame(clumpFreq)
  
  names_ <- c("idArea", "area_m2")
  colnames(df) <- names_
  
  return (df)
  
  
}


################################################LOAD LIDAR IMAGES CATALOG####################################################

#load Lidar catalog from data folder

dataFiles <- "data"
files<-list.files(path=dataFiles, full.names = TRUE)
ctg <- catalog(files)

# getExtents for every lidar image in catalog
extents <- getExtent (ctg)


###############################################CARGAMOS DATOS DE MALAGA DEL AEMET############################################################################


datos_aemet <- datos_malaga ()


#################

# Define server logic
shinyServer(function(input, output) {
  
   #Disable buttons

  # botones y controles para app normal
   shinyjs::hide("showSlopes")
   shinyjs::hide("showOrientation")
   shinyjs::hide("aspectMulti")
   shinyjs::hide("slopeMulti")
   shinyjs::hide("showArea")
   
   # para Concurso
   shinyjs::hide("aspectMulti2")
   shinyjs::hide("slopeMulti2")
   shinyjs::hide("showSlopes2")
   shinyjs::hide("showOrientation2")
   shinyjs::hide("showArea2")
   
 
   # Roof preferences vars
   planos <<- FALSE
   ligeramenteInclinados <<- FALSE
   inclinados <<- FALSE
   muyInclinados <<- FALSE
   
   planos <<- FALSE
   ligeramenteInclinados <<- FALSE
   inclinados <<- FALSE
   muyInclinados <<- FALSE

   
    #AOI
    feature <- NULL
    
    print ("Hola")
    
  
    # Mapa Leaflet
    output$mymap <- renderLeaflet({
      
        leaflet() %>% 
        
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
    
    # Cuando pulsemos mostrar tejados (boton), obtenemos el AOI en la variable feature
    observeEvent(input$showRoof, {
      
      # leemos las coordenadas del polígono trazado
      coords_ <- feature()$geometry$coordinates[[1]]
      print (coords_)
      
      #Creamos un df con las coordenadas (coords) del polígono
      df <- data.frame(matrix(unlist(coords_), nrow=length(coords_), byrow=T))
      colnames(df) <- c("x_coord","y_coord")
      print (df)
      
      # Creamos un SpatialPoints object con las coordenadas del polígono en (LAt,LNG)
      cord.dec = SpatialPoints(cbind(df$x_coord, df$y_coord), proj4string=CRS("+proj=longlat"))
      
      print ("coord dec")
      print (cord.dec)
      
      # Convertimos las coordenadas del polígono (lat,lng) a UTM que es como tenemos el catalogo y las imagenes LIDAR 
      cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "))
      
      print ("UTM COOORDS")
      print (cord.UTM)
      
      #Procedemos con el CROPPING (Corte del polígono trazado sobre el catalogo LIDAR)
      
      x_min <- xmin (extent (cord.UTM))
      x_max <- xmax (extent (cord.UTM))
      y_min <- ymin (extent (cord.UTM))
      y_max <- ymax (extent (cord.UTM))
      
      print (x_min)
      print (x_max)
      print (y_min)
      print (y_max)
      
      clipped <- lasclipRectangle(ctg, x_min, y_min, x_max, y_max)
      
      # Procesamos los tejados de la imagen eliminando vegetación y arboles
      
      
      segmentRoof <- segmentRoofs (clipped)
      
      plot (segmentRoof, backend="rgl")
      
      
      # Obtenemos el CHM modelo de alturas normalizado de los tejados del AOI
      #Make var accesible inside server 
      segmentRoofCHM <<- chmRoofs (segmentRoof)
      plot (segmentRoofCHM)
      
   
      #Salida en el panel lateral del área disponible
      output$graficPlot <- renderPlot({
        
        plot (segmentRoofCHM, main = "Tejados disponibles y alturas")
        
      })
      
      # Desactivar para el consurso. Activar como app normal
      #shinyjs::show("showSlopes")
      #shinyjs::show("slopeMulti")
      
      
      
      #Activar para el concurso. Desactivar para app normal
      shinyjs::show("slopeMulti2")
      shinyjs::show("showSlopes2")
  
      

    })
    
    # Cuando pulsemos mostrar pendientes (boton), mostramos el CHM con las pendientes de los tejados
    observeEvent(input$showSlopes, {
      
      #Obtenemos los valores del select multiple de pendientes para ver que valores tiene como interés el usuario (muy inclinados, ... planos y ligeramente inclinados, todos, ...)
      
      print (input$slopeMulti)
      
      #longitud de los arrays de string con las selecciones de preferencias de pendientes y orientaciones
      lengthSlopeMulti <- length(input$slopeMulti)
      

      planos <<- FALSE
      ligeramenteInclinados <<- FALSE
      inclinados <<- FALSE
      muyInclinados <<- FALSE
      
      if ("Planos" %in% input$slopeMulti) {
        planos <<- TRUE
      }
        
      if ("Ligeramente inclinados" %in% input$slopeMulti) {
        ligeramenteInclinados <<- TRUE
      }
        
      if ("Inclinados" %in% input$slopeMulti) {
        inclinados <<- TRUE
      }
          
      if ("Muy inclinados" %in% input$slopeMulti) {
        muyInclinados <<- TRUE
      }
      
    
      filteredHeight <- segmentRoofCHM
        
      filteredHeight [filteredHeight<2] <-  NA
      
      #Pendiente
      slope <<- terrain(filteredHeight, opt=c( 'slope'), unit='degrees')

      # Discretizamos las pendientes
      
      # Clasificamos a N,E,S,W la capa Raster Aspect con la orientación en º
      
      slopeDiscretization <<- slope
      
      if (planos) {
        slopeDiscretization [slopeDiscretization<5] <<- 1 ##flat
      }
      else {
        slopeDiscretization [slopeDiscretization<5] <<- NA ##flat
      }
      
      if (ligeramenteInclinados) {
        slopeDiscretization [slopeDiscretization>=5 & slopeDiscretization<22] <<- 2 #slight
      }
      else {
        slopeDiscretization [slopeDiscretization>=5 & slopeDiscretization<22] <<- NA #slight
      }
      
      if (inclinados) {
        slopeDiscretization [slopeDiscretization>=22 & slopeDiscretization<45] <<- 3 #Normal
      }
      else {
        slopeDiscretization [slopeDiscretization>=22 & slopeDiscretization<45] <<- NA #Normal
      }
      
      if (muyInclinados) {
        slopeDiscretization [slopeDiscretization>=45] <<- 4 #Step
      }
      else {
        slopeDiscretization [slopeDiscretization>=45] <<- NA #Step
      }
      
      
      

      #Salida en el panel lateral del área disponible
      output$graficPlot <- renderPlot({
        
        plot (slopeDiscretization, col= rainbow(lengthSlopeMulti), main ="1.Planos, 2.Ligeramente inclinados, 3.Inclinados, 4.Muy inclinados")
        
      })
      
      #Salida en el panel lateral del área disponible
      output$graficPlot2 <- renderPlot({
        
        plot (slope, col= rainbow(4), main ="Pendiente º" )
        
      })
      
      #Salida en el panel lateral del área disponible
      output$graficPlot3 <- renderImage({
        
        list(src = "www/images/roof_class.png", contentType = "image/png", width =600, height =500)
        
      }, deleteFile = FALSE)
      
      #Activar para el concurso. Desactivar para app normal
      shinyjs::show("aspectMulti")
      # Desactivar como app normal. Activar para el concurso
      shinyjs::show("showOrientation")
 
      
    })
    
    
    # Cuando pulsemos mostrar pendientes (boton), mostramos el CHM con las pendientes de los tejados
    observeEvent(input$showSlopes2, {
      
      # tipos de inclinaciones en las que está interesado el usuario
      inc_0_10 <<- FALSE
      inc_10_20 <<- FALSE
      inc_20_30 <<- FALSE
      inc_30_40 <<- FALSE
      inc_40_50 <<- FALSE
      inc_50_60 <<- FALSE
      inc_60_70 <<- FALSE
      
      
      filteredHeight <- segmentRoofCHM
      
      filteredHeight [filteredHeight<2] <-  NA
      
      #Pendiente. Calculamos las pendientes a partir del CHM
      slope <<- terrain(filteredHeight, opt=c( 'slope'), unit='degrees')
      
      # Discretizamos las pendientes en intervalos de 10
      
      slopeDiscretization <<- slope
      
      #Obtenemos los valores del select multiple de pendientes para ver que valores tiene como interés el usuario (10-20,20-30, ...)
      
      print (input$slopeMulti2)
      
      #longitud de los arrays de string con las selecciones de preferencias de pendientes y orientaciones
      lengthSlopeMulti2 <- length(input$slopeMulti2)
      
      #slopeDiscretization [slopeDiscretization>-1] <- NA  # Asignamos a todos los valores de la capa Raster NA
      
      #Eliminamos los tejados con inclinación mayor o iguala 70 º 
      
      #print (slopeDiscretization [slopeDiscretization>=70])
      #slopeDiscretization [slopeDiscretization>=70] <- NA
      
      if ("[0-10)" %in% input$slopeMulti2) {
        print ("en 1")
        inc_0_10 <<- TRUE
      }
      
      
      if ("[10-20)" %in% input$slopeMulti2) {
        print ("en 2")
        inc_10_20 <<- TRUE
      }
      
      if ("[20-30)" %in% input$slopeMulti2) {
        inc_20_30 <<- TRUE
      }
      
      if ("[30-40)" %in% input$slopeMulti2) {
        inc_30_40 <<- TRUE
      }
      
      if ("[40-50)" %in% input$slopeMulti2) {
        inc_40_50 <<- TRUE
      }
      
      if ("[50-60)" %in% input$slopeMulti2) {
        inc_50_60 <<- TRUE
      }
      
      if ("[60-70)" %in% input$slopeMulti2) {
        inc_60_70 <<- TRUE
      }
      
      if (inc_0_10) {
        slopeDiscretization [slopeDiscretization>=0 & slopeDiscretization<10] <<- 1 
        
      }
      else {
        slopeDiscretization [slopeDiscretization>=0 & slopeDiscretization<10] <<- NA
      }
      
      if (inc_10_20) {
        slopeDiscretization [slopeDiscretization>=10 & slopeDiscretization<20] <<- 2 
      }
      else {
        slopeDiscretization [slopeDiscretization>=10 & slopeDiscretization<20] <<- NA
      }
      
      if (inc_20_30) {
        slopeDiscretization [slopeDiscretization>=20 & slopeDiscretization<30] <<- 3 
      }
      else {
        slopeDiscretization [slopeDiscretization>=20 & slopeDiscretization<30] <<- NA
      }
      
      if (inc_30_40) {
        slopeDiscretization [slopeDiscretization>=30 & slopeDiscretization<40] <<- 4 
      }
      else {
        slopeDiscretization [slopeDiscretization>=30 & slopeDiscretization<40] <<- NA
      }
      
      if (inc_40_50) {
        slopeDiscretization [slopeDiscretization>=40 & slopeDiscretization<50] <<- 5 
      }
      else {
        slopeDiscretization [slopeDiscretization>40 & slopeDiscretization<50] <<- NA
      }
      
      if (inc_50_60) {
        slopeDiscretization [slopeDiscretization>=50 & slopeDiscretization<60] <<- 6 
      }
      else {
        slopeDiscretization [slopeDiscretization>=50 & slopeDiscretization<60] <<- NA
      }
      
      if (inc_60_70) {
        slopeDiscretization [slopeDiscretization>=60 & slopeDiscretization<=70] <<- 7 
      }
      else {
        slopeDiscretization [slopeDiscretization>=60 & slopeDiscretization<=70] <<- NA
      }
      
      slopeDiscretization [slopeDiscretization>70] <<- NA
      
      
      print (slopeDiscretization)
      
      
      #print (inf_slope_val)
      #print (sup_slope_val)
      
     # print (slopeDiscretization [slopeDiscretization>=inf_slope_val & slopeDiscretization<sup_slope_val ])
      
      #slopeDiscretization [slopeDiscretization>=inf_slope_val & slopeDiscretization<sup_slope_val ] <<- 1 
      
       #print (!is.na(values(slopeDiscretization)))
      
      #slopeDiscretization [slopeDiscretization<inf_slope_val | slopeDiscretization>=sup_slope_val ] <<- NA ## [0-10)
      
    
      #Salida en el panel lateral del área disponible
      output$graficPlot <- renderPlot({
        
        plot (slopeDiscretization, col = rainbow(lengthSlopeMulti2), main ="1.[0-10), 2.[10-20),3.[20-30), 4.[30-40), 5.[40-50), 6.[50-60), 7.[60-70)")
        
      })
      
      #Salida en el panel lateral del área disponible
      output$graficPlot2 <- renderPlot({
        
        plot (slope, main ="Pendiente º" )
        
      })
      
      #Salida en el panel lateral del área disponible
      output$graficPlot3 <- renderImage({
        
        list(src = "www/images/roof_class.png", contentType = "image/png", width =600, height =500)
        
      }, deleteFile = FALSE)
      
      
      
      #Activar para el concurso. Desactivar para app normal
      shinyjs::show("aspectMulti2")
      # Desactivar como app normal. Activar para el concurso
      shinyjs::show("showOrientation2")

      
      
    })
    
    
    
    
    # Cuando pulsemos mostrar pendientes (boton), mostramos el CHM con las pendientes de los tejados
    observeEvent(input$showOrientation, {
      
      filteredHeight <- segmentRoofCHM
      
      filteredHeight [filteredHeight<2] <-  NA
      
      lengthAspectMulti <- length(input$aspectMulti)
      
      
      # Observamos las preferencias en cuanto a orientaciones de tejados del usuario 
      
      norte <<- FALSE
      este <<- FALSE
      sur <<- FALSE
      oeste <<- FALSE
      
      if ("Norte" %in% input$aspectMulti) {
        norte <<- TRUE
      }
      
      if ("Este" %in% input$aspectMulti) {
        este <<- TRUE
      }
      
      if ("Sur" %in% input$aspectMulti) {
        sur <<- TRUE
      }
      
      if ("Oeste" %in% input$aspectMulti) {
        oeste <<- TRUE
      }
      
   
      aspect <<- terrain(filteredHeight, opt=c( 'aspect'), unit='degrees')
      
      aspect [(slope>=0 & slope<10)] <<- 0
      
      
      # Clasificamos a N,E,S,W la capa Raster Aspect con la orientación en º
      
      aspectDiscretization <<- aspect
      
      if (norte) {
        aspectDiscretization [aspectDiscretization>=0 & aspectDiscretization<45] <<- 1 ##N
        aspectDiscretization [aspectDiscretization>=315 & aspectDiscretization<=360] <<- 1 #N
      }
      else {
        aspectDiscretization [aspectDiscretization>=0 & aspectDiscretization<45] <<- NA ##N
      }
      
      if (este) {
        aspectDiscretization [aspectDiscretization>=45 & aspectDiscretization<135] <<- 2 #E
      }
      else {
        aspectDiscretization [aspectDiscretization>=45 & aspectDiscretization<135] <<- NA #E
      }
      
      if (sur) {
        aspectDiscretization [aspectDiscretization>=135 & aspectDiscretization<225] <<- 3 #S
      }
      else {
        aspectDiscretization [aspectDiscretization>=135 & aspectDiscretization<225] <<- NA #S
      }
      
      if (oeste) {
        aspectDiscretization [aspectDiscretization>=225 & aspectDiscretization<315] <<- 4 #W
      }
      else {
        aspectDiscretization [aspectDiscretization>=225 & aspectDiscretization<315] <<- NA #W
      }
      
      
    
      #Salida en el panel lateral del área disponible
      output$graficPlot <- renderPlot({
        
        
        plot (aspectDiscretization, col= rainbow(lengthAspectMulti), main ="Orientación 1.N, 2.E, 3.S, 4.W")
        
      })
      
      #Salida en el panel lateral del área disponible
      output$graficPlot4 <- renderPlot({
        
        plot(aspect, main ="Orientacion º")
        
      })
      
      #Salida en el panel lateral del área disponible
      output$graficPlot3 <- renderImage({
        
        list(src = "www/images/disc.png", contentType = "image/png", width =600, height =500)
        
      }, deleteFile = FALSE)
      
      # App normal. No concurso
      shinyjs::show("showArea") 
      
      
    })
    
    
    
    # Cuando pulsemos mostrar pendientes (boton), mostramos el CHM con las pendientes de los tejados
    observeEvent(input$showOrientation2, {
      
      # tipos de inclinaciones en las que está interesado el usuario
      ori_0_10 <<- FALSE
      ori_10_20 <<- FALSE
      ori_20_30 <<- FALSE
      ori_30_40 <<- FALSE
      ori_40_50 <<- FALSE
      ori_50_60 <<- FALSE
      ori_60_70 <<- FALSE
      ori_70_80 <<- FALSE
      ori_80_90 <<- FALSE
      ori_90_100 <<- FALSE
      
      ori_100_110 <<- FALSE
      ori_110_120 <<- FALSE
      ori_120_130 <<- FALSE
      ori_130_140 <<- FALSE
      ori_140_150 <<- FALSE
      ori_150_160 <<- FALSE
      ori_160_170 <<- FALSE
      ori_170_180 <<- FALSE
      ori_180_190 <<- FALSE
      ori_190_200 <<- FALSE
      
      ori_200_210 <<- FALSE
      ori_210_220 <<- FALSE
      ori_220_230 <<- FALSE
      ori_230_240 <<- FALSE
      ori_240_250 <<- FALSE
      ori_250_260 <<- FALSE
      ori_260_270 <<- FALSE
      ori_270_280 <<- FALSE
      ori_280_290 <<- FALSE
      ori_290_300 <<- FALSE
      
      ori_300_310 <<- FALSE
      ori_310_320 <<- FALSE
      ori_320_330 <<- FALSE
      ori_330_340 <<- FALSE
      ori_340_350 <<- FALSE
      ori_350_360 <<- FALSE
    
      
      filteredHeight <- segmentRoofCHM
      
      filteredHeight [filteredHeight<2] <-  NA
      
      #Orientaciones Calculamos las orientaciones a partir del CHM
      aspect <<- terrain(filteredHeight, opt=c( 'aspect'), unit='degrees')
      
      # asignamos cero a los valores de orientación en las coordenadas donde la pendiente está [0-10)
      aspect [(slope>=0 & slope<10)] <<- 0
      
      # Discretizamos las orientaciones en intervalos de 10
      
      aspectDiscretization <<- aspect
      
      #Obtenemos los valores del select multiple de pendientes para ver que valores tiene como interés el usuario (10-20,20-30, ...)
      
      print (input$aspectMulti2)
      
      #longitud de los arrays de string con las selecciones de preferencias  orientaciones
      lengthAspectMulti2 <- length(input$aspectMulti2)
      

      if ("[0-10)" %in% input$aspectMulti2) {
        ori_0_10 <<- TRUE
      }
      
      
      if ("[10-20)" %in% input$aspectMulti2) {
        ori_10_20 <<- TRUE
      }
      
      if ("[20-30)" %in% input$aspectMulti2) {
        ori_20_30 <<- TRUE
      }
      
      if ("[30-40)" %in% input$aspectMulti2) {
        ori_30_40 <<- TRUE
      }
      
      if ("[40-50)" %in% input$aspectMulti2) {
        ori_40_50 <<- TRUE
      }
      
      if ("[50-60)" %in% input$aspectMulti2) {
        ori_50_60 <<- TRUE
      }
      
      if ("[60-70)" %in% input$aspectMulti2) {
        ori_60_70 <<- TRUE
      }
      
      if ("[70-80)" %in% input$aspectMulti2) {
        ori_70_80 <<- TRUE
      }
      
      if ("[80-90)" %in% input$aspectMulti2) {
        ori_80_90 <<- TRUE
      }
      
      if ("[90-100)" %in% input$aspectMulti2) {
        ori_90_100 <<- TRUE
      }
      
      
      if ("[100-110)" %in% input$aspectMulti2) {
        ori_100_110 <<- TRUE
      }
      
      
      if ("[110-120)" %in% input$aspectMulti2) {
        ori_110_120 <<- TRUE
      }
      
      if ("[120-130)" %in% input$aspectMulti2) {
        ori_120_130 <<- TRUE
      }
      
      if ("[130-140)" %in% input$aspectMulti2) {
        ori_130_140 <<- TRUE
      }
      
      if ("[140-150)" %in% input$aspectMulti2) {
        ori_140_150 <<- TRUE
      }
      
      if ("[150-160)" %in% input$aspectMulti2) {
        ori_150_160 <<- TRUE
      }
      
      if ("[160-170)" %in% input$aspectMulti2) {
        ori_160_170 <<- TRUE
      }
      
      if ("[170-180)" %in% input$aspectMulti2) {
        ori_170_180 <<- TRUE
      }
      
      if ("[180-190)" %in% input$aspectMulti2) {
        ori_180_190 <<- TRUE
      }
      
      if ("[190-200)" %in% input$aspectMulti2) {
        ori_190_200 <<- TRUE
      }
      
      
      
      if ("[200-210)" %in% input$aspectMulti2) {
        ori_200_210 <<- TRUE
      }
      
      
      if ("[210-220)" %in% input$aspectMulti2) {
        ori_210_220 <<- TRUE
      }
      
      if ("[220-230)" %in% input$aspectMulti2) {
        ori_220_230 <<- TRUE
      }
      
      if ("[230-240)" %in% input$aspectMulti2) {
        ori_230_240 <<- TRUE
      }
      
      if ("[240-250)" %in% input$aspectMulti2) {
        ori_240_250 <<- TRUE
      }
      
      if ("[250-260)" %in% input$aspectMulti2) {
        ori_250_260 <<- TRUE
      }
      
      if ("[260-270)" %in% input$aspectMulti2) {
        ori_260_270 <<- TRUE
      }
      
      if ("[270-280)" %in% input$aspectMulti2) {
        ori_270_280 <<- TRUE
      }
      
      if ("[280-290)" %in% input$aspectMulti2) {
        ori_280_290 <<- TRUE
      }
      
      if ("[290-300)" %in% input$aspectMulti2) {
        ori_290_300 <<- TRUE
      }
      
      
      if ("[300-310)" %in% input$aspectMulti2) {
        ori_300_310 <<- TRUE
      }
      
      
      if ("[310-320)" %in% input$aspectMulti2) {
        ori_310_320 <<- TRUE
      }
      
      if ("[320-330)" %in% input$aspectMulti2) {
        ori_320_330 <<- TRUE
      }
      
      if ("[330-340)" %in% input$aspectMulti2) {
        ori_330_340 <<- TRUE
      }
      
      if ("[340-350)" %in% input$aspectMulti2) {
        ori_340_350 <<- TRUE
      }
      
      if ("[350-360)" %in% input$aspectMulti2) {
        ori_350_360 <<- TRUE
      }
      
      
      
      if (ori_0_10) {
        aspectDiscretization [aspectDiscretization>=0 & aspectDiscretization<10] <<- 1 
        
      }
      else {
        aspectDiscretization [aspectDiscretization>=0 & aspectDiscretization<10] <<- NA
      }
      
      if (ori_10_20) {
        aspectDiscretization [aspectDiscretization>=10 & aspectDiscretization<20] <<- 2 
      }
      else {
        aspectDiscretization [aspectDiscretization>=10 & aspectDiscretization<20] <<- NA
      }
      
      if (ori_20_30) {
        aspectDiscretization [aspectDiscretization>=20 & aspectDiscretization<30] <<- 3 
      }
      else {
        aspectDiscretization [aspectDiscretization>=20 & aspectDiscretization<30] <<- NA
      }
      
      if (ori_30_40) {
        aspectDiscretization [aspectDiscretization>=30 & aspectDiscretization<40] <<- 4 
      }
      else {
        aspectDiscretization [aspectDiscretization>=30 & aspectDiscretization<40] <<- NA
      }
      
      if (ori_40_50) {
        aspectDiscretization [aspectDiscretization>=40 & aspectDiscretization<50] <<- 5 
      }
      else {
        aspectDiscretization [aspectDiscretization>40 & aspectDiscretization<50] <<- NA
      }
      
      if (ori_50_60) {
        aspectDiscretization [aspectDiscretization>=50 & aspectDiscretization<60] <<- 6 
      }
      else {
        aspectDiscretization [aspectDiscretization>=50 & aspectDiscretization<60] <<- NA
      }
      
      if (ori_60_70) {
        aspectDiscretization [aspectDiscretization>=60 & aspectDiscretization<70] <<- 7 
      }
      else {
        aspectDiscretization [aspectDiscretization>=60 & aspectDiscretization<70] <<- NA
      }
      
      if (ori_70_80) {
        aspectDiscretization [aspectDiscretization>=70 & aspectDiscretization<80] <<- 8
      }
      else {
        aspectDiscretization [aspectDiscretization>70 & aspectDiscretization<80] <<- NA
      }
      
      if (ori_80_90) {
        aspectDiscretization [aspectDiscretization>=80 & aspectDiscretization<90] <<- 9
      }
      else {
        aspectDiscretization [aspectDiscretization>=80 & aspectDiscretization<90] <<- NA
      }
      
      if (ori_90_100) {
        aspectDiscretization [aspectDiscretization>=90 & aspectDiscretization<100] <<- 10
      }
      else {
        aspectDiscretization [aspectDiscretization>=90 & aspectDiscretization<100] <<- NA
      }
      
      
      
      if (ori_100_110) {
        aspectDiscretization [aspectDiscretization>=100 & aspectDiscretization<110] <<- 11 
        
      }
      else {
        aspectDiscretization [aspectDiscretization>=100 & aspectDiscretization<110] <<- NA
      }
      
      if (ori_110_120) {
        aspectDiscretization [aspectDiscretization>=110 & aspectDiscretization<120] <<- 12 
      }
      else {
        aspectDiscretization [aspectDiscretization>=110 & aspectDiscretization<120] <<- NA
      }
      
      if (ori_120_130) {
        aspectDiscretization [aspectDiscretization>=120 & aspectDiscretization<130] <<- 13 
      }
      else {
        aspectDiscretization [aspectDiscretization>=120 & aspectDiscretization<130] <<- NA
      }
      
      if (ori_130_140) {
        aspectDiscretization [aspectDiscretization>=130 & aspectDiscretization<140] <<- 14 
      }
      else {
        aspectDiscretization [aspectDiscretization>=130 & aspectDiscretization<140] <<- NA
      }
      
      if (ori_140_150) {
        aspectDiscretization [aspectDiscretization>=140 & aspectDiscretization<150] <<- 15 
      }
      else {
        aspectDiscretization [aspectDiscretization>140 & aspectDiscretization<150] <<- NA
      }
      
      if (ori_150_160) {
        aspectDiscretization [aspectDiscretization>=150 & aspectDiscretization<160] <<- 16 
      }
      else {
        aspectDiscretization [aspectDiscretization>=150 & aspectDiscretization<160] <<- NA
      }
      
      if (ori_160_170) {
        aspectDiscretization [aspectDiscretization>=160 & aspectDiscretization<170] <<- 17 
      }
      else {
        aspectDiscretization [aspectDiscretization>=160 & aspectDiscretization<170] <<- NA
      }
      
      if (ori_170_180) {
        aspectDiscretization [aspectDiscretization>=170 & aspectDiscretization<180] <<- 18
      }
      else {
        aspectDiscretization [aspectDiscretization>170 & aspectDiscretization<=180] <<- NA
      }
      
      if (ori_180_190) {
        aspectDiscretization [aspectDiscretization>=180 & aspectDiscretization<190] <<- 19
      }
      else {
        aspectDiscretization [aspectDiscretization>=180 & aspectDiscretization<190] <<- NA
      }
      
      if (ori_190_200) {
        aspectDiscretization [aspectDiscretization>=190 & aspectDiscretization<200] <<- 20
      }
      else {
        aspectDiscretization [aspectDiscretization>=190 & aspectDiscretization<200] <<- NA
      }
      
      
      
      if (ori_200_210) {
        aspectDiscretization [aspectDiscretization>=200 & aspectDiscretization<210] <<- 21 
        
      }
      else {
        aspectDiscretization [aspectDiscretization>=200 & aspectDiscretization<210] <<- NA
      }
      
      if (ori_210_220) {
        aspectDiscretization [aspectDiscretization>=210 & aspectDiscretization<220] <<- 22 
      }
      else {
        aspectDiscretization [aspectDiscretization>=210 & aspectDiscretization<220] <<- NA
      }
      
      if (ori_220_230) {
        aspectDiscretization [aspectDiscretization>=220 & aspectDiscretization<230] <<- 23 
      }
      else {
        aspectDiscretization [aspectDiscretization>=220 & aspectDiscretization<230] <<- NA
      }
      
      if (ori_230_240) {
        aspectDiscretization [aspectDiscretization>=230 & aspectDiscretization<240] <<- 24 
      }
      else {
        aspectDiscretization [aspectDiscretization>=230 & aspectDiscretization<240] <<- NA
      }
      
      if (ori_240_250) {
        aspectDiscretization [aspectDiscretization>=240 & aspectDiscretization<250] <<- 25 
      }
      else {
        aspectDiscretization [aspectDiscretization>240 & aspectDiscretization<250] <<- NA
      }
      
      if (ori_250_260) {
        aspectDiscretization [aspectDiscretization>=250 & aspectDiscretization<260] <<- 26 
      }
      else {
        aspectDiscretization [aspectDiscretization>=250 & aspectDiscretization<260] <<- NA
      }
      
      if (ori_260_270) {
        aspectDiscretization [aspectDiscretization>=260 & aspectDiscretization<270] <<- 27 
      }
      else {
        aspectDiscretization [aspectDiscretization>=260 & aspectDiscretization<270] <<- NA
      }
      
      if (ori_270_280) {
        aspectDiscretization [aspectDiscretization>=270 & aspectDiscretization<280] <<- 28
      }
      else {
        aspectDiscretization [aspectDiscretization>270 & aspectDiscretization<=280] <<- NA
      }
      
      if (ori_280_290) {
        aspectDiscretization [aspectDiscretization>=280 & aspectDiscretization<290] <<- 29
      }
      else {
        aspectDiscretization [aspectDiscretization>=280 & aspectDiscretization<290] <<- NA
      }
      
      if (ori_290_300) {
        aspectDiscretization [aspectDiscretization>=290 & aspectDiscretization<300] <<- 30
      }
      else {
        aspectDiscretization [aspectDiscretization>=290 & aspectDiscretization<300] <<- NA
      }
     
      
      
      if (ori_300_310) {
        aspectDiscretization [aspectDiscretization>=300 & aspectDiscretization<310] <<- 31 
        
      }
      else {
        aspectDiscretization [aspectDiscretization>=300 & aspectDiscretization<310] <<- NA
      }
      
      if (ori_310_320) {
        aspectDiscretization [aspectDiscretization>=310 & aspectDiscretization<320] <<- 32 
      }
      else {
        aspectDiscretization [aspectDiscretization>=310 & aspectDiscretization<320] <<- NA
      }
      
      if (ori_320_330) {
        aspectDiscretization [aspectDiscretization>320 & aspectDiscretization<330] <<- 33 
      }
      else {
        aspectDiscretization [aspectDiscretization>=320 & aspectDiscretization<330] <<- NA
      }
      
      if (ori_330_340) {
        aspectDiscretization [aspectDiscretization>=330 & aspectDiscretization<340] <<- 34 
      }
      else {
        aspectDiscretization [aspectDiscretization>=330 & aspectDiscretization<340] <<- NA
      }
      
      if (ori_340_350) {
        aspectDiscretization [aspectDiscretization>=340 & aspectDiscretization<350] <<- 35 
      }
      else {
        aspectDiscretization [aspectDiscretization>340 & aspectDiscretization<350] <<- NA
      }
      
      if (ori_350_360) {
        aspectDiscretization [aspectDiscretization>=350 & aspectDiscretization<=360] <<- 36 
      }
      else {
        aspectDiscretization [aspectDiscretization>=350 & aspectDiscretization<=360] <<- NA
      }
      
      
      print (aspectDiscretization)
      
      

      
      #Salida en el panel lateral del área disponible
      output$graficPlot <- renderPlot({
        
        plot (aspectDiscretization, col = rainbow(lengthAspectMulti2), main ="1.[0-10), 2.[10-20),3.[20-30), 4.[30-40), 5.[40-50), 6.[50-60), 7.[60-70), ... 36.4.[350-360]")
        
      })
      
      #Salida en el panel lateral del área disponible
      output$graficPlot4 <- renderPlot({
        
        plot (aspect, main ="Orientación º" )
        
      })
      
      #Salida en el panel lateral del área disponible
      output$graficPlot3 <- renderImage({
        
        list(src = "www/images/roof_class.png", contentType = "image/png", width =600, height =500)
        
      }, deleteFile = FALSE)
      
      #Activar para concurso. Desactivar para app normal
      shinyjs::show("showArea2") 
      
    })
    
    #Obseravmos el evento de presión del botón 'Mostrar áreas'
    observeEvent(input$showArea, {
      
      areasRaster <- showRoofAreas()
      #Salida en el panel lateral del área disponible
      output$graficPlot <- renderPlot({
        
        plot (areasRaster, legend=FALSE, main ="Tejados", col = "grey")
        
      })
      
      output$graficPlot5 <- renderPlot({
        
        plot (areasRaster, legend=FALSE, main ="Tejados", col = "grey")
        
      })
      
      #Generamos el dataset de las áreas de los tejados que cumplen las condiciones
      cC <<- createRoofAreaConnectedComponents(areasRaster)
      
      output$graficPlot6 <- renderPlot({
        
        plot(na.omit(cC), col= rev(rainbow(max(na.omit(values(cC))))), main=paste("Áreas totales disponibles:",max(na.omit(values(cC)))))
        
      })
      
      
      areaDS <- createRoofAreaDataset(cC)
      
      areaDS <- areaDS %>%
        filter(areaDS$area_m2>=4)
      
 
      
     
      
      
      #Añadir la lat y lng media del tejado (componente conexa) al dataset
      fullDS <- addnewDataToDataset (areaDS)
      
      output$areatable = DT::renderDataTable({
        fullDS
      })
      
      output$totalRoofAreaBox <- renderInfoBox({
        infoBox(
          "Areas disponibles > 4m2", nrow(fullDS), icon = icon("check"),
          color = "purple"
        )
      })
      
      #Obtenemos la producción total
      prodTotal <- fullDS %>%
        filter(as.numeric(fullDS$produccion)>0) %>%
        summarise(sum(fullDS$produccion))
      
      #Renderizamos
      output$totalEnergyAreaBox <- renderInfoBox({
        infoBox(
          "Producción total", paste(prodTotal," kWh"), icon = icon("solar-panel"),
          color = "orange"
        )
      })
      
      
  
      
  
    } )
    
    
    
    #CONCURSO Obseravmos el evento de presión del botón 'Mostrar áreas2 '
    observeEvent(input$showArea2, {
      
      areasRaster <- showRoofAreas2()
      #Salida en el panel lateral del área disponible
      output$graficPlot <- renderPlot({
        
        plot (areasRaster, legend=FALSE, main ="Tejados", col = "grey")
        
      })
      
      output$graficPlot5 <- renderPlot({
        
        plot (areasRaster, legend=FALSE, main ="Tejados", col = "grey")
        
      })
      
      #Generamos el dataset de las áreas de los tejados que cumplen las condiciones
      cC <<- createRoofAreaConnectedComponents(areasRaster)
      
      output$graficPlot6 <- renderPlot({
        
        plot(na.omit(cC), col= rev(rainbow(max(na.omit(values(cC))))), main=paste("Áreas totales disponibles:",max(na.omit(values(cC)))))
        
      })
      
      
      areaDS <- createRoofAreaDataset(cC)
      
      areaDS <- areaDS %>%
        filter(areaDS$area_m2>=4)
      
      val_ <- unique(cC)
      
 
      #Añadir la lat y lng media del tejado (componente conexa) al dataset
      fullDS <- addnewDataToDataset (areaDS)
      
      output$areatable = DT::renderDataTable({
        fullDS
      })
      
      output$totalRoofAreaBox <- renderInfoBox({
        infoBox(
          "Areas disponibles > 4m2", nrow(fullDS), icon = icon("check"),
          color = "purple"
        )
      })
      
      
      #Obtenemos la producción total
      prodTotal <- fullDS %>%
        filter(as.numeric(fullDS$produccion)>0) %>%
        summarise(sum(fullDS$produccion))
      
      #Renderizamos
      output$totalEnergyAreaBox <- renderInfoBox({
        infoBox(
          "Producción total", prodTotal, icon = icon("solar-panel"),
          color = "orange"
        )
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
