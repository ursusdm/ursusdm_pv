#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinyWidgets)
library(shinydashboard)

# Define UI for application

#shinyjs::useShinyjs()

shinyUI(dashboardPage(
    
    # Application title
    #titlePanel("Ubicación óptima de placas fotovoltaicas en tejados"),
    
    dashboardHeader(title = "URSUS_PV"),
    
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), badgeLabel = "Panel principal", badgeColor = "blue"),
            menuItem("¿Qué es URSUS PV?", tabName = "def", icon = icon("question")),
            menuItem("Cómo funciona?", tabName = "info", icon = icon("info")),
            menuItem("Predicción media diaria", tabName = "areas", icon = icon("solar-panel")),
            menuItem("Predicción horaria (next day)", tabName = "areas2", icon = icon("solar-panel"))
        )
        
    ),
    
    dashboardBody(
        
        shinyjs::useShinyjs(),
        
        tabItems(
            
            # First tab content
            tabItem(tabName = "dashboard",
        
                        fluidRow( class = "text-center",
                            
                            box(
                                title = tagList(shiny::icon("globe"), "Control de áreas de Interés (AOI)"),
                                status = "primary", 
                                solidHeader = TRUE,
                                selectInput("ciudad", "Ciudad:",
                                            c("Malaga" = "Malaga",
                                              "Casares" = "Casares",
                                              "Gaucin" = "Gaucin",
                                              "Tolox" = "Tolox"
                                            ), selected = "Málaga" ),
                                hr(),
                                helpText("Seleccione el municipio de interés"),
                                leafletOutput("mymap"),
                                p(),
                                actionButton("showRoof", "Mostrar tejados")
                            ),
                            
                            
                            box(
                                title = tagList(shiny::icon("chart-area"), "Tejados disponibles en el área de interés"),
                                status = "primary",
                                solidHeader = TRUE,
                                plotOutput("graficPlotTejados")
                            )
                        ),
                        
                        fluidRow( class = "text-center",
                            
                            box(
                                
                                title = tagList(shiny::icon("caret-square-up"), "Control de pendientes"),
                                
                                status = "primary", 
                                solidHeader = TRUE,
                                
    
                                #Para el concurso
                                p(),
                                # multiInput(
                                #   inputId = "slopeMulti2", label = "Seleccione la inclinación de los tejados",
                                #   choices = c("[0-10)", "[10-20)", "[20-30)" , "[30-40)", "[40-50)", "[50-60)", "[60-70)")
                                # ),
                                
                                sliderInput("slopeMulti2", label = h3("Rango de interés de pendientes"), min = 0, 
                                            max = 70, value = c(0, 70)),
                                
                                actionButton("showSlopes2", "Mostrar pendientes"),
                                
                                plotOutput("graficPlotSlopesAllRoofs"),
                                
                                plotOutput("graficPlot9")
                                
                            ),
                            
                            box(
                                
                                title = tagList(shiny::icon("compass"), "Control de orientación"),
                                
                                status = "primary", 
                                solidHeader = TRUE,
                                
                                # multiInput(
                                #     inputId = "aspectMulti", label = "Seleccione la orientación de los tejados",
                                #     choices = c("Norte", "Sur", "Este",
                                #                 "Oeste")
                                # ),
                                
                                #actionButton("showOrientation", "Mostrar orientaciones"),
                                
                                #Para el concurso
                                p(),
                                
                                # multiInput(
                                #   inputId = "aspectMulti2", label = "Orientaciones de interés (grados)",
                                #   selected = "Banana",
                                #   choices = c("[0-10)", "[10-20)", "[20-30)" , "[30-40)", "[40-50)", "[50-60)", "[60-70)", "[70-80)", "[80-90)", "[90-100)",
                                #               "[100-110)", "[110-120)", "[120-130)" , "[130-140)", "[140-150)" )
                                # ),
                                
                                
                                sliderInput("aspectMulti2", label = h3("Rango de interés de orientaciones"), min = 0, 
                                            max = 270, value = c(90, 270)),
                               
                                actionButton("showOrientation2", "Mostrar orientaciones"),
                                
                                plotOutput("graficPlotOrientationAllRoofs"),
                                
                                plotOutput("graficPlotRoofFilteredOrientation")
                                
                            )
                            
                        )
                    
                       
                    
                    
                    
            ),
            
            # Second tab content
            tabItem(tabName = "info",
                    
                    h2("Cómo funciona?"),
                    
                    p( tags$em("Las zonas disponibles para el estudio se mostraran en el mapa dentro de una cuadrícula azul.") ),
                    
                    tags$ol(
                        tags$li(style="color:#5886C0","Seleccione el área de interés"), 
                        tags$ol(
                            tags$li(tags$span("De forma gráfica.",style="color:#58B2C0")," Navegue por el mapa del dashboard con el ratón y las herramientas de zoom hasta el área de interés. A continuación, seleccione la herramienta rectángulo, haga click izquierdo con el ratón sobre un punto de la zona de interés, trace un rectángulo sin soltar el botón, y suelte el botón del ratón cuando esté satisfecho con el área dibujada. Si desea eliminar el área trazada, utilice la herramienta gráfica eliminar, haga click en el polígono anteriormente trazado a eliminar, y presione el botón 'Save'. ") 
                          
                        ),
                        tags$li(style="color:#5886C0","Mostrar Tejados.", tags$span("Presione el botón 'Mostrar tejados' y se mostrarán en el área gráfica de arriba a la derecha del dashboard los tejados disponibles susceptibles de instalaciones fotovoltaicas en el área de interés. Se activarán los controles de pendientes y orientciones de los tejados disponibles.",style="color:black")), 
                        tags$li(style="color:#5886C0","Mostrar Orientación.", tags$span("Seleccione las orientaciones de interés de los tejados. A continuación, presione el botón 'Mostrar Orientación', y la aplicación le mostrará en las áreas gráficas del control de orientaciones, las orientaciones de todos los tejados y las orientaciones de los tejados en las que tiene interés.",style="color:black")), 
                        tags$li(style="color:#5886C0","Mostrar Pendientes.", tags$span("Seleccione lads inclinaciones de interes de los tejados. A continuación, presione el botón 'Mostrar Pendientes', y la aplicación le mostrará en las áreas gráficas del control de pendientes, las pendientes de todos los tejados y las pendientes de los tejados en las que está interesado",style="color:black")), 
                        tags$li(style="color:#5886C0","Estimación de energía a un día vista (c/p).", tags$span("Acceda a la pestaña de estimaciones de energía  a un día vista dashboard. Presione el botón 'Estimación de energía  a un día vista'. (sólo se tendrán en cuenta los tejados que cumplan las condiciones de orientación y pendiente establecidos por el usuario así como criterios mínimos de las instalaciones: área,...). Podrá consultar la energía horaria para cada tejado y para el área de interés  ",style="color:black")), 
                        tags$li(style="color:#5886C0","Estimación de energía media diaria (l/p).", tags$span("Acceda a la pestaña de estimaciones de energía media diaria del dashboard. Presione el botón 'Estimación de energía media diaria'. (sólo se tendrán en cuenta los tejados que cumplan las condiciones de orientación y pendiente establecidos por el usuario). Aparecerá en un panel informativo el número de tejados que cumplen las condiciones mínimas (área mínima de instalación, orientaciones válidas, ...) y la producción total estimada media diaria para el área de interés. También podrá navegar por las pestañas (tejados disponibles y características de los tejados) en las que podrá acceder a las características de los tejados procesados (producción del tejado, área, inclinación, ...)",style="color:black")) 
                        
                    )
                    
            ),
            
            # Second tab content
            tabItem(tabName = "def",
                    h2("¿Qué es URSUS PV?"),
                    p(tags$em("URSUS PV")," es una herramienta para la ayuda a la toma de decisiones en cuanto a las posibles ubicaciones óptimas de instalaciones fotovoltaicas en áreas de interés urbana. La herramienta tomará como entrada un área de interés de gráfica y mostrará al usuario los tejados disponibles. Permitirá el filtro de tejados en los que se tiene interés para las instalaciones de placas fotovoltaicas en cuanto a orientación e inclinación. Permite realizar predicciones de energía solar (c/p) horaria para el siguiente día para cada tejado y para el área de interés completa,  así como la predicción media diaria de cada tejado y del área de interés.")
            ),
            
            # Second tab content
            tabItem(tabName = "areas",
                    h2("Predicciones de energía solar media diaria"),
                    fluidRow( class = "text-center",
                              infoBoxOutput("totalRoofAreaBox"),
                              infoBoxOutput ("totalEnergyAreaBox")          
                    ),
                    fluidRow( class = "text-center",
                              
                              tabBox(
                                  title = tagList(shiny::icon("home"), "Energía solar media diaria"),
                                  width = 100,
                                  # The id lets us use input$tabset1 on the server to find the current tab
                                  id = "tabset1", height = "250px",
                                  
                                  #app normal / concurso
                                  tabPanel("Tejados disponibles",
                                  actionButton("estimar", "Estimar energía solar media diaria"),
                                  plotOutput("roofMatchtCriteria") ,
                                  helpText("Tejados que cumplen los criterios de orientación e inclinación")),

                                  #tabPanel("Plot Areas", plotOutput("graficPlot6") ),
                                  tabPanel("Producción y características de los tejados",
                                  helpText("Tejados válidos: área mín.,..."),
                                  DT::dataTableOutput("areatable"))
                                  
                              ),
                   
                              
                              
                    ),
            ),
            
            # Second tab content
            tabItem(tabName = "areas2",
                    h2("Predicciones de energía solar horaria para el siguiente día"),
                    fluidRow( class = "text-center",
                              tabBox(
                                title = tagList(shiny::icon("home"), "Energía solar horaria estimada para el próximo día"),
                                width = 100,
                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabset1",
                                
                                #app normal / concurso
                                tabPanel("Tejados disponibles",
                                actionButton("estimarCP", "Estimar energía solar horaria para el próximo día"),
                                plotOutput("roofMatchtCriteriaCP"), 
                                
                                helpText("Tejados que cumplen los criterios de orientación e inclinación") ),
                                
                                tags$head(tags$style('h4 {color:green;}')),
                                #tabPanel("Plot Areas", plotOutput("graficPlot6") ),
                                tabPanel("Tejados válidos. Producción",
                                h4("Predicciones horarias de energía solar en kWh para cada tejado entre las 5:00h y las 20:00h"),
                                         
                                # fluidRow(
                                #   column(
                                #     dataTableOutput(outputId = "tejadosProcesados"), width = 6)
                                # ) ,    
                                DT::dataTableOutput("tejadosProcesados",  width = "auto"),
                                
                      
                                #infoBoxOutput("totalRoofAreaBox2"),
                                #infoBoxOutput ("totalEnergyAreaBox2")
                                h4("Predicciones horarias (Kwh) del área de interés"),
                                
                                DT::dataTableOutput("produccionHorariaTotalTejados")),
                                
                              ),
                              
                              
                              
                              
                    ),
                   
            )
            
            
            
            
        
    )
    
  )
    
))
    

    

