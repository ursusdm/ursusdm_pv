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
    
    dashboardHeader(title = "URSUS PV"),
    
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), badgeLabel = "Panel principal", badgeColor = "blue"),
            menuItem("¿Qué es URSUS PV?", tabName = "def", icon = icon("question")),
            menuItem("Cómo funciona?", tabName = "info", icon = icon("info")),
            menuItem("Predicción media diaria", tabName = "areas", icon = icon("solar-panel")),
            menuItem("Predicción a un día vista", tabName = "areas2", icon = icon("solar-panel"))
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
                                leafletOutput("mymap"),
                                p(),
                                actionButton("showRoof", "Mostrar tejados")
                            ),
                            
                            
                            box(
                                title = tagList(shiny::icon("chart-area"), "Tejados disponibles en el área de interés"),
                                status = "primary",
                                solidHeader = TRUE,
                                plotOutput("graficPlot")
                            )
                        ),
                        
                        fluidRow( class = "text-center",
                            
                            box(
                                
                                title = tagList(shiny::icon("caret-square-up"), "Control de pendientes"),
                                
                                status = "primary", 
                                solidHeader = TRUE,
                                
                                multiInput(
                                    inputId = "slopeMulti", label = "Seleccione el tipo de inclinación de los tejados",
                                    choices = c("Planos", "Ligeramente inclinados", "Inclinados",
                                                "Muy inclinados")
                                ),
                                
                                actionButton("showSlopes", "Mostrar pendientes"),
                                
                                
                                
                                #Para el concurso
                                p(),
                                multiInput(
                                  inputId = "slopeMulti2", label = "Seleccione la inclinación de los tejados",
                                  choices = c("[0-10)", "[10-20)", "[20-30)" , "[30-40)", "[40-50)", "[50-60)", "[60-70)")
                                ),
                                
                                actionButton("showSlopes2", "Mostrar pendientes"),
                                
                                plotOutput("graficPlot2"),
                                
                                plotOutput("graficPlot9")
                                
                            ),
                            
                            box(
                                
                                title = tagList(shiny::icon("compass"), "Control de orientación"),
                                
                                status = "primary", 
                                solidHeader = TRUE,
                                
                                multiInput(
                                    inputId = "aspectMulti", label = "Seleccione la orientación de los tejados",
                                    choices = c("Norte", "Sur", "Este",
                                                "Oeste")
                                ),
                                
                                actionButton("showOrientation", "Mostrar orientaciones"),
                                
                                #Para el concurso
                                p(),
                                
                                multiInput(
                                  inputId = "aspectMulti2", label = "Seleccione la orientación de los tejados",
                                  choices = c("[0-10)", "[10-20)", "[20-30)" , "[30-40)", "[40-50)", "[50-60)", "[60-70)", "[70-80)", "[80-90)", "[90-100)",
                                              "[100-110)", "[110-120)", "[120-130)" , "[130-140)", "[140-150)", "[150-160)", "[160-170)", "[170-180)", "[180-190)", "[190-200)",
                                              "[200-210)", "[210-220)", "[220-230)" , "[230-240)", "[240-250)", "[250-260)", "[260-270)", "[270-280)", "[280-290)", "[290-300)",
                                              "[300-310)", "[310-320)", "[320-330)" , "[330-340)", "[340-350)", "[350-360]")
                                ),
                                
                               
                                actionButton("showOrientation2", "Mostrar orientaciones"),
                                
                                plotOutput("graficPlot4"),
                                
                                plotOutput("graficPlot10")
                                
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
                        tags$li(style="color:#5886C0","Estimación de energía a un día vista (c/p).", tags$span("Acceda a la pestaña de estimaciones de energía  a un día vista dashboard. Presione el botón 'Estimación de energía  a un día vista'. (sólo se tendrán en cuenta los tejados que cumplan las condiciones de orientación y pendiente establecidos por el usuario). Aparecerá en un panel informativo el número de tejados que cumplen las condiciones mínimas (área mínima de instalación, orientaciones válidas, ...) y la producción total estimada a un día vista para el área de interés. También podrá navegar por las pestañas (tejados disponibles y características de los tejados) en las que podrá acceder a las características de los tejados procesados (producción del tejado, área, inclinación, ...) y a un panel visual dónde podrá ver los tejados.  ",style="color:black")), 
                        tags$li(style="color:#5886C0","Estimación de energía media diaria (l/p).", tags$span("Acceda a la pestaña de estimaciones de energía media diaria del dashboard. Presione el botón 'Estimación de energía media diaria'. (sólo se tendrán en cuenta los tejados que cumplan las condiciones de orientación y pendiente establecidos por el usuario). Aparecerá en un panel informativo el número de tejados que cumplen las condiciones mínimas (área mínima de instalación, orientaciones válidas, ...) y la producción total estimada media diaria para el área de interés. También podrá navegar por las pestañas (tejados disponibles y características de los tejados) en las que podrá acceder a las características de los tejados procesados (producción del tejado, área, inclinación, ...) y a un panel visual dónde podrá ver los tejados.  ",style="color:black")) 
                        
                    )
                    
            ),
            
            # Second tab content
            tabItem(tabName = "def",
                    h2("¿Qué es URSUS PV?"),
                    p(tags$em("URSUS PV")," es una herramienta para la ayuda a la toma de decisiones en cuanto a las posibles ubicaciones óptimas de instalaciones fotovoltaicas en áreas de interés urbana. La herramienta tomará como entrada un área de interés de gráfica y mostrará al usuario los tejados disponibles. Permitirá el filtro de tejados en los que se tiene interés para las instalaciones de placas fotovoltaicas en cuanto a orientación e inclinación, y realizará predicciones de energía solar a un día vista y media diaria si se instalaran placas fotovoltaicas.")
            ),
            
            # Second tab content
            tabItem(tabName = "areas",
                    h2("Predicciones de energía solar media diaria"),
                    fluidRow( class = "text-center",
                              
                              
                              tabBox(
                                  title = tagList(shiny::icon("home"), "Energía solar media diaria"),
                                  
                                  # The id lets us use input$tabset1 on the server to find the current tab
                                  id = "tabset1", height = "250px",
                                  
                                  #app normal / concurso
                                  tabPanel("Tejados disponibles", actionButton("showArea", "Estimar energía solar media diaria"), actionButton("showArea2", "Estimar energía solar media diaria"), plotOutput("graficPlot6") ),
                                 
                                  #tabPanel("Plot Areas", plotOutput("graficPlot6") ),
                                  tabPanel("Características y producción de los tejados disponibles",  DT::dataTableOutput("areatable"))
                                  
                              ),
                              
                              
                              
                              infoBoxOutput("totalRoofAreaBox"),
                              infoBoxOutput ("totalEnergyAreaBox")
                              
                    ),
            ),
            
            # Second tab content
            tabItem(tabName = "areas2",
                    h2("Predicciones de energía solar a un día vista"),
                    fluidRow( class = "text-center",
                              
                              
                              tabBox(
                                title = tagList(shiny::icon("home"), "Energía solar a un día vista"),
                                
                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabset1", height = "250px",
                                
                                #app normal / concurso
                                tabPanel("Tejados disponibles", actionButton("showArea4", "Estimar energía solar a un día vista"), plotOutput("graficPlot20") ),
                                
                                #tabPanel("Plot Areas", plotOutput("graficPlot6") ),
                                tabPanel("Características y producción de los tejados disponibles",  DT::dataTableOutput("areatable2"))
                                
                              ),
                              
                              
                              
                              infoBoxOutput("totalRoofAreaBox2"),
                              infoBoxOutput ("totalEnergyAreaBox2")
                              
                    ),
            )
            
            
            
            
        
    )
    
  )
    
))
    

    # Sidebar LAyout
    # sidebarLayout(
    #     
    #     #SideBar Panel
    #     sidebarPanel(
    #         plotOutput("graficPlot")
    #     ),
    # 
    #     # Main Panel
    #     mainPanel(
    #         
    #         leafletOutput("mymap"),
    #         
    #         absolutePanel(top = 80, right = 30,
    #                       actionButton("showRoof", "Mostrar tejados"),
    #                       p(),
    #                       actionButton("showArea", "Mostrar áreas"),
    #                       p()
    #                       #actionButton("coords", "Insertar coordenadas")
    #         ),
    #         
    #         p(),
    #         
    #         #plotOutput("graficPlot"),
    #    
    #         p(),
    #         
    #         fluidRow(
    #             
    #             column(8, align="center",
    #                    
    #                    multiInput(
    #                        inputId = "aspectMulti", label = "Seleccione la orientación de los tejados",
    #                        choices = c("Norte", "Sur", "Este",
    #                                    "Oeste")
    #                    ),
    #                    
    #                    actionButton("showOrientation", "Mostrar orientación"),
    #                    
    #                    p(),
    #             )
    #             
    #         ),
    #         
    #         p(),
    #         
    #         
    #         fluidRow(
    #             
    #             column(8, align="center",
    #                    
    #                    multiInput(
    #                        inputId = "slopeMulti", label = "Seleccione el tipo de inclinación de los tejados",
    #                        choices = c("Planos", "Ligeramente inclinados", "Inclinados",
    #                                    "Muy inclinados")
    #                    ),
    #                    
    #                    actionButton("showSlopes", "Mostrar pendientes"),
    #                    
    #                    p(),
    #             )
    #             
    #         ),
    #        
    #         
    #         #verbatimTextOutput(outputId = "aspect"),
    #         
    #        
    #         
    #         #verbatimTextOutput(outputId = "slope"),
    #         p(),
    #         plotOutput("graficPlot2"),
    #         p(),
    #         plotOutput("graficPlot3"),
    #        
    #         
    #     )
    #     
    # )
    

