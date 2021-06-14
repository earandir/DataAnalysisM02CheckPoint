
library(class)
library(dplyr)
library(stringr)
library(lubridate)
library(plotly)

library(shiny)
library(shinydashboard)


df <- as.data.frame(read.csv(file = 'source/registros.csv'))

df <- dplyr::count(df, idSolicitud, Agente, Solicitante, Modo, Modulo, Categoria)
testdata <- as.data.frame(read.csv(file = 'source/registros.csv')) %>% group_by(idSolicitud, Agente, Solicitante, Modo, Modulo, Categoria) %>% summarise(TiempoInvertido = sum(TiempoTranscurrido), n = n())



TotalSolicitudes <- 

shinyUI(
    fluidPage(
        
        dashboardPage(
            
            dashboardHeader(title = "Soporte Técnico KPI"),
            
            skin = "green",
            
            dashboardSidebar(
                
                sidebarMenu(
                    menuItem("Rendimiento Clave", tabName = "Dashboard", icon = icon("dashboard")),
                    menuItem("Tiempo Invertido", tabName = "graph", icon = icon("area-chart")),
                    menuItem("Desempeño", tabName = "performance", icon = icon("theater-masks")),
                    menuItem("Tabla", tabName = "data_table", icon = icon("table"))
                ) ##end sidebarmenu
                
            ),
            
            dashboardBody(
                
                tabItems(
                    
                    # Histograma
                    tabItem(tabName = "Dashboard",
                            fluidRow(
                                valueBox(
                                    #inputId = "total_today",
                                    value = nrow(df), 
                                    subtitle = "Total Solicitudes",
                                    color = "purple",
                                    icon = icon("comment-dots"),
                                    width = 4),
                                valueBox(
                                    #inputId = "total_today",
                                    value = n_distinct(df$Solicitante, na.rm = FALSE), 
                                    subtitle = "Total Solicitantes",
                                    color = "orange",
                                    icon = icon("user-circle"),
                                    width = 4),
                                valueBox(
                                    #inputId = "total_today",
                                    value = seconds_to_period(sum(testdata$TiempoInvertido)), 
                                    subtitle = "Tiempo Invertido",
                                    color = "green",
                                    icon = icon("hourglass-half"),
                                    width = 4)
                            ), #End fluidrow 1 histograma
                            fluidRow(
                                titlePanel("Análisis de Casos"), 
                                # selectInput("x", "Seleccione el valor del eje X",
                                #             choices = c("Agente", "Solicitante", "Modo", "Modulo", "Categoria")),
                                # 
                                # selectInput("zz", "Selecciona la variable del grid", 
                                #             
                                #             choices = c("Modo")),
                                box(plotOutput("plotAgentes", height = 250)),
                                box(plotOutput("plotSolicitantes", height = 250))
                                ),
                            fluidRow(
                                box(
                                    #title = "Controls",
                                    sliderInput("bins", "Número de columnas:", 1, 10, 3)
                                )    
                            )
                    ),
                    
                    # Dispersión
                    tabItem(tabName = "graph", 
                            fluidRow(
                                titlePanel(h3("Historial de Solicitudes")),
                                dateRangeInput(
                                    "dates",
                                    "Fecha:",
                                    start = "2021-01-01", 
                                    end = Sys.Date() + 5,
                                    min = "2019-01-01",
                                    max = Sys.Date() + 5,
                                    format = "yyyy-mm-dd",
                                    startview = "year",
                                    weekstart = 1,
                                    language = "es",
                                    separator = " al ",
                                    width = NULL,
                                    autoclose = TRUE
                                ),
                                fluidRow(plotlyOutput("output_plot") )
                                
                            )
                    ),
                    
                    tabItem(tabName = "performance",
                            fluidRow(
                                titlePanel("Análisis de Desempeño"),
                                box(plotlyOutput("opcategoria", height = 300)),
                                box(plotlyOutput("opmodo", height = 300))
                            ),
                            fluidRow(
                                box(plotlyOutput("optrabajo", height = 300)),
                                box(plotlyOutput("opmodulo", height = 300))
                            )
                    ), #end tabitem performance
                    
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(h3("Data Table")),
                                dataTableOutput ("data_table")
                            )
                    )
                    
                )
            
            ), #end tabitems#
        )
    )
)

