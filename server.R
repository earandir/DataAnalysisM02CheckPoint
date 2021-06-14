
library(shiny)
library(dplyr)
library(stringr)
library(lubridate)
library(plotly)
library(hrbrthemes)

datos <- as.data.frame(read.csv(file = 'source/registros.csv'))

#datos %>% group_by(conjunto, "idSolicitud")

shinyServer(function(input, output) {
 
  library(ggplot2)
  data <- dplyr::count(datos, idSolicitud, Agente, Solicitante, Modo, Modulo, Categoria)
  testdata <- as.data.frame(read.csv(file = 'source/registros.csv')) %>% group_by(idSolicitud, Agente, Solicitante, Modo, Modulo, Categoria, Fecha = as_date(FechaCreacionSolicitud, tz = NULL, format = NULL)) %>% summarise(TiempoInvertido = sum(TiempoTranscurrido), n = n())
  
  
  #GrÃ¡fico de Histograma
  output$plotAgentes <- renderPlot({
    bin <- input$bins
    #datox <- input$x
    columna <-slice_max(dplyr::count(data, Agente), order_by = n, n = bin)
  
    ggplot(columna, aes(x=reorder(Agente, -n), y=n, fill = Agente)) +
    geom_bar(stat ="identity", color = "gray") +
    xlab("Agente") + ylab("Conteo") + 
    ggtitle("Agentes más activos") + 
    scale_fill_brewer(palette = "Spectral") +  
    guides(fill = FALSE)  
    
  })
  
  output$plotSolicitantes <- renderPlot({
    bin <- input$bins
    #datox <- input$x
    
    columna <-slice_max(dplyr::count(data, Solicitante), order_by = n, n = bin)
    
    ggplot(columna, aes(x=reorder(Solicitante, -n), y=n, fill = Solicitante)) +
      geom_bar(stat ="identity", color = "gray") +
      xlab("Solicitante") + ylab("Conteo") + 
      ggtitle("Solicitantes más activos") + 
      scale_fill_brewer(palette = "Spectral") +  
      guides(fill = FALSE)  
    
  })
  

  
  # Análisis por tiempo
  output$output_plot <- renderPlotly({ 
    
    #output$DateRange <- renderText({
      # # make sure end date later than start date
      # validate(
      #   need(input$dates[2] > input$dates[1], "end date is earlier than start date"
      #   )
      # )
      # 
      # # make sure greater than 2 week difference
      # validate(
      #   need(difftime(input$dates[2], input$dates[1], "days") > 14, "date range less the 14 days"
      #   )
      # )
    figdata <- testdata %>% group_by(Fecha = as_date(Fecha, tz = NULL, format = NULL)) %>% summarise(Tiempo = sum(TiempoInvertido), n = n())
    
    p <- figdata %>%
      ggplot( aes(x=Fecha, y=Tiempo)) +
      geom_area(fill="#69b3a2", alpha=0.5) +
      geom_line(color="#69b3a2") +
      xlab("") + ylab("Tiempo Invertido") + 
      scale_x_date(date_labels = "%b-%y", #breaks = "1 days",
                   limit=c(as.Date(input$dates[1]),as.Date(input$dates[2]))) +
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      theme_ipsum()
    
    # Turn it interactive with ggplotly
    p <- ggplotly(p)
    p  

  })   

  # Desempeño
  output$opcategoria <- renderPlotly({
    
    figdata <- testdata %>% group_by(Categoria) %>% summarise(Tiempo = sum(TiempoInvertido))
    
    fig <- plot_ly(type='pie', labels=figdata$Categoria, values=figdata$Tiempo, 
                   textinfo='label+percent',
                   textposition = 'inside',
                   text = ~paste(seconds_to_period(figdata$Tiempo)),
                   hoverinfo = 'text',
                   marker = list(colors = colors,
                                 line = list(color = "gray", width = 1)),
                   showlegend = FALSE,
                   insidetextorientation='radial')
    fig <- fig %>% layout(title = "Por Categoría")
    fig
  })
  # Modo
  output$opmodo <- renderPlotly({
    figdata <- testdata %>% filter(Categoria == "Soporte Externo") %>% group_by(Modo) %>% summarise(Tiempo = sum(TiempoInvertido))
    
    fig <- plot_ly(type='pie', labels=figdata$Modo, values=figdata$Tiempo, 
                   textinfo='label+percent',
                   textposition = 'inside',
                   text = ~paste(seconds_to_period(figdata$Tiempo)),
                   hoverinfo = 'text',
                   showlegend = FALSE,
                   insidetextorientation='radial')
    fig <- fig %>% layout(title = "Por Modo")
    fig
  })
  
  # trabajo
  output$optrabajo <- renderPlotly({
    
    figdata <- datos %>% group_by(Trabajo) %>% summarise(Tiempo = sum(TiempoTranscurrido))
    
    fig <- plot_ly(type='pie', labels=figdata$Trabajo, values=figdata$Tiempo, 
                   textinfo='label+percent',
                   textposition = 'inside',
                   text = ~paste(seconds_to_period(figdata$Tiempo)),
                   hoverinfo = 'text',
                   showlegend = FALSE,
                   insidetextorientation='radial')
    fig <- fig %>% layout(title = "Por Tipo")
    fig
  })
  
  # Móduilo
  output$opmodulo <- renderPlotly({
   
    figdata <- testdata %>% filter(Categoria == "Soporte Externo") %>% group_by(Modulo) %>% summarise(Tiempo = sum(TiempoInvertido))
    
    fig <- plot_ly(type='pie', labels=figdata$Modulo, values=figdata$Tiempo, 
                   textinfo='label+percent',
                   textposition = 'inside',
                   text = ~paste(seconds_to_period(figdata$Tiempo)),
                   hoverinfo = 'text',
                   showlegend = FALSE,
                   insidetextorientation='radial')
    fig <- fig %>% layout(title = "Por Módulo")
    fig
  })
  #Data Table
  output$data_table <- renderDataTable( testdata, 
                                        options = list(aLengthMenu = c(5,25,50, 100),
                                                       iDisplayLength = 5)
  )
  
})
