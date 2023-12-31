# Instale e carregue as bibliotecas necessárias
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("ggsoccer", quietly = TRUE)) {
  install.packages("ggsoccer")
}
if (!requireNamespace("ggforce", quietly = TRUE)) {
  install.packages("ggforce")
}
library(ggforce)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(shiny)

# Carregue o arquivo JSON com os eventos
dados_eventos <- jsonlite::fromJSON("Bahia-Gremio-Whoscored.json")
# Obtenha os tipos únicos de eventos disponíveis
tipos_eventos <- unique(dados_eventos$matchCentreData$events$type$displayName)

# Defina a função para filtrar os eventos com base no intervalo de tempo, tipo e time selecionado
filtrar_eventos <- function(eventos, minuto_inicial, minuto_final, tipo_evento, time_selecionado, is_touch) {
  eventos <- eventos[eventos$minute >= minuto_inicial & eventos$minute <= minuto_final, ]
  
  if (!is.null(tipo_evento)) {
    eventos <- eventos[eventos$type$displayName %in% tipo_evento, ]
  }
  
  if (!is.null(time_selecionado)) {
    eventos <- eventos[eventos$teamId == time_selecionado, ]
  }
  
  if (!is.null(is_touch)) {
    eventos <- eventos[eventos$isTouch == is_touch, ]
  }
  
  return(eventos)
}

shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          inputId = "intervalo_tempo",
          label = "Intervalo de Tempo (minutos)",
          min = 0,
          max = 100,
          value = c(0, 100)
        ),
        selectInput(
          inputId = "tipo_evento",
          label = "Tipo de Evento",
          choices = tipos_eventos,
          multiple = TRUE
        ),
        selectInput(  # Alterado de checkboxInput para selectInput
          inputId = "is_touch",
          label = "Tipo de Toque",
          choices = c("É Toque", "Não É Toque", "Ambos"),
          selected = "Ambos"  # Selecionado por padrão
        ),
        selectInput(
          inputId = "time_selecionado",
          label = "Time Selecionado",
          choices = c("Todos os Times", "Home", "Away"),
          multiple = FALSE
        )
      ),
      mainPanel(
        plotOutput("grafico", height = "800", width = "1200px")  
      )
    )
  ),
  server = function(input, output) {
    # Renderize o gráfico com base nos eventos filtrados
    output$grafico <- renderPlot({
      # Filtrar os eventos com base no intervalo de tempo, tipo e time selecionado
      if (input$time_selecionado == "Todos os Times") {
        eventos_filtrados <- filtrar_eventos(
          dados_eventos$matchCentreData$events,
          minuto_inicial = input$intervalo_tempo[1],
          minuto_final = input$intervalo_tempo[2],
          tipo_evento = if (input$is_touch == "Ambos") input$tipo_evento else NULL,
          is_touch = if (input$is_touch %in% c("É Toque", "Ambos")) TRUE else FALSE,
          time_selecionado = NULL
        )
      } else if (input$time_selecionado == "Home") {
        time_id <- dados_eventos$matchCentreData$home$teamId
        eventos_filtrados <- filtrar_eventos(
          dados_eventos$matchCentreData$events,
          minuto_inicial = input$intervalo_tempo[1],
          minuto_final = input$intervalo_tempo[2],
          tipo_evento = if (input$is_touch == "Ambos") input$tipo_evento else NULL,
          is_touch = if (input$is_touch %in% c("É Toque", "Ambos")) TRUE else FALSE,
          time_selecionado = time_id
        )
      } else if (input$time_selecionado == "Away") {
        time_id <- dados_eventos$matchCentreData$away$teamId
        eventos_filtrados <- filtrar_eventos(
          dados_eventos$matchCentreData$events,
          minuto_inicial = input$intervalo_tempo[1],
          minuto_final = input$intervalo_tempo[2],
          tipo_evento = if (input$is_touch == "Ambos") input$tipo_evento else NULL,
          is_touch = if (input$is_touch %in% c("É Toque", "Ambos")) TRUE else FALSE,
          time_selecionado = time_id
        )
      }
      
      
      # Obter informações dos times
      home_team <- dados_eventos$matchCentreData$home$name
      away_team <- dados_eventos$matchCentreData$away$name
      home_team_id <- dados_eventos$matchCentreData$home$teamId
      away_team_id <- dados_eventos$matchCentreData$away$teamId
      team_colors <- c("#FF0000", "#0000FF")  # Define as cores dos times
      
      # Plotar os eventos filtrados dentro do campo
      
      p <- ggplot() +
        annotate_pitch() +
        geom_segment(data = eventos_filtrados, 
                     aes(x = ifelse(teamId == home_team_id, x, 100 - x),
                         y = ifelse(teamId == home_team_id, y, 100 - y),
                         xend = ifelse(teamId == home_team_id, 100, 0),
                         yend = ifelse(teamId == home_team_id, goalMouthY, 100 - goalMouthY),
                         color = as.factor(teamId)),
                     arrow = arrow(length = unit(0.25, "cm")),
                     size = 1) +
        geom_text(data = eventos_filtrados,
                  aes(x = ifelse(teamId == home_team_id, x, 100 - x),
                      y = ifelse(teamId == home_team_id, y, 100 - y),
                      label = type$displayName,
                      color = as.factor(teamId)),  # Define a cor do texto com base no teamId
                  vjust = -0.75) +
        scale_color_manual(values = team_colors,
                           breaks = c(home_team_id, away_team_id),
                           labels = c(home_team, away_team)) +
        labs(title = paste(home_team, "vs", away_team)) +
        theme(legend.position = "top")
      
      p <- p +
        geom_segment(
          data = eventos_filtrados[eventos_filtrados$type$displayName == "Pass", ],
          aes(x = ifelse(teamId == home_team_id, x, 100 - x),
              y = ifelse(teamId == home_team_id, y, 100 - y),
              xend = ifelse(teamId == home_team_id, endX, 100 - endX),
              yend = ifelse(teamId == home_team_id, endY, 100 - endY),
              color = as.factor(teamId)),  # Adicionado color para atualizar a cor da seta
          arrow = arrow(length = unit(0.125, "cm")),
          size = 0.5
        ) +
        geom_point(
          data = eventos_filtrados,
          aes(x = ifelse(teamId == home_team_id, x, 100 - x),
              y = ifelse(teamId == home_team_id, y, 100 - y),
              fill = type$displayName),
          shape = 21,
          color = "black",
          size = 3,
          stroke = 0.5
        ) +
        
        scale_fill_manual(
          values = c("#FF0000", "#0000FF", "#00FF00", "#FFFF00", "#FF00FF", "#CD1343", "#1A237E", "#4CAF50", "#FFC107", "#673AB7",
                     "#F44336", "#2196F3", "#8BC34A", "#FF9800", "#3F51B5", "#E91E63", "#03A9F4", "#CDDC39", "#FF5722", "#9C27B0",
                     "#009688", "#FFEB3B", "#795548", "#9E9E9E", "#FF4081", "#00BCD4", "#FFCDD2", "#C8E6C9", "#FFF9C4", "#B2DFDB",
                     "#F8BBD0", "#BBDEFB", "#AAAAAA"),
          labels = tipos_eventos
        ) +
        xlim(0, 100) +
        ylim(0, 100)
      
      return(p)
    })
  }
)
