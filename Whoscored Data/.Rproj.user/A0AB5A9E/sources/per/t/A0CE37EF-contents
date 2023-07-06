# Instale e carregue as bibliotecas necessárias
if (!requireNamespace("ggsoccer", quietly = TRUE)) {
  install.packages("ggsoccer")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}
library(ggsoccer)
library(jsonlite)

# Carregue o arquivo JSON com os eventos
json_data <- jsonlite::fromJSON("Bahia-Gremio-Whoscored.json")

# Obtenha a lista de eventos do objeto json_dataa
eventos <- json_data$matchCentreData$events

# Filtrar os eventos que possuem os atributos 'isShot' e 'isGoal'
eventos_filtrados <- eventos[ !is.na(eventos$isGoal), ]


# Obter informações dos times
home_team <- json_data$matchCentreData$home$name
away_team <- json_data$matchCentreData$away$name
home_team_id <- json_data$matchCentreData$home$teamId
away_team_id <- json_data$matchCentreData$away$teamId

# Definir as cores dos times
team_colors <- c(home = "#0000FF", away = "#FF0000")
ggplot() +
  annotate_pitch() +
  theme_pitch() +
  geom_segment(data = eventos_filtrados, 
               aes(x = ifelse(teamId == home_team_id, 100 - x, x),
                   y = ifelse(teamId == home_team_id, 100 - y, y),
                   xend = ifelse(teamId == home_team_id, 0, 100),
                   yend = ifelse(teamId == home_team_id, 100 - goalMouthY, goalMouthY),
                   color = as.factor(teamId)),
               arrow = arrow(length = unit(0.25, "cm")),
               size = 1) +
  geom_text(data = eventos_filtrados,
            aes(x = ifelse(teamId == home_team_id, 100 - x, x),
                y = ifelse(teamId == home_team_id, 100 - y, y),
                label = type$displayName),
            color = "black",
            vjust = -0.75) +
  scale_color_manual(values = team_colors,
                     breaks = c(home_team_id, away_team_id),
                     labels = c(home_team, away_team)) +
  labs(title = paste(home_team, "vs", away_team)) +
  theme(legend.position = "top")
