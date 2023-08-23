# ...
if (!requireNamespace("ggforce", quietly = TRUE)) {
  install.packages("ggforce")
}
library(ggforce)
library(dplyr)
library(ggplot2)
library(ggsoccer)


# Carregue o arquivo JSON com os eventos
dados_eventos <- jsonlite::fromJSON("Vasco-Cuiaba-Whoscored.json")

# Obtenha os tipos únicos de eventos disponíveis
eventos <- dados_eventos$matchCentreData$events

# Filtrar eventos com type.value = 1
filtered_events <- eventos %>%
  filter(type$value == 1)

# Extrair os dados relevantes
data <- data.frame(
  minute = filtered_events$minute,
  second = filtered_events$second,
  teamId = filtered_events$teamId,
  x = filtered_events$x,
  y = filtered_events$y,
  outcomeType = filtered_events$outcomeType$value,
  playerId = filtered_events$playerId,
  endX = filtered_events$endX,
  endY = filtered_events$endY,
  TargetPlayer = c(filtered_events$playerId[-1], NA)
)
player_positions <- data %>%
  group_by(playerId) %>%
  summarise(mean_x = mean(x), mean_y = mean(y), .groups = "drop")

# Create a new dataframe for successful passes
successful_passes <- data %>%
  filter(!is.na(TargetPlayer) & outcomeType == "1")

# Create graphs for each team
unique_teams <- unique(data$teamId)



for (team_id in unique_teams) {
  team_data <- filter(data, teamId == team_id)
  team_player_positions <- filter(player_positions, playerId %in% team_data$playerId)
  team_successful_passes <- filter(successful_passes, playerId %in% team_data$playerId)
  
  pass_coords <- data.frame()
  
  for (player_id in unique(team_successful_passes$playerId)) {
    player_passes <- filter(team_successful_passes, playerId == player_id)
    pass_targets <- player_passes$TargetPlayer
    
    if (length(pass_targets) > 0) {
      player_position <- filter(team_player_positions, playerId == player_id)
      
      if (nrow(player_position) > 0) {
        target_positions <- filter(team_player_positions, playerId %in% pass_targets)
        
        if (nrow(target_positions) > 0) {
          pass_coord <- data.frame(
            mean_x_origem = rep(player_position$mean_x, nrow(target_positions)),
            mean_y_origem = rep(player_position$mean_y, nrow(target_positions)),
            mean_x_destino = target_positions$mean_x,
            mean_y_destino = target_positions$mean_y,
            passes = nrow(player_passes),
            pass_percentage = nrow(player_passes) / nrow(team_successful_passes)
          )
          
          pass_coords <- bind_rows(pass_coords, pass_coord)
        }
      }
    }
  }
  
  max_passes <- max(pass_coords$passes)
  min_passes <- min(pass_coords$passes)
  
  # Calculate the mean passes for each player
  mean_passes <- team_successful_passes %>%
    group_by(playerId) %>%
    summarize(mean_passes = n())
  
  # Merge mean_passes with team_player_positions
  team_player_positions <- left_join(team_player_positions, mean_passes, by = "playerId")
  
  plot <- ggplot() +
    annotate_pitch() +
    geom_point(data = team_player_positions, aes(x = mean_x, y = mean_y, size = mean_passes)) +
    geom_segment(data = pass_coords %>% filter(!is.na(mean_x_destino) & !is.na(mean_y_destino)),
                 aes(x = mean_x_origem, y = mean_y_origem,
                     xend = mean_x_destino, yend = mean_y_destino),
                 size = ((pass_coords$passes - min_passes) / (max_passes - min_passes)) + 0.5,
                 lineend = "round",
                 alpha = 0.5) +
    geom_text(data = team_player_positions, aes(x = mean_x, y = mean_y, label = playerId), vjust = -1.5, size = 3) +
    ggtitle(paste("Equipe ID:", team_id)) +
    coord_flip() +
    theme_minimal() +
    scale_alpha_continuous(range = c(0.2, 1)) +
    scale_size_continuous(range = c(3, 10))  # Adjust the size range of points
  
  print(plot)
}