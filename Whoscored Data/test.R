# Instale e carregue as bibliotecas necessárias
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("ggsoccer", quietly = TRUE)) {
  install.packages("ggsoccer")
}
library(jsonlite)
library(ggplot2)
library(ggsoccer)
library(shiny)

# Carregue o arquivo JSON com os eventos
dados_eventos <- jsonlite::fromJSON("Bahia-Gremio-Whoscored.json")
# Obtenha os tipos únicos de eventos disponíveis
eventos <- dados_eventos$matchCentreData$events

# Função para adicionar a coluna nextPlayerId
addNextPlayerId <- function(data) {
  nextPlayerIds <- c(data[-1], NA)
  data$nextPlayerId <- nextPlayerIds
  return(data)
}

# Filtrar e modificar os eventos
eventos_filtrados <- lapply(eventos, function(evento) {
  if (evento$type == "1" && evento$outcomeType == "1") {
    evento <- addNextPlayerId(evento)
  }
  return(evento)
})

# Exibir o resultado
View(eventos_filtrados)


