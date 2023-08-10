library(jsonlite)
json_data <- jsonlite::fromJSON("Bahia-Gremio-Whoscored.json")
# View(json_data)
# Supondo que json_data seja a estrutura de dados que contém os seus dados


pass_events <- json_data[["matchCentreData"]][["events"]]

for (i in seq_along(pass_events)) {
  event <- pass_events[[i]]
  event_type <- event[["type"]][["displayName"]]
  outcome_type <- event[["outcomeType"]][["displayName"]]
  
  if (event_type == "Pass" && outcome_type == "Unsuccessful") {
    cat("Pass malsucedido encontrado:\n")
    cat("Posição inicial (X, Y):", event[["x"]], ", ", event[["y"]], "\n")
    cat("Posição final (endX):", event[["endX"]], "\n")
    cat("ID do time:", event[["teamId"]], "\n")
    cat("------\n")
  }
}