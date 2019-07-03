PLAYER_FILE <- "/tmp/petanque-players.rds"

#' Get a vector of all saved player names
#' @param file Path to the RDS file with saved players
getPlayers <- function(file = PLAYER_FILE) {
  if (file.exists(file))
    out <- readRDS(file)
  else 
    out <- c()
  out
}

#' Add new players to the saved list
#' @param players vector with new player names 
#' @param file Path to the RDS file with saved players 
addPlayers <- function(players, file = PLAYER_FILE) {
  if (file.exists(file))
    out <- readRDS(file)
  else 
    out <- c()
  
  out <- unique(c(out, players))
  saveRDS(out, file)  
}