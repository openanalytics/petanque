RANKING_FILE <- "/tmp/petanque-ranking.rds"

#' Update ranking based on the result of the game
#' @param players Player names (character vector of length 2)
#' @param winner Winning player's number 1 or 2
#' @param score Score
#' @param file Path to the RDS file with saved ranking
updateRanking <- function(players, winner, score, file = RANKING_FILE) {
  if (file.exists(file))
    oldRanking <- readRDS(file)

  # TODO
}

getRanking <- function(file = RANKING_FILE) {
  # TODO
  data.frame(player = c("Maxim", "Jason"), rating = c(100, 99))
}