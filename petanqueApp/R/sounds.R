## sound effects
#' @importFrom beepr beep
playSound <- function(which = c("ball", "win", "hit")) {
  file <- switch(which,
      "ball" = "ball.wav", # FIXME
      "win"  = "fanfare.wav",
      "hit"  = "ball.wav", # FIXME
      NULL)
  
  if (!is.null(file)) {
    path <- system.file("sounds", file, package = "petanqueApp")
    try(beep(path), silent = TRUE)
  }
  
}