## sound effects
#' @importFrom beepr beep
playSound <- function(which = c("ball", "win", "hit")) {
  file <- switch(which,
      "ball" = "button-pressed.ogg",
      "win"  = "fanfare.wav",
      "hit"  = "hit.m4a",
      NULL)
  
  if (!is.null(file)) {
    path <- system.file("sounds", file, package = "petanqueApp")
    try(beep(path), silent = TRUE)
  }
  
}