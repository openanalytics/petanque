## sound effects
#' @importFrom beepr beep
playSound <- function(which = c("ball", "win", "hit")) {
  which <- match.arg(which)
  
  file <- switch(which,
      "ball" = "button-pressed.ogg",
      "win"  = "fanfare.wav",
      "hit"  = "hit.m4a")
  
  path <- system.file("sounds", file, package = "petanqueApp")
  
  beep(path)
  
}