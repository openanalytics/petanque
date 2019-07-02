#' Launch 'Petanque' Shiny Application
#' @param debug whether to allow to connect to console for debugging
#' @param ... arguments (eventually) passed to \code{\link[shiny]{runApp}}
#' @author Maxim Nazarov
#' @import shiny
#' @export
runShinyApp <- function(debug = FALSE, ...) {
  shinyApp(
      ui = petanqueUI(debug = debug), 
      server = petanqueServer, options = list(...)
  )
}

# NB: this is not exactly the same as shiny::`%OR%`
`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x)) || x == "") y else x
}
