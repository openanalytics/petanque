#' Sample possible distributions choices from the big list
#' @param n Number of distributions to sample 
sampleDistribution <- function(n = 3) {
  # TODO: distribution selection
  sample(c("normal(4, 3)", "normal(5, 2)", "poisson(6)", "poisson(8)", "gamma(6, 2)", "gamma(5, 1)"), size = n, replace = FALSE)
}

#' Sample target (jack) location
sampleTarget <- function() {
  # TODO
  runif(1, 3, 9)
}