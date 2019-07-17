#' Get distance from initial speed
#' @param v0 Initial speed, in m/s
#' @param a Acceleration (currently fixed), decceleration of 10m/s^2 by default.
#' @return Achieved distance, in m
#' @author Laure Cougnaud, Maxim Nazarov
getDistanceFromSpeed <- function(v0, a = 10){
  # vEnd = vInit + 2 * a * d
  # vEnd = 0
  d <- (v0^2)/(2*abs(a))
  return(d) 
}


#' Get initial speed of the projectile from the distance
#' 
#' From https://en.wikipedia.org/wiki/Projectile_motion, 
#' section: 'Maximum distance of projectile'
#' @param d Distance in m
#' @param theta Launching angle, 45 by default
#' @return Speed in m/s
#' @author Laure Cougnaud
getSpeedFromDistance <- function(d, theta = pi / 4){
  g <- 9.81 # gravitation constant (m/s^2)
  v0 <- sqrt( (g * abs(d)) / sin(2 * theta) )
  return(v0)
}

#' Get Kinetic Energy
#' @param m Mass of the object in kg
#' @param v Object velocity, in m/s
#' @return Kinetic Energy in Joules
#' @author Laure Cougnaud
getKE <- function(m, v){
  return(1/2 * m * v^2)
}

#' Get velocity from Kinetic Energy and Mass
#' @param KE Kinetic Energy in Joules
#' @param m Mass of the object in kg
#' @return Object velocity, in m/s
#' @author Laure Cougnaud
getVFromKE <- function(KE, m){
  return(sqrt((2*KE)/m))
}

#' Get Mass of a ball
#' @param type String with ball type
#' @return Mass of the target in grams
#' @author Laure Cougnaud
getMass <- function(type){
  # normal ball: between 680 et 710 grams
  # target ball: between 10 and 18 grams
  ifelse(type == "target", 0.015, 0.680)
}


