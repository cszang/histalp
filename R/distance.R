##' Convert a coordinate from decimal degrees to radians
##'
##' This function converts coordinates from decimal degrees to radians.
##' @title Convert degrees to radians
##' @param x the coordinate in decimal degrees
##' @return the coordinate as radians
##' @examples
##' degrees_to_radians(40.81)
##' @export
degrees_to_radians <- function(x) {
  x * pi / 180
}

##' Calculate the distance between two points as great circle distance
##' on the globe
##'
##' Uses information from
##' http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates, where
##' dist = arccos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) *
##' cos(lon1 - lon2)) * R
##' @title Great circle distance
##' @param x1 coordinate pair 1, as list with $lat and $lon
##' @param x2 coordinate pair 2, as list with $lat and $lon
##' @return The distance on the globe in kilometers.
##' @examples
##' great_circle_dist(list(lon = 0.3, lat = 40.91), list(lon = 1.3,
##' lat = 41.02))
##' @export
great_circle_dist <- function(x1, x2) {
  X1 <- degrees_to_radians(x1$lat)
  X2 <- degrees_to_radians(x2$lat)
  Y1 <- degrees_to_radians(x1$lon)
  Y2 <- degrees_to_radians(x2$lon)
  dist <- acos(sin(X1) * sin(X2) + cos(X1) * cos(X2) * cos(Y1 - Y2)) * 6370
  dist
}
