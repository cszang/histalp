#' Extract climate data from nearest HISTALP gridcell
#'
#' @param lon Longitude
#' @param lat Latitude
#' @param na.rm Should the output be restricted to the common time span for all
#'   variables (default is TRUE)
#' @param base_dir Where to look for the HISTALP netCDF files
#'
#' @return a \code{data.frame}
#' @import ncdf4
#' @export
#'
#' @examples
#' get_histalp(10.2, 44.8)
get_histalp <- function(lon, lat, na.rm = TRUE, base_dir = "~") {
  clim_params <- CLIMPARAMS()
  nc_param_names <- c("T_2M", "TOT_PREC", "PREC_solid")
  start_dates <- as.Date(c("1780/01/01", "1801/01/01", "1801/01/01"))
  base_names <- BASENAMES()
  local_locations <- sapply(base_names, function(x) file.path(base_dir, x))
  ncs <- lapply(local_locations, nc_open)
  on.exit(lapply(ncs, nc_close))
  names(ncs) <- clim_params

  obs_length <- mapply(function(nc, pname){
    slice <- ncvar_get(nc, pname, start = c(1, 1, 1),
                       count = c(1, 1, -1))
    length(slice)
  }, ncs, nc_param_names)

  lats <- ncvar_get(ncs[[1]], "lat")
  lons <- ncvar_get(ncs[[1]], "lon")
  lat_lon <- expand.grid(lons, lats)
  lat_lon_rad <- sapply(lat_lon, degrees_to_radians)
  lon_rad <- degrees_to_radians(lon)
  lat_rad <- degrees_to_radians(lat)
  min_dist_index <- which.min(apply(lat_lon_rad, 1, function(x) {
    great_circle_dist(list(lon = x[1], lat = x[2]), list(lon = lon_rad, lat = lat_rad))
  }))
  best_match <- lat_lon[min_dist_index,][1,]
  best_lon_index <- which(lons == best_match[1,1])
  best_lat_index <- which(lats == best_match[1,2])

  cat(sprintf("Extracting climate variables for the nearest HISTALP gridcell (Lon: %.3f, Lat: %.3f)\n",
              best_match[1, 1], best_match[1, 2]))

  dates <- mapply(function(start, count) {
    .dates <- seq(start, length.out = count, by = "1 month")
    year <- as.integer(substr(.dates, 1, 4))
    month <- as.integer(substr(.dates, 6, 7))
    data.frame(year, month)
  }, start_dates, obs_length, SIMPLIFY = FALSE)

  slices <- mapply(function(nc, pname, date) {
    x <- ncvar_get(nc, pname,
              start = c(best_lon_index, best_lat_index, 1),
              count = c(1, 1, -1))
    date[[pname]] <- x
    date
  }, ncs, nc_param_names, dates, SIMPLIFY = FALSE)
  out <- Reduce(function(x, y) merge(x, y, all = !na.rm), slices)
  out[order(out$year, out$month), ]
}
