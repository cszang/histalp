

#' Check for HISTALP data on system
#'
#' Check, if HISTALP data is already present on the system, if not download and
#' install locally.
#' @param base_dir where to store the HISTALP grid files
#' @return nothing, invoked for side effects
#' @importFrom R.utils bunzip2
#' @export
#'
#' @examples
#' \dontrun{
#' check_and_down
#' }
check_and_download <- function(base_dir = "~") {
  # if (!interactive() || stats::runif(1) > 0.1) return()
  clim_params <- c("temperature",
                   "all precipiation",
                   "solid precipitation")
  base_names <- c("HISTALP_temperature_1780-2008.nc",
                  "HISTALP_precipitation_all_abs_1801-2010.nc",
                  "HISTALP_precipitation_solid_abs_1801-2008.nc")
  local_locations <- sapply(base_names, function(x) file.path(base_dir, x))
  zip_base_names <- paste0(base_names, ".bz2")
  zip_locations <- paste0(local_locations, ".bz2")
  web_locations <- paste0("http://www.zamg.ac.at/histalp/download/grid5m/",
                          zip_base_names)

  do_check <- function(param_name,
                       web_location,
                       zip_location,
                       local_location) {
    cat(sprintf("Checking for %s data...", param_name))
    if (!file.exists(local_location)) {
      cat(sprintf(" downloading %s data...", param_name))
      download.file(web_location, zip_location, quiet = TRUE)
      bunzip2(zip_location, local_location)
    }
    cat(" OK.\n")
  }
  u <- mapply(do_check, clim_params, web_locations, zip_locations, local_locations)
  invisible(NULL)
}

.onAttach <- function(...) {
  check_and_download()
}
