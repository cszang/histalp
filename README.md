# histalp

An R package to extract data from the 5'x5' [HISTALP](http://www.zamg.ac.at/histalp/dataset/grid/five_min.php) grid.

## Dependencies

```r
install.packages("ncdf4", "devtools")
```

## Installation

```r
devtools::install_github("cszang/histalp")
```

## Startup

After loading the package, it will check for the HISTALP netCDF files, and if it does not find them, it will download and unzip them in `~`.

## Usage

```r
extract <- get_histalp(lon = 11.23, lat = 44.21)
```

