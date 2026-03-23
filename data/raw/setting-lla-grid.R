rm(list = ls())

library(terra)
source("R/spatial_data_fusion/fill_grid.R")



lat_lon_alt <- rast("/scratch2/ahuerta/datasets/grid/topographic_variables/PCA_topo_vars_sa_gf_resample-10km.nc")
lat_lon_alt <- c(lat_lon_alt[[c(3, 2, 1)]])

current_extent <- ext(lat_lon_alt)
new_xmin <- current_extent[1]
new_xmax <- current_extent[2] + 2
new_ymin <- current_extent[3]
new_ymax <- current_extent[4]

new_extent <- ext(new_xmin, new_xmax, new_ymin, new_ymax)
extended_lat_lon_alt <- extend(lat_lon_alt, new_extent)
extended_lat_lon_alt <-  rast(lapply(extended_lat_lon_alt, fill_na_iteratively))

extended_lat_lon_alt <- sds(as.list(extended_lat_lon_alt))
names(extended_lat_lon_alt) <- c("LON", "LAT", "ALT")

writeCDF(
  extended_lat_lon_alt,
  "data/processed/lla-sa-10km.nc"
)
