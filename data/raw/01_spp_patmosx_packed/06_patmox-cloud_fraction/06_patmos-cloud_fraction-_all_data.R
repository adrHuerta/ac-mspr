# cloud_fraction
rm(list = ls())

library(terra)
library(zoo)

years_data <- 1998:2021 

for(year_x in years_data){
  
  path_data <- paste("/mnt/climstor2/vol01_ecmwf/download/patmosx/cloud_fraction/raw/", year_x, sep = "")
  raster_files <- list.files(path_data, pattern = "\\.tif$", full.names = TRUE)
  raster_files <- raster_files[order(raster_files)]  
  
  r_stack <- rast(raster_files)
  names(r_stack) <- as.Date(unlist(strsplit(sapply(strsplit(raster_files, "/"), function(x) x[10]), ".tif")))
  
  # ---- Step 2: Convert to a 3D array for easier time-wise access ----
  arr <- as.array(r_stack)  # dims: [rows, cols, time]
  
  # ---- Step 3: Apply linear interpolation for each pixel time series ----
  fill_time_series <- function(x) {
    if (all(is.na(x))) return(x)  # leave all-NA series untouched
    approx(seq_along(x), x, xout = seq_along(x), rule = 2)$y
  }
  
  filled_arr <- apply(arr, c(1, 2), fill_time_series)
  filled_r <- rast(aperm(filled_arr, c(2, 3, 1)), crs = crs(r_stack))
  ext(filled_r) <- ext(r_stack)
  
  main_tp = "/scratch2/ahuerta/patmosx_gf/cloud_fraction/raw"
  if (!dir.exists(file.path(main_tp, year_x))) {dir.create(file.path(main_tp, year_x), showWarnings = FALSE, recursive = TRUE)}
  
  # same values preserved!
  max_poss <- 127
  min_poss <- -127
  
  lapply(seq_along(names(r_stack)),
         function(ijx){
           
           out_path <- file.path(file.path(main_tp, year_x), paste(names(r_stack)[ijx], ".tif", sep = ""))
           placeholder <- filled_r[[ijx]]
           placeholder <- round(placeholder, 2)
           placeholder[placeholder > max_poss] <- max_poss
           placeholder[placeholder < min_poss] <- min_poss
           writeRaster(placeholder, out_path, overwrite = TRUE)
         })
  
  cat("Created:", year_x, "\n")
  
}


