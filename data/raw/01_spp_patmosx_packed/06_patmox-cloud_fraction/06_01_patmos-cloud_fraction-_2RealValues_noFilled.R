# cloud_fraction		-127*	127*	0.00393701	0.5	meters	None	Cloud fraction computed over a 3x3 pixel array at the native resolution centered on this pixel
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
  
  main_tp = "/scratch2/ahuerta/patmosx/cloud_fraction/raw_rv"
  if (!dir.exists(file.path(main_tp, year_x))) {dir.create(file.path(main_tp, year_x), showWarnings = FALSE, recursive = TRUE)}
  
  lapply(seq_along(names(r_stack)),
         function(ijx){
           
           out_path <- file.path(file.path(main_tp, year_x), paste(names(r_stack)[ijx], ".tif", sep = ""))
           placeholder <- r_stack[[ijx]]
           placeholder <- (placeholder * 0.00393701) + 0.5 # SCALING + Offset
           placeholder[placeholder < 0] <- 0 # CAN NO BE NEGATIVE 
           writeRaster(placeholder, out_path, overwrite = TRUE)
         })
  
  cat("Created:", year_x, "\n")
  
}


