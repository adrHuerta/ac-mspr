# cld_press_acha	hPa	-32767*	32767*	0.0167852	550	meters	None	Cloud-top pressure computed using ACHA
rm(list = ls())

library(terra)
library(zoo)

years_data <- 1998:2021 

for(year_x in years_data){
  
  path_data <- paste("/scratch2/ahuerta/patmosx_gf/cld_press_acha/raw/", year_x, sep = "")
  raster_files <- list.files(path_data, pattern = "\\.tif$", full.names = TRUE)
  raster_files <- raster_files[order(raster_files)]  
  
  r_stack <- rast(raster_files)
  names(r_stack) <- as.Date(unlist(strsplit(sapply(strsplit(raster_files, "/"), function(x) x[8]), ".tif")))
  
  main_tp = "/scratch2/ahuerta/patmosx_gf/cld_press_acha/raw_rv"
  if (!dir.exists(file.path(main_tp, year_x))) {dir.create(file.path(main_tp, year_x), showWarnings = FALSE, recursive = TRUE)}

  lapply(seq_along(names(r_stack)),
         function(ijx){
           
           out_path <- file.path(file.path(main_tp, year_x), paste(names(r_stack)[ijx], ".tif", sep = ""))
           placeholder <- r_stack[[ijx]]
           placeholder <- (placeholder * 0.0167852) + 550 # SCALING + Offset
           placeholder[placeholder < 0] <- 0 # CAN NO BE NEGATIVE 
           writeRaster(placeholder, out_path, overwrite = TRUE)
         })
  
  cat("Created:", year_x, "\n")
  
}


