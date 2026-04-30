rm(list = ls())

source("./features_utils.R")
library(terra)

main_path_to_read <- "/scratch2/ahuerta/patmosx_gf"
main_gsmap_path_to_read <- "/mnt/climstor2/vol01_ecmwf/download/gsmap_v8_op"
main_path_to_save <- file.path(main_path_to_read, "features_gsmap")

fill_na_iteratively <- function(raster_object, window_size = 3) {
  # Keep looping until there are no more NA pixels
  repeat {
    # Count the number of NA pixels before filling
    na_count_before <- sum(is.na(terra::values(raster_object)), na.rm = TRUE)
    
    # Fill missing pixels using the focal function
    raster_object[is.na(raster_object)] <- focal(raster_object, 
                                                 w = matrix(1, window_size, window_size), 
                                                 fun = mean, na.rm = TRUE)[is.na(raster_object)]
    
    # Count the number of NA pixels after filling
    na_count_after <- sum(is.na(terra::values(raster_object)), na.rm = TRUE)
    
    # If no NA pixels were filled in this iteration, exit the loop
    if (na_count_after == na_count_before) {
      break
    }
  }
  
  return(raster_object)
}

for(year_x in 1998:2021){
  
  year_x_date <- seq(as.Date(paste0(year_x, "-", "01-01")), as.Date(paste0(year_x, "-", "12-31")), freq = "day")
  year_x_date_file <- paste0(year_x_date, ".tif")
  
  
  if (!dir.exists(file.path(main_path_to_save, year_x))) {dir.create(file.path(main_path_to_save, year_x), showWarnings = FALSE, recursive = TRUE)}
  
  
  lapply(
    year_x_date_file,
    function(year_x_date_i){
      
      pr_sat_data <- file.path(main_gsmap_path_to_read, "raw", year_x, year_x_date_i)
      pr_sat_data <- rast(pr_sat_data)
      
      cld_height_acha <- file.path(main_path_to_read, "cld_height_acha", "raw_rv", year_x, year_x_date_i)
      cld_height_acha <- rast(cld_height_acha)
      
      cld_temp_acha <- file.path(main_path_to_read, "cld_temp_acha", "raw_rv", year_x, year_x_date_i)
      cld_temp_acha <- rast(cld_temp_acha)
      
      cld_opd_acha <- file.path(main_path_to_read, "cld_opd_acha", "raw_rv", year_x, year_x_date_i)
      cld_opd_acha <- rast(cld_opd_acha)
      
      cld_press_acha <- file.path(main_path_to_read, "cld_press_acha", "raw_rv", year_x, year_x_date_i)
      cld_press_acha <- rast(cld_press_acha)
      
      cloud_fraction <- file.path(main_path_to_read, "cloud_fraction", "raw_rv", year_x, year_x_date_i)
      cloud_fraction <- rast(cloud_fraction)
      
      cloud_probability <- file.path(main_path_to_read, "cloud_probability", "raw_rv", year_x, year_x_date_i)
      cloud_probability <- rast(cloud_probability)
      
      cloud_water_path <- file.path(main_path_to_read, "cloud_water_path", "raw_rv", year_x, year_x_date_i)
      cloud_water_path <- rast(cloud_water_path)
      
      
      PrSat <- pr_sat_data # pr sat
      PrSat_bin <- precip_binary(pr_sat_data) # pr sat binary same as for analogues
      
      Hn   <- normalize_f(cld_height_acha) # high height
      Hn[Hn < 0] <- 0
      H_   <- cld_height_acha # high height
      Tn   <- normalize_f(cld_temp_acha)
      Tn[Tn < 0] <- 0
      T_   <- cld_temp_acha
      OPDn <- normalize_f(cld_opd_acha) # thick cloud
      OPDn[OPDn < 0] <- 0
      OPD_ <- cld_opd_acha # thick cloud
      Pn   <- normalize_f(cld_press_acha)
      Pn[Pn < 0] <- 0
      P_   <- cld_press_acha
      CFn  <- normalize_f(cloud_fraction)
      CFn[CFn < 0] <- 0
      CF_  <- cloud_fraction
      CPn  <- normalize_f(cloud_probability) # cloud fraction ~ cloud probability
      CPn[CPn < 0] <- 0
      CP_  <- (cloud_probability) # cloud fraction ~ cloud probability
      CWPn <- normalize_f(cloud_water_path) # large water path
      CWPn[CWPn < 0] <- 0
      CWP_ <- cloud_water_path # large water path
      Tcold <- 1 - Tn # cold temp
      Phigh <- 1 - Pn # low pressure
      
      DSI <- sqrt(Hn * OPDn)
      DCI <- (Hn + Tcold + Phigh) / 3
      MDI <- (OPDn + CWPn) / 2
      FDI_core <- (Hn * Tcold * Phigh * OPDn * CWPn)^(1/5)
      FDI <- FDI_core * CFn # CPn
      FDI[is.na(FDI)] <- focal(FDI, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)[is.na(FDI)]
      
      OPD_eff <- OPDn * CFn # CPn
      CWP_eff <- CWPn * CFn # CPn
      CP_gra <- get_cloud_gradient(cloud_probability)
      CF_gra <- get_cloud_gradient(cloud_fraction)
      CP_gra_n <- normalize_f(CP_gra)
      CF_gra_n <- normalize_f(CF_gra)
      
      patmoxs_features <- c(PrSat, PrSat_bin,
                            H_, T_, OPD_, P_, CF_, CWP_, DSI, DCI, MDI,
                            FDI, OPD_eff, CWP_eff, CF_gra)
      
      patmoxs_features <- lapply(patmoxs_features, fill_na_iteratively)
      patmoxs_features <- rast(patmoxs_features)
      # patmoxs_features <- crop(patmoxs_features, ext(-85.0, -29.9, -30, 13))

      patmoxs_features <- sds(as.list(patmoxs_features))
      
      names(patmoxs_features) <- c("PrSat", "PrSatB",
                                   "H", "T", "OPD", "P", "CF", "CWP", "DSI", "DCI", "MDI",
                                   "FDI", "OPD_eff", "CWP_eff", "CF_gra")
      
      out_path <- file.path(main_path_to_save, year_x, gsub(".tif", ".nc", year_x_date_i))
      writeCDF(patmoxs_features, out_path, overwrite = TRUE, split = TRUE, compresion = 5)

    }
    )
  
  cat("Created:", year_x, "\n")
  
}
