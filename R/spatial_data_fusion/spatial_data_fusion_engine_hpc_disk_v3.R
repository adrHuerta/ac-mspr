source("R/spatial_data_fusion/get_nearby_points3.R")
source("R/spatial_data_fusion/local-model-params.R")
source("R/spatial_data_fusion/point-grid-filling.R")

spatial_data_fusion_engine_hpc_disk <- function(pr_xyz,
                                                pr_data,
                                                features_grid,
                                                shapefile_path,       # Path to shapefile
                                                output_dir,
                                                output_name,
                                                params_mod = list(
                                                  LLAorg = TRUE,
                                                  Nstations = 60,
                                                  Covars = NULL,
                                                  Model = NULL,
                                                  Mc.Cores = NULL,
                                                  pixel_chunk_size = 1000
                                                )) {

  library(terra)
  
  #-------------------------
  # Prepare PrObs points
  PrObs <- pr_xyz
  PrObs$PrObs <- as.numeric(pr_data)
  PrObs <- terra::na.omit(PrObs, "PrObs")
  
  #-------------------------
  # Prepare features grid
  na_grid <- features_grid[[1]]
  na_grid[!is.na(na_grid[])] <- NA
  names(na_grid) <- "PrObs"
  features_i <- c(features_grid, na_grid)
  
  #-------------------------
  # PrObs to dataframe with covariates
  StatObsCovars <- c("ID", "LON", "LAT", "ALT", "PrObs")
  PrObs_df <- PrObs[, StatObsCovars]
  
  # Extract covariates for all stations first
  if(params_mod$LLAorg){
    Extr_df <- terra::extract(features_i, PrObs_df)[, params_mod$Covars, drop=FALSE]
    PrObs_df <- cbind(PrObs_df, Extr_df)
  } else {
    Extr_df <- terra::extract(features_i, PrObs_df)[, c("LON","LAT","ALT",params_mod$Covars), drop=FALSE]
    PrObs_df <- cbind(PrObs_df[, c("ID","PrObs")], Extr_df)
  }
  
  PrObs_df <- as.data.frame(PrObs_df)
  
  #-------------------------
  # Mask grid pixels with shapefile (only for prediction)
  shp <- terra::vect(shapefile_path)
  features_i[[1]] <- terra::mask(features_i[[1]], shp)
  
  #-------------------------
  # Features grid to dataframe
  GrObs_df <- as.data.frame(features_i, cells = TRUE)
  colnames(GrObs_df)[1] <- "ID"
  GrObs_df$ID <- as.character(GrObs_df$ID)
  
  # Keep only valid pixels (inside shapefile)
  valid_cells <- GrObs_df$ID[!is.na(features_i[[1]][as.numeric(GrObs_df$ID)])]
  GrObs_df <- GrObs_df[GrObs_df$ID %in% valid_cells, ]
  
  #-------------------------
  # Chunk pixels
  pixel_chunks <- split(
    GrObs_df$ID,
    ceiling(seq_along(GrObs_df$ID) / params_mod$pixel_chunk_size)
  )
  
  #-------------------------
  # Create output dir
  if(!dir.exists(output_dir)){
    dir.create(output_dir, recursive = TRUE)
  }
  
  #-------------------------
  # Initialize final rasters
  mod_rast_final <- features_i[[1]]
  mod_rast_final[] <- NA
  
  err_rast_final <- features_i[[1]]
  err_rast_final[] <- NA
  
  #-------------------------
  # Loop over chunks
  for(chunk_idx in seq_along(pixel_chunks)){
    
    chunk <- pixel_chunks[[chunk_idx]]
    message("Processing chunk ", chunk_idx, " / ", length(pixel_chunks))
    
    Pred_chunk <- parallel::mclapply(chunk, function(jxi){
      
      # Merge grid pixel + stations
      df_jxi <- GrObs_df[GrObs_df$ID == jxi, , drop = FALSE]
      df_jxi <- rbind(df_jxi, PrObs_df)
      
      # Nearby stations
      nearby_ID <- get_nearby_points3(
        xy_target = jxi,
        xy_database = df_jxi,
        lmt_xy = NA,
        lmt_n = params_mod$Nstations
      )
      
      dists <- nearby_ID$distance[-1]
      dz <- nearby_ID$dz[-1]
      
      nearby_ID <- df_jxi[match(nearby_ID$out, df_jxi$ID), ]
      predictors_data_df <- nearby_ID[, setdiff(colnames(nearby_ID), c("ID","PrObs")), drop=FALSE]
      
      # Model input
      target_data <- list(
        values_data = nearby_ID$PrObs,
        predictors_data = predictors_data_df,
        covars = params_mod$Covars,
        case.weights = epanechnikov_weights_3D(
          dists_2D = dists,
          dz = dz,
          alpha = compute_alpha(dists_2D = dists, dz = dz)
        )
      )
      
      # Model prediction
      response <- grid_filling(
        target_data = target_data,
        FUN = params_mod$Model
      )
      
      list(
        ID = jxi,
        mod = response[1],
        err = response[2]
      )
      
    }, mc.cores = params_mod$Mc.Cores)
    
    #-------------------------
    # Fill final rasters
    for(pixel_res in Pred_chunk){
      cell_id <- as.numeric(pixel_res$ID)
      mod_rast_final[cell_id] <- pixel_res$mod
      err_rast_final[cell_id] <- pixel_res$err
    }
    
    gc()
  }
  
  #-------------------------
  # Write final rasters

  out_rast <- c(mod_rast_final, err_rast_final)
  names(out_rast) <- c("pred", "err")

  terra::writeRaster(
    out_rast,
    filename = file.path(output_dir,  paste0("pr_", output_name, "_.tif")),
    overwrite = TRUE
  )
  
  return(TRUE)
}