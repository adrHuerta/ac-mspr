source("R/spatial_data_fusion/get_nearby_points3.R")
source("R/spatial_data_fusion/local-model-params.R")
source("R/spatial_data_fusion/point-grid-filling.R")

spatial_data_fusion_engine <- function(pr_xyz,
                                       pr_data,
                                       features_grid,
                                       mask_raster = NULL,
                                       params_mod = list(
                                         LLAorg = TRUE,
                                         Nstations = 60,
                                         Covars =  NULL,
                                         Model = NULL,
                                         Mc.Cores = 175
                                       )
)
{
  
  # 1) creating PrObs points
  PrObs <- pr_xyz
  PrObs$PrObs <- as.numeric(pr_data)
  PrObs <- terra::na.omit(PrObs, "PrObs")
  
  # 2) features
  ## 2.1 creating an empty grid
  na_grid <- features_grid[[1]]
  na_grid[na_grid >= 0 | na_grid < 0] <- NA
  names(na_grid) <- "PrObs"
  
  ## 2.2 stack features
  features_i <- c(features_grid, na_grid)
  
  # 3) PrObs to data.frame
  StatObsCovars <- c("ID", "LON", "LAT", "ALT", "PrObs")
  PrObs_df <- PrObs[, StatObsCovars]
  
  if(params_mod$LLAorg == TRUE){
    Extr_df <- terra::extract(features_i, PrObs_df)[, c(params_mod$Covars)]
    PrObs_df <- cbind(PrObs_df, Extr_df)
  } else {
    Extr_df <- terra::extract(features_i, PrObs_df)[, c("LON", "LAT", "ALT", params_mod$Covars)]
    PrObs_df <- cbind(PrObs_df[, c("ID", "PrObs")], Extr_df)
  }
  
  PrObs_df <- as.data.frame(PrObs_df)
  
  ## 2.3 APPLY MASK (NEW)
  if (!is.null(mask_raster)) {
    features_i <- terra::mask(features_i, mask_raster)
  }
  
  # 4) features to data.frame
  GrObs_df <- as.data.frame(features_i, cells = TRUE)
  colnames(GrObs_df)[1] <- "ID"
  GrObs_df$ID <- as.character(GrObs_df$ID)
  
  # 5) executing spatial model
  PredObs_df <- parallel::mclapply(
    GrObs_df$ID,
    function(jxi) {
      
      df_jxi <- GrObs_df[GrObs_df$ID == jxi, ]
      df_jxi <- rbind(df_jxi, PrObs_df)
      
      nearby_ID <-
        get_nearby_points3(
          xy_target = jxi,
          xy_database = df_jxi,
          lmt_xy = NA,
          lmt_n = params_mod$Nstations
        )
      
      dists <- nearby_ID$distance[-1]
      dz <- nearby_ID$dz[-1]
      
      nearby_ID <- df_jxi[match(nearby_ID$out, df_jxi$ID),]
      
      predictors_data_df <- nearby_ID[, -match(c("ID", "PrObs"), colnames(nearby_ID))]
      
      ## SKIP EMPTY CELLS (NEW safety)
      if (all(is.na(predictors_data_df))) {
        return(data.frame(mod = NA, err = NA))
      }
      
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
      
      response <- grid_filling(target_data = target_data, FUN = params_mod$Model)
      
      data.frame(mod = response[1], err = response[2])
    },
    mc.cores = params_mod$Mc.Cores
  )
  
  # 6) rebuild rasters
  PredObs_df <- do.call(rbind, PredObs_df)
  
  PredObs_df_f <- GrObs_df
  PredObs_df_f[, c("mod", "err")] <- PredObs_df
  
  pr_new <- features_i$PrObs
  pr_err <- features_i$PrObs
  
  terra::values(pr_new)[as.numeric(PredObs_df_f$ID)] <- PredObs_df_f$mod
  names(pr_new) <- "pr"
  
  terra::values(pr_err)[as.numeric(PredObs_df_f$ID)] <- PredObs_df_f$err
  names(pr_err) <- "err"
  
  return(c(pr_new, pr_err))
}