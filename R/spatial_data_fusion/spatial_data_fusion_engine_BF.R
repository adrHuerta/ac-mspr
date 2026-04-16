source("R/spatial_data_fusion/get_nearby_points3.R")
source("R/spatial_data_fusion/local-model-params.R")
source("R/spatial_data_fusion/point-grid-filling.R")

spatial_data_fusion_engine_BF <- function(pr_xyz,
                                       pr_data,
                                       features_grid,
                                       params_mod = list(
                                         LLAorg = TRUE,
                                         Nstations = 60,
                                         Covars =  NULL,
                                         Model = NULL,
                                         Mc.Cores = 175
                                       ))
{
  
  # 1) creating PrObs points
  PrObs <- pr_xyz
  PrObs$PrObs <- as.numeric(pr_data)
  # PrObs <- terra::crop(PrObs, terra::ext(pr_box))
  PrObs <- terra::na.omit(PrObs, "PrObs")
  
  # 2) features
  ## 2.1 creating a empty grid
  na_grid <- features_grid[[1]]
  na_grid[na_grid >= 0 | na_grid < 0] <- NA
  names(na_grid) <- "PrObs"
  ## 2.2 add new empty grid
  features_i <- c(features_grid, na_grid)
  
  # 3) PrObs to data.frame (used for spatial model building)
  # at this stage we can opt not to use or to use original LATLONALT
  # 3.1 condition
  StatObsCovars <- c("ID", "LON", "LAT", "ALT", "PrObs")
  PrObs_df <- PrObs[, StatObsCovars]
  
  if(params_mod$LLAorg == TRUE){
    # Keep original lat, lon, and alt in the dataframe and add extracted covariates
    Extr_df <- extract(features_i, PrObs_df)[, c(params_mod$Covars)]
    PrObs_df <- cbind(PrObs_df, Extr_df)
    
  } else {
    # Include lat, lon, and alt with extracted covariates in this case
    Extr_df <- extract(features_i, PrObs_df)[, c("LON", "LAT", "ALT", params_mod$Covars)]
    PrObs_df <- cbind(PrObs_df[, c("ID", "PrObs")], Extr_df)
  }
  
  PrObs_df <- as.data.frame(PrObs_df)
  
  
  # 4) features to data.frame (used for spatial model building)
  GrObs_df <- as.data.frame(features_i, cells = TRUE)
  colnames(GrObs_df)[1] <- "ID"
  GrObs_df$ID <- as.character(GrObs_df$ID)
  
  # 5) executing spatial model
  PredObs_df <- parallel::mclapply(
    GrObs_df$ID,
    function(jxi) {
      
      ## 5.1 merging both point station and target grid
      df_jxi <- GrObs_df[GrObs_df$ID == jxi, ]
      df_jxi <- rbind(df_jxi, PrObs_df)
      
      ## 5.2 selecting close nearby point station to target grid
      nearby_ID <-
        get_nearby_points3(xy_target = jxi,
                           xy_database = df_jxi,
                           lmt_xy = NA,
                           lmt_n = params_mod$Nstations)
      
      dists <- nearby_ID$distance[-1]
      dz <- nearby_ID$dz[-1]
      
      nearby_ID <- df_jxi[match(nearby_ID$out, df_jxi$ID),]
      nearby_ID <- nearby_ID
      
      predictors_data_df <- nearby_ID[, -match(c("ID", "PrObs"), colnames(nearby_ID))]
      
      ## 5.3 building input data to spatial model
      target_data <- list(values_data = nearby_ID$PrObs,
                          predictors_data = predictors_data_df,
                          covars = params_mod$Covars,
                          case.weights = epanechnikov_weights_3D(
                            dists_2D = dists,
                            dz = dz,
                            alpha = compute_alpha(dists_2D = dists, dz = dz)
                          )
      )
      
      ## 5.4 spatial model inference
      response <- grid_filling(target_data = target_data, FUN = params_mod$Model)
      data.frame(response)
      
    },
    mc.cores = params_mod$Mc.Cores
  )
  
  
  PredObs_df <- do.call(rbind, PredObs_df)
  PredObs_df_f <- GrObs_df
  PredObs_df_f[match(PredObs_df_f$ID, PredObs_df_f$ID), c("mod", "err", "clas_best", "clas_bestP", "reg_best", "reg_bestP")] <- PredObs_df
  pr_new <- features_i$PrObs; pr_err <- features_i$PrObs
  pr_clas_best <- features_i$PrObs; pr_clas_bestP <- features_i$PrObs
  pr_reg_best <- features_i$PrObs; pr_reg_bestP <- features_i$PrObs
  values(pr_new)[as.numeric(PredObs_df_f$ID)] <- PredObs_df_f$mod
  names(pr_new) <- "pr"
  values(pr_err)[as.numeric(PredObs_df_f$ID)] <- PredObs_df_f$err
  names(pr_err) <- "err"
  values(pr_clas_best)[as.numeric(PredObs_df_f$ID)] <- PredObs_df_f$clas_best
  names(pr_clas_best) <- "clas_best"
  values(pr_clas_bestP)[as.numeric(PredObs_df_f$ID)] <- PredObs_df_f$clas_bestP
  names(pr_clas_bestP) <- "clas_bestP"
  values(pr_reg_best)[as.numeric(PredObs_df_f$ID)] <- PredObs_df_f$reg_best
  names(pr_reg_best) <- "reg_best"
  values(pr_reg_bestP)[as.numeric(PredObs_df_f$ID)] <- PredObs_df_f$reg_bestP
  names(pr_reg_bestP) <- "reg_bestP"
  
  return(c(pr_new, pr_err, pr_clas_best, pr_clas_bestP, pr_reg_best, pr_reg_bestP))
}