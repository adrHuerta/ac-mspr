rm(list = ls())

library(terra)
library(xts)
source("R/spatial_data_fusion/spatial_data_fusion_engine_CV.R")
source("R/spatial_data_fusion/fill_grid.R")

pr_box = c(-83, -34, -25, 15)
pr_box_1 = c(-83, -34, -25 - .5, 15 + .5)

# pp data
sc_prec4sa <- readRDS("/scratch2/ahuerta/datasets/observed_precipitation/sc-prec4sa/rds_xgb/SC-PREC4SA_obs_bc.RDS")
pr_xyz <- sc_prec4sa$xyz
pr_data <- sc_prec4sa$data

# wts data
wts_data <- read.csv("output/00_weather-types/WTs_tSA.csv")
wts_data$time <- as.Date(wts_data$time)

# cv days
cv_days <- read.csv("output/02_ENSO-cv-sample-days/CV_days.csv")

features_path = "/scratch2/ahuerta/patmosx_gf"

for(date_to_merge in cv_days$dates){
  
  # date_to_merge = cv_days$dates[1]
  date_to_merge <- as.Date(date_to_merge)
  print(date_to_merge)
  
  pr_xyz_r <- terra::vect(pr_xyz, geom = c("LON", "LAT"), crs = "+proj=longlat +datum=WGS84", keepgeom = TRUE)
  pr_xyz_r <- crop(pr_xyz_r, terra::ext(pr_box))
  pr_data_r <- pr_data[date_to_merge, match(pr_xyz_r$ID, colnames(pr_data))]
  
  # choosing the "best" analogue (one single member)
  get_analogues_dates <- readRDS(paste0("output/03_precipitation-pattern-analogue/cv/cv-obs_bc/",
                                         date_to_merge, ".RDS"))
  get_analogues_dates <- transform(get_analogues_dates, mean_metric = (dr  + dr_p90 + mcc)/3)
  get_analogues_dates <- get_analogues_dates[order(get_analogues_dates$mean_metric, decreasing = TRUE), ][nrow(get_analogues_dates), ]
  
  analogue_date_i <- get_analogues_dates$date[1]
  sat_date_i <- get_analogues_dates$sat[1]
  
  features_dir <- file.path(
    features_path, 
    paste0("features_", sat_date_i)
  )
  features_dir <- rast(
    file.path(
      features_dir, format(as.Date(analogue_date_i), "%Y"),
      paste0(analogue_date_i, ".nc"))
  )
  # features_grid <- focal(features_dir, w = 3, fun = "mean", expand = TRUE) # same as exploration
  # features_grid <- crop(features_grid, terra::ext(pr_box_1)) # it creates son NA pixel not good for griddign
  
  features_grid <- focal(features_dir, w = 5, mean) # for 0.1°
  features_grid <- crop(features_grid, terra::ext(pr_box_1))
  features_grid <- rast(lapply(features_grid, fill_na_iteratively))
  
  output_cv <- spatial_data_fusion_engine_CV(
    pr_xyz = pr_xyz_r,
    pr_data = pr_data_r,
    features_grid = features_grid,
    params_mod = list(
      Nstations = 60,
      Covars =  c("PrSat", "PrSatB", "H", "T", "OPD", "P", "CF", "CWP",
                  "DSI", "DCI", "MDI", "FDI", "OPD_eff", "CWP_eff", "CF_gra"),
      Model = fillData_rf_ranger,
      Mc.Cores = 100
    )
  )
  
  saveRDS(
    output_cv,
    file = file.path("output/04_spatial-data-fusion/cv_worst/cv-obs_bc",
                     paste0(date_to_merge, ".RDS"))
  )
  
  }
