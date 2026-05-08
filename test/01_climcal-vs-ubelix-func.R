rm(list = ls())
library(terra)
library(xts)

# Source project functions
source("R/analogues/analogue_search_engine.R")
source("R/spatial_data_fusion/spatial_data_fusion_engine_hpc_disk_v3.R") # ubelix
source("R/spatial_data_fusion/spatial_data_fusion_engine_mask.R")        # climcal
source("R/spatial_data_fusion/fill_grid.R")

# TEST DAY
test_date <- as.Date("1960-01-10")
pr_box <- c(-83, -34, -25, 12.5)
pr_box_1 <- c(-83, -67 + 1, -18 - 1, 0 + 0.5)

sc_prec4sa <- readRDS("/scratch2/ahuerta/datasets/observed_precipitation/sc-prec4sa/rds_xgb/SC-PREC4SA_obs_bc.RDS")
pr_xyz <- sc_prec4sa$xyz
pr_data <- sc_prec4sa$data

wts_data <- read.csv("output/00_weather-types/WTs_tSA.csv")
wts_data$time <- as.Date(wts_data$time)

# ecr
ecoregions <- vect("/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp")

# 1. ANALOGUES
## just imerg
analogues_imerg <- analogue_search_engine(date2search = test_date,
                                          pr_xyz = pr_xyz,
                                          pr_data = pr_data,
                                          pr_box = pr_box_1,
                                          wts_data = wts_data,
                                          pr_sat_dir = "/mnt/climstor2/vol01_ecmwf/download/gpm_imerg/raw3",
                                          pr_sat_per = c(as.Date("1998-01-01"), as.Date("2021-12-31")),
                                          days_param = 60,
                                          prob_param = 0.95)
analogues_imerg$sat <- "imerg"
analogues_imerg <- transform(analogues_imerg, mean_metric = (dr  + dr_p90 + mcc)/3)
analogues_imerg <- analogues_imerg[order(analogues_imerg$mean_metric, decreasing = TRUE), ]
analogues_imerg$name <- formatC(1:nrow(analogues_imerg), width = 2, format = "d", flag = "0")


# 2. SPATIAL DATA FUSION

pr_xyz_r <- terra::vect(pr_xyz, geom = c("LON","LAT"), crs="+proj=longlat +datum=WGS84", keepgeom=TRUE)
pr_xyz_r <- crop(pr_xyz_r, terra::ext(pr_box_1))
pr_data_r <- pr_data[test_date, match(pr_xyz_r$ID, colnames(pr_data))]

features_path = "/scratch2/ahuerta/patmosx_gf"
best_analogue <- analogues_imerg[1, ]

i_sat <- best_analogue$sat  
i_date <- best_analogue$date
i_name <- best_analogue$name

features_dir <- file.path(features_path, paste0("features_", i_sat))
features_rast <- rast(file.path(features_dir, format(as.Date(i_date), "%Y"), paste0(i_date, ".nc")))

lla_grid <- rast("data/processed/lla-sa-10km.nc")
lla_grid <- resample(lla_grid, features_rast)

features_grid <- c(lla_grid, focal(features_rast, w = 5, fun = "mean", expand = TRUE))
features_grid <- crop(features_grid, terra::ext(pr_box_1))
features_grid <- rast(lapply(features_grid, fill_na_iteratively))

climcal <- spatial_data_fusion_engine(
  pr_xyz = pr_xyz_r,
  pr_data = pr_data_r,
  features_grid = features_grid,
  mask_raster = ecoregions,
  params_mod = list(
    LLAorg = TRUE, # this is key
    Nstations = 60,
    Covars =  c("PrSat", "PrSatB", "H", "T", "OPD", "P", "CF", "CWP",
                "DSI", "DCI", "MDI", "FDI", "OPD_eff", "CWP_eff", "CF_gra"),
    Model = fillData_rf_ranger,
    Mc.Cores = 200
  )
)

ubelix <- spatial_data_fusion_engine_hpc_disk(
  pr_xyz = pr_xyz_r,
  pr_data = pr_data_r,
  features_grid = features_grid,
  shapefile_path = "/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp",
  output_dir = "test",
  output_name = i_name,
  params_mod = list(
    LLAorg = TRUE,  # this is key
    Nstations = 60,
    Covars = c("PrSat", "PrSatB", "H","T","OPD","P","CF","CWP",
               "DSI","DCI","MDI","FDI","OPD_eff","CWP_eff","CF_gra"),
    Model = fillData_rf_ranger,
    Mc.Cores = 75,
    pixel_chunk_size = 3000
  )
)

ubelix <- rast("test/pr_01_.tif")

plot(c(climcal, ubelix))
plot(c(climcal - ubelix))
plot(c(climcal[[1]] - ubelix[[1]]), breaks = c(-50, -10, -5, -1, 1, 5, 10, 50))
plot(c(climcal[[1]], ubelix[[1]]))
plot(as.matrix(climcal[[1]]), as.matrix(ubelix[[1]])) # same results!
