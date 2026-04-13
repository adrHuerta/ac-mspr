rm(list = ls())
# start_time <- Sys.time()

library(terra)
library(xts)
source("R/spatial_data_fusion/spatial_data_fusion_engine_mask.R")
source("R/spatial_data_fusion/fill_grid.R")

# pp data
sc_prec4sa <- readRDS("/scratch2/ahuerta/datasets/observed_precipitation/sc-prec4sa/rds_xgb/SC-PREC4SA_obs_bc.RDS")
pr_xyz <- sc_prec4sa$xyz
pr_data <- sc_prec4sa$data

# range dates
dates_range <- seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day")
years <- unique(format(dates_range, "%Y"))

# features_path
features_path = "/scratch2/ahuerta/patmosx_gf"

# ecr
ecoregions <- vect("/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp")

# area
pr_box_1 = c(-83, -65, -20, 2 + 1)

for (yy in years) {
  
  cat("Starting year:", yy, "at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  dates_year <- dates_range[format(dates_range, "%Y") == yy]
  
  lapply(
    dates_year,
    function(ijx){
      
      # best analogue
      year_folder  <- file.path("output", "05_prec4pe/prec4pe_obs_bc", format(ijx, "%Y"))
      day_folder   <- file.path(year_folder, format(ijx, "%Y-%m-%d"))
      nlgs_file  <- file.path(day_folder, paste0("analogues_", ijx,".RDS"))
      
      best_analogue <- readRDS(nlgs_file)[1, ]
      
      i_sat <- best_analogue$sat
      i_date <- best_analogue$date

      # vect data
      pr_xyz_r <- terra::vect(pr_xyz, geom = c("LON", "LAT"), crs = "+proj=longlat +datum=WGS84", keepgeom = TRUE)
      pr_xyz_r <- crop(pr_xyz_r, terra::ext(pr_box_1))
      pr_data_r <- pr_data[ijx, match(pr_xyz_r$ID, colnames(pr_data))]
      
      # covars
      features_dir <- file.path(features_path, paste0("features_", i_sat))
      features_dir <- rast(
        file.path(
          features_dir, format(as.Date(i_date), "%Y"),
          paste0(i_date, ".nc"))
      )
      
      lla_grid <- rast("data/processed/lla-sa-10km.nc")
      lla_grid <- resample(lla_grid, features_dir)
      
      features_grid <- c(lla_grid, focal(features_dir, w = 5, fun = "mean", expand = TRUE)) # same as exploration
      features_grid <- crop(features_grid, terra::ext(pr_box_1)) # it creates son NA pixel not good for griddign
      features_grid <- rast(lapply(features_grid, fill_na_iteratively))
      
      # grids_folder <- file.path(day_folder, "grids")
      # dir.create(grids_folder, recursive = TRUE, showWarnings = FALSE)
      
      writeRaster(
        spatial_data_fusion_engine(
          pr_xyz = pr_xyz_r,
          pr_data = pr_data_r,
          features_grid = features_grid,
          mask_raster = ecoregions,
          params_mod = list(
            LLAorg = FALSE,
            Nstations = 60,
            Covars =  c("PrSat", "PrSatB", "H", "T", "OPD", "P", "CF", "CWP",
                        "DSI", "DCI", "MDI", "FDI", "OPD_eff", "CWP_eff", "CF_gra"),
            Model = fillData_rf_ranger,
            Mc.Cores = 200
          )
        ),
        file.path(day_folder, paste0("grid_", ijx, ".tif")),
        overwrite = TRUE
      )
      
      return(NULL)
      
    }
  )
  
}
# end_time <- Sys.time()
# print(end_time - start_time)