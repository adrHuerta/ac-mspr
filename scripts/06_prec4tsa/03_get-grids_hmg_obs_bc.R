rm(list = ls())
source("renv/activate.R")
# start_time <- Sys.time()
library(terra)
library(xts)

# Source project functions
source("R/spatial_data_fusion/spatial_data_fusion_engine_hpc_disk_v3.R")
source("R/spatial_data_fusion/fill_grid.R")

# TEST DAY
args <- commandArgs(trailingOnly = TRUE)
test_date <- as.Date(args[1])
pr_box <- c(-83, -34, -25, 12.5)
pr_box_1 <- c(-83, -34 + 1, -25 - 1, 12.5 + 0.5)

# Output folders
year_folder  <- file.path("output", "06_prec4tsa", "prec4tsa_hmg_obs_bc", format(test_date, "%Y"))
day_folder   <- file.path(year_folder, format(test_date, "%Y-%m-%d"))

# Data
sc_prec4sa <- readRDS("data/processed/sc-prec4sa/SC-PREC4SA_hmg_obs_bc.RDS")
pr_xyz <- sc_prec4sa$xyz
pr_data <- sc_prec4sa$data

# 1. ANALOGUES
# already done

nlgs_path    <- file.path(day_folder, paste0("analogues_", test_date, ".RDS"))
analogues_df <- readRDS(nlgs_path)

# 2. SPATIAL DATA FUSION

for(nlg in analogues_df$role[1]){ # 1 MOST BALANCED ANALOGUE

    pr_xyz_r <- terra::vect(pr_xyz, geom = c("LON","LAT"), crs="+proj=longlat +datum=WGS84", keepgeom=TRUE)
    pr_xyz_r <- crop(pr_xyz_r, terra::ext(pr_box_1))
    pr_data_r <- pr_data[test_date, match(pr_xyz_r$ID, colnames(pr_data))]

    features_path = "data/grids"
    best_analogue <- analogues_df[analogues_df$role == nlg, ]
    print(nlg)

    i_sat <- best_analogue$sat  
    i_date <- best_analogue$date
    i_name <- best_analogue$role

    features_dir <- file.path(features_path, paste0("features_", i_sat))
    features_rast <- rast(file.path(features_dir, format(as.Date(i_date), "%Y"), paste0(i_date, ".nc")))

    lla_grid <- sds("data/processed/lla-sa-10km.nc")
    lla_grid <- rast(lla_grid)
    names(lla_grid) <- varnames(lla_grid)
    lla_grid <- resample(lla_grid, features_rast)

    features_grid <- c(lla_grid, focal(features_rast, w = 5, fun = "mean", expand = TRUE))
    features_grid <- crop(features_grid, terra::ext(pr_box_1))
    features_grid <- rast(lapply(features_grid, fill_na_iteratively))

    # print(features_grid)
    Mc.CoresMc.Cores = 8
    # print(Mc.CoresMc.Cores)

    # Run HPC-friendly disk-writing engine
    spatial_data_fusion_engine_hpc_disk(
        pr_xyz = pr_xyz_r,
        pr_data = pr_data_r,
        features_grid = features_grid,
        shapefile_path = "data/processed/sa_eco2/sa_eco_l3_2_paper.shp",
        output_dir = day_folder,
        output_name = i_name,
        params_mod = list(
            LLAorg = FALSE,
            Nstations = 60,
            Covars = c("PrSat", "PrSatB", "H","T","OPD","P","CF","CWP",
                        "DSI","DCI","MDI","FDI","OPD_eff","CWP_eff","CF_gra"),
            Model = fillData_rf_ranger,
            Mc.Cores = Mc.CoresMc.Cores,
            pixel_chunk_size = 3000
            )
    )

}

# end_time <- Sys.time()
# print(end_time - start_time)