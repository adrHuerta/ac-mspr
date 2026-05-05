rm(list = ls())

library(terra)
library(xts)
source("R/analogues/analogue_search_engine.R")
source("R/analogues/resolve_duplicates.R")

# pp data
sc_prec4sa <- readRDS("/scratch2/ahuerta/datasets/observed_precipitation/sc-prec4sa/rds_xgb/SC-PREC4SA_hmg_obs_bc.RDS")
pr_xyz <- sc_prec4sa$xyz
pr_data <- sc_prec4sa$data

# area
pr_box_1 = c(-83, -65, -20, 2 + 1)

# wts data
wts_data <- read.csv("output/00_weather-types/WTs_tSA.csv")
wts_data$time <- as.Date(wts_data$time)

# range dates
dates_range <- seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day")
years <- unique(format(dates_range, "%Y"))


for (yy in years) {
  
  cat("Starting year:", yy, "at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  dates_year <- dates_range[format(dates_range, "%Y") == yy]
  
  parallel::mclapply(
    dates_year,
    function(ijx){
      
      year_folder  <- file.path("output", "05_prec4pe/prec4pe_hmg_obs_bc", format(ijx, "%Y"))
      day_folder   <- file.path(year_folder, format(ijx, "%Y-%m-%d"))
      
      dir.create(day_folder, recursive = TRUE, showWarnings = FALSE)
      
      # imerg
      pr_sat_dir = "/mnt/climstor2/vol01_ecmwf/download/gpm_imerg/raw3"
      analogues_imerg <- analogue_search_engine(date2search = ijx,
                                                pr_xyz = pr_xyz,
                                                pr_data = pr_data,
                                                pr_box = pr_box_1,
                                                wts_data = wts_data,
                                                pr_sat_dir = pr_sat_dir,
                                                pr_sat_per = c(as.Date("1998-01-01"), as.Date("2021-12-31")),
                                                days_param = 60,
                                                prob_param = 0.95)
      analogues_imerg$sat <- "imerg"
      
      # gsmap
      pr_sat_dir = "/mnt/climstor2/vol01_ecmwf/download/gsmap_v8_op/raw"
      analogues_gsmap <- analogue_search_engine(date2search = ijx,
                                                pr_xyz = pr_xyz,
                                                pr_data = pr_data,
                                                pr_box = pr_box_1,
                                                wts_data = wts_data,
                                                pr_sat_dir = pr_sat_dir,
                                                pr_sat_per = c(as.Date("1998-01-01"), as.Date("2021-12-31")),
                                                days_param = 60,
                                                prob_param = 0.95)
      analogues_gsmap$sat <- "gsmap"
      
      # pdirnow
      pr_sat_dir = "/mnt/climstor2/vol01_ecmwf/download/pdirnow/raw4"
      analogues_pdirnow <- analogue_search_engine(date2search = ijx,
                                                  pr_xyz = pr_xyz,
                                                  pr_data = pr_data,
                                                  pr_box = pr_box_1,
                                                  wts_data = wts_data,
                                                  pr_sat_dir = pr_sat_dir,
                                                  pr_sat_per = c(as.Date("2000-03-01"), as.Date("2021-12-31")),
                                                  days_param = 60,
                                                  prob_param = 0.95)
      analogues_pdirnow$sat <- "pdirnow"
      
      # repeated analogue dates? define the best based on average metric
      analogues_full <- rbind(analogues_imerg, analogues_gsmap, analogues_pdirnow)
      final_analogues <- resolve_duplicates(analogues_full)
      final_analogues$target <- ijx
      final_analogues <- transform(final_analogues, mean_metric = (dr  + dr_p90 + mcc)/3)
      final_analogues <- final_analogues[order(final_analogues$mean_metric, decreasing = TRUE), ]
      
      saveRDS(
        final_analogues,
        file.path(day_folder,  paste0("analogues_", ijx,".RDS"))
      )
      
      return(NULL)
      
    },
    mc.cores = 50
  )
  
}
