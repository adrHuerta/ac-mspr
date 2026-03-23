rm(list = ls())

library(terra)
library(xts)
source("R/analogues/analogue_search_engine.R")
source("R/analogues/dr_drp90_mcc.R")
source("R/analogues/pareto_selection.R")
terra::terraOptions(parallel = FALSE)

# pp data
sc_prec4sa <- readRDS("/scratch2/ahuerta/datasets/observed_precipitation/sc-prec4sa/rds_xgb/SC-PREC4SA_hmg_obs_bc.RDS")
pr_xyz <- sc_prec4sa$xyz
pr_data <- sc_prec4sa$data

# wts data
wts_data <- read.csv("output/00_weather-types/WTs_tSA.csv")
wts_data$time <- as.Date(wts_data$time)

# cv sample data
cv_days <- read.csv("output/02_ENSO-cv-sample-days/CV_days.csv")

parallel::mclapply(
  cv_days$dates,
  function(ijx){
    
    dates_to_explore <- as.Date(ijx)
    
    # imerg
    pr_sat_dir = "/mnt/climstor2/vol01_ecmwf/download/gpm_imerg/raw3"
    analogues_imerg <- analogue_search_engine(date2search = dates_to_explore,
                                              pr_xyz = pr_xyz,
                                              pr_data = pr_data,
                                              pr_box = c(-83, -34, -25, 15),
                                              wts_data = wts_data,
                                              pr_sat_dir = pr_sat_dir,
                                              pr_sat_per = c(as.Date("1998-01-01"), as.Date("2021-12-31")),
                                              days_param = 60,
                                              prob_param = 0.95)
    analogues_imerg$sat <- "imerg"
    
    # gsmap
    pr_sat_dir = "/mnt/climstor2/vol01_ecmwf/download/gsmap_v8_op/raw"
    analogues_gsmap <- analogue_search_engine(date2search = dates_to_explore,
                                              pr_xyz = pr_xyz,
                                              pr_data = pr_data,
                                              pr_box = c(-83, -34, -25, 15),
                                              wts_data = wts_data,
                                              pr_sat_dir = pr_sat_dir,
                                              pr_sat_per = c(as.Date("1998-01-01"), as.Date("2021-12-31")),
                                              days_param = 60,
                                              prob_param = 0.95)
    analogues_gsmap$sat <- "gsmap"
    
    # pdirnow
    pr_sat_dir = "/mnt/climstor2/vol01_ecmwf/download/pdirnow/raw4"
    analogues_pdirnow <- analogue_search_engine(date2search = dates_to_explore,
                                                pr_xyz = pr_xyz,
                                                pr_data = pr_data,
                                                pr_box = c(-83, -34, -25, 15),
                                                wts_data = wts_data,
                                                pr_sat_dir = pr_sat_dir,
                                                pr_sat_per = c(as.Date("2000-03-01"), as.Date("2021-12-31")),
                                                days_param = 60,
                                                prob_param = 0.95)
    analogues_pdirnow$sat <- "pdirnow"
    
    # repeated analogue dates? define the best based on average metric
    analogues_full <- rbind(analogues_imerg, analogues_gsmap, analogues_pdirnow)
    duplicated_dates <- analogues_full[duplicated(analogues_full$date), ]$date
    
    if(length(duplicated_dates) >= 1) {
      
      to_eval <- analogues_full[which(analogues_full$date %in% duplicated_dates), ]
      
      to_add <- by(to_eval, to_eval$date,
                   function(jx){
                     
                     jx <- transform(
                       jx,
                       mean_metric = (dr  + dr_p90 + mcc)/3
                     )
                     jx[order(jx$mean_metric, decreasing = TRUE), ][1, ]
                     
                   }
      )
      
      to_add <- as.list(to_add)
      to_add <- do.call(rbind, to_add)
      to_add <- to_add[, -match("mean_metric", colnames(to_add))]
      
      rest_df <- analogues_full[-which(analogues_full$date %in% duplicated_dates), ]
      
      final_df <- rbind(rest_df, to_add)
      final_df <- final_df[order(final_df$sat), ]
      rownames(final_df) <- NULL
      
    } else {
      
      final_df <- analogues_full[order(analogues_full$sat), ]
      rownames(final_df) <- NULL
      
    }
    
    saveRDS(final_df,
            file.path("output/03_precipitation-pattern-analogue/cv/cv-hmg_obs_bc", # CHECK folder
                      paste0(dates_to_explore, ".RDS"))
    )
    
  },
  mc.cores = 150
)
