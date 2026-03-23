rm(list = ls())

library(xts)
source("R/analogues/analogue_search_engine.R")
library(terra)
library(ggplot2)
library(ggridges)
library(patchwork)
library(tidyterra)

date_to_merge <- as.Date("1960-01-10")
pr_box <- c(-83, -34, -25, 12.5)
pr_box_1 <- c(-83, -34 + 1, -25 - 1, 12.5 + 0.5)

# pp data
sc_prec4sa <- readRDS("/scratch2/ahuerta/datasets/observed_precipitation/sc-prec4sa/rds_xgb/SC-PREC4SA_hmg_obs_bc.RDS")
pr_xyz <- sc_prec4sa$xyz
pr_data <- sc_prec4sa$data

# wts data
wts_data <- read.csv("output/00_weather-types/WTs_tSA.csv")
wts_data$time <- as.Date(wts_data$time)


pr_sat_dir = "/mnt/climstor2/vol01_ecmwf/download/gpm_imerg/raw3"
analogues_imerg <- analogue_search_engine(date2search = date_to_merge,
                                          pr_xyz = pr_xyz,
                                          pr_data = pr_data,
                                          pr_box = c(-83, -34, -25, 15),
                                          wts_data = wts_data,
                                          pr_sat_dir = pr_sat_dir,
                                          pr_sat_per = c(as.Date("1998-01-01"), as.Date("2021-12-31")),
                                          days_param = 60,
                                          prob_param = 0.95)

ecoregions <- vect("/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp")

# features (gap-filled)
features_path = "/scratch2/ahuerta/patmosx_gf/"

i_date <- analogues_imerg$date[1]

features_dir <- file.path(
  features_path, 
  paste0("features_", "imerg")
)
features_dir <- rast(
  file.path(
    features_dir, format(as.Date(i_date), "%Y"),
    paste0(i_date, ".nc"))
)

features_grid <- features_dir
features_grid_gf <- crop(features_grid, pr_box)

# features (NO gap-filled)
features_path = "/scratch2/ahuerta/patmosx/"

i_date <- analogues_imerg$date[1]

features_dir <- file.path(
  features_path, 
  paste0("features_", "imerg")
)
features_dir <- rast(
  file.path(
    features_dir, format(as.Date(i_date), "%Y"),
    paste0(i_date, ".nc"))
)

features_grid <- features_dir
features_grid_ngf <- crop(features_grid, pr_box)
names(features_grid_ngf) <- paste0(names(features_grid_ngf), "_NGF")

pdf("output/01_example-ac-mspr/exp-features.pdf", width = 6.25, height = 7.85)
local({
  counter <- 0
  plot(c(features_grid_gf, features_grid_ngf) , fun = function(x) {
    counter <<- counter + 1
    if (counter > 0) {
      lines(ecoregions, col = "black", pch = 20)
    }
  }, axes = FALSE, legend = FALSE, mar = c(0, 0, 0, 0), nc = 5, maxnl = 30)
})
dev.off()
