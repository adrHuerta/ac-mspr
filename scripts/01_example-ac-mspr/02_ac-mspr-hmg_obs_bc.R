rm(list = ls())

library(terra)
library(xts)
source("R/analogues/analogue_search_engine.R")
source("R/spatial_data_fusion/spatial_data_fusion_engine.R")
source("R/spatial_data_fusion/metrics.R")
source("R/spatial_data_fusion/fill_grid.R")
terra::terraOptions(parallel = FALSE)
library(ggplot2)
library(ggridges)
library(patchwork)

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

## 1) ANALOGUES

analogues_df <- lapply(
  date_to_merge,
  function(ijx){
    
    # imerg
    pr_sat_dir = "/mnt/climstor2/vol01_ecmwf/download/gpm_imerg/raw3"
    analogues_imerg <- analogue_search_engine(date2search = ijx,
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
    analogues_gsmap <- analogue_search_engine(date2search = ijx,
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
    analogues_pdirnow <- analogue_search_engine(date2search = ijx,
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
    final_df
  }
)

analogues_df <- data.frame(analogues_df)
analogues_df$anlg <- formatC(1:nrow(analogues_df), width = 2, format = "d", flag = "0")

exp_plot <- analogues_df
exp_plot$date <- as.Date(exp_plot$date)
exp_plot_label <- exp_plot[exp_plot$anlg %in% c("19", "41", "48"),]

exp_plot$sat <- factor(exp_plot$sat, 
                       levels = c("gsmap", "imerg", "pdirnow"),
                       labels = c("GSMaP_NRT", "IMERG−Early", "PDIR−Now"))

plot1 <- 
  ggplot(data = exp_plot) + 
  geom_density_ridges(aes(x = date, y = factor(sat), point_shape = factor(sat)),
                      jittered_points = TRUE,
                      # position = position_points_jitter(width = 0.001, height = 0),
                      # point_shape = '|', point_size = 5, point_width = 1, point_alpha = 0.5, alpha = 0.5,
                      point_alpha = 0.15, alpha = 0.35, scale = 0.95,
                      point_size = 2.75,
                      lwd = .375) +
  xlab("") + ylab("satellite products") +
  scale_x_date(limits = c(as.Date("1997-01-01"), as.Date("2022-12-31"))) +
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5),
        legend.margin = margin(t = -5),
        axis.text = element_text(size = 7.75),
        axis.title = element_text(size = 8.5),
        legend.title = element_text(size = 8),
        axis.title.y =element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(.35, 'cm'),
        plot.margin = margin(-15, 5, -15, 5),
        legend.position = "none")


plot2 <- 
  ggplot(data = exp_plot) + 
  geom_point(aes(x = mcc, y = dr_p90, colour = dr, shape = sat), size = 2.75, alpha = 0.8) + 
  guides(shape = "none") +
  viridis::scale_color_viridis(alpha = 0.8, latex2exp::TeX("\\textit{$d_{r}$}")) +
  xlab(latex2exp::TeX("\\textit{$MCC$}")) + ylab(latex2exp::TeX("\\textit{$d_{r}^{p90}$}")) +
  ggrepel::geom_text_repel(data = exp_plot_label, aes(label = anlg, x = mcc, y = dr_p90),
                           box.padding = unit(0.7, "lines"), size = 3.25) +
  theme_bw() +
  theme(legend.margin = margin(t = -5),
        axis.text = element_text(size = 7.75),
        axis.title = element_text(size = 8.25),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        plot.margin = margin(-15, 5, -15, 5),
        legend.key.size = unit(.35, 'cm'))


##  1) SPATIAL DATA FUSION

features_path = "/scratch2/ahuerta/patmosx_gf"

pr_xyz_r <- terra::vect(pr_xyz, geom = c("LON", "LAT"), crs = "+proj=longlat +datum=WGS84", keepgeom = TRUE)
pr_xyz_r <- crop(pr_xyz_r, terra::ext(pr_box_1))
pr_data_r <- pr_data[date_to_merge, match(pr_xyz_r$ID, colnames(pr_data))]

pr_new_data <- list()

for(ijx in seq_len(nrow(analogues_df))){
  
  print(ijx)
  i_date <- analogues_df$date[ijx]
  i_sat <- analogues_df$sat[ijx]
  
  features_dir <- file.path(
    features_path, 
    paste0("features_", i_sat)
  )
  features_dir <- rast(
    file.path(
      features_dir, format(as.Date(i_date), "%Y"),
      paste0(i_date, ".nc"))
  )
  
  lla_grid <- rast("data/processed/lla-sa-10km.nc")
  lla_grid <- resample(lla_grid, features_dir) 
  
  # features_grid <- c(lla_grid, focal(features_dir, w = 3, fun = "mean", expand = TRUE, na.policy="only", na.rm=FALSE))
  # features_grid <- crop(features_grid, terra::ext(pr_box_1))
  
  features_grid <- c(lla_grid, focal(features_dir, w = 5, fun = "mean", expand = TRUE)) # same as exploration
  features_grid <- crop(features_grid, terra::ext(pr_box_1)) # it creates son NA pixel not good for griddign
  features_grid <- rast(lapply(features_grid, fill_na_iteratively))
  # features_grid <- terra::aggregate(features_grid, 10)
  
  output_pr <- spatial_data_fusion_engine(
    pr_xyz = pr_xyz_r,
    pr_data = pr_data_r,
    features_grid = features_grid,
    params_mod = list(
      LLAorg = TRUE,
      Nstations = 60,
      Covars =  c("PrSat", "PrSatB", "H", "T", "OPD", "P", "CF", "CWP",
                  "DSI", "DCI", "MDI", "FDI", "OPD_eff", "CWP_eff", "CF_gra"),
      Model = fillData_rf_ranger,
      Mc.Cores = 200
    )
  )
  
  pr_new_data[[ijx]] <- output_pr
  
}

pr_new_s <- rast(
  lapply(
  pr_new_data,
  function(x) x[[1]]
)
)

ecoregions <- vect("/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp")

pr_new_df <- pr_new_s[[which(exp_plot$anlg %in% c("19", "41", "48"))]]
pr_new_df <- c(pr_new_df, app(pr_new_s, median), app(pr_new_s, sd))
names(pr_new_df) <- c("analogue-19", "analogue-41", "analogue-48", "analogue-median", "analogue-sd")

pr_new_df_metric <- fast_dr_mcc_v(obs = matrix(pr_data_r, ncol = 1), mod_mat = extract(pr_new_df, pr_xyz_r))
pr_new_df_metric$layer <- c("ID", names(pr_new_df) )
pr_new_df_metric <- pr_new_df_metric[-c(1, 6), ]
colnames(pr_new_df_metric)[5] <- c("variable")
pr_new_df_metric$my_label <- with(pr_new_df_metric, paste0(
  "<i>d</i><sub><i>r</i></sub> : ", formatC(round(dr, 2), digits = 2, format = "fg", drop0trailing = FALSE, flag = "#"), "<br>",
  "<i>KGE''</i> : ", formatC(round(kge2, 2), digits = 2, format = "fg", drop0trailing = FALSE, flag = "#"), "<br>",
  "<i>MCC</i> : ", formatC(round(mcc, 2), digits = 2, format = "fg", drop0trailing = FALSE, flag = "#"), "<br>",
  "<i>BAcc</i> : ", formatC(round(bcc, 2), digits = 2, format = "fg", drop0trailing = FALSE, flag = "#")
))

pr_new_df <- mask(pr_new_df, ecoregions)
pr_new_df <- as.data.frame(pr_new_df, xy = TRUE)
pr_new_df <- reshape2::melt(pr_new_df, id.vars = c("x", "y"))
pr_new_df$value_cut <- cut(pr_new_df$value,
                           breaks = c(-Inf, 0.1, 1, 5, 10,
                                      20, 30, 50, 
                                      75, 100, 120, 140, Inf),
                           right = FALSE,
                           labels = c("0 - .1", ".1 - 1", "1 - 5", "5 - 10",
                                      "10 - 20", "20 - 30", "30 - 50", "50 - 75",
                                      "75 - 100", "100 - 120", "120 - 140", "140 - Max")
                           # labels = c("[0,0.1)", "[0.1,1)", "[1,5)", "[5,10)",
                           #            "[10,20)", "[20,30)", "[30,50)", "[50,75)",
                           #            "[75,100)", "[100,120)", "[120,140)", "[140, Max)")
                           )

colors <- c(
  "white","#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca",
  "#0868ac", "#225ea8", "#88419d", "#e34a33",
  "#b30000", "#7f0000", "black"
)

pr_xyz_r_df <- as.data.frame(pr_xyz_r)
pr_xyz_r_df$PrObs <- as.numeric(pr_data_r)
pr_xyz_r_df$PrObs_cut <- cut(pr_xyz_r_df$PrObs,
                             breaks = c(-Inf, 0.1, 1, 5, 10,
                                        20, 30, 50, 
                                        75, 100, 120, 140, Inf),
                             right = FALSE,
                             labels = c("[0,0.1)", "[0.1,1)", "[1,5)", "[5,10)",
                                        "[10,20)", "[20,30)", "[30,50)", "[50,75)",
                                        "[75,100)", "[100,120)", "[120,140)", "[140, Max)"))
pr_xyz_r_df$PrObs_cut[pr_xyz_r_df$PrObs_cut == "[0,0.1)"] <- NA
pr_xyz_r_df <- pr_xyz_r_df[complete.cases(pr_xyz_r_df), ]

eco_centroids <- centroids(crop(ecoregions, ext(pr_box)), inside = TRUE)
eco_centroids <- as.data.frame(eco_centroids, geom = "XY")
eco_centroids[eco_centroids$nr_id == "GCH", c("x", "y")] <- c(-62.5, -22)
eco_centroids[eco_centroids$nr_id == "EHL", c("x", "y")] <- c(-50, -14)
eco_centroids[eco_centroids$nr_id == "AOL", c("x", "y")] <- c(-62, -1)

plt0 <-
  ggplot() +
  tidyterra::geom_spatvector(data = ecoregions, fill = "white", size = .11) +
  ggrepel::geom_text_repel(
    data = eco_centroids[eco_centroids$nr_id == "CAS", ],
    aes(label = nr_id, x = x, y = y), size = 2, alpha = 0.65, box.padding = 0.01, nudge_y = -8, nudge_x = -4,
    segment.alpha = 0.35, segment.curvature = -0.1, segment.linetype = 1, segment.size = .25, segment.colour = "gray50"
  ) +
  ggrepel::geom_text_repel(
    data = eco_centroids[eco_centroids$nr_id == "PAD", ],
    aes(label = nr_id, x = x, y = y), size = 2, alpha = 0.65, box.padding = 0.01, nudge_y = -5, nudge_x = -4,
    segment.alpha = 0.35, segment.curvature = -0.1, segment.linetype = 1, segment.size = .25, segment.colour = "gray50"
  ) +
  ggrepel::geom_text_repel(
    data = eco_centroids[eco_centroids$nr_id == "NAS", ],
    aes(label = nr_id, x = x, y = y), size = 2, alpha = 0.65, box.padding = 0.01, nudge_y = 8.5, nudge_x = -3,
    segment.alpha = 0.35, segment.curvature = -0.1, segment.linetype = 1, segment.size = .25, segment.colour = "gray50"
  ) +
  ggrepel::geom_text_repel(
    data = eco_centroids[eco_centroids$nr_id == "GCH", ],
    aes(label = nr_id, x = x, y = y), size = 2, alpha = 0.65, box.padding = 0, point.padding = 0, nudge_y = -.2, nudge_x = .1
  ) +
  ggrepel::geom_text_repel(
    data = eco_centroids[eco_centroids$nr_id == "EHL", ],
    aes(label = nr_id, x = x, y = y), size = 2, alpha = 0.65, box.padding = 0.01, nudge_y = 15, nudge_x = 10.5,
    segment.alpha = 0.35, segment.curvature = -0.1, segment.linetype = 1, segment.size = .25, segment.colour = "gray50"
  ) +
  ggrepel::geom_text_repel(
    data = eco_centroids[eco_centroids$nr_id == "AOL", ],
    aes(label = nr_id, x = x, y = y), size = 2, alpha = 0.65, box.padding = 0.01, nudge_y = 11, nudge_x = 7,
    segment.alpha = 0.35, segment.curvature = -0.1, segment.linetype = 1, segment.size = .25, segment.colour = "gray50"
  ) +
  geom_point(data = pr_xyz_r_df, aes(x = LON, y = LAT, colour = PrObs_cut), size = .1) + 
  scale_colour_manual(values = colors, name = "precipitation (mm)", drop = FALSE) +
  coord_sf(ylim = c(-25, 13), xlim = c(-82, -34), expand = c(0, 0)) +
  theme_bw() + xlab("") + ylab("") +
  theme(legend.margin = margin(t = -5),
        # axis.title = element_blank(),
        axis.text = element_text(size = 7.5),
        axis.title = element_text(size = 8.5),
        legend.text = element_text(size = 7),
        legend.key.size = unit(.35, 'cm'),
        plot.margin = margin(-15, 5, -15, 5),
        legend.position = "none")

plt3 <-
  ggplot() +
  geom_raster(data = pr_new_df, aes(x = x, y = y, fill = value_cut)) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .11) +
  scale_fill_discrete(palette = colors, name = "precipitation (mm)", drop = FALSE) +
  coord_sf(ylim = c(-25, 13), xlim = c(-82, -34), expand = c(0, 0)) +
  theme_bw() +
  theme(plot.tag = element_text(size = 10),
        plot.tag.position = c(0.01, 1),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 8.5),
        legend.direction = "horizontal",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        plot.margin = margin(-245, 25, -45, 25),
        legend.key.size = unit(0.01, "npc"),
        legend.background = element_blank()) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, ncol = 3)) +
  facet_wrap( ~ variable, ncol = 3, drop = FALSE) +
  ggtext::geom_richtext(
    data = pr_new_df_metric,
    aes(x = -34.8, y = 7, label = my_label),
    fill = "white", 
    color = "gray50",
    label.padding = unit(0.5, "mm"),
    label.size = 0.01,
    label.color = NA,
    hjust = 1,
    size = 2.5)


((plt0 | plot1 | plot2) / lemon::reposition_legend(plt3,
                                                   "center",
                                                   panel = "panel-3-2") ) + 
  plot_layout(heights = c(0.25, .65)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ")") &
  theme(
    plot.tag = element_text(size = 9),
    plot.tag.position = c(0, 1.05)
  )


ggsave(
  "output/01_example-ac-mspr/exp-hmg_obs_bc.pdf",
  units = "in",
  width = 6,
  height = 7,
  dpi = 300,
  scale = 1.25
)

