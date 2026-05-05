rm(list = ls())

library(xts)
library(terra)
library(ggplot2)
library(ggridges)
source("R/spatial_data_fusion/metrics.R")

pr_box = c(-83, -34, -25, 15)
pr_box_1 = c(-83, -34, -25 - .5, 15 + .5)

sc_prec4sa <- readRDS("/scratch2/ahuerta/datasets/observed_precipitation/sc-prec4sa/rds_xgb/SC-PREC4SA_qc_obs.RDS")
pr_data <- sc_prec4sa$data
pr_xyz <- sc_prec4sa$xyz
pr_xyz <- terra::vect(pr_xyz, geom = c("LON", "LAT"), crs="+proj=longlat +datum=WGS84", keepgeom = TRUE)
pr_xyz <- crop(pr_xyz, terra::ext(pr_box))

cv_days <- read.csv("output/02_ENSO-cv-sample-days/CV_days.csv")

# cv output files
obs_bc_f <- dir("output/04_spatial-data-fusion/cv_median/cv-obs_bc", full.names = TRUE, recursive = TRUE)
hmg_obs_bc_f <- dir("output/04_spatial-data-fusion/cv_median/cv-hmg_obs_bc", full.names = TRUE, recursive = TRUE)
qc_obs_f <- dir("output/04_spatial-data-fusion/cv_median/cv-qc_obs", full.names = TRUE, recursive = TRUE)

# # SPATIAL ################
# # ENSO ########################################
# ## obs_bc
# spatial_CV_obs_bc <- parallel::mclapply(
#   seq_along(obs_bc_f),
#   function(x){
#     
#     res <- readRDS(obs_bc_f[x])
#     res <- fast_dr_mcc_v(obs = res$obs,
#                        mod_mat = as.matrix(res$mod))
#     data.frame(reshape2::melt(res),
#                type = cv_days$type[x])
#     
#   }, mc.cores = 10
# )
# spatial_CV_obs_bc <- do.call(rbind, spatial_CV_obs_bc)
# spatial_CV_obs_bc$data <- "obs_bc"
# 
# ## hmg_obs_bc
# spatial_CV_hmg_obs_bc <- parallel::mclapply(
#   seq_along(hmg_obs_bc_f),
#   function(x){
#     
#     res <- readRDS(hmg_obs_bc_f[x])
#     res <- fast_dr_mcc_v(obs = res$obs,
#                        mod_mat = as.matrix(res$mod))
#     data.frame(reshape2::melt(res),
#                type = cv_days$type[x])
#     
#   }, mc.cores = 10
# )
# spatial_CV_hmg_obs_bc <- do.call(rbind, spatial_CV_hmg_obs_bc)
# spatial_CV_hmg_obs_bc$data <- "hmg_obs_bc"
# 
# ## qc_obs
# spatial_CV_qc_obs <- parallel::mclapply(
#   seq_along(qc_obs_f),
#   function(x){
# 
#     res <- readRDS(qc_obs_f[x])
#     res <- fast_dr_mcc_v(obs = res$obs,
#                        mod_mat = as.matrix(res$mod))
#     data.frame(reshape2::melt(res),
#                type = cv_days$type[x])
# 
#   }, mc.cores = 10
# )
# spatial_CV_qc_obs <- do.call(rbind, spatial_CV_qc_obs)
# spatial_CV_qc_obs$data <- "qc_obs"
# 
# ## plt
# spatial_CV <- rbind(spatial_CV_obs_bc, spatial_CV_hmg_obs_bc, spatial_CV_qc_obs)
# spatial_CV$data <- factor(spatial_CV$data,
#                           levels = c("obs_bc", "hmg_obs_bc", "qc_obs"))
# spatial_CV$type <- factor(spatial_CV$type,
#                           levels = c("C_Neutral", "C_Nina", "C_Nino", "E_Neutral", "E_Nina", "E_Nino"),
#                           labels = c("C_Neutral", "C_Niña", "C_Niño", "E_Neutral", "E_Niña", "E_Niño"))
# levels(spatial_CV$variable) <- c(dr = latex2exp::TeX("\\textit{$d_{r}$}"),
#                                  kge2 = latex2exp::TeX("\\textit{$KGE''$}"),
#                                  mcc = latex2exp::TeX("\\textit{$MCC$}"),
#                                  bac = latex2exp::TeX("\\textit{$BAcc$}"))
# plt1 <- 
#   ggplot(data = spatial_CV) +
#   geom_boxplot(aes(x = type, y = value, colour = data)) + 
#   facet_wrap(~ variable, ncol = 1, scales = "free_y", labeller = label_parsed) + 
#   scale_x_discrete(labels= gsub("_", "\n", levels(factor(spatial_CV$type)))) +
#   scale_colour_viridis_d(option = "inferno",
#                          begin = 0.2, end = 0.8,
#                          guide = guide_legend("observed\ndataset",
#                                             nrow = 1,
#                                             byrow = TRUE,
#                                             label.position = "top",
#                                             barheight = .5)
#   ) +
#   xlab("") + ylab("") +
#   theme_bw() + 
#   theme(strip.background = element_blank(),
#         legend.position = "bottom",
#         legend.box = "horizontal",
#         legend.margin = margin(t = -15),
#         axis.text = element_text(size = 8),
#         axis.title = element_text(size = 9),
#         legend.title = element_text(size = 9),
#         plot.margin = unit(c(0,0,0,0), "lines"),
#         plot.background = element_blank(),
#         panel.spacing = unit(0, "lines"))
#   
# ggsave(
#   "output/04_spatial-data-fusion/SPDF_metrics-01.pdf",
#   plt1,
#   units = "in",
#   width = 3,
#   height = 3,
#   dpi = 300,
#   scale = 1.5
# )
# 
# 
# # ECRGNs  ########################################
# ## obs_bc
# spatial_CV2_obs_bc <- parallel::mclapply(
#   seq_along(obs_bc_f),
#   function(x){
#     
#     res <- readRDS(obs_bc_f[x])
#     res$ecr <- pr_xyz$ECOREGIONS
#     res <- by(res, res$ecr,
#               function(y){
#                 data.frame(
#                   reshape2::melt(
#                     fast_dr_mcc_v(obs = y$obs, mod_mat = as.matrix(y$mod)),
#                     measure.vars = c("dr", "kge2", "mcc", "bcc")),
#                   ecr = unique(y$ecr)
#                 )
#               })
#     
#     res <- do.call(rbind, res)
#     
#   }, mc.cores = 10
# )
# spatial_CV2_obs_bc <- do.call(rbind, spatial_CV2_obs_bc)
# spatial_CV2_obs_bc$data <- "obs_bc"
# 
# ## hmg_obs_obc
# spatial_CV2_hmg_obs_bc <- parallel::mclapply(
#   seq_along(hmg_obs_bc_f),
#   function(x){
#     
#     res <- readRDS(hmg_obs_bc_f[x])
#     res$ecr <- pr_xyz$ECOREGIONS
#     res <- by(res, res$ecr,
#               function(y){
#                 data.frame(
#                   reshape2::melt(
#                     fast_dr_mcc_v(obs = y$obs, mod_mat = as.matrix(y$mod)),
#                     measure.vars = c("dr", "kge2", "mcc", "bcc")),
#                   ecr = unique(y$ecr)
#                 )
#               })
#     
#     res <- do.call(rbind, res)
#     
#   }, mc.cores = 10
# )
# spatial_CV2_hmg_obs_bc <- do.call(rbind, spatial_CV2_hmg_obs_bc)
# spatial_CV2_hmg_obs_bc$data <- "hmg_obs_bc"
# 
# ## qc_obs
# spatial_CV2_qc_obs <- parallel::mclapply(
#   seq_along(qc_obs_f),
#   function(x){
# 
#     PrObs <- pr_xyz
#     PrObs$PrObs <- as.numeric(pr_data[cv_days$dates[x]][, pr_xyz$ID])
#     PrObs_ <- na.omit(PrObs, "PrObs")
# 
#     res <- readRDS(qc_obs_f[x])
#     res$ecr <- PrObs_$ECOREGIONS
#     res <- by(res, res$ecr,
#               function(y){
#                 data.frame(
#                   reshape2::melt(fast_dr_mcc_v(obs = y$obs, mod_mat = as.matrix(y$mod)), measure.vars = c("dr", "kge2", "mcc", "bcc")),
#                   ecr = unique(y$ecr)
#                 )
#               })
# 
#     res <- do.call(rbind, res)
#     
#   }, mc.cores = 10
# )
# spatial_CV2_qc_obs <- do.call(rbind, spatial_CV2_qc_obs)
# spatial_CV2_qc_obs$data <- "qc_obs"
# 
# spatial_CV2 <-  rbind(spatial_CV2_qc_obs, spatial_CV2_obs_bc, spatial_CV2_hmg_obs_bc)
# spatial_CV2$data <- factor(spatial_CV2$data,
#                           levels = c("obs_bc", "hmg_obs_bc", "qc_obs"))
# 
# order_scpre4sa <- c("NAS", "PAD", "CAS", "AOL", "EHL", "GCH")
# spatial_CV2$ecr <- factor(spatial_CV2$ecr,
#                           order_scpre4sa)
# 
# levels(spatial_CV2$variable) <- c(dr = latex2exp::TeX("\\textit{$d_{r}$}"),
#                                   kge2 = latex2exp::TeX("\\textit{$KGE''$}"),
#                                   mcc = latex2exp::TeX("\\textit{$MCC$}"),
#                                   bac = latex2exp::TeX("\\textit{$BAcc$}"))
# 
# plt2 <- 
#   ggplot(data = spatial_CV2) +
#   geom_boxplot(aes(x = ecr, y = value, colour = data)) + 
#   scale_y_continuous(limits = c(0.1, 1)) +
#   facet_wrap(~ variable, ncol = 1, scales = "free_y", labeller = label_parsed) + 
#   scale_colour_viridis_d(option = "inferno",
#                          begin = 0.2, end = 0.8,
#                          guide = guide_legend("observed\ndataset",
#                                               nrow = 1,
#                                               byrow = TRUE,
#                                               label.position = "top",
#                                               barheight = .5)
#   ) +
#   xlab("") + ylab("") +
#   theme_bw() + 
#   theme(strip.background = element_blank(),
#         legend.position = "bottom",
#         legend.box = "horizontal",
#         legend.margin = margin(t = -15),
#         axis.text = element_text(size = 8),
#         axis.title = element_text(size = 9),
#         legend.title = element_text(size = 9),
#         plot.margin = unit(c(0,0,0,0), "lines"),
#         plot.background = element_blank(),
#         panel.spacing = unit(0, "lines"))
# 
# ggsave(
#   "output/04_spatial-data-fusion/SPDF_metrics-02.pdf",
#   plt2,
#   units = "in",
#   width = 3,
#   height = 3,
#   dpi = 300,
#   scale = 1.5
# )
# 

# TEMPORAL ##########

# eval
## obs_bc
temporal_CV_obs_bc <- lapply(
  seq_along(obs_bc_f),
  function(x){
    
    res <- readRDS(obs_bc_f[x])
    list(obs = res$obs, mod = res$mod) 
    
  }
)
obs_all <- do.call(cbind, lapply(temporal_CV_obs_bc, function(x) x$obs))
mod_all <- do.call(cbind, lapply(temporal_CV_obs_bc, function(x) x$mod))

temporal_CV_obs_bc <- parallel::mclapply(
  1:nrow(obs_all),
  function(xy){
    
    obs_obs <- as.numeric(obs_all[xy, ])
    mod_mod <- as.numeric(mod_all[xy, ])
    station_ID <- pr_xyz$ID[xy]
    
    if(sum(obs_obs, na.rm = TRUE) == 0) {
      
      data.frame(
        variable = c("dr", "mcc", "bcc"),
        value = c(NA, NA, NA),
        ID = station_ID
      )
      
      
    } else {
      
      obs_mod_df <- data.frame(obs_obs = obs_obs, mod_mod = mod_mod)
      obs_mod_df <- obs_mod_df[complete.cases(obs_mod_df), ]
      
      data.frame(
        reshape2::melt(fast_dr_mcc_v(obs = obs_mod_df$obs_obs,
                                   mod_mat = as.matrix(obs_mod_df$mod_mod)),
                       measure.vars = c("dr", "kge2", "mcc", "bcc")),
        ID = station_ID
      )
      
    }
  }, mc.cores = 10)
temporal_CV_obs_bc <- do.call(rbind, temporal_CV_obs_bc)
temporal_CV_obs_bc <- merge(temporal_CV_obs_bc, pr_xyz, by = "ID")
temporal_CV_obs_bc$data <- "obs_bc"


## hmg_obs_bc
temporal_CV_hmg_obs_bc <- lapply(
  seq_along(hmg_obs_bc_f),
  function(x){
    
    res <- readRDS(hmg_obs_bc_f[x])
    list(obs = res$obs, mod = res$mod) 
    
  }
)
obs_all <- do.call(cbind, lapply(temporal_CV_hmg_obs_bc, function(x) x$obs))
mod_all <- do.call(cbind, lapply(temporal_CV_hmg_obs_bc, function(x) x$mod))

temporal_CV_hmg_obs_bc <- parallel::mclapply(
  1:nrow(obs_all),
  function(xy){
    
    obs_obs <- as.numeric(obs_all[xy, ])
    mod_mod <- as.numeric(mod_all[xy, ])
    station_ID <- pr_xyz$ID[xy]
    
    if(sum(obs_obs, na.rm = TRUE) == 0) {
      
      data.frame(
        variable = c("dr", "mcc", "bcc"),
        value = c(NA, NA, NA),
        ID = station_ID
      )
      
      
      
    } else {
      
      obs_mod_df <- data.frame(obs_obs = obs_obs, mod_mod = mod_mod)
      obs_mod_df <- obs_mod_df[complete.cases(obs_mod_df), ]
      
      data.frame(
        reshape2::melt(fast_dr_mcc_v(obs = obs_mod_df$obs_obs,
                                   mod_mat = as.matrix(obs_mod_df$mod_mod)),
                       measure.vars = c("dr", "kge2", "mcc", "bcc")),
        ID = station_ID
      )
      
    }
  }, mc.cores = 10)
temporal_CV_hmg_obs_bc <- do.call(rbind, temporal_CV_hmg_obs_bc)
temporal_CV_hmg_obs_bc <- merge(temporal_CV_hmg_obs_bc, pr_xyz, by = "ID")
temporal_CV_hmg_obs_bc$data <- "hmg_obs_bc"


## qc_obs
temporal_CV_qc_obs <- parallel::mclapply(
  seq_along(qc_obs_f),
  function(x){
    
    # network change per day
    PrObs <- pr_xyz
    PrObs$PrObs <- as.numeric(pr_data[cv_days$dates[x]][, pr_xyz$ID])
    PrObs_ <- na.omit(PrObs, "PrObs")
    PrObs <- as.data.frame(PrObs)
    
    res <- readRDS(qc_obs_f[x])
    PrObs_$obs <- res$obs
    PrObs_$mod <- res$mod
    PrObs_ <- as.data.frame(PrObs_)
    PrObs_ <- merge(PrObs, PrObs_, by = "ID", all = TRUE)
    
    
    list(obs = PrObs_$obs, mod = PrObs_$mod)
    
  }, mc.cores = 10
)
obs_all <- do.call(cbind, lapply(temporal_CV_qc_obs, function(x) x$obs))
mod_all <- do.call(cbind, lapply(temporal_CV_qc_obs, function(x) x$mod))

temporal_CV_qc_obs <- parallel::mclapply(
  1:nrow(obs_all),
  function(xy){
    
    obs_obs <- as.numeric(obs_all[xy, ])
    mod_mod <- as.numeric(mod_all[xy, ])
    station_ID <- pr_xyz$ID[xy]
    
    if(sum(obs_obs, na.rm = TRUE) == 0) {
      
      data.frame(
        variable = c("dr", "mcc", "bcc"),
        value = c(NA, NA, NA),
        ID = station_ID
      )
      
      
    } else {
      
      obs_mod_df <- data.frame(obs_obs = obs_obs, mod_mod = mod_mod)
      obs_mod_df <- obs_mod_df[complete.cases(obs_mod_df), ]
      
      data.frame(
        reshape2::melt(fast_dr_mcc_v(obs = obs_mod_df$obs_obs,
                                   mod_mat = as.matrix(obs_mod_df$mod_mod)),
                       measure.vars = c("dr", "kge2", "mcc", "bcc")),
        ID = station_ID
      )
      
    }
  }, mc.cores = 10)
temporal_CV_qc_obs <- do.call(rbind, temporal_CV_qc_obs)
temporal_CV_qc_obs <- merge(temporal_CV_qc_obs, pr_xyz, by = "ID")
temporal_CV_qc_obs$data <- "qc_obs"

temporal_CV <- rbind(temporal_CV_qc_obs, temporal_CV_obs_bc, temporal_CV_hmg_obs_bc)
temporal_CV$data <- factor(temporal_CV$data,
                           levels = c("obs_bc", "hmg_obs_bc", "qc_obs"))
levels(temporal_CV$variable) <- c(dr = latex2exp::TeX("\\textit{$d_{r}$}"),
                                  kge2 = latex2exp::TeX("\\textit{$KGE''$}"),
                                  mcc = latex2exp::TeX("\\textit{$MCC$}"),
                                  bac = latex2exp::TeX("\\textit{$BAcc$}"))

plt3 <-
  ggplot(data = temporal_CV) + 
  geom_boxplot(aes(x = as.factor(ECOREGIONS), y = value, colour = data)) + 
  facet_wrap(~ variable, ncol = 1, scales = "free_y", labeller = label_parsed) + 
  scale_colour_viridis_d(option = "inferno",
                         begin = 0.2, end = 0.8,
                         guide = guide_legend("observed\ndataset",
                                              nrow = 1,
                                              byrow = TRUE,
                                              label.position = "top",
                                              barheight = .5)
  ) +
  scale_y_continuous(limits = c(0.1, 1)) +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = margin(t = -15),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.title = element_text(size = 9),
        plot.margin = unit(c(0,0,0,0), "lines"),
        plot.background = element_blank(),
        panel.spacing = unit(0, "lines"))

# ggsave(
#   "output/04_spatial-data-fusion/SPDF_metrics-03.pdf",
#   plt3,
#   units = "in",
#   width = 3,
#   height = 3,
#   dpi = 300,
#   scale = 1.5
# )

ecoregions <- vect("/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp")
order_scpre4sa <- c("NAS", "PAD", "CAS", "AOL", "EHL", "GCH")

temporal_CV$value_c <- cut(
  temporal_CV$value,
  breaks =  c(-Inf, .1, .2 , .3, .4, .5, .6, .7, .8, .9, Inf),
  # labels = c("[-Inf, .1)", "[.1, .2)", "[.2, .3)", "[.3, .4)", 
  #            "[.4, .5)", "[.5, .6)", "[.6, .7)", "[.7, .8)",
  #            "[.8, .9)", "[.9, 1]"),
  labels = c("< .1", ".1 - .2", ".2 - .3", ".3 - .4", ".4 - 5",
             ".5 - .6", ".6 - .7", ".7 - .8", ".8 - .9", "> .9"),
  right = FALSE)

color_bar_scpr4sa <- viridis::viridis(6, direction = -1)
to_add <- viridis::magma(5)

# metrics per ecoregion
temporal_CV_metrics <- aggregate(value ~ ECOREGIONS + data + variable, data = temporal_CV, mean)
temporal_CV_metrics$ECOREGIONS <- factor(temporal_CV_metrics$ECOREGIONS, 
                                         levels = order_scpre4sa)
temporal_CV_metrics <- temporal_CV_metrics[order(temporal_CV_metrics$data,
                                                 temporal_CV_metrics$variable,
                                                 temporal_CV_metrics$ECOREGIONS), ]
temporal_CV_metrics$formatted_entry <- paste0(
  temporal_CV_metrics$ECOREGIONS, ": ", 
  formatC(temporal_CV_metrics$value, digits = 2, format = "f")
)
temporal_CV_metrics <- aggregate(formatted_entry ~ data + variable,
                                 data = temporal_CV_metrics,
                                 FUN = function(x) paste(x, collapse = "<br>"))
colnames(temporal_CV_metrics)[3] <- "my_label"


plt4 <-
  ggplot(data = temporal_CV) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .4) +
  coord_sf(ylim = c(-25, 13), xlim = c(-82, -34), expand = c(0, 0)) +
  geom_point(aes(x = LON, y = LAT, colour = value_c), size = .25) +
  scale_colour_manual("",
                      values = c(to_add, color_bar_scpr4sa),
                      drop = FALSE,
                      na.translate = F) +
  guides(color = guide_legend(override.aes = list(size = 2), nrow = 1, label.position = "top")) +
  facet_grid(data ~ variable, switch = "y", labeller = label_parsed) +
  xlab("") + ylab("") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = margin(t = -5),
        legend.text = element_text(margin = margin(l = -5, unit = "pt"), size = 8),
        legend.key.spacing.x = unit(.5, "cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"),
        plot.background = element_blank(),
        panel.spacing = unit(0, "lines")) +
  ggtext::geom_richtext(
    data = temporal_CV_metrics,
    aes(x = -34.7, y = 5.75, label = my_label),
    fill = "white", 
    color = "gray60",
    label.padding = unit(0.5, "mm"),
    label.size = 0.01,
    label.color = NA,
    hjust = 1,
    size = 1.6)


ggsave(
  "output/04_spatial-data-fusion/SPDF_metrics-median-04.pdf",
  plt4,
  units = "in",
  width = 4.5,
  height = 3.5,
  dpi = 300,
  scale = 1.5
)
