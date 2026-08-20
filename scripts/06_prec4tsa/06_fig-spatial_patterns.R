rm(list = ls())

library(terra)
library(ggplot2)
library(trend)
library(patchwork)
source("/home/ahuerta/repos/exploration/prototypes/2025-12-12_application/src/funciones_indices_extremos_pp_v3_06092015.R")
source("R/trend/trend.R")

################# plt parameters #################

ecoregions <- vect("/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp")


generate_split_legend_map_v1 <- function(data, 
                                         fill_var = "prcptot_cut", 
                                         vector_data = ecoregions,
                                         palette_name = "Spectral") {
  library(ggplot2)
  library(ggnewscale)
  library(RColorBrewer)
  
  # 1. Dynamically extract factor levels from the chosen column
  all_levels <- levels(data[[fill_var]])
  num_levels <- length(all_levels)
  
  if (num_levels < 2) {
    stop("The target fill variable must have at least 2 factor levels to split.")
  }
  
  # 2. Automatically split the palette and factor levels roughly in half
  mid_point  <- floor(num_levels / 2)
  spec_cols  <- brewer.pal(max(8, num_levels), palette_name)
  
  levels_g1  <- all_levels[1:mid_point]
  levels_g2  <- all_levels[(mid_point + 1):num_levels]
  
  # 3. Filter data layers based on dynamic inputs
  data_g1 <- data[data[[fill_var]] %in% levels_g1, ]
  data_g2 <- data[data[[fill_var]] %in% levels_g2, ]
  
  # 4. Generate the map
  m_plot <- ggplot() + 
    
    # --- LAYER 1 & SCALE 1: Low Values (Bottom-Left) ---
    geom_raster(
      data = data_g1, 
      aes(x = x, y = y, fill = .data[[fill_var]])
    ) + 
    scale_fill_manual(
      values = setNames(spec_cols[1:mid_point], levels_g1),
      drop = FALSE, na.translate = FALSE,
      guide = guide_legend(
        position = "inside",
        theme = theme(
          legend.position.inside = c(0.125, 0.15), 
          legend.justification = c(0, 0),
          legend.title = element_blank(),                  
          legend.text = element_text(size = 2.75, margin = margin(l = 2)),            
          legend.key.size = unit(0.08, "cm"),
          legend.key.width = unit(0.05, "cm"),              
          legend.spacing.y = unit(0.1, "cm"),     
          legend.spacing.x = unit(-15, "cm"),        
          legend.background = element_blank(),             
          legend.box.background = element_blank()          
        )
      )
    ) + 
    
    # --- SPLIT THE FILL CHANNEL ---
    new_scale_fill() + 
    
    # --- LAYER 2 & SCALE 2: High Values (Top-Right) ---
    geom_raster(
      data = data_g2, 
      aes(x = x, y = y, fill = .data[[fill_var]])
    ) + 
    scale_fill_manual(
      values = setNames(spec_cols[(mid_point + 1):num_levels], levels_g2),
      drop = FALSE, na.translate = FALSE,
      guide = guide_legend(
        position = "inside",
        theme = theme(
          legend.position.inside = c(0.85, 0.8), 
          legend.justification = c(1, 1),
          legend.title = element_blank(),                  
          legend.text = element_text(size = 2.75, margin = margin(l = 2)),            
          legend.key.size = unit(0.08, "cm"),
          legend.key.width = unit(0.05, "cm"),              
          legend.spacing.y = unit(0.1, "cm"),     
          legend.spacing.x = unit(-15, "cm"),            
          legend.background = element_blank(),             
          legend.box.background = element_blank()          
        )
      )
    ) + 
    
    # --- Standard Spatial Maps & Coordinates ---
    tidyterra::geom_spatvector(data = vector_data, fill = NA, size = .085, color = "gray45") +
    coord_sf(expand = c(0, 0), ylim = c(-26, 12.85), xlim = c(-85, -34)) +
    theme_bw() +
    
    # --- Clean Global Layout ---
    theme(
      axis.text = element_blank(),                         
      axis.ticks = element_blank(),                        
      # axis.title = element_blank(),
      legend.box = "none",
      plot.margin = unit(c(0, 0, 0, 0), "cm"),             
      panel.spacing = unit(c(0, 0, 0, 0), "cm"),
      panel.grid.minor = element_line(size = 0.01), panel.grid.major = element_line(size = 0.01),
      panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.01)
    )
  
  return(m_plot)
}

theme_plt <- 
  theme(
    legend.justification = c(0, 1),    # Align the top-left corner of the legend box
    legend.key.size = unit(0.15, "cm"),      # Shrinks the symbols/boxes
    legend.key.width = unit(0.05, "cm"),
    legend.title = element_blank(),    # Shrinks the title
    legend.margin = margin(0, 0, 0, 0, "pt"),
    axis.title = element_text(size = 6.25),
    # axis.title = element_blank(),   # Removes "wt" and "mpg"
    axis.text = element_blank(),    # Removes the numbers (0, 10, 20...)
    axis.ticks = element_blank(),   # Removes the little tick marks
    axis.line = element_blank(),     # Removes the x and y axis lines
    plot.margin = margin(0, 0, 0, 0, "pt"),
    legend.key.spacing.x = unit(-5, "cm"),
    legend.spacing.x = unit(-5, "cm"),
    legend.text = element_text(size = 4, margin = margin(l = 2)),
    legend.background = element_blank(),
    panel.grid.minor = element_line(size = 0.01), panel.grid.major = element_line(size = 0.01),
    panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.01)
  )


theme_plt_r <- 
  theme(
    legend.justification = c(0, 1),    # Align the top-left corner of the legend box
    legend.key.size = unit(0.15, "cm"),      # Shrinks the symbols/boxes
    legend.key.width = unit(0.05, "cm"),
    legend.title = element_blank(),    # Shrinks the title
    legend.margin = margin(0, 0, 0, 0, "pt"),
    axis.title = element_blank(),   # Removes "wt" and "mpg"
    axis.text = element_blank(),    # Removes the numbers (0, 10, 20...)
    axis.ticks = element_blank(),   # Removes the little tick marks
    axis.line = element_blank(),     # Removes the x and y axis lines
    plot.margin = margin(0, 0, 0, 0, "pt"),
    legend.key.spacing.x = unit(-5, "cm"),
    legend.spacing.x = unit(-5, "cm"),
    legend.text = element_text(size = 4, margin = margin(l = 2)),
    legend.background = element_blank(),
    panel.grid.minor = element_line(size = 0.01), panel.grid.major = element_line(size = 0.01),
    panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.01)
  )


################# OBS_BC #################
################# MEAN #################

extInd_concat_MEAN <- sds("output/06_prec4tsa/obs_bc-prec4tsa-yearly-mean.nc")
names_vars <- names(extInd_concat_MEAN)
extInd_concat_MEAN <- rast(extInd_concat_MEAN) 
names(extInd_concat_MEAN) <- names_vars

extInd_concat_MEAN_df <- as.data.frame(extInd_concat_MEAN, xy = TRUE)

extInd_concat_MEAN_df$prcptot_cut <- 
  cut(extInd_concat_MEAN_df$prcptot,
      breaks = c(-Inf, 50, 150, 300, 500, 700, 1000, 1500, Inf),
      labels = c("0-50", "50-150", "150-500", "500-1K", "1-1.5k", "1.5-2K", "2-2.5K", ">2.5K")) 

extInd_concat_MEAN_df$r1mm_cut <- 
  cut(extInd_concat_MEAN_df$r1mm,
      breaks = c(-Inf, 5, 10, 15, 30, 50, 75, 100, Inf),
      labels = c("0-2", "2-5", "5-7.5", "7.5-10", "10-12.5", "12.5-15", "15-17.5", ">17.5")) 

extInd_concat_MEAN_df$p95_cut <- 
  cut(extInd_concat_MEAN_df$p95,
      breaks =  c(-Inf, 10, 15, 20, 25, 30, 35, 40, Inf),
      labels = c("0-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", ">40")) 

extInd_concat_MEAN_df$cdd_cut <- 
  cut(extInd_concat_MEAN_df$cdd,
      breaks = c(-Inf, 10, 20, 40, 50, 80, 100, 150, Inf),
      labels = c("0-10", "10-20", "20-40", "40-50", "50-80", "80-100" ,"100-150",">150")) 

extInd_concat_MEAN_df$cwd_cut <- 
  cut(extInd_concat_MEAN_df$cwd,
      breaks = c(-Inf, 2, 5, 7.5, 10, 12.5, 15, 20, Inf),
      labels = c("0-2", "2-5", "5-7.5", "7.5-10", "10-12.5", "12.5-15", "15-20", ">20")) 


m_prcptot <- generate_split_legend_map_v1(data = extInd_concat_MEAN_df,
                                          fill_var = "prcptot_cut",
                                          vector_data = ecoregions,
                                          palette_name = "Spectral") +
  xlab("PRCPTOT") + ylab("a) mean (obs_bc)") +
  scale_x_discrete(position = "left") +
  theme(axis.title = element_text(size = 5.25)) +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))


m_r1mm <- generate_split_legend_map_v1(data = extInd_concat_MEAN_df,
                                       fill_var = "r1mm_cut",
                                       vector_data = ecoregions,
                                       palette_name = "Spectral") + 
  xlab("R1mm") + ylab("") +
  scale_x_discrete(position = "left")  +
  theme(axis.title = element_text(size = 5.25)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

m_p95 <- generate_split_legend_map_v1(data = extInd_concat_MEAN_df,
                                      fill_var = "p95_cut",
                                      vector_data = ecoregions,
                                      palette_name = "Spectral") +
  xlab("P95") + ylab("") +
  scale_x_discrete(position = "left") +
  theme(axis.title = element_text(size = 5.25)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

m_cdd <- generate_split_legend_map_v1(data = extInd_concat_MEAN_df,
                                      fill_var = "cdd_cut",
                                      vector_data = ecoregions,
                                      palette_name = "Spectral") +
  xlab("CDD") + ylab("") +
  scale_x_discrete(position = "left") +
  theme(axis.title = element_text(size = 5.25)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))


m_cwd <- generate_split_legend_map_v1(data = extInd_concat_MEAN_df,
                                      fill_var = "cwd_cut",
                                      vector_data = ecoregions,
                                      palette_name = "Spectral") +
  xlab("CWD") + ylab("") +
  scale_x_discrete(position = "left") +
  theme(axis.title = element_text(size = 5.25)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))


##################### TRENDS ###########################

extInd_concat_SENp <- sds("output/06_prec4tsa/obs_bc-prec4tsa-yearly-trend.nc")
names_vars <- names(extInd_concat_SENp)
extInd_concat_SENp <- rast(lapply(extInd_concat_SENp, function(x) x[[1]])) # just slope
names(extInd_concat_SENp) <- names_vars

extInd_concat_SEN_df <- as.data.frame(extInd_concat_SENp, xy = TRUE)

extInd_concat_SEN_df$prcptot_1_cut <- 
  cut(extInd_concat_SEN_df$prcptot,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$r1mm_1_cut <- 
  cut(extInd_concat_SEN_df$r1mm,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$p95_1_cut <- 
  cut(extInd_concat_SEN_df$p95,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10 - -5", "-5 - -2.5", "-2.5 - -1", "-1 - 1", "1 - 2.5", "2.5 - 5", "5 - 10", "> 10"))

extInd_concat_SEN_df$cdd_1_cut <- 
  cut(extInd_concat_SEN_df$cdd,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$cwd_1_cut <- 
  cut(extInd_concat_SEN_df$cwd,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))


t_prcptot <- generate_split_legend_map_v1(data = extInd_concat_SEN_df,
                                          fill_var = "prcptot_1_cut",
                                          vector_data = ecoregions,
                                          palette_name = "PiYG") + 
  xlab("") + ylab("b) trend (obs_bc)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position="none")  +
  theme(axis.title = element_text(size = 5.25)) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

t_r1mm <- generate_split_legend_map_v1(data = extInd_concat_SEN_df,
                                       fill_var = "r1mm_1_cut",
                                       vector_data = ecoregions,
                                       palette_name = "PiYG") + 
  theme(legend.position="none")  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))


t_p95 <- generate_split_legend_map_v1(data = extInd_concat_SEN_df,
                                      fill_var = "p95_1_cut",
                                      vector_data = ecoregions,
                                      palette_name = "PiYG") + 
  # theme(legend.position="none")  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))


t_cdd <- generate_split_legend_map_v1(data = extInd_concat_SEN_df,
                                      fill_var = "cdd_1_cut",
                                      vector_data = ecoregions,
                                      palette_name = "PiYG") + 
  theme(legend.position="none")  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

t_cwd <- generate_split_legend_map_v1(data = extInd_concat_SEN_df,
                                      fill_var = "cwd_1_cut",
                                      vector_data = ecoregions,
                                      palette_name = "PiYG") + 
  theme(legend.position="none")  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))


################# HMG_OBS_BC #################

##################### TRENDS ###########################

extInd_concat_SENp_hmg <- sds("output/06_prec4tsa/hmg_obs_bc-prec4tsa-yearly-trend.nc")
names_vars <- names(extInd_concat_SENp_hmg)
extInd_concat_SENp_hmg <- rast(lapply(extInd_concat_SENp_hmg, function(x) x[[1]])) # just slope
names(extInd_concat_SENp_hmg) <- names_vars

extInd_concat_SEN_df <- as.data.frame(extInd_concat_SENp_hmg, xy = TRUE)

extInd_concat_SEN_df$prcptot_1_cut <- 
  cut(extInd_concat_SEN_df$prcptot,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$r1mm_1_cut <- 
  cut(extInd_concat_SEN_df$r1mm,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$p95_1_cut <- 
  cut(extInd_concat_SEN_df$p95,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10 - -5", "-5 - -2.5", "-2.5 - -1", "-1 - 1", "1 - 2.5", "2.5 - 5", "5 - 10", "> 10"))

extInd_concat_SEN_df$cdd_1_cut <- 
  cut(extInd_concat_SEN_df$cdd,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$cwd_1_cut <- 
  cut(extInd_concat_SEN_df$cwd,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

t_prcptot_hmg <- generate_split_legend_map_v1(data = extInd_concat_SEN_df,
                                              fill_var = "prcptot_1_cut",
                                              vector_data = ecoregions,
                                              palette_name = "PiYG") + 
  xlab("") + ylab("c) trend (hmg_obs_bc)")  +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position="none")  +
  theme(axis.title = element_text(size = 5.25)) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

t_r1mm_hmg <- generate_split_legend_map_v1(data = extInd_concat_SEN_df,
                                           fill_var = "r1mm_1_cut",
                                           vector_data = ecoregions,
                                           palette_name = "PiYG") + 
  theme(legend.position="none")  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

t_p95_hmg <- generate_split_legend_map_v1(data = extInd_concat_SEN_df,
                                          fill_var = "p95_1_cut",
                                          vector_data = ecoregions,
                                          palette_name = "PiYG") + 
  theme(legend.position="none")  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

t_cdd_hmg <- generate_split_legend_map_v1(data = extInd_concat_SEN_df,
                                          fill_var = "cdd_1_cut",
                                          vector_data = ecoregions,
                                          palette_name = "PiYG") + 
  theme(legend.position="none")  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

t_cwd_hmg <- generate_split_legend_map_v1(data = extInd_concat_SEN_df,
                                          fill_var = "cwd_1_cut",
                                          vector_data = ecoregions,
                                          palette_name = "PiYG") + 
  theme(legend.position="none")  +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

################# TREND COMPARISON: obs_vs and hmg_obs_bc #####################

agrrTrend_concat <- sds("output/06_prec4tsa/prec4tsa-yearly-trend-comparison.nc")
agrrTrend_concat <- rast(agrrTrend_concat)
names(agrrTrend_concat) <- paste0(rep(names(extInd_concat_SENp), each = 2), "_", 1:2)
agrrTrend_concat_df <- as.data.frame(agrrTrend_concat, xy = TRUE)

aT_prcptot <- ggplot(data = agrrTrend_concat_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(prcptot_1)), show.legend = TRUE) + 
  scale_fill_discrete(palette = c("-1" = "#DE77AE", "0" = "#F7F7F7", "1" = "#7FBC41"),
                      labels = c("-1" = "--", "0" = "-+", "1" = "++"),
                      drop = FALSE,  na.translate = FALSE) +
  geom_point(aes(x = x, y = y, color = factor(prcptot_2)), na.rm = TRUE, show.legend = FALSE, size = 0.001, alpha = 0.01) +
  scale_color_discrete(palette = c("0" = NA, "1" = "black"), na.value = NA) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-26, 12.85), xlim = c(-85, -34.25)) +
  theme_bw() + 
  theme_plt +
  theme(legend.position="none") +
  xlab("") + ylab("d) trend agreement")  +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(size = 5.25),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  
aT_r1mm <- ggplot(data = agrrTrend_concat_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(r1mm_1)), show.legend = TRUE) + 
  scale_fill_discrete(palette = c("-1" = "#DE77AE", "0" = "#F7F7F7", "1" = "#7FBC41"),
                      labels = c("-1" = "(--)", "0" = "(-+)", "1" = "(++)"),
                      drop = FALSE,  na.translate = FALSE) +
  geom_point(aes(x = x, y = y, color = factor(r1mm_2)), na.rm = TRUE, show.legend = FALSE, size = 0.001, alpha = 0.01) +
  scale_color_discrete(palette = c("0" = NA, "1" = "black"), na.value = NA) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-26, 12.85), xlim = c(-85, -34.25)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

aT_p95 <- ggplot(data = agrrTrend_concat_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(p95_1)), show.legend = TRUE) + 
  scale_fill_discrete(palette = c("-1" = "#DE77AE", "0" = "#F7F7F7", "1" = "#7FBC41"),
                      labels = c("-1" = "--", "0" = "-+", "1" = "++"),
                      drop = FALSE,  na.translate = FALSE) +
  geom_point(aes(x = x, y = y, color = factor(p95_2)), na.rm = TRUE, show.legend = FALSE, size = 0.001, alpha = 0.01) +
  scale_color_discrete(palette = c("0" = NA, "1" = "black"), na.value = NA) +
  guides(color = "none") + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-26, 12.85), xlim = c(-85, -34.25)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position = c(0.06, 0.285))  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

aT_cdd <- ggplot(data = agrrTrend_concat_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(cdd_1)), show.legend = TRUE) + 
  scale_fill_discrete(palette = c("-1" = "#DE77AE", "0" = "#F7F7F7", "1" = "#7FBC41"),
                      labels = c("-1" = "(--)", "0" = "(-+)", "1" = "(++)"),
                      drop = FALSE,  na.translate = FALSE) +
  geom_point(aes(x = x, y = y, color = factor(cdd_2)), na.rm = TRUE, show.legend = FALSE, size = 0.001, alpha = 0.01) +
  scale_color_discrete(palette = c("0" = NA, "1" = "black"), na.value = NA) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-26, 12.85), xlim = c(-85, -34.25)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

aT_cwd <- ggplot(data = agrrTrend_concat_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(cwd_1)), show.legend = TRUE) + 
  scale_fill_discrete(palette = c("-1" = "#DE77AE", "0" = "#F7F7F7", "1" = "#7FBC41"),
                      labels = c("-1" = "(--)", "0" = "(-+)", "1" = "(++)"),
                      drop = FALSE,  na.translate = FALSE) +
  geom_point(aes(x = x, y = y, color = factor(cwd_2)), na.rm = TRUE, show.legend = FALSE, size = 0.001, alpha = 0.01) +
  scale_color_discrete(palette = c("0" = NA, "1" = "black"), na.value = NA) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-26, 12.85), xlim = c(-85, -34.25)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


#########################################################

((m_prcptot / t_prcptot / t_prcptot_hmg / aT_prcptot) | 
    (m_r1mm / t_r1mm / t_r1mm_hmg / aT_r1mm) |
    (m_p95 / t_p95 / t_p95_hmg / aT_p95) |
    (m_cdd / t_cdd / t_cdd_hmg / aT_cdd) | 
    (m_cwd / t_cwd / t_cwd_hmg / aT_cwd)) &
  # plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(plot.tag.position = c(0.05, 0.95),
        plot.tag = element_text(size = 4.5))

#########################################################
# final plot 

ggsave(
  "output/enhanced_paper/fig_prec4tsa_mt.pdf",
  width = 5, height = 5.45, device = "pdf",
  dpi = 500,
  scale = 1
)

