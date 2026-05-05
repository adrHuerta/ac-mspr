rm(list = ls())

library(terra)
library(ggplot2)
library(trend)
library(patchwork)
source("/home/ahuerta/repos/exploration/prototypes/2025-12-12_application/src/funciones_indices_extremos_pp_v3_06092015.R")
source("R/trend/trend.R")

################# plt parameters #################

ecoregions <- vect("/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp")

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
    panel.border = element_rect(colour = "black", fill=NA, linewidth=0.1),
    legend.background = element_blank()
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
    panel.border = element_rect(colour = "black", fill=NA, linewidth=0.1),
    legend.background = element_blank()
  )

################# OBS_BC #################

path_data <- "/home/ahuerta/repos_phd/ac-mspr/output/05_prec4pe/prec4pe_obs_bc"
year_range <- seq(1960, 2015, 1)

# extInd <- vector("list", length(year_range))
# names(extInd) <- year_range

extInd_packed <- parallel::mclapply(
  year_range,
  function(yy){
   
    # message("Processing year: ", yy)
    
    # Get files
    grid_files <- dir(
      file.path(path_data, yy),
      recursive = TRUE,
      pattern = "\\.tif$",
      full.names = TRUE
    )
    
    # Read rasters (first layer only)
    rasters <- lapply(grid_files, function(f) rast(f)[[1]])
    
    # Create collection and get common extent
    ext_total <- ext(sprc(rasters))
    
    # Align rasters to same extent
    rasters_aligned <- lapply(rasters, extend, y = ext_total)
    
    # Stack rasters
    raster_stack <- do.call(c, rasters_aligned)
    
    # Compute indices
    res <- list(
      prcptot = app(raster_stack, PRCPTOT),
      sdii   = app(raster_stack, SDII),
      p95    = app(raster_stack, R95p),
      r95p   = app(raster_stack, R95pTOT),
      cdd    = app(raster_stack, CDD),
      cwd    = app(raster_stack, CWD)
    )
    
    # WRAP each SpatRaster so it can be passed back to the main process
    lapply(res, wrap)
    
  },
  mc.cores = 50
)

extInd <- lapply(extInd_packed, function(yr_list) {
  lapply(yr_list, unwrap)
})

indices <- names(extInd[[1]])
extInd_concat <- lapply(names(extInd[[1]]), function(idx) {
  
  r_list <- lapply(extInd, function(x) x[[idx]])
  r <- rast(r_list)
  names(r) <- names(extInd)
  r
})
names(extInd_concat) <- names(extInd[[1]])

################# MEAN ###################

extInd_concat_MEAN <- lapply(
  extInd_concat,
  function(x) app(x, mean, na.rm = TRUE)
)

extInd_concat_MEAN_df <- as.data.frame(rast(extInd_concat_MEAN), xy = TRUE)


extInd_concat_MEAN_df$prcptot_cut <- 
  cut(extInd_concat_MEAN_df$prcptot,
      breaks = c(-Inf, 50, 150, 500, 1000, 1500, 2000, 2500, Inf),
      labels = c("0-50", "50-150", "150-500", "500-1K", "1-1.5k", "1.5-2K", "2-2.5K", ">2.5K")) 

extInd_concat_MEAN_df$sdii_cut <- 
  cut(extInd_concat_MEAN_df$sdii,
      breaks = c(-Inf, 2, 5, 7.5, 10, 12.5, 15, 17.5, Inf),
      labels = c("0-2", "2-5", "5-7.5", "7.5-10", "10-12.5", "12.5-15", "15-17.5", ">17.5")) 

extInd_concat_MEAN_df$p95_cut <- 
  cut(extInd_concat_MEAN_df$p95,
      breaks =  c(-Inf, 10, 15, 20, 25, 30, 35, 40, Inf),
      labels = c("0-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", ">40")) 

extInd_concat_MEAN_df$r95p_cut <- 
  cut(extInd_concat_MEAN_df$r95p,
      breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 70, Inf),
      labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", ">70")) 

extInd_concat_MEAN_df$cdd_cut <- 
  cut(extInd_concat_MEAN_df$cdd,
      breaks = c(-Inf, 10, 20, 40, 50, 80, 100, 150, Inf),
      labels = c("0-10", "10-20", "20-40", "40-50", "50-80", "80-100" ,"100-150",">150")) 

extInd_concat_MEAN_df$cwd_cut <- 
  cut(extInd_concat_MEAN_df$cwd,
      breaks = c(-Inf, 2, 5, 7.5, 10, 12.5, 15, 20, Inf),
      labels = c("0-2", "2-5", "5-7.5", "7.5-10", "10-12.5", "12.5-15", "15-20", ">20")) 

m_prcptot <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = prcptot_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt +
  theme(legend.position = c(0.025, 0.4)) +
  xlab("PRCPTOT") + ylab("a) mean (obs_bc)") + scale_x_discrete(position = "left") # top did not work?


m_sdii <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = sdii_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt +
  theme(legend.position = c(0.025, 0.4)) +
  xlab("SDII") + ylab("") + scale_x_discrete(position = "left")   +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

m_p95 <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = p95_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt +
  theme(legend.position = c(0.025, 0.4)) +
  xlab("P95") + ylab("") + scale_x_discrete(position = "left")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

m_r95p <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = r95p_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt +
  theme(legend.position = c(0.025, 0.4))  +
  xlab("R95p") + ylab("") + scale_x_discrete(position = "left")   +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

m_cdd <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = cdd_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt +
  theme(legend.position = c(0.025, 0.4))  +
  xlab("CDD") + ylab("") + scale_x_discrete(position = "left")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

m_cwd <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = cwd_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt +
  theme(legend.position = c(0.025, 0.4)) +
  xlab("CWD") + ylab("") + scale_x_discrete(position = "left")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

################# TREND ###################

# 1. Prepara la lista envolviendo cada raster
extInd_wrapped <- lapply(extInd_concat, wrap)

# 2. Ejecuta mclapply
extInd_concat_SEN <- parallel::mclapply(extInd_wrapped, function(x_packed) {
  # IMPORTANTE: Desempaquetar dentro del núcleo hijo
  x <- unwrap(x_packed)
  
  # Ejecutar tu análisis
  res <- app(x, sen_slope_trend)
  
  # OPCIONAL: Si quieres devolver un SpatRaster, debes volver a envolverlo
  return(wrap(res))
}, mc.cores = 6)

# 3. Al terminar, recuperas los objetos originales si lo necesitas
extInd_concat_SEN <- lapply(extInd_concat_SEN, unwrap)

extInd_concat_SENp <- lapply(
  names(extInd_concat_SEN),
  function(x){
    extInd_concat_SEN[[x]][[1]] <- 10*((extInd_concat_SEN[[x]][[1]] * 100)/(extInd_concat_MEAN[[x]] + 0.01))
    extInd_concat_SEN[[x]]
  }
)
names(extInd_concat_SENp) <- names(extInd_concat_SEN)

extInd_concat_SEN_df <- as.data.frame(rast(extInd_concat_SENp), xy = TRUE)

extInd_concat_SEN_df$prcptot_1_cut <- 
  cut(extInd_concat_SEN_df$prcptot_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$sdii_1_cut <- 
  cut(extInd_concat_SEN_df$sdii_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$p95_1_cut <- 
  cut(extInd_concat_SEN_df$p95_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10 - -5", "-5 - -2.5", "-2.5 - -1", "-1 - 1", "1 - 2.5", "2.5 - 5", "5 - 10", "> 10"))

extInd_concat_SEN_df$r95p_1_cut <- 
  cut(extInd_concat_SEN_df$r95p_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$cdd_1_cut <- 
  cut(extInd_concat_SEN_df$cdd_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$cwd_1_cut <- 
  cut(extInd_concat_SEN_df$cwd_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))


t_prcptot <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = prcptot_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt +
  theme(legend.position="none") +
  xlab("") + ylab("b) trend (obs_bc)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


t_sdii <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = sdii_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

t_p95 <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = p95_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE, na.translate = FALSE) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position = c(0.025, 0.45))  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

t_r95p <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = r95p_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

t_cdd <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = cdd_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

t_cwd <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = cwd_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")   +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


################# HMG_OBS_BC #################

path_data <- "/home/ahuerta/repos_phd/ac-mspr/output/05_prec4pe/prec4pe_hmg_obs_bc"
year_range <- seq(1960, 2015, 1)

extInd_packed <- parallel::mclapply(
  year_range,
  function(yy){
    
    # message("Processing year: ", yy)
    
    # Get files
    grid_files <- dir(
      file.path(path_data, yy),
      recursive = TRUE,
      pattern = "\\.tif$",
      full.names = TRUE
    )
    
    # Read rasters (first layer only)
    rasters <- lapply(grid_files, function(f) rast(f)[[1]])
    
    # Create collection and get common extent
    ext_total <- ext(sprc(rasters))
    
    # Align rasters to same extent
    rasters_aligned <- lapply(rasters, extend, y = ext_total)
    
    # Stack rasters
    raster_stack <- do.call(c, rasters_aligned)
    
    # Compute indices
    res <- list(
      prcptot = app(raster_stack, PRCPTOT),
      sdii   = app(raster_stack, SDII),
      p95    = app(raster_stack, R95p),
      r95p   = app(raster_stack, R95pTOT),
      cdd    = app(raster_stack, CDD),
      cwd    = app(raster_stack, CWD)
    )
    
    # WRAP each SpatRaster so it can be passed back to the main process
    lapply(res, wrap)
    
  },
  mc.cores = 50
)

extInd <- lapply(extInd_packed, function(yr_list) {
  lapply(yr_list, unwrap)
})

indices <- names(extInd[[1]])
extInd_concat <- lapply(names(extInd[[1]]), function(idx) {
  
  r_list <- lapply(extInd, function(x) x[[idx]])
  r <- rast(r_list)
  names(r) <- names(extInd)
  r
})
names(extInd_concat) <- names(extInd[[1]])

################# MEAN ###################

extInd_concat_MEAN <- lapply(
  extInd_concat,
  function(x) app(x, mean, na.rm = TRUE)
)

extInd_concat_MEAN_df <- as.data.frame(rast(extInd_concat_MEAN), xy = TRUE)


extInd_concat_MEAN_df$prcptot_cut <- 
  cut(extInd_concat_MEAN_df$prcptot,
      breaks = c(-Inf, 50, 150, 500, 1000, 1500, 2000, 2500, Inf),
      labels = c("0-50", "50-150", "150-500", "500-1K", "1-1.5k", "1.5-2K", "2-2.5K", ">2.5K")) 

extInd_concat_MEAN_df$sdii_cut <- 
  cut(extInd_concat_MEAN_df$sdii,
      breaks = c(-Inf, 2, 5, 7.5, 10, 12.5, 15, 17.5, Inf),
      labels = c("0-2", "2-5", "5-7.5", "7.5-10", "10-12.5", "12.5-15", "15-17.5", ">17.5")) 

extInd_concat_MEAN_df$p95_cut <- 
  cut(extInd_concat_MEAN_df$p95,
      breaks =  c(-Inf, 10, 15, 20, 25, 30, 35, 40, Inf),
      labels = c("0-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", ">40")) 

extInd_concat_MEAN_df$r95p_cut <- 
  cut(extInd_concat_MEAN_df$r95p,
      breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 70, Inf),
      labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", ">70")) 

extInd_concat_MEAN_df$cdd_cut <- 
  cut(extInd_concat_MEAN_df$cdd,
      breaks = c(-Inf, 10, 20, 40, 50, 80, 100, 150, Inf),
      labels = c("0-10", "10-20", "20-40", "40-50", "50-80", "80-100" ,"100-150",">150")) 

extInd_concat_MEAN_df$cwd_cut <- 
  cut(extInd_concat_MEAN_df$cwd,
      breaks = c(-Inf, 2, 5, 7.5, 10, 12.5, 15, 20, Inf),
      labels = c("0-2", "2-5", "5-7.5", "7.5-10", "10-12.5", "12.5-15", "15-20", ">20")) 

m_prcptot_hmg <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = prcptot_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt +
  theme(legend.position = c(0.025, 0.4))


m_sdii_hmg <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = sdii_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +  theme_bw() + 
  theme_plt_r +
  theme(legend.position = c(0.025, 0.4))

m_p95_hmg <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = p95_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position = c(0.025, 0.4))

m_r95p_hmg <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = r95p_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position = c(0.025, 0.4))

m_cdd_hmg <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = cdd_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position = c(0.025, 0.4))

m_cwd_hmg <- ggplot(data = extInd_concat_MEAN_df) + 
  geom_raster(aes(x = x, y = y, fill = cwd_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(8, "Spectral"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position = c(0.025, 0.4))


################# TREND  ###############################

# 1. Prepara la lista envolviendo cada raster
extInd_wrapped <- lapply(extInd_concat, wrap)

# 2. Ejecuta mclapply
extInd_concat_SEN <- parallel::mclapply(extInd_wrapped, function(x_packed) {
  # IMPORTANTE: Desempaquetar dentro del núcleo hijo
  x <- unwrap(x_packed)
  
  # Ejecutar tu análisis
  res <- app(x, sen_slope_trend)
  
  # OPCIONAL: Si quieres devolver un SpatRaster, debes volver a envolverlo
  return(wrap(res))
}, mc.cores = 6)

# 3. Al terminar, recuperas los objetos originales si lo necesitas
extInd_concat_SEN <- lapply(extInd_concat_SEN, unwrap)

extInd_concat_SENp_hmg <- lapply(
  names(extInd_concat_SEN),
  function(x){
    extInd_concat_SEN[[x]][[1]] <- 10*((extInd_concat_SEN[[x]][[1]] * 100)/(extInd_concat_MEAN[[x]] + 0.01))
    extInd_concat_SEN[[x]]
  }
)
names(extInd_concat_SENp_hmg) <- names(extInd_concat_SEN)

extInd_concat_SEN_df <- as.data.frame(rast(extInd_concat_SENp_hmg), xy = TRUE)

extInd_concat_SEN_df$prcptot_1_cut <- 
  cut(extInd_concat_SEN_df$prcptot_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$sdii_1_cut <- 
  cut(extInd_concat_SEN_df$sdii_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$p95_1_cut <- 
  cut(extInd_concat_SEN_df$p95_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10 - -5", "-5 - -2.5", "-2.5 - -1", "-1 - 1", "1 - 2.5", "2.5 - 5", "5 - 10", "> 10"))

extInd_concat_SEN_df$r95p_1_cut <- 
  cut(extInd_concat_SEN_df$r95p_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$cdd_1_cut <- 
  cut(extInd_concat_SEN_df$cdd_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))

extInd_concat_SEN_df$cwd_1_cut <- 
  cut(extInd_concat_SEN_df$cwd_1,
      breaks = c(-Inf, -10, -5, -2.5, -1, 1, 2.5, 5, 10,  Inf),
      labels = c("< -10", "-10--5", "-5--2.5", "-2.5--1", "-1-1", "1-2.5", "2.5-5", "5-10", "> 10"))


t_prcptot_hmg <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = prcptot_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt +
  theme(legend.position="none") +
  xlab("") + ylab("c) trend (hmg_obs_bc)")  +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


t_sdii_hmg <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = sdii_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

t_p95_hmg <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = p95_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE, na.translate = FALSE) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position = c(0.025, 0.45))  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

t_r95p_hmg <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = r95p_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

t_cdd_hmg <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = cdd_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

t_cwd_hmg <- ggplot(data = extInd_concat_SEN_df) + 
  geom_raster(aes(x = x, y = y, fill = cwd_1_cut), show.legend = TRUE) + 
  scale_fill_discrete("", palette = RColorBrewer::brewer.pal(9, "PiYG"),
                      drop = FALSE,  na.translate = FALSE) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


################# TREND COMPARISON: obs_vs and hmg_obs_bc #####################

agrrTrend_concat <- lapply(
  seq_along(extInd_concat_SENp),
  function(s){
    
    sds_obj <- sds(extInd_concat_SENp[[s]]$slope,
                   extInd_concat_SENp[[s]]$p_value,
                   extInd_concat_SENp_hmg[[s]]$slope,
                   extInd_concat_SENp_hmg[[s]]$p_value)
    lapp(sds_obj, fun = trend_agreement)
    
  }
  )

names(agrrTrend_concat) <- names(extInd_concat_SENp)
agrrTrend_concat_df <- as.data.frame(rast(agrrTrend_concat), xy = TRUE)

aT_prcptot <- ggplot(data = agrrTrend_concat_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(prcptot_1)), show.legend = TRUE) + 
  scale_fill_discrete(palette = c("-1" = "#DE77AE", "0" = "#F7F7F7", "1" = "#7FBC41"),
                      labels = c("-1" = "(-) Decreasing", "0" = "Disagree", "1" = "(+) Increasing"),
                      drop = FALSE,  na.translate = FALSE) +
  geom_point(aes(x = x, y = y, color = factor(prcptot_2)), na.rm = TRUE, show.legend = FALSE, size = 0.001, alpha = 0.05) +
  scale_color_discrete(palette = c("0" = NA, "1" = "black"), na.value = NA) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt +
  theme(legend.position="none") +
  xlab("") + ylab("d) trend agreement")  +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

aT_sdii <- ggplot(data = agrrTrend_concat_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(sdii_1)), show.legend = TRUE) + 
  scale_fill_discrete(palette = c("-1" = "#DE77AE", "0" = "#F7F7F7", "1" = "#7FBC41"),
                      labels = c("-1" = "(-) Decreasing", "0" = "Disagree", "1" = "(+) Increasing"),
                      drop = FALSE,  na.translate = FALSE) +
  geom_point(aes(x = x, y = y, color = factor(sdii_2)), na.rm = TRUE, show.legend = FALSE, size = 0.001, alpha = 0.05) +
  scale_color_discrete(palette = c("0" = NA, "1" = "black"), na.value = NA) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

aT_p95 <- ggplot(data = agrrTrend_concat_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(p95_1)), show.legend = TRUE) + 
  scale_fill_discrete(palette = c("-1" = "#DE77AE", "0" = "#F7F7F7", "1" = "#7FBC41"),
                      labels = c("-1" = "(-) Decreasing", "0" = "Disagree", "1" = "(+) Increasing"),
                      drop = FALSE,  na.translate = FALSE) +
  geom_point(aes(x = x, y = y, color = factor(p95_2)), na.rm = TRUE, show.legend = FALSE, size = 0.001, alpha = 0.05) +
  scale_color_discrete(palette = c("0" = NA, "1" = "black"), na.value = NA) +
  guides(color = "none") + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position = c(0.025, 0.175))  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

aT_cdd <- ggplot(data = agrrTrend_concat_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(cdd_1)), show.legend = TRUE) + 
  scale_fill_discrete(palette = c("-1" = "#DE77AE", "0" = "#F7F7F7", "1" = "#7FBC41"),
                      labels = c("-1" = "(-) Decreasing", "0" = "Disagree", "1" = "(+) Increasing"),
                      drop = FALSE,  na.translate = FALSE) +
  geom_point(aes(x = x, y = y, color = factor(cdd_2)), na.rm = TRUE, show.legend = FALSE, size = 0.001, alpha = 0.05) +
  scale_color_discrete(palette = c("0" = NA, "1" = "black"), na.value = NA) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

aT_cwd <- ggplot(data = agrrTrend_concat_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(cwd_1)), show.legend = TRUE) + 
  scale_fill_discrete(palette = c("-1" = "#DE77AE", "0" = "#F7F7F7", "1" = "#7FBC41"),
                      labels = c("-1" = "(-) Decreasing", "0" = "Disagree", "1" = "(+) Increasing"),
                      drop = FALSE,  na.translate = FALSE) +
  geom_point(aes(x = x, y = y, color = factor(cwd_2)), na.rm = TRUE, show.legend = FALSE, size = 0.001, alpha = 0.05) +
  scale_color_discrete(palette = c("0" = NA, "1" = "black"), na.value = NA) + 
  tidyterra::geom_spatvector(data = ecoregions, fill = NA, size = .085, color = "gray45") +
  coord_sf(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-82.25, -67.175)) +
  theme_bw() + 
  theme_plt_r +
  theme(legend.position="none")  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

################# PLOT #################

((m_prcptot / t_prcptot / t_prcptot_hmg / aT_prcptot) | 
   (m_sdii / t_sdii / t_sdii_hmg / aT_sdii) |
   (m_p95 / t_p95 / t_p95_hmg / aT_p95) |
   (m_cdd / t_cdd / t_cdd_hmg / aT_cdd) | 
   (m_cwd / t_cwd / t_cwd_hmg / aT_cwd)) &
  # plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(plot.tag.position = c(0.05, 0.95),
        plot.tag = element_text(size = 4.5))

ggsave(
  "output/05_prec4pe/prec4pe_mt.pdf",
  width = 5, height = 5.45, device = "pdf",
  dpi = 500,
  scale = 1
)
