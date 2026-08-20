rm(list = ls())

library(terra)
library(ggplot2)
library(trend)
library(patchwork)
source("/home/ahuerta/repos/exploration/prototypes/2025-12-12_application/src/funciones_indices_extremos_pp_v3_06092015.R")
source("R/trend/trend.R")

ecoregions <- vect("/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp")

master_template <- rast("/mnt/climstor2/vol01_ecmwf/06_prec4tsa/prec4tsa_obs_bc/1960/1960-01-01/pr_balanced__best_dr_.tif")

################# OBS_BC #################

path_data <- "/mnt/climstor2/vol01_ecmwf/06_prec4tsa/prec4tsa_obs_bc/"
year_range <- seq(1960, 2015, 1)
hydro_year_range <- 1961:2015

############################################

extInd_packed <- parallel::mclapply(
  hydro_year_range,
  function(hy){
    
    message("Processing hydrological year: ", hy)
    
    # ---------------------------------------------------------
    # Define hydrological-year dates
    # November of previous year -> April of current year
    # ---------------------------------------------------------
    
    dates <- seq(
      as.Date(paste0(hy - 1, "-11-01")),
      as.Date(paste0(hy, "-04-30")),
      by = "day"
    )
    
    # ---------------------------------------------------------
    # Construct daily folder paths
    # ---------------------------------------------------------
    
    daily_dirs <- file.path(
      path_data,
      format(dates, "%Y"),
      format(dates, "%Y-%m-%d")
    )

    grid_files <- vapply(
      daily_dirs,
      function(d) {
        
        files <- list.files(
          d,
          pattern = "\\.tif$",
          full.names = TRUE
        )
        
        if (length(files) == 0) {
          return(NA_character_)
        }
        
        # There should be one TIFF per daily folder
        files[1]
      },
      character(1)
    )
    
    missing_files <- is.na(grid_files)
    
    if (any(missing_files)) {
      
      warning(
        "HY ", hy, ": ",
        sum(missing_files),
        " daily TIFF files are missing"
      )
      
    }
    
    # Keep only existing TIFF files
    grid_files <- grid_files[!missing_files]
    
    rasters <- lapply(
      grid_files,
      function(f) rast(f)[[1]]
    )

    rasters_aligned <- lapply(
      rasters,
      function(r) {
        resample(
          r,
          master_template,
          method = "near"
        )
      }
    )
    
    raster_stack <- rast(
      rasters_aligned
    )

    res <- list(
      prcptot = app(raster_stack, PRCPTOT),
      r1mm    = app(raster_stack, R11mm),
      p95     = app(raster_stack, R95p),
      cdd     = app(raster_stack, CDD),
      cwd     = app(raster_stack, CWD)
    )
    
    
    lapply(
      res,
      wrap
    )
    
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

writeCDF(
  sds(extInd_concat),
  "output/06_prec4tsa/obs_bc-prec4tsa-yearly.nc",
  overwrite = TRUE
)

################# MEAN

extInd_concat_MEAN <- lapply(
  extInd_concat,
  function(x) app(mask(x, ecoregions), mean, na.rm = TRUE)
)

writeCDF(
  sds(extInd_concat_MEAN),
  "output/06_prec4tsa/obs_bc-prec4tsa-yearly-mean.nc",
  overwrite = TRUE
)

################# TREND

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

writeCDF(
  sds(extInd_concat_SENp),
  "output/06_prec4tsa/obs_bc-prec4tsa-yearly-trend.nc",
  overwrite = TRUE
)


################# HMG_OBS_BC #################

path_data <- "/mnt/climstor2/vol01_ecmwf/06_prec4tsa/prec4tsa_hmg_obs_bc/"
year_range <- seq(1960, 2015, 1)
hydro_year_range <- 1961:2015

#############################################

extInd_packed <- parallel::mclapply(
  hydro_year_range,
  function(hy){
    
    message("Processing hydrological year: ", hy)
    
    # ---------------------------------------------------------
    # Define hydrological-year dates
    # November of previous year -> April of current year
    # ---------------------------------------------------------
    
    dates <- seq(
      as.Date(paste0(hy - 1, "-11-01")),
      as.Date(paste0(hy, "-04-30")),
      by = "day"
    )
    
    # ---------------------------------------------------------
    # Construct daily folder paths
    # ---------------------------------------------------------
    
    daily_dirs <- file.path(
      path_data,
      format(dates, "%Y"),
      format(dates, "%Y-%m-%d")
    )
    
    grid_files <- vapply(
      daily_dirs,
      function(d) {
        
        files <- list.files(
          d,
          pattern = "\\.tif$",
          full.names = TRUE
        )
        
        if (length(files) == 0) {
          return(NA_character_)
        }
        
        # There should be one TIFF per daily folder
        files[1]
      },
      character(1)
    )
    
    missing_files <- is.na(grid_files)
    
    if (any(missing_files)) {
      
      warning(
        "HY ", hy, ": ",
        sum(missing_files),
        " daily TIFF files are missing"
      )
      
    }
    
    # Keep only existing TIFF files
    grid_files <- grid_files[!missing_files]
    
    rasters <- lapply(
      grid_files,
      function(f) rast(f)[[1]]
    )
    
    rasters_aligned <- lapply(
      rasters,
      function(r) {
        resample(
          r,
          master_template,
          method = "near"
        )
      }
    )
    
    raster_stack <- rast(
      rasters_aligned
    )
    
    res <- list(
      prcptot = app(raster_stack, PRCPTOT),
      r1mm    = app(raster_stack, R11mm),
      p95     = app(raster_stack, R95p),
      cdd     = app(raster_stack, CDD),
      cwd     = app(raster_stack, CWD)
    )
    
    
    lapply(
      res,
      wrap
    )
    
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

writeCDF(
  sds(extInd_concat),
  "output/06_prec4tsa/hmg_obs_bc-prec4tsa-yearly.nc",
  overwrite = TRUE
)

################# MEAN

extInd_concat_MEAN <- lapply(
  extInd_concat,
  function(x) app(mask(x, ecoregions), mean, na.rm = TRUE)
)

writeCDF(
  sds(extInd_concat_MEAN),
  "output/06_prec4tsa/hmg_obs_bc-prec4tsa-yearly-mean.nc",
  overwrite = TRUE
)

################# TREND

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

writeCDF(
  sds(extInd_concat_SENp_hmg),
  "output/06_prec4tsa/hmg_obs_bc-prec4tsa-yearly-trend.nc",
  overwrite = TRUE
)

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

writeCDF(
  sds(agrrTrend_concat),
  "output/06_prec4tsa/prec4tsa-yearly-trend-comparison.nc"
)
