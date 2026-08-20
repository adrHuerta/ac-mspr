rm(list = ls())

path_data <- "/mnt/climstor2/vol01_ecmwf/06_prec4tsa/prec4tsa_obs_bc/"
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

    length(grid_files) - length(seq(as.Date(paste0(yy, "-01-01")), as.Date(paste0(yy, "-12-31")), by = "day"))

  },
  mc.cores = 50
)

sum(unlist(extInd_packed))

path_data <- "/mnt/climstor2/vol01_ecmwf/06_prec4tsa/prec4tsa_hmg_obs_bc/"
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
    
    length(grid_files) - length(seq(as.Date(paste0(yy, "-01-01")), as.Date(paste0(yy, "-12-31")), by = "day"))
    
  },
  mc.cores = 50
)

sum(unlist(extInd_packed))
