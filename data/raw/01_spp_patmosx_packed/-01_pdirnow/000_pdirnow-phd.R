rm(list = ls())
library(terra)

# main_link <- "https://persiann.eng.uci.edu/CHRSdata/PDIRNow/PDIRNowdaily"
# main_tp = "/mnt/climstor2/vol01_ecmwf/download/pdirnow/raw"
# 
# options(timeout=1000)
# 
# 
# for(ix in 2000:2000){
#   
#   if (!dir.exists(file.path(main_tp, ix))) {dir.create(file.path(main_tp, ix), showWarnings = FALSE, recursive = TRUE)}
#   
#   daily_time_stps <- seq(as.Date(paste(ix, "03", "01", sep = "-")), as.Date(paste(ix, "12", "31", sep = "-")), "day")
#   lapply(daily_time_stps,
#          function(ij){
#            
#            file_to_download <- format(ij, "%y%m%d")
#            file_to_download <- file.path(main_link, paste("pdirnow1d", file_to_download, ".bin.gz", sep = ""))
#            file_to_save <- file.path(main_tp, ix, paste(ij, ".bin.gz", sep = ""))
#            download.file(file_to_download, file_to_save)
#            
#          }
#          )
#   
#   
# }

# Donwloaded manually from the main web page 
pdirnow_files <- "/mnt/climstor2/vol01_ecmwf/download/pdirnow/raw3"
pdirnow_files <- dir(pdirnow_files, pattern = ".nc", full.names = TRUE)

lapply(pdirnow_files[-(1)][21],
       function(idx){
         
         idx_all <- rast(idx)
         create_dir <- unlist(strsplit(strsplit(idx, "_")[[1]][4], ".nc"))
         dir.create(file.path("/mnt/climstor2/vol01_ecmwf/download/pdirnow/raw4", create_dir), showWarnings = FALSE, recursive = TRUE)
         
         idx_time <- seq(as.Date(paste(create_dir, "-01-01", sep = "")), as.Date(paste(create_dir, "-12-31", sep = "")), by = "day")
         
         # parallel::mclapply(seq_along(idx_time),
         for(adx in seq_along(idx_time)){
                # function(adx){
                  
                  tosave <- idx_all[[adx]]
                  # tosave <- crop(tosave, terra::ext(-90, -30, -30, 10)) # tsa
                  tosave[tosave < 0] <- 0
                  
                  writeRaster(tosave,
                              file.path("/mnt/climstor2/vol01_ecmwf/download/pdirnow/raw4", create_dir,
                                        paste(idx_time[adx], ".tif", sep = "")),
                              overwrite = TRUE)
                  print(idx_time[adx])
                }         
                # }, mc.cores = 10)
         
         
       })

# JUST FOR 2000
lapply(pdirnow_files[1],
       function(idx){
         
         idx_all <- rast(idx)
         create_dir <- unlist(strsplit(strsplit(idx, "_")[[1]][4], ".nc"))
         dir.create(file.path("/mnt/climstor2/vol01_ecmwf/download/pdirnow/raw4", create_dir), showWarnings = FALSE, recursive = TRUE)
         
         idx_time <- seq(as.Date(paste(create_dir, "-03-01", sep = "")), as.Date(paste(create_dir, "-12-31", sep = "")), by = "day")
         
         lapply(seq_along(idx_time),
                            function(adx){
                              
                              tosave <- idx_all[[adx]]
                              # tosave <- crop(tosave, terra::ext(-90, -30, -30, 10)) # tsa
                              tosave[tosave < 0] <- 0
                              
                              writeRaster(tosave,
                                          file.path("/mnt/climstor2/vol01_ecmwf/download/pdirnow/raw4", create_dir, paste(idx_time[adx], ".tif", sep = "")),
                                          overwrite = TRUE)
                              
                            })
         
         
       })

main_tp <- "/mnt/climstor2/vol01_ecmwf/download/pdirnow/raw4"

check_dates <- lapply(
  2000:2000,
  function(adx)
  {
    
    size_date <- seq(as.Date(paste(adx, "-03-01", sep = "")), as.Date(paste(adx, "-12-31", sep = "")), by = "day")
    size_date <- paste(size_date, ".tif", sep = "")
    size_files <- dir(file.path(main_tp, adx))
    setdiff(size_date, size_files)
    
  }
)

unlist(check_dates)

