rm(list = ls())
library(googledrive)

options(googledrive_quiet = TRUE)

main_tp = "/mnt/climstor2/vol01_ecmwf/download/patmosx/cld_temp_acha/raw"
drive_auth()
drive_user()

for(ix in 1998:2021){
  
  print(ix)
  
  if (!dir.exists(file.path(main_tp, ix))) {dir.create(file.path(main_tp, ix), showWarnings = FALSE, recursive = TRUE)}
  
  era5land_tp_pad <- drive_ls(path = paste("datasets/PATMOSX/new-cld_temp_acha/patmosx-cld_temp_acha", ix, sep = "-"),
                              pattern = as.character(ix),
                              q = paste("name contains ", "'", ix, "'", sep = ""))
  era5land_tp_pad <- era5land_tp_pad[order(era5land_tp_pad$name), ]
  
  size_file_date <- seq(as.Date(paste(ix, "-01-01", sep = "")), as.Date(paste(ix, "-12-31", sep = "")), by = "day")
  
  if(length(size_file_date) == length(era5land_tp_pad$name)){
    
    print(paste("OK", ix, sep = "-"))
    
  }
  
  lapply(1:nrow(era5land_tp_pad),
         function(ii){
           
           ii_n <- era5land_tp_pad[ii, ]
           public_file <-  drive_get(as_id(ii_n$id))
           drive_download(public_file,
                          path = file.path(main_tp, ix, ii_n$name),
                          overwrite = TRUE)
           
         })
  

  
}
# 
check_dates <- lapply(
  1998:2021,
  function(adx)
  {

    size_date <- seq(as.Date(paste(adx, "-01-01", sep = "")), as.Date(paste(adx, "-12-31", sep = "")), by = "day")
    size_date <- paste(size_date, ".tif", sep = "")
    size_files <- dir(file.path(main_tp, adx))
    setdiff(size_date, size_files)

  }
)

unlist(check_dates)
# # GAP in "2019-11-19.tif"
# 
