rm(list = ls())
library(googledrive)

main_tp = "/mnt/climstor2/vol01_ecmwf/download/gsmap_v8_op/raw"
drive_auth()
drive_user()

for(ix in 2024:2024){

  print(ix)

  repeat{


    if (!dir.exists(file.path(main_tp, ix))) {dir.create(file.path(main_tp, ix), showWarnings = FALSE, recursive = TRUE)}

    era5land_tp_pad <- drive_ls(path = paste("gsmap", ix, sep = "-"),
                                pattern = as.character(ix),
                                q = paste("name contains ", "'", ix, "'", sep = ""))
    era5land_tp_pad <- era5land_tp_pad[order(era5land_tp_pad$name), ]
    lapply(1:nrow(era5land_tp_pad),
           function(ii){

             ii_n <- era5land_tp_pad[ii, ]
             public_file <-  drive_get(as_id(ii_n$id))
             drive_download(public_file,
                            path = file.path(main_tp, ix, ii_n$name),
                            overwrite = TRUE)

           })

    size_file_date <- seq(as.Date(paste(ix, "-01-01", sep = "")), as.Date(paste(ix, "-12-31", sep = "")), by = "day")
    if(length(size_file_date) == length(dir(file.path(main_tp, ix)))){

      sapply(era5land_tp_pad$id, function(ijx) drive_rm(as_id(ijx)))

      break

    }

  }


}


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
# NO GAP!
# MAY SOME Negatives -> any < 0 -> 0