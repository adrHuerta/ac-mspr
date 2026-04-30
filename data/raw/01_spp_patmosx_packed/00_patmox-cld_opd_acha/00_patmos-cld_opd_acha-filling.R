# it was noticed that for cld_opd_acha, there was one date that was empty: 2019-11-19
# writing files can only be done on climcal3
rm(list = ls())

library(terra)
library(zoo)

expected_dates <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = "day")

# Step 1: Load rasters and assign dates
raster_files <- list.files("/mnt/climstor2/vol01_ecmwf/download/patmosx/cld_opd_acha/raw/2019", pattern = "\\.tif$", full.names = TRUE)
raster_files <- raster_files[order(raster_files)]  # sort chronologically

# Extract dates from filenames (assuming 'terra_YYYYMMDD.tif')
dates <- as.Date(unlist(strsplit(sapply(strsplit(raster_files, "/"), function(x) x[10]), ".tif")))

# Missing dates
dates_missing <- setdiff(expected_dates, dates)
dates_missing <- as.character(dates_missing)
# ---- Step 4: Use a template raster for dimensions and metadata ----
template_file <- raster_files[1]
template_raster <- rast(template_file)

# ---- Step 5: Create placeholder rasters for missing dates ----
for (date_str in dates_missing) {
  out_path <- file.path("output_2019_cld_opd_acha/", paste(date_str, ".tif", sep = ""))
  placeholder <- setValues(template_raster, NA) 
  writeRaster(placeholder, out_path, overwrite = TRUE)
  cat("Created placeholder:", out_path, "\n")
}

# Now within climcal3 move the files
# cp -a output_2019_cld_opd_acha/. /mnt/climstor2/vol01_ecmwf/download/patmosx/cld_opd_acha/raw/2019/

#I can complete the empty values, I think I can test if works well or there is no difference using -999 approach
#but just for easy processing I will complete the gap-day without care about the gaps of the previous days!
#the gap-filling will be using the raw values not the transformed (without scale + offset)

rm(list = ls())

library(terra)
library(zoo)

raster_files <- list.files("/mnt/climstor2/vol01_ecmwf/download/patmosx/cld_opd_acha/raw/2019", pattern = "\\.tif$", full.names = TRUE)
length(raster_files)
raster_files <- raster_files[order(raster_files)]  

r_stack <- rast(raster_files)
names(r_stack) <- as.Date(unlist(strsplit(sapply(strsplit(raster_files, "/"), function(x) x[10]), ".tif")))

# ---- Step 2: Convert to a 3D array for easier time-wise access ----
arr <- as.array(r_stack)  # dims: [rows, cols, time]

# ---- Step 3: Apply linear interpolation for each pixel time series ----
fill_time_series <- function(x) {
  if (all(is.na(x))) return(x)  # leave all-NA series untouched
  approx(seq_along(x), x, xout = seq_along(x), rule = 2)$y
}

filled_arr <- apply(arr, c(1, 2), fill_time_series)
filled_r <- rast(aperm(filled_arr, c(2, 3, 1)), crs = crs(r_stack))
ext(filled_r) <- ext(r_stack)


plot(filled_r[[1]])
plot(r_stack[[1]])

getRealNAs <- filled_r[[1]] - r_stack[[1]]
getRealNAs[is.na(getRealNAs)] <- 1
getRealNAs[getRealNAs == 0] <- NA
plot(getRealNAs*filled_r[[1]])

dates_with_full_NA <- c("2019-11-19")
dates_with_full_NA_index <- match(dates_with_full_NA, names(r_stack))

plot(filled_r[[c( dates_with_full_NA_index - c(2,1), dates_with_full_NA_index, dates_with_full_NA_index + 1:2)]])
plot(r_stack[[c( dates_with_full_NA_index - c(2,1), dates_with_full_NA_index, dates_with_full_NA_index + 1:2)]])

# same values preserved!
max_poss <- 127
min_poss <- -127


for (index_date in 1:1) {
  
  date_str <- dates_with_full_NA[index_date]
  out_path <- file.path("output_2019_cld_opd_acha", paste(date_str, ".tif", sep = ""))
  placeholder <- filled_r[[dates_with_full_NA_index[index_date]]]
  placeholder <- round(placeholder, 2)
  placeholder[placeholder > max_poss] <- max_poss
  placeholder[placeholder < min_poss] <- min_poss
  plot(placeholder)
  writeRaster(placeholder, out_path, overwrite = TRUE)
  cat("Created placeholder:", out_path, "\n")
}

# Now within climcal3 move the files
# cp -a output_2019_cld_opd_acha/. /mnt/climstor2/vol01_ecmwf/download/patmosx/cld_opd_acha/raw/2019/
