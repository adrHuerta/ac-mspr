normalize_f <- function(x){
  min_max <- minmax(x)
  min_r <- min_max[1]
  max_r <- min_max[2]
  (x - min_r) / (max_r - min_r)
}

precip_binary <- function(x, threshold = 0.1) { # threshold same as the analogue metrics!
  # x: raster of precipitation values
  # threshold: value above which precipitation counts as '1'
  
  # Create a binary raster
  binary_raster <- x >= threshold
  
  # Convert logical TRUE/FALSE to numeric 1/0
  binary_raster <- as.numeric(binary_raster)
  
  return(binary_raster)
}

get_cloud_gradient <- function(raster_data){
  
  sobel_x <- matrix(c(-1, 0, 1,
                      -2, 0, 2,
                      -1, 0, 1), nrow = 3, byrow = TRUE)
  
  sobel_y <- matrix(c(-1, -2, -1,
                      0,  0,  0,
                      1,  2,  1), nrow = 3, byrow = TRUE)
  
  dx <- focal(raster_data, w = sobel_x, fun = sum, na.policy = "omit", pad = TRUE)
  dy <- focal(raster_data, w = sobel_y, fun = sum, na.policy = "omit", pad = TRUE)
  grad_mag <- round(sqrt(dx^2 + dy^2), 2)
  
  return(grad_mag)
}