fill_na_iteratively <- function(raster_object, window_size = 3) {
  # Keep looping until there are no more NA pixels
  repeat {
    # Count the number of NA pixels before filling
    na_count_before <- sum(is.na(terra::values(raster_object)), na.rm = TRUE)
    
    # Fill missing pixels using the focal function
    raster_object[is.na(raster_object)] <- focal(raster_object, 
                                                 w = matrix(1, window_size, window_size), 
                                                 fun = mean, na.rm = TRUE)[is.na(raster_object)]
    
    # Count the number of NA pixels after filling
    na_count_after <- sum(is.na(terra::values(raster_object)), na.rm = TRUE)
    
    # If no NA pixels were filled in this iteration, exit the loop
    if (na_count_after == na_count_before) {
      break
    }
  }
  
  return(raster_object)
}