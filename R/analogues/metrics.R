jaccard_index <- function(setA, setB) {
  # setA, setB: character vectors of dates "YYYY-MM-DD"
  
  A <- unique(setA)
  B <- unique(setB)
  
  intersection <- length(intersect(A, B))
  union <- length(union(A, B))
  
  if (union == 0) return(NA_real_)
  intersection / union
}

median_temporal_offset <- function(target_date, analogue_dates) {
  # target_date: single character string "YYYY-MM-DD"
  # analogue_dates: character vector "YYYY-MM-DD"
  
  if (length(analogue_dates) == 0) return(NA_real_)
  
  # Convert to Date class
  target <- as.Date(target_date)
  analogues <- as.Date(analogue_dates)
  
  # Absolute difference in days (including year)
  offsets <- abs(as.numeric(analogues - target))
  
  median(offsets, na.rm = TRUE)
}

convert_offset <- function(offset_days) {
  list(
    days   = offset_days,
    months = offset_days / 30.44,
    years  = offset_days / 365.25
  )
}