epanechnikov_weights_3D <- function(dists_2D, dz, alpha = 1, bw = NULL) {
  # alpha: scaling factor for altitude (to make dz comparable to horizontal distances)
  # bw: 3D bandwidth (optional)
  
  # combine 2D distance + scaled vertical difference
  d3 <- sqrt(dists_2D^2 + (alpha * dz)^2)
  
  # bandwidth default: median of positive 3D distances
  if (is.null(bw)) {
    bw <- median(d3[d3 > 0], na.rm = TRUE)
    if (is.na(bw) || bw == 0) bw <- 1
  }
  
  # Epanechnikov weights
  w <- numeric(length(d3))
  inside <- d3 <= bw
  w[inside] <- 0.75 * (1 - (d3[inside] / bw)^2)
  
  # Weights outside bandwidth remain zero
  # Normalize weights so sum = 1
  # if(sum(w) > 0) {
  #   w <- w / sum(w)
  # }
  # 
  return(w)
}

compute_alpha <- function(dists_2D, dz) {
  # Only positive dz
  median_d2D <- median(dists_2D[dists_2D > 0], na.rm = TRUE)
  median_dz <- median(abs(dz), na.rm = TRUE)
  
  if (median_dz == 0) return(1)   # fallback if flat region
  alpha <- median_d2D / median_dz
  return(alpha)
}

Gaussian_weights_3D <- function(dists_2D, dz, alpha = 1, bw = NULL) {
  # alpha: scaling factor for altitude (to make dz comparable to horizontal distances)
  # bw: 3D bandwidth (optional)
  
  # combine 2D distance + scaled vertical difference
  d3 <- sqrt(dists_2D^2 + (alpha * dz)^2)
  
  # bandwidth default: median of positive 3D distances
  if (is.null(bw)) {
    bw <- median(d3[d3 > 0], na.rm = TRUE)
    if (is.na(bw) || bw == 0) bw <- 1
  }
  
  # Gaussian weights
  w <- numeric(length(d3))
  inside <- d3 <= 3 * bw
  w[inside] <- (1 / sqrt(2 * pi)) * exp(-0.5 * (d3[inside] / bw)^2)
  
  # Weights outside bandwidth remain zero
  # Normalize weights so sum = 1
  # if(sum(w) > 0) {
  #   w <- w / sum(w)
  # }
  # 
  return(w)
}