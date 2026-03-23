## Need to be reviewed yet

# Fast IOA function using matrix operations
fast_ioa <- function(obsS, mod_matS) {
  
  obsS <- (obsS - min(obsS, na.rm = TRUE)) / (max(obsS, na.rm = TRUE) - min(obsS, na.rm = TRUE))
  mod_matS <- apply(mod_matS, 2, function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  })
  
  lhs <- colSums(abs(sweep(mod_matS, 1, obsS)))         # LHS = |mod - obs| summed
  rhs <- 2 * sum(abs(obsS - mean(obsS, na.rm = TRUE)))  # RHS = 2 * |obs - mean(obs)|
  ioa <- ifelse(lhs <= rhs, 1 - lhs / rhs, rhs / lhs - 1)
  return(ioa)
}

# Vectorized version of your get_dr_bcc function
fast_dr_mcc <- function(obs, mod_mat) {
  stopifnot(length(obs) == nrow(mod_mat))
  
  # Binary classification
  obs_bin <- as.integer(obs >= 0.1)
  mod_bin <- (mod_mat >= 0.1) * 1L
  
  # IOA
  ioa_all <- fast_ioa(obsS = obs, mod_matS = mod_mat)
  
  # IOA on 90th percentile of obs
  p90_threshold <- quantile(obs, 0.9, na.rm = TRUE)
  idx_p90 <- obs >= p90_threshold
  ioa_p90 <- fast_ioa(obs[idx_p90], mod_mat[idx_p90, , drop = FALSE])
  
  # MCC (still looped but lightweight)
  mcc_vals <- mcc_vectorized(obs_bin, mod_bin)
  
  # Final data frame
  data.frame(
    ID = colnames(mod_mat),
    dr = ioa_all,
    dr_p90 = ioa_p90,
    mcc = mcc_vals,
    row.names = NULL
  )
}

mcc_vectorized <- function(obs_bin, mod_bin_mat) {
  TP <- colSums(obs_bin == 1 & mod_bin_mat == 1)
  TN <- colSums(obs_bin == 0 & mod_bin_mat == 0)
  FP <- colSums(obs_bin == 0 & mod_bin_mat == 1)
  FN <- colSums(obs_bin == 1 & mod_bin_mat == 0)
  
  numerator <- (TP * TN) - (FP * FN)
  denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  # Avoid division by zero
  mcc <- ifelse(denominator == 0, NA, numerator / denominator)
  
  # Optional: if all values identical, MCC = 1
  mcc[apply(mod_bin_mat, 2, function(col) all(col == obs_bin))] <- 1
  
  return(mcc)
}