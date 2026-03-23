## Need to be reviewed yet

# Fast IOA function using matrix operations
fast_ioa_v <- function(obsS, mod_matS) {
  lhs <- colSums(abs(sweep(mod_matS, 1, obsS)))         # LHS = |mod - obs| summed
  rhs <- 2 * sum(abs(obsS - mean(obsS, na.rm = TRUE)))  # RHS = 2 * |obs - mean(obs)|
  ioa <- ifelse(lhs <= rhs, 1 - lhs / rhs, rhs / lhs - 1)
  return(ioa)
}

# Vectorized version of your get_dr_bcc function
fast_dr_mcc_v <- function(obs, mod_mat) {
  stopifnot(length(obs) == nrow(mod_mat))
  
  # Binary classification
  obs_bin <- as.integer(obs >= 0.1)
  mod_bin <- (mod_mat >= 0.1) * 1L
  
  # IOA
  ioa_all <- fast_ioa_v(obsS = obs, mod_matS = mod_mat)
  kge2_all <- fast_kge2_v(obsS = obs, mod_matS = mod_mat)
  
  # MCC (still looped but lightweight)
  mcc_vals <- mcc_vectorized_v(obs_bin, mod_bin)
  # bcc_vals <- tryCatch(
  #   balacc_binary(obs_bin, mod_bin),
  #   error = function(e) {
  #     
  #     NA
  #     
  #   }
  # )
  # 
  bcc_vals <- balacc_vectorized_v(obs_bin, mod_bin)
  
  # Final data frame
  data.frame(
    dr = ioa_all,
    kge2 = kge2_all,
    mcc = mcc_vals,
    bcc = bcc_vals,
    row.names = NULL
  )
}

mcc_vectorized_v <- function(obs_bin, mod_bin_mat) {
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

balacc_binary_v <- function(obs, pred, pos_level = 2) {
  
  # Construct the confusion matrix
  conf_matrix <- table(pred = pred, obs = obs)
  
  # Check if matrix is binary
  if (nrow(conf_matrix) == 2 && ncol(conf_matrix) == 2) {
    
    # Extract values for binary classification
    if (pos_level == 1) {
      TP <- conf_matrix[1, 1]  # True Positive
      FN <- conf_matrix[1, 2]  # False Negative
      TN <- conf_matrix[2, 2]  # True Negative
      FP <- conf_matrix[2, 1]  # False Positive
    } else {
      TP <- conf_matrix[2, 2]  # True Positive
      FN <- conf_matrix[2, 1]  # False Negative
      TN <- conf_matrix[1, 1]  # True Negative
      FP <- conf_matrix[1, 2]  # False Positive
    }
    
    # Calculate recall (sensitivity) and specificity
    recall <- TP / (TP + FN)   # Sensitivity
    specificity <- TN / (TN + FP)  # Specificity
    
    
    # Calculate balanced accuracy
    balacc <- (recall + specificity) / 2
    
    return(balacc)
  } else {
    stop("The confusion matrix is not binary.")
  }
}

balacc_vectorized_v <- function(obs_bin, mod_bin_mat) {
  
  TP <- colSums(obs_bin == 1 & mod_bin_mat == 1)
  TN <- colSums(obs_bin == 0 & mod_bin_mat == 0)
  FP <- colSums(obs_bin == 0 & mod_bin_mat == 1)
  FN <- colSums(obs_bin == 1 & mod_bin_mat == 0)
  
  recall <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  balacc <- (recall + specificity) / 2
  
  balacc[!is.finite(balacc)] <- NA
  
  return(balacc)
}

# Fast KGE'' (Santos et al., SC-Earth)
# obsS      : numeric vector of observations (length n)
# mod_matS  : numeric matrix of simulations (n x m), each column = one model
fast_kge2_v <- function(obsS, mod_matS) {
  
  # Remove NA pairs (row-wise)
  ok <- is.finite(obsS)
  obs <- obsS[ok]
  mod <- mod_matS[ok, , drop = FALSE]
  
  # --- Observed statistics ---
  mu_obs <- mean(obs)
  sd_obs <- sd(obs)
  
  # --- Simulated statistics (vectorized over columns) ---
  mu_sim <- colMeans(mod)
  sd_sim <- apply(mod, 2, sd)
  
  # --- Components ---
  r <- apply(mod, 2, function(x) cor(x, obs, method = "spearman"))
  
  alpha <- sd_sim / sd_obs
  beta  <- (mu_sim - mu_obs) / sd_obs   # normalized bias
  
  # --- KGE'' ---
  kge2 <- 1 - sqrt((r - 1)^2 + (alpha - 1)^2 + beta^2)
  
  return(kge2)
}