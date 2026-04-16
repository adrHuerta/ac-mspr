grid_filling <- function(target_data,
                         FUN){
  
  max_step <- max(target_data$values_data[-1], na.rm = TRUE)
  all_equal <- var(target_data$values_data[-1], na.rm = TRUE)
  
  if (max_step == "0" | all_equal == 0) {
    
    res <- FUN(values_data = target_data$values_data,
               predictors_data = target_data$predictors_data,
               covars = target_data$covars,
               set2naOR0 = "0")
    
  } else {
    
    res <- FUN(values_data = target_data$values_data,
               predictors_data = target_data$predictors_data,
               covars = target_data$covars,
               case_weights = target_data$case.weights,
               set2naOR0 = FALSE)
    
  }
  
  return(res)
  
}


# normalizeF <- function(x) {
#   q25 <- as.numeric(quantile(x, 0.25))
#   q50 <- as.numeric(quantile(x, 0.50))
#   q75 <- as.numeric(quantile(x, 0.75))
#   minc <- max(min(x) - (q50 - q25), 0)
#   maxc <- max(x) + (q75 - q50)
#   range <- maxc - minc
#   list(norm = (x - minc) / range, minc = minc, range = range)
# }

normalizeF <- function(x) {
  # Check if the data is constant
  if (length(unique(x)) == 1) {
    return(list(norm = rep(0, length(x)), minc = x[1], range = 0))
  }
  
  q25 <- as.numeric(quantile(x, 0.25))
  q50 <- as.numeric(quantile(x, 0.50))
  q75 <- as.numeric(quantile(x, 0.75))
  
  minc <- max(min(x) - (q50 - q25), 0)
  maxc <- max(x) + (q75 - q50)
  range <- maxc - minc
  
  # If the range is 0, return zero for the normalized values
  if (range == 0) {
    return(list(norm = rep(0, length(x)), minc = minc, range = range))
  }
  
  # Normalization step
  norm_x <- (x - minc) / range
  return(list(norm = norm_x, minc = minc, range = range))
}

fillData_rf_ranger <- function(values_data,
                               predictors_data,
                               covars,
                               case_weights,
                               set2naOR0 = FALSE) {
  
  # Combine input data
  full_data <- predictors_data
  full_data$val <- as.numeric(values_data)
  
  # # Check if PR_SAT exists
  # has_pr_sat <- "PR_SAT" %in% names(full_data)
  # 
  # # Normalize PR_SAT if present
  # if (has_pr_sat) {
  #   if (all(full_data$PR_SAT == 0)) {
  #     full_data$pr_sat_c <- full_data$PR_SAT
  #     full_data$pr_sat_r <- full_data$PR_SAT
  #   } else {
  #     norm_pr <- normalizeF(full_data$PR_SAT)
  #     full_data$pr_sat_c <- ifelse(full_data$PR_SAT > 0, 1, 0)
  #     full_data$pr_sat_r <- norm_pr$norm
  #   }
  # }
  
  # Split candidate and reference
  can <- full_data[1, ]
  ref <- full_data[-1, ]
  ref <- ref[complete.cases(ref), ]
  ref$PrSat <- normalizeF(ref$PrSat)$norm # local scaling for PrSat as well
  
  # Classification:
  rr_class <- ref
  rr_class$val[rr_class$val > 0] <- 1
  rr_class$val <- factor(rr_class$val, levels = c(0, 1))
  
  if (set2naOR0 == "0") {
    return(c(unique(ref$val), 0))
  }
  
  # if (has_pr_sat) covars[1] <- "pr_sat_c"
  
  f_class <- as.formula(paste("val ~", paste(covars, collapse = " + ")))
  
  if (length(unique(rr_class$val)) != 1) {
    
    set.seed(123)
    
    model_class <- ranger::ranger(
      formula = f_class,
      data = rr_class[, c("val", covars)],
      case.weights = case_weights,
      probability = TRUE,
      num.threads = 1
    )
    
    prob <- predict(model_class, can[, covars], num.threads = 1)
    prob <- data.frame(prob)
    prob <- prob[, "X1"]
    prob <- round(as.numeric(prob), 2)
    
  } else {
    
    prob <- as.numeric(levels(unique(rr_class$val)))[unique(rr_class$val)]
    
  }
  
  # Regression
  norm_val <- normalizeF(ref$val)
  rr_reg <- ref
  rr_reg$val <- norm_val$norm
  
  # if (has_pr_sat) covars[1] <- "pr_sat_r"
  
  f_reg <- as.formula(paste("val ~", paste(covars, collapse = " + ")))
  set.seed(123)
  
  model_reg <- ranger::ranger(
    formula = f_reg,
    data = rr_reg[, c("val", covars)],
    case.weights = case_weights,
    num.threads = 1,
  )
  pred_val <- predict(model_reg, data = can, num.threads = 1)$predictions
  final_pred <- round((pred_val * norm_val$range) + norm_val$minc, 2)
  
  # Error
  residuals <- rr_reg$val - predict(model_reg, data = rr_reg[, covars])$predictions
  e <- sqrt(sum(residuals^2) / (length(rr_reg$val) - length(covars)))
  final_error <- round((e * norm_val$range) + norm_val$minc, 2)
  
  # Final output
  out <- c(ifelse(prob <= 0.5, 0, final_pred), final_error)
  return(out)
}
# 
# fillData_rf_rangerNOSAT <- function(values_data,
#                                predictors_data,
#                                covars,
#                                case_weights,
#                                set2naOR0 = FALSE) {
#   
#   # Combine input data
#   full_data <- predictors_data
#   full_data$val <- as.numeric(values_data)
#   
#   # # Check if PR_SAT exists
#   # has_pr_sat <- "PR_SAT" %in% names(full_data)
#   # 
#   # # Normalize PR_SAT if present
#   # if (has_pr_sat) {
#   #   if (all(full_data$PR_SAT == 0)) {
#   #     full_data$pr_sat_c <- full_data$PR_SAT
#   #     full_data$pr_sat_r <- full_data$PR_SAT
#   #   } else {
#   #     norm_pr <- normalizeF(full_data$PR_SAT)
#   #     full_data$pr_sat_c <- ifelse(full_data$PR_SAT > 0, 1, 0)
#   #     full_data$pr_sat_r <- norm_pr$norm
#   #   }
#   # }
#   
#   # Split candidate and reference
#   can <- full_data[1, ]
#   ref <- full_data[-1, ]
#   ref <- ref[complete.cases(ref), ]
#   # ref$PrSat <- normalizeF(ref$PrSat)$norm # local scaling for PrSat as well
#   
#   # Classification:
#   rr_class <- ref
#   rr_class$val[rr_class$val > 0] <- 1
#   rr_class$val <- factor(rr_class$val, levels = c(0, 1))
#   
#   if (set2naOR0 == "0") {
#     return(c(unique(ref$val), 0))
#   }
#   
#   # if (has_pr_sat) covars[1] <- "pr_sat_c"
#   
#   f_class <- as.formula(paste("val ~", paste(covars, collapse = " + ")))
#   
#   if (length(unique(rr_class$val)) != 1) {
#     
#     set.seed(123)
#     
#     model_class <- ranger::ranger(
#       formula = f_class,
#       data = rr_class[, c("val", covars)],
#       case.weights = case_weights,
#       probability = TRUE,
#       num.threads = 1
#     )
#     
#     prob <- predict(model_class, can[, covars], num.threads = 1)
#     prob <- data.frame(prob)
#     prob <- prob[, "X1"]
#     prob <- round(as.numeric(prob), 2)
#     
#   } else {
#     
#     prob <- as.numeric(levels(unique(rr_class$val)))[unique(rr_class$val)]
#     
#   }
#   
#   # Regression
#   norm_val <- normalizeF(ref$val)
#   rr_reg <- ref
#   rr_reg$val <- norm_val$norm
#   
#   # if (has_pr_sat) covars[1] <- "pr_sat_r"
#   
#   f_reg <- as.formula(paste("val ~", paste(covars, collapse = " + ")))
#   set.seed(123)
#   
#   model_reg <- ranger::ranger(
#     formula = f_reg,
#     data = rr_reg[, c("val", covars)],
#     case.weights = case_weights,
#     num.threads = 1,
#   )
#   pred_val <- predict(model_reg, data = can, num.threads = 1)$predictions
#   final_pred <- round((pred_val * norm_val$range) + norm_val$minc, 2)
#   
#   # Error
#   residuals <- rr_reg$val - predict(model_reg, data = rr_reg[, covars])$predictions
#   e <- sqrt(sum(residuals^2) / (length(rr_reg$val) - length(covars)))
#   final_error <- round((e * norm_val$range) + norm_val$minc, 2)
#   
#   # Final output
#   out <- c(ifelse(prob <= 0.5, 0, final_pred), final_error)
#   return(out)
# }

fillData_rf_ranger_f1 <- function(values_data,
                                  predictors_data,
                                  covars,
                                  case_weights,
                                  set2naOR0 = FALSE) {
  
  # Combine input data
  full_data <- predictors_data
  full_data$val <- as.numeric(values_data)
  
  # # Check if PR_SAT exists
  # has_pr_sat <- "PR_SAT" %in% names(full_data)
  # 
  # # Normalize PR_SAT if present
  # if (has_pr_sat) {
  #   if (all(full_data$PR_SAT == 0)) {
  #     full_data$pr_sat_c <- full_data$PR_SAT
  #     full_data$pr_sat_r <- full_data$PR_SAT
  #   } else {
  #     norm_pr <- normalizeF(full_data$PR_SAT)
  #     full_data$pr_sat_c <- ifelse(full_data$PR_SAT > 0, 1, 0)
  #     full_data$pr_sat_r <- norm_pr$norm
  #   }
  # }
  
  # Split candidate and reference
  can <- full_data[1, ]
  ref <- full_data[-1, ]
  ref <- ref[complete.cases(ref), ]
  ref$PrSat <- normalizeF(ref$PrSat)$norm # local scaling for PrSat as well
  
  # Classification:
  rr_class <- ref
  rr_class$val[rr_class$val > 0] <- 1
  rr_class$val <- factor(rr_class$val, levels = c(0, 1))
  
  if (set2naOR0 == "0") {
    return(data.frame(mod = unique(ref$val),
                      err = 0,
                      clas_best = NA,
                      clas_bestP = NA,
                      reg_best = NA,
                      reg_bestP = NA))
  }
  
  # if (has_pr_sat) covars[1] <- "pr_sat_c"
  
  f_class <- as.formula(paste("val ~", paste(covars, collapse = " + ")))
  
  if (length(unique(rr_class$val)) != 1) {
    
    set.seed(123)
    
    model_class <- ranger::ranger(
      formula = f_class,
      data = rr_class[, c("val", covars)],
      case.weights = case_weights,
      probability = TRUE,
      importance = "permutation",  # or "impurity"
      num.threads = 1
    )
    
    prob <- predict(model_class, can[, covars], num.threads = 1)
    prob <- data.frame(prob)
    prob <- prob[, "X1"]
    prob <- round(as.numeric(prob), 2)
    
    ### feature importance
    importance <- model_class$variable.importance
    importance_pct <- 100 * abs(importance) / sum(abs(importance))
    clas_best <- names(importance_pct)[which.max(importance_pct)]
    clas_bestP <- round(max(importance_pct), 2)
    if(sum(importance) == 0){
      clas_best <- NA
      clas_bestP <- NA
    }

    
  } else {
    
    prob <- as.numeric(levels(unique(rr_class$val)))[unique(rr_class$val)]
    
    ### feature importance
    clas_best <- NA
    clas_bestP <- NA
  }
  
  # Regression
  norm_val <- normalizeF(ref$val)
  rr_reg <- ref
  rr_reg$val <- norm_val$norm
  
  # if (has_pr_sat) covars[1] <- "pr_sat_r"
  
  f_reg <- as.formula(paste("val ~", paste(covars, collapse = " + ")))
  set.seed(123)
  
  model_reg <- ranger::ranger(
    formula = f_reg,
    data = rr_reg[, c("val", covars)],
    case.weights = case_weights,
    importance = "permutation",  # or "impurity"
    num.threads = 1,
  )
  pred_val <- predict(model_reg, data = can, num.threads = 1)$predictions
  final_pred <- round((pred_val * norm_val$range) + norm_val$minc, 2)
  
  ### feature importance
  importance <- model_reg$variable.importance
  importance_pct <- 100 * abs(importance) / sum(abs(importance))
  reg_best <- names(importance_pct)[which.max(importance_pct)]
  reg_bestP <- round(max(importance_pct), 2)
  if(sum(importance) == 0){
    reg_best <- NA
    reg_bestP <- NA
  }
  # Error
  residuals <- rr_reg$val - predict(model_reg, data = rr_reg[, covars])$predictions
  e <- sqrt(sum(residuals^2) / (length(rr_reg$val) - length(covars)))
  final_error <- round((e * norm_val$range) + norm_val$minc, 2)
  
  # Final output
  # out <- c(ifelse(prob <= 0.5, 0, final_pred), final_error)
  out <- data.frame(mod = ifelse(prob <= 0.5, 0, final_pred),
                    err = final_error,
                    clas_best = clas_best,
                    clas_bestP = clas_bestP,
                    reg_best = reg_best,
                    reg_bestP = reg_bestP)
  return(out)
}