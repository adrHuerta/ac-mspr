sen_slope_trend <- function(v) {
  
  if (any(is.na(v))) return(c(slope = NA, p_value = NA))
  
  res <- trend::sens.slope(v)
  
  return(
    c(slope = as.numeric(res$estimates),
      p_value = as.numeric(res$p.value))
    )
}

trend_agreement <- function(x, x_p, y, y_p) {
  res <- ifelse(x > 0 & y > 0, 1, 
                ifelse(x < 0 & y < 0, -1, 0))
  res[is.na(x) | is.na(y)] <- NA
  
  res_p <- ifelse(x_p < 0.05 & y_p < 0.05, 1, 0)
  res_p[is.na(x_p) | is.na(x_p)] <- 0
  res_p <- ifelse(res == 1 | res == -1, res_p, 0)
  
  return(c("aggr" = as.numeric(res), "sig" = as.numeric(res_p)))
}