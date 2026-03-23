select_pareto_dates_base <- function(dates, ria, mcc) {
  # Create data frame
  df <- data.frame(date = dates, ria = ria, mcc = mcc)
  # Order by decreasing ria, then decreasing mcc
  ord <- order(-df$ria, -df$mcc)
  df <- df[ord, ]
  
  pareto_dates <- c()
  max_mcc <- -Inf
  
  for(i in seq_len(nrow(df))) {
    if(df$mcc[i] > max_mcc) {
      pareto_dates <- c(pareto_dates, df$date[i])
      max_mcc <- df$mcc[i]
    }
  }
  
  df[df$date %in% pareto_dates, ]
}

plot_pareto_results <- function(dates, ria, mcc) {
  # Select Pareto-optimal points using base R function from before
  pareto_df <- select_pareto_dates_base(dates, ria, mcc)
  
  # Plot all points in light gray
  plot(mcc, ria, pch = 16, col = "lightgray",
       xlab = "mcc", ylab = "dr",
       main = "RIA vs MCC with Pareto-optimal points")
  
  # Highlight Pareto points in red and larger size
  points(pareto_df$mcc, pareto_df$ria, pch = 19, col = "red", cex = 1.2)
  
  
  # Add grid for easier reading
  grid()
}

plot_pareto_pairs <- function(dates, m1, m2, m3) {
  pareto_df <- select_pareto_3d_base(dates, m1, m2, m3)
  
  par(mfrow = c(1,3))
  
  plot(m1, m2, pch = 16, col = "lightgray", xlab = "Metric 1", ylab = "Metric 2", main = "Metric1 vs Metric2")
  points(pareto_df$metric1, pareto_df$metric2, col = "red", pch = 19)
  
  plot(m2, m3, pch = 16, col = "lightgray", xlab = "Metric 2", ylab = "Metric 3", main = "Metric2 vs Metric3")
  points(pareto_df$metric2, pareto_df$metric3, col = "red", pch = 19)
  
  plot(m1, m3, pch = 16, col = "lightgray", xlab = "Metric 1", ylab = "Metric 3", main = "Metric1 vs Metric3")
  points(pareto_df$metric1, pareto_df$metric3, col = "red", pch = 19)
  
  par(mfrow = c(1,1))
}

select_pareto_3d_base <- function(dates, m1, m2, m3) {
  n <- length(dates)
  is_dominated <- logical(n)  # initialize all FALSE
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j) {
        # Check if j dominates i:
        # j is >= i in all metrics AND > i in at least one metric
        if ( (m1[j] >= m1[i]) && (m2[j] >= m2[i]) && (m3[j] >= m3[i]) &&
             ( (m1[j] > m1[i]) || (m2[j] > m2[i]) || (m3[j] > m3[i]) ) ) {
          is_dominated[i] <- TRUE
          break
        }
      }
    }
  }
  
  # Return non-dominated points (Pareto front)
  data.frame(
    date = dates[!is_dominated],
    metric1 = m1[!is_dominated],
    metric2 = m2[!is_dominated],
    metric3 = m3[!is_dominated]
  )
}
