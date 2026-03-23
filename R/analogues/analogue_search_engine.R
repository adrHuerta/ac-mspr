source("R/analogues/dr_drp90_mcc.R")
source("R/analogues/pareto_selection.R")

analogue_search_engine <- function(date2search,
                                   pr_xyz,
                                   pr_data,
                                   pr_box = c(-83, -34, -25, 15),
                                   wts_data,
                                   pr_sat_dir,
                                   pr_sat_per = c(as.Date("1998-01-01"), as.Date("2021-12-31")),
                                   days_param = 60,
                                   prob_param = 0.95)
{
  
  
  ### 1. getting potential analogue (global) dates
  
  # get the pool of wt for the target up to 95% prob
  wts_prob <- wts_data[match(date2search, wts_data$time), ]
  wts_prob <- unlist(wts_prob[, paste("Prob_WT", 1:9, sep = "")])
  wts_prob <- get_count_WTs(wts_prob, threshold = prob_param)$WTs_names
  
  # get all the dates that belong to the wt pool
  wts_dates_all <- wts_data[which(wts_data$MetaWT %in% wts_prob), "time"]
  # get pool only for days up to days_param and pr_sat_per
  wts_dates <- format(seq(date2search - days_param, date2search + days_param, freq = "days"), "%m-%d")  
  wts_dates <- (format(wts_dates_all, "%m-%d") %in% wts_dates) & 
    (wts_dates_all >= pr_sat_per[1]) &
    (wts_dates_all <= pr_sat_per[2])
  # merge both to get potential pool
  wts_dates_restricted <- wts_dates_all[wts_dates] 
  
  
  ### 2. getting pr obs data
  
  pr_xyz <- terra::vect(pr_xyz, geom = c("LON", "LAT"), crs="+proj=longlat +datum=WGS84", keepgeom = TRUE)
  
  if(is.null(pr_box)) {
    
    pr_xyz <- pr_xyz
    
  } else {
    
    pr_xyz <- crop(pr_xyz, terra::ext(pr_box))
  }
  
  pr_data <- pr_data[date2search, pr_xyz$ID]
  
  # omit NA pr values 
  pr_xyz$pr <- as.numeric(pr_data)
  pr_xyz <- na.omit(pr_xyz, field = "pr", geom = TRUE)
  
  
  ### 3. getting pr sat data
  
  # this format may change based on how the sat is downloaded
  pr_sat_data <- file.path(pr_sat_dir,
                           format(wts_dates_restricted, "%Y"),
                           paste(wts_dates_restricted, ".tif", sep = ""))
  
  pr_sat_data <- terra::rast(pr_sat_data)
  
  # extracting pr sat data
  getting_cell <- terra::extract(pr_sat_data[[1]], pr_xyz, cells = TRUE)[, 3]
  pr_sat_data <- pr_sat_data[getting_cell]
  colnames(pr_sat_data) <- paste("X-", wts_dates_restricted, sep = "")
  
  ### 4. computing metrics and pareto frontier
  
  pr_vs_pr_sat_metrics <- fast_dr_mcc(obs = pr_xyz$pr, mod_mat = as.matrix(pr_sat_data))
  # ommiting some NAs that may appear in the computation
  pr_vs_pr_sat_metrics <- pr_vs_pr_sat_metrics[complete.cases(pr_vs_pr_sat_metrics), ]
  
  best_analogues <- select_pareto_3d_base(dates = pr_vs_pr_sat_metrics$ID,
                                          m1 = pr_vs_pr_sat_metrics$dr,
                                          m2 = pr_vs_pr_sat_metrics$dr_p90,
                                          m3 = pr_vs_pr_sat_metrics$mcc)
  ### 5. best analogues
  
  best_analogues$date <- gsub("X-", "", best_analogues$date)
  colnames(best_analogues) <- c("date", "dr", "dr_p90", "mcc")
  rownames(best_analogues) <- NULL
  
  return(best_analogues)
  
}


get_count_WTs <- function(probs, threshold = 0.95) {
  probs_sorted <- sort(probs, decreasing = TRUE)
  cumsum_probs <- cumsum(probs_sorted)
  size_wts <- sum(cumsum_probs <= threshold) + 1
  WTs_names <- names(cumsum_probs[1:size_wts])
  WTs_probs <- probs_sorted
  
  return(list(WTs_names = WTs_names, WTs_probs = WTs_probs))
}
