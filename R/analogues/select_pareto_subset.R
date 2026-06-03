
# used to subset the parefo frontier output for operational purposes and sensitivity anaylsis
##  operational subset:
# the subset retained the closest analogues to the ideal point in normalized metric space
# together with the best-performing specialist analogue for each individual metric (dr, dr_p90 and mcc)
# it also create a temporal filtering that reduces overrepresentation of persistent events, i.e.,
# does not retain another analogue within ±3 days of an already selected analogue of the balanced analogues
# default parameters: 3 balanced analogues + 3 specialist analogues = max 6
##  sensitivity subset:
# best, mid, worst analogues from the balanced (scaled) metric


# select_pareto_subset <- function(analogue_df,
#                                  n_extra = 2,
#                                  temporal_buffer = 3)
# {
#   #min-max scaling to [0,1]
#   scale01 <- function(x)
#   {
#     xmin <- min(x, na.rm = TRUE)
#     xmax <- max(x, na.rm = TRUE)
    
#     if(xmax == xmin) {
#       return(rep(1, length(x)))
#     }
    
#     return((x - xmin) / (xmax - xmin))
#   }
  
#   #Ensure date format
#   analogue_df$date <- as.Date(analogue_df$date)
  
#   #Scale metrics
#   analogue_df$dr_s     <- scale01(analogue_df$dr)
#   analogue_df$dr_p90_s <- scale01(analogue_df$dr_p90)
#   analogue_df$mcc_s    <- scale01(analogue_df$mcc)
  
#   #Balanced score (optional) more reliable that simple averaging
#   analogue_df$balanced_score <- rowMeans(
#     analogue_df[, c("dr_s", "dr_p90_s", "mcc_s")],
#     na.rm = TRUE
#   )
  
#   #Distance to ideal point (1,1,1) knee approach!
#   analogue_df$ideal_distance <- sqrt(
#     (1 - analogue_df$dr_s)^2 +
#       (1 - analogue_df$dr_p90_s)^2 +
#       (1 - analogue_df$mcc_s)^2
#   )
  
#   #Core representative analogues

#   #Best balanced analogue
#   idx_balanced <- which.min(analogue_df$ideal_distance)
  
#   #Specialist analogues
#   idx_dr       <- which.max(analogue_df$dr)
#   idx_dr_p90   <- which.max(analogue_df$dr_p90)
#   idx_mcc      <- which.max(analogue_df$mcc)
  
#   selected_idx <- unique(c(
#     idx_balanced,
#     idx_dr,
#     idx_dr_p90,
#     idx_mcc
#   ))
  
#   #Roles
#   roles <- c(
#     balanced     = idx_balanced,
#     best_dr      = idx_dr,
#     best_dr_p90  = idx_dr_p90,
#     best_mcc     = idx_mcc
#   )
  
#   #Create selected dataframe
#   selected <- analogue_df[selected_idx, ]
#   selected$role <- ""
  
#   for(i in seq_along(roles))
#   {
#     idx <- roles[i]

#     row_match <- which(
#       selected$date == analogue_df$date[idx] &
#       selected$sat  == analogue_df$sat[idx]
#     )

#     selected$role[row_match] <- ifelse(
#       selected$role[row_match] == "",
#       names(roles)[i],
#       paste(selected$role[row_match], names(roles)[i], sep = "_")
#     )
#   }
    
#   #Add extra balanced analogues
  
#   if(n_extra > 0)
#   {
#     #Order by best balanced score
#     remaining_idx <- setdiff(
#       order(analogue_df$ideal_distance),
#       selected_idx
#     )
    
#     selected_dates <- selected$date
    
#     extras_added <- 0
    
#     for(idx in remaining_idx)
#     {
#       candidate_date <- analogue_df$date[idx]
      
#       keep_candidate <- TRUE
      
#       #Temporal filtering that reduces overrepresentation of persistent events
#       if(temporal_buffer > 0)
#       {
#         date_diff <- abs(as.numeric(candidate_date - selected_dates))
        
#         if(any(date_diff <= temporal_buffer)) {
#           keep_candidate <- FALSE
#         }
#       }
      
#       if(keep_candidate)
#       {
#         extra_row <- analogue_df[idx, ]
#         extra_row$role <- paste0(
#           "extra_balanced_",
#           extras_added + 1
#         )
        
#         selected <- rbind(selected, extra_row)
        
#         selected_dates <- c(selected_dates, candidate_date)
        
#         extras_added <- extras_added + 1
#       }
      
#       if(extras_added >= n_extra) break
#     }
#   }
  
#   #Final ordering
#   selected <- selected[
#     order(selected$ideal_distance),
#   ]
  
#   rownames(selected) <- NULL
#   selected <- selected[order(selected$role), ]
#   return(selected)
  
# }

select_pareto_subset <- function(analogue_df,
                                 n_extra = 2,
                                 temporal_buffer = 3)
{
  scale01 <- function(x)
  {
    xmin <- min(x, na.rm = TRUE)
    xmax <- max(x, na.rm = TRUE)

    if(xmax == xmin) return(rep(1, length(x)))

    (x - xmin) / (xmax - xmin)
  }

  analogue_df$date <- as.Date(analogue_df$date)

  # Scaled metrics
  analogue_df$dr_s     <- scale01(analogue_df$dr)
  analogue_df$dr_p90_s <- scale01(analogue_df$dr_p90)
  analogue_df$mcc_s    <- scale01(analogue_df$mcc)

  # Balanced score and ideal distance
  analogue_df$balanced_score <- rowMeans(
    analogue_df[, c("dr_s", "dr_p90_s", "mcc_s")],
    na.rm = TRUE
  )

  analogue_df$ideal_distance <- sqrt(
    (1 - analogue_df$dr_s)^2 +
    (1 - analogue_df$dr_p90_s)^2 +
    (1 - analogue_df$mcc_s)^2
  )

  # 1) Operational subset

  idx_balanced <- which.min(analogue_df$ideal_distance)
  idx_dr       <- which.max(analogue_df$dr)
  idx_dr_p90   <- which.max(analogue_df$dr_p90)
  idx_mcc      <- which.max(analogue_df$mcc)

  selected_idx <- unique(c(
    idx_balanced,
    idx_dr,
    idx_dr_p90,
    idx_mcc
  ))

  roles <- c(
    balanced    = idx_balanced,
    best_dr     = idx_dr,
    best_dr_p90 = idx_dr_p90,
    best_mcc    = idx_mcc
  )

  operational_subset <- analogue_df[selected_idx, ]
  operational_subset$role <- ""

  for(i in seq_along(roles))
  {
    idx <- roles[i]

    row_match <- which(
      operational_subset$date == analogue_df$date[idx] &
      operational_subset$sat  == analogue_df$sat[idx]
    )

    operational_subset$role[row_match] <- ifelse(
      operational_subset$role[row_match] == "",
      names(roles)[i],
      paste(operational_subset$role[row_match],
            names(roles)[i],
            sep = "__")
    )
  }

  # Add extra balanced analogues
  if(n_extra > 0)
  {
    remaining_idx <- setdiff(
      order(analogue_df$ideal_distance),
      selected_idx
    )

    selected_dates <- operational_subset$date
    extras_added <- 0

    for(idx in remaining_idx)
    {
      candidate_date <- analogue_df$date[idx]
      keep_candidate <- TRUE

      if(temporal_buffer > 0)
      {
        date_diff <- abs(as.numeric(candidate_date - selected_dates))

        if(any(date_diff <= temporal_buffer)) {
          keep_candidate <- FALSE
        }
      }

      if(keep_candidate)
      {
        extra_row <- analogue_df[idx, ]
        extra_row$role <- paste0("extra_balanced_", extras_added + 1)

        operational_subset <- rbind(operational_subset, extra_row)

        selected_dates <- c(selected_dates, candidate_date)
        extras_added <- extras_added + 1
      }

      if(extras_added >= n_extra) break
    }
  }

  operational_subset <- operational_subset[order(operational_subset$role), ]

  rownames(operational_subset) <- NULL

  # 2) Sensitivity subset: best, mid, worst

  ord <- order(analogue_df$ideal_distance)

  idx_best  <- ord[1]
  idx_mid   <- ord[ceiling(length(ord) / 2)]
  idx_worst <- ord[length(ord)]

  sensitivity_subset <- analogue_df[c(idx_best, idx_mid, idx_worst), ]
  sensitivity_subset$role <- c(
    "best_balanced",
    "mid_balanced",
    "worst_balanced"
  )

  rownames(sensitivity_subset) <- NULL
  rownames(analogue_df) <- NULL

  return(list(
    operational_subset = operational_subset,
    sensitivity_subset = sensitivity_subset,
    full_frontier = analogue_df
  ))

}