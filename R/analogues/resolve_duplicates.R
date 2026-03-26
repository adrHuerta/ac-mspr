resolve_duplicates <- function(analogues_full){

  duplicated_dates <- analogues_full[duplicated(analogues_full$date), ]$date
  
  if(length(duplicated_dates) >= 1) {

    to_eval <- analogues_full[analogues_full$date %in% duplicated_dates, ]
    to_add <- by(to_eval, to_eval$date, function(jx){
      jx <- transform(jx, mean_metric = (dr + dr_p90 + mcc)/3)
      jx[order(jx$mean_metric, decreasing = TRUE), ][1, ]
    })

    to_add <- do.call(rbind, as.list(to_add))
    to_add <- to_add[, -match("mean_metric", colnames(to_add))]
    rest_df <- analogues_full[-which(analogues_full$date %in% duplicated_dates), ]
    
    final_df <- rbind(rest_df, to_add)
    final_df <- final_df[order(final_df$sat), ]
    rownames(final_df) <- NULL


    } else {
      
      final_df <- analogues_full[order(analogues_full$sat), ]
      rownames(final_df) <- NULL

    }

  final_df
  
}