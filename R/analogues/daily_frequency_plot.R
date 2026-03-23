# Stacked frequency plot

library(ggplot2)
library(viridis)

daily_frequency_plot <- function(df, time_col = "time", discrete_col, 
                                 palette = "turbo", to_percent = TRUE) {
  
  # Ensure the time column is Date or POSIXct
  df[[time_col]] <- as.Date(df[[time_col]])
  
  # Extract month and DOY
  df$Month <- as.integer(format(df[[time_col]], "%m"))
  df$DOY <- as.integer(format(df[[time_col]], "%j"))
  
  # Compute relative daily frequencies
  rel_list <- lapply(split(df[[discrete_col]], df$DOY), function(x) {
    tab <- table(x)
    data.frame(WTs = names(tab), Freq = as.numeric(tab)/sum(tab))
  })
  
  # Add DOY
  for(i in seq_along(rel_list)){
    rel_list[[i]]$DOY <- as.integer(names(rel_list)[i])
  }
  
  # Combine all days
  daily_freq <- do.call(rbind, rel_list)
  
  # Convert to percent if requested
  if(to_percent){
    daily_freq$Freq <- daily_freq$Freq * 100
    y_label <- "Relative daily frequency (%)"
  } else {
    y_label <- "Relative daily frequency"
  }
  
  # Month boundaries for vertical lines
  month_max <- as.numeric(tapply(df$DOY, df$Month, max))
  
  # Plot
  p <- ggplot(daily_freq, aes(x = DOY, y = Freq, fill = WTs)) +
    geom_bar(stat = "identity", position = "stack", width = 1) +
    scale_fill_viridis_d(option = palette,
                         guide = guide_legend(nrow = 1,
                                              byrow = TRUE,
                                              label.position = "top",
                                              barheight = .5)
    ) +
    geom_vline(xintercept = month_max[1:11], linetype="dashed", color="grey") + 
    scale_x_continuous(breaks = c(1, month_max), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(y = y_label, x = "Day of Year") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = -5),
      axis.title=element_text(size = 8.5)
    )
  
  return(p)
}
