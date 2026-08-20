rm(list = ls())

library(terra)
library(ggplot2)

#########

my_breaks <- function(limits) {
  # Define your ideal sequence spanning your entire dataset's range
  master_seq <- seq(-.1, .1, by = .2) 
  
  # Keep only the breaks that fall within the current panel's limits
  panel_breaks <- master_seq[master_seq >= limits[1] & master_seq <= limits[2]]
  
  return(panel_breaks)
}

calculate_sig_percentages <- function(r, mag_lyr = 1, p_lyr = 2, p_threshold = 0.05) {
  # 1. Extract the specific layers
  magnitude <- r[[mag_lyr]]
  p_value   <- r[[p_lyr]]
  
  # 2. Get total number of valid (non-NA) pixels from the magnitude layer
  total_pixels <- terra::global(magnitude, "notNA")$notNA
  
  # 3. Create boolean rasters for significant positive and negative pixels
  sig_pos <- (magnitude > 0) & (p_value < p_threshold)
  sig_neg <- (magnitude < 0) & (p_value < p_threshold)
  
  # 4. Count the TRUE (1) pixels
  count_pos <- terra::global(sig_pos, "sum", na.rm = TRUE)$sum
  count_neg <- terra::global(sig_neg, "sum", na.rm = TRUE)$sum
  
  # 5. Calculate percentages
  pct_pos <- (count_pos / total_pixels) * 100
  pct_neg <- (count_neg / total_pixels) * 100
  
  # Return results as a clean vector
  return(data.frame(
    "pos" = round(pct_pos, 1),
    "neg" = round(pct_neg, 1)
  ))
}

#########

ecoregions <- vect("/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp")

obs_bc <- sds("output/06_prec4tsa/obs_bc-prec4tsa-yearly.nc")
hmg_obs_bc <- sds("output/06_prec4tsa/hmg_obs_bc-prec4tsa-yearly.nc")

obs_bc_trends <- sds("output/06_prec4tsa/obs_bc-prec4tsa-yearly-trend.nc")
hmg_obs_bc_trends <- sds("output/06_prec4tsa/hmg_obs_bc-prec4tsa-yearly-trend.nc")

#########

obs_bc <- lapply(obs_bc, function(x) {
  (x - mean(x)) / app(x, sd)
})
obs_bc <- sds(obs_bc)

hmg_obs_bc <- lapply(hmg_obs_bc, function(x) {
  (x - mean(x)) / app(x, sd)
})
hmg_obs_bc <- sds(hmg_obs_bc)

main_ecr <- c("NAS", "PAD", "CAS", "AOL", "EHL", "GCH")
ext_ind <- c("prcptot", "r1mm", "p95", "cdd", "cwd")

#################
# extreme indices 

ext_indices_df <- vector("list", 6)
names(ext_indices_df) <- main_ecr

for(ecr in main_ecr)
  {
  
  ext_indices_obs_bc <- lapply(
    ext_ind,
    function(x){
      
      data.frame(year = 1961:2015,
                 var = x,
                 ecoregion = ecr,
                 dataset = "obs_bc",
                 value = unlist(
                   global(mask(obs_bc[[x]],
                               ecoregions[ecoregions$nr_id %in% ecr,]),
                          fun = "mean",
                          na.rm = TRUE)
                   )
                 )
      
    }
  )
  
  ext_indices_hmg_obs_bc <- lapply(
    ext_ind,
    function(x){
      
      data.frame(year = 1961:2015,
                 var = x,
                 ecoregion = ecr,
                 dataset = "hmg_obs_bc",
                 value = unlist(
                   global(mask(hmg_obs_bc[[x]],
                               ecoregions[ecoregions$nr_id %in% ecr,]),
                          fun = "mean",
                          na.rm = TRUE)
                 )
      )
      
    }
  )
  
  ext_indices_obs_bc <- do.call(rbind, ext_indices_obs_bc)
  ext_indices_hmg_obs_bc <- do.call(rbind, ext_indices_hmg_obs_bc)
  ext_indices <- rbind(ext_indices_obs_bc, ext_indices_hmg_obs_bc)
  
  ext_indices_df[[ecr]] <- ext_indices
  
}

ext_indices_df <- do.call(rbind, ext_indices_df)
rownames(ext_indices_df) <- NULL

ext_indices_df$var <- factor(ext_indices_df$var,
                             levels = ext_ind, labels = c("PRCPTOT", "R1mm", "P95", "CDD", "CWD"))
ext_indices_df$ecoregion <- factor(ext_indices_df$ecoregion,
                                   levels = main_ecr)
ext_indices_df$dataset <- factor(ext_indices_df$dataset,
                                 levels = c("obs_bc", "hmg_obs_bc"))

#####################################
# extreme indices significant percent

ext_indices_SIG_df <- vector("list", 6)
names(ext_indices_SIG_df) <- main_ecr
for(ecr in main_ecr){
  
  ext_indices_T_obs_bc <- lapply(
    ext_ind,
    function(x){
      
      ecr_masked <- mask(obs_bc_trends[[x]], 
                         ecoregions[ecoregions$nr_id %in% ecr,])
      pos_neg <- calculate_sig_percentages(ecr_masked)
      
      data.frame(var = x,
                 ecoregion = ecr,
                 dataset = "obs_bc",
                 pos_neg
                 )
      
    }
  )
  
  ext_indices_T_hmg_obs_bc <- lapply(
    ext_ind,
    function(x){
      
      ecr_masked <- mask(hmg_obs_bc_trends[[x]], 
                         ecoregions[ecoregions$nr_id %in% ecr,])
      pos_neg <- calculate_sig_percentages(ecr_masked)
      
      data.frame(var = x,
                 ecoregion = ecr,
                 dataset = "hmg_obs_bc",
                 pos_neg
      )
      
    }
  )
  
  ext_indices_T_obs_bc <- do.call(rbind, ext_indices_T_obs_bc)
  ext_indices_T_hmg_obs_bc <- do.call(rbind, ext_indices_T_hmg_obs_bc)
  ext_indices_T <- rbind(ext_indices_T_obs_bc, ext_indices_T_hmg_obs_bc)
  
  ext_indices_SIG_df[[ecr]] <- ext_indices_T
  
}

ext_indices_SIG_df <- do.call(rbind, ext_indices_SIG_df)
rownames(ext_indices_SIG_df) <- NULL
merged_df <- aggregate(cbind(pos, neg) ~ ecoregion + var, data = ext_indices_SIG_df, FUN = mean, na.rm = TRUE)

merged_df$var <- factor(merged_df$var,
                        levels = ext_ind, labels = c("PRCPTOT", "R1mm", "P95", "CDD", "CWD"))
merged_df$ecoregion <- factor(merged_df$ecoregion,
                              levels = main_ecr)

#############
# all data

plot_df <- ext_indices_df[ext_indices_df$dataset == "hmg_obs_bc", ]
plot_df <- plot_df[order(plot_df$ecoregion, plot_df$var, plot_df$year), ]

split_df <- split(plot_df, list(plot_df$ecoregion, plot_df$var), drop = TRUE)
split_df <- lapply(split_df, function(sub_df) {
  sub_df$moving_avg <- as.numeric(stats::filter(sub_df$value, filter = rep(1/10, 10), sides = 2))
  return(sub_df)
})

plot_df <- do.call(rbind, split_df)

column_colors <- c(
  "PRCPTOT.pos" = "royalblue3",  "PRCPTOT.neg" = "firebrick3",
  "R1mm.pos"    = "royalblue3",  "R1mm.neg"    = "firebrick3",
  "P95.pos"     = "royalblue3",  "P95.neg"     = "firebrick3",
  "CWD.pos"     = "royalblue3",  "CWD.neg"     = "firebrick3",
  "CDD.pos"     = "firebrick3", "CDD.neg"     = "royalblue3"
)

ggplot(data = plot_df, aes(x = year)) +
  
  geom_ribbon(aes(ymin = 0, ymax = pmax(moving_avg, 0), fill = interaction(var, "pos")), alpha = 0.5, na.rm = TRUE) +
  geom_ribbon(aes(ymin = pmin(moving_avg, 0), ymax = 0, fill = interaction(var, "neg")), alpha = 0.5, na.rm = TRUE) +
  
  geom_line(aes(y = moving_avg), color = "black", linewidth = .5, na.rm = TRUE) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.5) +
  
  geom_text(
    data = merged_df,
    aes(x = -Inf, y = Inf, label = paste0("P = ", sprintf("%.2f", pos), "% / N = ", sprintf("%.2f", neg), "%")),
    color = "grey20", fontface = "bold", size = 2.5, alpha = 0.7,
    hjust = -0.05, vjust = 1.4, inherit.aes = FALSE
  ) +

  facet_grid(ecoregion ~ var, scale = "free", switch = "y") +
  scale_fill_manual(values = column_colors, guide = "none") + 
  scale_y_continuous(breaks = my_breaks, expand = c(0.1, 0.1)) + 
  scale_x_continuous(expand = c(0, 0),
                     labels = function(x) paste0("'", sub("^\\d{2}", "", as.character(x)))) +
  theme_bw() +
  theme(
    panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.3),
    axis.text = element_text(color = "grey45", size = 7.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.text.x = element_text(margin = margin(t = 2, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(margin = margin(t = 0, r = 2, b = 0, l = 0)),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    strip.background = element_blank() 
  ) +
  xlab("") + ylab("")

ggsave(
  "output/enhanced_paper/fig_ecr_mean.pdf",
  width = 8.45, height = 7, device = "pdf",
  dpi = 500,
  scale = .85
)
