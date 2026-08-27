rm(list = ls())

library(terra)
library(ggplot2)
library(ggh4x)
library(patchwork)

########
plot_climate_trends <- function(data, plot_title = "E)", line_color = "black") {
  
  # 1. Calculate the rolling 10-year mean on the input data
  data$value_10yr <- ave(
    data$value,
    data$dataset,
    data$var,
    FUN = function(x) zoo::rollmean(x, k = 10, fill = NA, align = "center")
  )
  
  # 2. Generate the ggplot object mapping dataset to linetype
  p <- ggplot(data, aes(x = year, y = value, linetype = dataset)) +
    geom_line(
      colour = line_color,
      alpha = 0.25,
      linewidth = 0.8
    ) +
    geom_line(
      aes(y = value_10yr),
      colour = line_color,
      linewidth = 1.2
    ) +
    geom_hline(
      yintercept = 0,
      linewidth = 0.75, linetype = "dotted",
      colour = "gray50", alpha = .85
    ) +
    facet_grid(
      var ~ .,
      scales = "free_y",
      switch = "y"
    ) +
    labs(
      x = NULL,
      y = "",
      linetype = NULL
    ) +
    coord_cartesian(ylim = c(-1.25, 1.25)) +
    scale_y_continuous(expand = c(0, 0)) + 
    scale_x_continuous(
      expand = c(0, 0),
      labels = function(x) paste0("'", sub("^\\d{2}", "", as.character(x)))
    ) +
    theme_minimal() +
    theme_bw(base_size = 12) + 
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.1),
      axis.text = element_text(color = "grey25", size = 7.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.text.x = element_text(margin = margin(t = 2, r = 0, b = 0, l = 0)),
      axis.text.y = element_text(margin = margin(t = 0, r = 2, b = 0, l = 0)),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 90, size = 9, hjust = 0.5),
      strip.background = element_blank(),
      plot.title = element_text(size = 8)
    ) +
    ggtitle(plot_title)
  
  ggh4x::force_panelsizes(rows = unit(3, "cm"))
  
  return(p)
}

# plot_climate_trends <- function(data, plot_title = "E)") {
#   
#   # 1. Calculate the rolling 10-year mean on the input data
#   data$value_10yr <- ave(
#     data$value,
#     data$dataset,
#     data$var,
#     FUN = function(x) zoo::rollmean(x, k = 10, fill = NA, align = "center")
#   )
#   
#   # 2. Generate the ggplot object
#   p <- ggplot(data, aes(x = year, y = value, colour = dataset)) +
#     geom_line(
#       alpha = 0.25,
#       linewidth = 0.8
#     ) +
#     geom_line(
#       aes(y = value_10yr),
#       linewidth = 1.2
#     ) +
#     geom_hline(
#       yintercept = 0,
#       linewidth = 0.75, linetype = "dotted",
#       colour = "gray50", alpha = .85
#     ) +
#     facet_grid(
#       var ~ .,
#       scales = "free_y",
#       switch = "y"
#     ) +
#     labs(
#       x = NULL,
#       y = "",
#       colour = NULL
#     ) +
#     coord_cartesian(ylim = c(-1.25, 1.25)) +
#     scale_y_continuous(expand = c(0, 0)) + 
#     scale_x_continuous(
#       expand = c(0, 0),
#       labels = function(x) paste0("'", sub("^\\d{2}", "", as.character(x)))
#     ) +
#     theme_bw(base_size = 12) + 
#     theme(
#       legend.position = "none",
#       panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.3),
#       axis.text = element_text(color = "grey45", size = 7.5),
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       axis.ticks = element_blank(),
#       axis.ticks.length = unit(0, "pt"),
#       axis.text.x = element_text(margin = margin(t = 2, r = 0, b = 0, l = 0)),
#       axis.text.y = element_text(margin = margin(t = 0, r = 2, b = 0, l = 0)),
#       strip.placement = "outside",
#       strip.text.y.left = element_text(angle = 90, size = 9, hjust = 0.5),
#       strip.background = element_blank(),
#       plot.title = element_text(size = 9)
#     ) +
#     ggtitle(plot_title)
#   
#   ggh4x::force_panelsizes(rows = unit(3, "cm"))
#   
#   return(p)
# }


#########
# box map

ecoregions <- vect("/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp")

pr_box = c(-83, -34, -25, 15)
pr_box_1 = c(-83, -34, -25 - .5, 15 + .5)

# trend agreement 
trend_agg <- sds("output/06_prec4tsa/prec4tsa-yearly-trend-comparison.nc")
trend_agg <- rast(lapply(trend_agg, function(x) x[[2]]))
trend_agg <- sum(trend_agg)

# plot(sum(trend_agg), breaks = c(2:5))
# drawn_extent <- draw(x = "extent", col = "red", lwd = 2)

northWestern_amazon <- ext(-74.626639784684897, -61.178032904209275, -2.8810823994064987, 5.5498025602179188)
wtsa <- ext(-80.070123522020253, -66.381362947250423, -15.766019790530613, -0.57451953309415638)
casalt <- ext(-72.3338668478658, -59.1797736879245, -23.6363241792782, -12.8473777340978)
northeast_region <- ext(-47.326841168602598, -35.77722292963881, -15.371035058430722, -3.2445742904841444)
sehl <- ext(-52.896869822854633, -44.623739027568519, -26.195325542570956, -17.812604340567617)
# Number of indices with significant trend agreement

pltt1 <- 
  ggplot() +
  tidyterra::geom_spatraster(data = trim(trend_agg),
                             ) +
  scale_fill_gradientn(
    colors = c("white", "yellow", "#DE2D26", "#DE2D26"),
    na.value = NA,
    name = "",              
    breaks = c(0, 1, 2, 3, 4, 5),
    guide = guide_colorbar(barheight = unit(3, "mm"),
                           barwidth = unit(20, "mm"))
  ) +
  tidyterra::geom_spatvector(data = mask(ecoregions, terra::ext(pr_box)),
                             colour = "gray70",
                             fill = NA,
                             alpha = .5,
                             size = .4) +
  coord_sf(ylim = c(-25, 13), xlim = c(-82, -34), expand = c(0, 0)) +
  #A
  annotate(geom = "rect", 
           xmin = northWestern_amazon[1], xmax = northWestern_amazon[2], 
           ymin = northWestern_amazon[3], ymax = northWestern_amazon[4],
           fill = NA, alpha = 0.2, color = "blue") +
  annotate(geom = "text", 
           x = (sum(northWestern_amazon[1:2])) / 2,
           y = (sum(northWestern_amazon[3:4])) / 2,
           label = "A", 
           hjust = 0.5, vjust = 0.5,
           fontface = "bold", color = "black", alpha = 0.725) +
  #B
  annotate(geom = "rect", 
           xmin = wtsa[1], xmax = wtsa[2], 
           ymin = wtsa[3], ymax = wtsa[4],
           fill = NA, alpha = 0.2, color = "#009E73") +
  annotate(geom = "text", 
           x = (sum(wtsa[1:2])) / 2,
           y = (sum(wtsa[3:4])) / 2,
           label = "B", 
           hjust = 0.5, vjust = 0.5,
           fontface = "bold", color = "black", alpha = 0.725) +
  #C
  annotate(geom = "rect", 
           xmin = casalt[1], xmax = casalt[2], 
           ymin = casalt[3], ymax = casalt[4],
           fill = NA, alpha = 0.2, color = "purple") +
  annotate(geom = "text", 
           x = (sum(casalt[1:2])) / 2,
           y = (sum(casalt[3:4])) / 2,
           label = "C", 
           hjust = 0.5, vjust = 0.5,
           fontface = "bold", color = "black", alpha = 0.725) +
  #D
  annotate(geom = "rect", 
           xmin = northeast_region[1], xmax = northeast_region[2], 
           ymin = northeast_region[3], ymax = northeast_region[4],
           fill = NA, alpha = 0.2, color = "orange") +
  annotate(geom = "text", 
           x = (sum(northeast_region[1:2])) / 2,
           y = (sum(northeast_region[3:4])) / 2,
           label = "E", 
           hjust = 0.5, vjust = 0.5,
           fontface = "bold", color = "black", alpha = 0.725) +
  #E
  annotate(geom = "rect", 
           xmin = sehl[1], xmax = sehl[2], 
           ymin = sehl[3], ymax = sehl[4],
           fill = NA, alpha = 0.2, color = "red") +
  annotate(geom = "text", 
           x = (sum(sehl[1:2])) / 2,
           y = (sum(sehl[3:4])) / 2,
           label = "D", 
           hjust = 0.5, vjust = 0.5,
           fontface = "bold", color = "black", alpha = 0.725) +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification.inside = c(1, 1),
    legend.direction = "horizontal",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.box = "horizontal",
    strip.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.3),
    plot.title = element_text(size = 8)
  ) +
  ggtitle("Multi-index trend agreement (1960–2015)")

###

obs_bc <- sds("output/06_prec4tsa/obs_bc-prec4tsa-yearly.nc")
hmg_obs_bc <- sds("output/06_prec4tsa/hmg_obs_bc-prec4tsa-yearly.nc")

obs_bc <- lapply(obs_bc, function(x) {
  (x - mean(x)) / app(x, sd)
})
obs_bc <- sds(obs_bc)

hmg_obs_bc <- lapply(hmg_obs_bc, function(x) {
  (x - mean(x)) / app(x, sd)
})
hmg_obs_bc <- sds(hmg_obs_bc)

# northeast_region
df <- rbind(
  data.frame(year = 1961:2015, var = "P95", dataset = "obs_bc", value = unlist(global(crop(obs_bc$p95, northeast_region), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "P95", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$p95, northeast_region), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "CDD", dataset = "obs_bc", value = unlist(global(crop(obs_bc$cdd, northeast_region), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "CDD", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$cdd, northeast_region), fun = "mean", na.rm = TRUE)))
)
df$var <- factor(df$var, levels = c("P95", "CDD"))
df$dataset <- factor(df$dataset, levels = c("obs_bc", "hmg_obs_bc"))
Eplot <- plot_climate_trends(data = df, plot_title = "E) Northeast Brazil", line_color = "red")

# sehl
df <- rbind(
  data.frame(year = 1961:2015, var = "PRCPTOT", dataset = "obs_bc", value = unlist(global(crop(obs_bc$prcptot, sehl), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "PRCPTOT", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$prcptot, sehl), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "P95", dataset = "obs_bc", value = unlist(global(crop(obs_bc$p95, sehl), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "P95", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$p95, sehl), fun = "mean", na.rm = TRUE)))
)
df$var <- factor(df$var, levels = c("PRCPTOT", "P95"))
df$dataset <- factor(df$dataset, levels = c("obs_bc", "hmg_obs_bc"))
Dplot <- plot_climate_trends(data = df, plot_title = "D) Southeastern Brazil", line_color = "orange")

# casalt
df <- rbind(
  data.frame(year = 1961:2015, var = "PRCPTOT", dataset = "obs_bc", value = unlist(global(crop(obs_bc$prcptot, casalt), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "PRCPTOT", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$prcptot, casalt), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "P95", dataset = "obs_bc", value = unlist(global(crop(obs_bc$p95, casalt), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "P95", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$p95, casalt), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "CDD", dataset = "obs_bc", value = unlist(global(crop(obs_bc$cdd, casalt), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "CDD", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$cdd, casalt), fun = "mean", na.rm = TRUE)))
)
df$var <- factor(df$var, levels = c("PRCPTOT", "P95", "CDD"))
df$dataset <- factor(df$dataset, levels = c("obs_bc", "hmg_obs_bc"))
Cplot <- plot_climate_trends(data = df, plot_title = "C) Central Andes Altiplano", line_color = "purple")

# wtsa
df <- rbind(
  data.frame(year = 1961:2015, var = "R1mm", dataset = "obs_bc", value = unlist(global(crop(obs_bc$r1mm, wtsa), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "R1mm", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$r1mm, wtsa), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "CDD", dataset = "obs_bc", value = unlist(global(crop(obs_bc$cdd, wtsa), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "CDD", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$cdd, wtsa), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "CWD", dataset = "obs_bc", value = unlist(global(crop(obs_bc$cwd, wtsa), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "CWD", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$cwd, wtsa), fun = "mean", na.rm = TRUE)))
)
df$var <- factor(df$var, levels = c("R1mm", "CDD", "CWD"))
df$dataset <- factor(df$dataset, levels = c("obs_bc", "hmg_obs_bc"))
Bplot <- plot_climate_trends(data = df, plot_title = "B) Western Tropical South America", line_color = "#009E73")

# northWestern_amazon
df <- rbind(
  data.frame(year = 1961:2015, var = "R1mm", dataset = "obs_bc", value = unlist(global(crop(obs_bc$r1mm, northWestern_amazon), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "R1mm", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$r1mm, northWestern_amazon), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "P95", dataset = "obs_bc", value = unlist(global(crop(obs_bc$p95, northWestern_amazon), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "P95", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$p95, northWestern_amazon), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "CDD", dataset = "obs_bc", value = unlist(global(crop(obs_bc$cdd, northWestern_amazon), fun = "mean", na.rm = TRUE))),
  data.frame(year = 1961:2015, var = "CDD", dataset = "hmg_obs_bc", value = unlist(global(crop(hmg_obs_bc$cdd, northWestern_amazon), fun = "mean", na.rm = TRUE)))
)
df$var <- factor(df$var, levels = c("R1mm", "P95", "CDD"))
df$dataset <- factor(df$dataset, levels = c("obs_bc", "hmg_obs_bc"))
Aplot <- plot_climate_trends(data = df, plot_title = "A) Northwestern Amazon", line_color = "blue")

layout_design <- "
  MED
  CBA
"

final_figure <- wrap_plots(
  M = pltt1,
  E = Dplot,
  D = Eplot,
  C = Aplot,
  B = Bplot,
  A = Cplot,
  design = layout_design
)

final_figure <- final_figure + 
  plot_layout(
    widths = c(1.3, 1.3, 1.3), 
    heights = c(1.0, 1.4)
  ) & 
  theme(plot.margin = margin(t = 3, r = 3, b = 3, l = 3, unit = "pt"))

print(final_figure)

ggsave(
  "output/enhanced_paper/fig_hotspots.pdf",
  width = 10, height = 6, device = "pdf",
  dpi = 500,
  scale = .75
)
