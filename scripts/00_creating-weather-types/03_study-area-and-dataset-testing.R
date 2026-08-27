rm(list = ls())

library(xts)
library(terra)
library(ggplot2)
library(grid)
library(ggtext)
library(patchwork)
# library(magick)
library(cowplot)
library(png)

pr_box = c(-83, -34, -25, 15)
pr_box_1 = c(-83, -34, -25 - .5, 15 + .5)

sc_prec4sa <- readRDS("/scratch2/ahuerta/datasets/observed_precipitation/sc-prec4sa/rds_xgb/SC-PREC4SA_qc_obs.RDS")
pr_data <- sc_prec4sa$data
pr_xyz <- sc_prec4sa$xyz
pr_xyz <- terra::vect(pr_xyz, geom = c("LON", "LAT"), crs="+proj=longlat +datum=WGS84", keepgeom = TRUE)
pr_xyz <- crop(pr_xyz, terra::ext(pr_box))
pr_data <- pr_data[, pr_xyz$ID]

ecoregions <- vect("/scratch2/ahuerta/datasets/vector/sa_eco2/sa_eco_l3_2_paper.shp")
ecoregions$nr_id <- factor(
  ecoregions$nr_id,
  levels = c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")
)


# study area

palette_OkabeIto <- c("#56B4E9", "#F0E442", "gray40", "#E69F00", "#009E73",
                      "#0072B2", "#D55E00", "#999999", "#CC79A7")

pltt1 <- 
  ggplot(data = data.frame(pr_xyz)) + 
  tidyterra::geom_spatvector(data = mask(ecoregions, terra::ext(pr_box)), aes(fill = nr_id),
                             colour = "gray50",
                             alpha = .5,
                             size = .4) +
  coord_sf(ylim = c(-25, 13), xlim = c(-82, -34), expand = c(0, 0)) +
  # geom_point(aes(x = LON, y = LAT), size = .65, alpha = .2) + 
  scale_fill_manual(values = palette_OkabeIto,
                    guide = guide_legend("",
                                         override.aes = list(color = NA),
                                         label.position = "top",
                                         order = 1,
                                         barheight = .9,
                                         barwidth = .6,
                                         nrow = 1
                    )) +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 6.25),
    legend.box.margin = margin(t = -15),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 7),
    legend.key.spacing.x = unit(0.1, "cm"), 
    legend.spacing.x = unit(0.0, "cm"), 
    legend.key.size = unit(0.2, "cm"),
    legend.key.width = unit(0.1, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    plot.background = element_blank(),
    legend.title = element_text(size = 6.5),
    legend.box = "horizontal",
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.3),
    plot.title = element_text(size = 7, hjust = 0)) +
  ggtitle("a) Study domain and ecoregions")

# wetday percentage


wetday_p <- parallel::mclapply(
  pr_data,
  function(xxx){
    sample_ts <- xxx
    sample_ts <- sample_ts[!is.na(sample_ts)]
    100 * round(length(sample_ts[sample_ts >= 0.1]) / length(sample_ts), 2)
  },
  mc.cores = 1
)

pr_xyz$wet_p <- round(as.numeric(wetday_p), 2)
pr_xyz$Wet_Day <- cut(pr_xyz$wet_p, right = TRUE,
                      breaks = c(-Inf, 5,
                                 10, 20,
                                 30, 50,
                                 70, Inf),
                      labels = c("<= 5", "5-10",
                                 "10-20", "20-30",
                                 "30-50", "50-70",
                                 ">= 70"))

pltt12 <- 
  ggplot(data = data.frame(pr_xyz)) + 
  tidyterra::geom_spatvector(data = mask(ecoregions, terra::ext(pr_box)),
                             colour = "black",
                             fill = NA,
                             size = .4) +
  coord_sf(ylim = c(-25, 13), xlim = c(-82, -34), expand = c(0, 0)) +
  geom_point(aes(x = LON, y = LAT, colour = Wet_Day), size = .5, shape = 19) + 
  scale_colour_manual(values = RColorBrewer::brewer.pal(7, name = "RdYlBu"),
                      guide = guide_legend("",
                                           label.position = "top",
                                           order = 1,
                                           nrow = 1
                      )) +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 5),
    legend.box.margin = margin(t = -15),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 7),
    legend.key.spacing.x = unit(0.1, "cm"), 
    legend.spacing.x = unit(0.0, "cm"), 
    legend.key.size = unit(0.1, "cm"),
    legend.key.width = unit(0.1, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    plot.background = element_blank(),
    legend.title = element_text(size = 6.5),
    legend.box = "horizontal",
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.3),
    plot.title = element_text(size = 7, hjust = 0)) +
  ggtitle("b) Station network and wet-day (%)")


# number of stations per time per ecoregion
time_step <- seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day")

obs_bc_s <- rep(nrow(pr_xyz), length(time_step))
hmg_obs_bc_s <- rep(nrow(pr_xyz), length(time_step))
qc_obs_s <- apply(pr_data, 1, function(x) sum(!is.na(x)))

toplt <- rbind(
  data.frame(value = obs_bc_s, time = time_step, dataset = "obs_bc"),
  data.frame(value = hmg_obs_bc_s, time = time_step, dataset = "hmg_obs_bc"),
  data.frame(value = qc_obs_s, time = time_step, dataset = "qc_obs")
)
toplt$dataset <- factor(toplt$dataset,
                        levels = c("obs_bc", "hmg_obs_bc", "qc_obs"))

pltt2 <- 
  ggplot() +
  geom_line(data = toplt,
            aes(x = time, y = value,
                lty = dataset, colour = dataset), lwd = 0.75) +
  scale_colour_manual(values = c("#F8766D", "#00BFC4", "#619CFF")) +
  annotate("text", x = as.Date("1965-01-01"), y = 6500, label = "paste(bold('obs_bc:'), ' serially complete')", 
           color = "#F8766D", size = 2.5, fontface = "bold", hjust = 0, parse = TRUE) +
  
  annotate("text", x = as.Date("1965-01-01"), y = 6100, label = "paste(bold('hmg_obs_bc:'), ' homogenized, serially complete')", 
           color = "#00BFC4", size = 2.5, fontface = "bold", hjust = 0, parse = TRUE) +
  
  annotate("text", x = as.Date("1965-01-01"), y = 5700, label = "paste(bold('qc_obs:'), ' quality-controlled')", 
           color = "#619CFF", size = 2.5, fontface = "bold", hjust = 0, parse = TRUE) +
  
  scale_x_date(breaks = "10 years", date_labels = "%Y",
               limits = c(as.Date("1960-01-01"), as.Date("2015-12-31")),
               expand = c(0, 0)) +
  xlab("") +
  ylab("Number of stations") +
  theme_bw() +
  theme(
    legend.position = "none", 
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 7),
    plot.background = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.3),
    plot.title = element_text(size = 7, hjust = 0)) +
  ggtitle("c) Temporal availability of SC-PREC4SA")

# combined
# pdf_image <- image_read_pdf("~/repos_phd/ac-mspr/test/f-01_ac-mspr-workflow_cropped_v2.pdf")
# right_panel <- ggdraw() + draw_image(pdf_image) + 
#   theme(plot.title = element_text(size = 7, hjust = 0)) +
#   ggtitle("d) AC-MSPR framework")

png_path <- "~/repos_phd/ac-mspr/test/f-01_ac-mspr-workflow_cropped_v2.png"
png_data <- png::readPNG(png_path)
# 2. Turn the raw image pixels into a graphical object (grob)
framework_grob <- grid::rasterGrob(png_data, interpolate = TRUE)

# 3. Use cowplot's canvas to draw it perfectly
right_panel <- ggdraw() + 
  annotation_custom(framework_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) + 
  theme(plot.title = element_text(size = 7, hjust = 0)) + 
  ggtitle("d) AC-MSPR framework")

left_panel <- (pltt1 + pltt12) / pltt2

final_plot <- wrap_plots(
  left_panel, 
  plot_spacer(),
  right_panel,
  nrow = 1
) + 
  plot_layout(
    widths = c(0.45, 0.01, .37)
  )

final_plot

ggsave(
  "output/enhanced_paper/fig_ac-mspr.pdf",
  width = 9, height = 5.25, device = "pdf",
  dpi = 500,
  scale = 0.75
)
