rm(list = ls())

library(xts)
library(terra)
library(ggplot2)
library(patchwork)
source("R/analogues/metrics.R")

cv_days <- read.csv("output/02_ENSO-cv-sample-days/CV_days.csv")

obs_bc_anlg <- lapply(
  dir("output/03_precipitation-pattern-analogue/cv/cv-obs_bc", full.names = TRUE),
  function(x){
    
    rsss <- readRDS(x)
    rsss$target <- as.character(unlist(strsplit(strsplit(x, "/")[[1]][4], ".RDS")))
    rsss <- rsss[c("date", "dr", "dr_p90", "mcc", "target", "sat")]
    rsss
    
  }
)

hmg_obs_bc_anlg <- lapply(
  dir("output/03_precipitation-pattern-analogue/cv/cv-hmg_obs_bc", full.names = TRUE),
  function(x){
    
    rsss <- readRDS(x)
    rsss$target <- as.character(unlist(strsplit(strsplit(x, "/")[[1]][4], ".RDS")))
    rsss <- rsss[c("date", "dr", "dr_p90", "mcc", "target", "sat")]
    rsss
    
  }
)

qc_obs_anlg <- lapply(
  dir("output/03_precipitation-pattern-analogue/cv/cv-qc_obs", full.names = TRUE),
  function(x){
    
    rsss <- readRDS(x)
    rsss$target <- as.character(unlist(strsplit(strsplit(x, "/")[[1]][4], ".RDS")))
    rsss <- rsss[c("date", "dr", "dr_p90", "mcc", "target", "sat")]
    rsss
    
  }
)

# jaccard

jaccard_per_day <- rbind(
  data.frame(
    jaccard = sapply(
      seq_along(obs_bc_anlg), function(i) {
        jaccard_index(obs_bc_anlg[[i]]$date, hmg_obs_bc_anlg[[i]]$date)
        }),
    dataset = "obs_bc|hmg_obs_bc",
    enso = cv_days$type),
  
  data.frame(
    jaccard = sapply(
      seq_along(obs_bc_anlg), function(i) {
        jaccard_index(hmg_obs_bc_anlg[[i]]$date, qc_obs_anlg[[i]]$date)
      }),
    dataset = "hmg_obs_bc|qc_obs",
    enso = cv_days$type),
  
  data.frame(
    jaccard = sapply(
      seq_along(obs_bc_anlg), function(i) {
        jaccard_index(qc_obs_anlg[[i]]$date, obs_bc_anlg[[i]]$date)
      }),
    dataset = "qc_obs|obs_bc",
    enso = cv_days$type)
)

jaccard_per_day$dataset <- factor(jaccard_per_day$dataset, levels = c("obs_bc|hmg_obs_bc",
                                                                      "hmg_obs_bc|qc_obs",
                                                                      "qc_obs|obs_bc"))
# jaccard_per_day$enso <- factor(jaccard_per_day$enso)
jaccard_per_day$enso <- factor(jaccard_per_day$enso,
                            levels = c("C_Neutral", "C_Nina", "C_Nino", "E_Neutral", "E_Nina", "E_Nino"),
                            labels = c("C_Neutral", "C_Niña", "C_Niño", "E_Neutral", "E_Niña", "E_Niño"))
plt1 <- 
  ggplot(data = jaccard_per_day, aes(x = enso, y = jaccard)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  facet_wrap(~ dataset) +
  scale_x_discrete(labels= gsub("_", "\n", levels(factor(jaccard_per_day$enso)))) +
  theme_bw() +
  xlab("") +
  theme(strip.background = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))


# analogue size

size_per_day <- rbind(
  data.frame(
    nsize = sapply(
      seq_along(obs_bc_anlg), function(i) {
        length(obs_bc_anlg[[i]]$date)
      }),
    dataset = "obs_bc",
    enso = cv_days$type),
  
  data.frame(
    nsize = sapply(
      seq_along(obs_bc_anlg), function(i) {
        length(hmg_obs_bc_anlg[[i]]$date)
      }),
    dataset = "hmg_obs_bc",
    enso = cv_days$type),
  
  data.frame(
    nsize = sapply(
      seq_along(obs_bc_anlg), function(i) {
        length(qc_obs_anlg[[i]]$date)
      }),
    dataset = "qc_obs",
    enso = cv_days$type)
)

size_per_day$dataset <- factor(size_per_day$dataset, levels = c("obs_bc",
                                                                "hmg_obs_bc",
                                                                "qc_obs"))
# size_per_day$enso <- factor(size_per_day$enso)
size_per_day$enso <- factor(size_per_day$enso,
                         levels = c("C_Neutral", "C_Nina", "C_Nino", "E_Neutral", "E_Nina", "E_Nino"),
                         labels = c("C_Neutral", "C_Niña", "C_Niño", "E_Neutral", "E_Niña", "E_Niño"))
plt2 <- 
  ggplot(data = size_per_day, aes(x = enso, y = nsize)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  facet_wrap(~ dataset) +
  scale_x_discrete(labels= gsub("_", "\n", levels(factor(size_per_day$enso)))) +
  theme_bw() +
  xlab("") + ylab("size (# analogues)") +
  theme(strip.background = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

# sat distribution

sat_obs_bc <- lapply(
  seq_along(obs_bc_anlg), function(i) {
    i_df <- obs_bc_anlg[[i]]
    i_df$sat <- factor(i_df$sat, levels = c("gsmap", "imerg", "pdirnow"))
    data.frame(as.list(prop.table(table(i_df$sat)) * 100))
  })
sat_obs_bc <- do.call(rbind, sat_obs_bc)
sat_obs_bc$enso <- cv_days$type

sat_obs_bc_mean <- aggregate(cbind(gsmap, imerg, pdirnow) ~ enso, data = sat_obs_bc, mean)
sat_obs_bc_mean <- reshape2::melt(sat_obs_bc_mean)
sat_obs_bc_mean$p25 <- unlist(reshape2::melt(
  aggregate(cbind(gsmap, imerg, pdirnow) ~ enso, data = sat_obs_bc, function(x) quantile(x, 0.25))
  )["value"])
sat_obs_bc_mean$p75 <- unlist(reshape2::melt(
  aggregate(cbind(gsmap, imerg, pdirnow) ~ enso, data = sat_obs_bc, function(x) quantile(x, 0.75))
  )["value"])


sat_hmg_obs_bc <- lapply(
  seq_along(obs_bc_anlg), function(i) {
    i_df <- hmg_obs_bc_anlg[[i]]
    i_df$sat <- factor(i_df$sat, levels = c("gsmap", "imerg", "pdirnow"))
    data.frame(as.list(prop.table(table(i_df$sat)) * 100))
  })
sat_hmg_obs_bc <- do.call(rbind, sat_hmg_obs_bc)
sat_hmg_obs_bc$enso <- cv_days$type
sat_hmg_obs_bc_mean <- aggregate(cbind(gsmap, imerg, pdirnow) ~ enso, data = sat_hmg_obs_bc, mean)
sat_hmg_obs_bc_mean <- reshape2::melt(sat_hmg_obs_bc_mean)
sat_hmg_obs_bc_mean$p25 <- unlist(reshape2::melt(
  aggregate(cbind(gsmap, imerg, pdirnow) ~ enso, data = sat_hmg_obs_bc, function(x) quantile(x, 0.25))
)["value"])
sat_hmg_obs_bc_mean$p75 <- unlist(reshape2::melt(
  aggregate(cbind(gsmap, imerg, pdirnow) ~ enso, data = sat_hmg_obs_bc, function(x) quantile(x, 0.75))
)["value"])




sat_qc_obs <- lapply(
  seq_along(obs_bc_anlg), function(i) {
    i_df <- qc_obs_anlg[[i]]
    i_df$sat <- factor(i_df$sat, levels = c("gsmap", "imerg", "pdirnow"))
    data.frame(as.list(prop.table(table(i_df$sat)) * 100))
  })
sat_qc_obs <- do.call(rbind, sat_qc_obs)
sat_qc_obs$enso <- cv_days$type
sat_qc_obs_mean <- aggregate(cbind(gsmap, imerg, pdirnow) ~ enso, data = sat_qc_obs, mean)
sat_qc_obs_mean <- reshape2::melt(sat_qc_obs_mean)
sat_qc_obs_mean$p25 <- unlist(reshape2::melt(
  aggregate(cbind(gsmap, imerg, pdirnow) ~ enso, data = sat_qc_obs, function(x) quantile(x, 0.25))
)["value"])
sat_qc_obs_mean$p75 <- unlist(reshape2::melt(
  aggregate(cbind(gsmap, imerg, pdirnow) ~ enso, data = sat_qc_obs, function(x) quantile(x, 0.75))
)["value"])


sat_distr <- rbind(
  data.frame(sat_obs_bc_mean, dataset = "obs_bc"),
  data.frame(sat_hmg_obs_bc_mean, dataset = "hmg_obs_bc"),
  data.frame(sat_qc_obs_mean, dataset = "qc_obs")
)

sat_distr$dataset <- factor(sat_distr$dataset, levels = c("obs_bc",
                                                          "hmg_obs_bc",
                                                          "qc_obs"))
# sat_distr$enso <- factor(sat_distr$enso)
sat_distr$enso <- factor(sat_distr$enso,
                         levels = c("C_Neutral", "C_Nina", "C_Nino", "E_Neutral", "E_Nina", "E_Nino"),
                         labels = c("C_Neutral", "C_Niña", "C_Niño", "E_Neutral", "E_Niña", "E_Niño"))

sat_distr$variable <- factor(sat_distr$variable,
                             levels = c("gsmap", "imerg", "pdirnow"),
                             labels = c("GSMaP_NRT", "IMERG-Early", "PDIR-Now"))


# plt3 <- 
#   ggplot(sat_distr2, aes(x = enso, y = value, fill = variable)) + 
#   geom_violin(position = position_dodge(width = .81), draw_quantiles = c(0.5), lwd = .25, alpha = 0.75) +
#   scale_fill_manual(
#         values = c("#4682B4", "#B4464B", "#B4AF46"),
#         guide = guide_legend("satellite\nproduct",
#                                          nrow = 1,
#                                          byrow = TRUE,
#                                          label.position = "top",
#                                          barheight = .5)
#   ) +
#   facet_wrap(~ dataset) + 
#   scale_x_discrete(labels= gsub("_", "\n", levels(factor(sat_distr$enso)))) +
#   theme_bw() +
#   xlab("") +
#   theme(strip.background = element_blank(),
#         legend.position = "bottom",
#         legend.box = "horizontal",
#         legend.margin = margin(t = -15))
# 
# plt3 <- 
#   ggplot(sat_distr, aes(x = enso, y = value, fill = variable, label = round(value, 1))) + 
#   geom_bar(stat="identity") +
#   geom_text(size = 3, position = position_stack(vjust = 0.5)) +
#   scale_fill_viridis_d(guide = guide_legend("satellite\nproduct",
#                                             nrow = 1,
#                                             byrow = TRUE,
#                                             label.position = "top",
#                                             size = .5)
#   ) +
#   facet_wrap(~ dataset) +
#   scale_x_discrete(labels= gsub("_", "\n", levels(factor(sat_distr$enso)))) +
#   theme_bw() +
#   xlab("") +
#   theme(strip.background = element_blank(),
#         legend.position = "bottom",
#         legend.box = "horizontal",
#         legend.margin = margin(t = -15))

plt3 <- 
  ggplot(sat_distr, aes(x = enso, y = value, fill = variable, label = round(value, 1))) + 
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_viridis_d(option = "inferno",
                       begin = 0.2, end = 0.8,
                       guide = guide_legend("satellite\nproduct",
                                         nrow = 1,
                                         byrow = TRUE,
                                         label.position = "top",
                                         barheight = .5)
  ) +
  geom_errorbar(aes(ymin = p25, ymax = p75), width = .45,
                position = position_dodge(.9), colour = "gray50") +
  facet_wrap(~ dataset) +
  scale_x_discrete(labels= gsub("_", "\n", levels(factor(sat_distr$enso)))) +
  theme_bw() +
  xlab("") + ylab("frequency (%)") +
  theme(strip.background = element_blank(),
        # legend.position = "bottom",
        # legend.box = "horizontal",
        legend.margin = margin(t = -15),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.position = "none")

# analogue metrics by satellite

sat_metric_obs_bc <- lapply(
  seq_along(obs_bc_anlg), function(i) {
    i_df <- obs_bc_anlg[[i]]
    i_df$sat <- factor(i_df$sat,
                       levels = c("gsmap", "imerg", "pdirnow"),
                       labels = c("GSMaP_NRT", "IMERG-Early", "PDIR-Now"))
    i_df$enso <- cv_days$type[i]
    i_df
  })

sat_metric_obs_bc <- do.call(rbind, sat_metric_obs_bc)
sat_metric_obs_bc <- aggregate(cbind(dr, dr_p90, mcc) ~ sat + enso, median, data = sat_metric_obs_bc)
sat_metric_obs_bc <- reshape2::melt(sat_metric_obs_bc, c("sat", "enso"))


sat_metric_hmg_obs_bc <- lapply(
  seq_along(obs_bc_anlg), function(i) {
    i_df <- hmg_obs_bc_anlg[[i]]
    i_df$sat <- factor(i_df$sat,
                       levels = c("gsmap", "imerg", "pdirnow"),
                       labels = c("GSMaP_NRT", "IMERG-Early", "PDIR-Now"))
    i_df$enso <- cv_days$type[i]
    i_df
  })

sat_metric_hmg_obs_bc <- do.call(rbind, sat_metric_hmg_obs_bc)
sat_metric_hmg_obs_bc <- aggregate(cbind(dr, dr_p90, mcc) ~ sat + enso, median, data = sat_metric_hmg_obs_bc)
sat_metric_hmg_obs_bc <- reshape2::melt(sat_metric_hmg_obs_bc, c("sat", "enso"))


sat_metric_qc_obs <- lapply(
  seq_along(obs_bc_anlg), function(i) {
    i_df <- qc_obs_anlg[[i]]
    i_df$sat <- factor(i_df$sat,
                       levels = c("gsmap", "imerg", "pdirnow"),
                       labels = c("GSMaP_NRT", "IMERG-Early", "PDIR-Now")
                       )
    i_df$enso <- cv_days$type[i]
    i_df
  })

sat_metric_qc_obs <- do.call(rbind, sat_metric_qc_obs)
sat_metric_qc_obs <- aggregate(cbind(dr, dr_p90, mcc) ~ sat + enso, median, data = sat_metric_qc_obs)
sat_metric_qc_obs <- reshape2::melt(sat_metric_qc_obs, c("sat", "enso"))


sat_metric_df <- rbind(
  data.frame(sat_metric_obs_bc, dataset = "obs_bc"),
  data.frame(sat_metric_hmg_obs_bc, dataset = "hmg_obs_bc"),
  data.frame(sat_metric_qc_obs, dataset = "qc_obs")
)

sat_metric_df$dataset <- factor(sat_metric_df$dataset, levels = c("obs_bc",
                                                          "hmg_obs_bc",
                                                          "qc_obs"))
sat_metric_df$enso <- factor(sat_metric_df$enso,
                             levels = c("C_Neutral", "C_Nina", "C_Nino", "E_Neutral", "E_Nina", "E_Nino"),
                             labels = c("C_Neutral", "C_Niña", "C_Niño", "E_Neutral", "E_Niña", "E_Niño"))

levels(sat_metric_df$variable) <- c(dr = latex2exp::TeX("\\textit{$d_{r}$}"),
                                    dr_p90 = latex2exp::TeX("\\textit{$d_{r}^{p90}$}"),
                                    mcc = latex2exp::TeX("\\textit{$MCC$}"))

plt4 <- 
  ggplot(sat_metric_df, aes(x = enso, y = value, fill = sat)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_viridis_d(option = "inferno",
                       begin = 0.2, end = 0.8,
                       guide = guide_legend("satellite\nproduct",
                                            nrow = 1,
                                            byrow = TRUE,
                                            label.position = "top",
                                            barheight = .5)
  ) +
  facet_grid(variable ~ dataset, scales = "free_y", labeller = label_parsed) +
  ggh4x::facetted_pos_scales(
    y = list(
      variable == "italic(d[r])" ~ scale_y_continuous(limits = c(0.55, 0.7)),
      variable == "italic(d[r]^{\n    p90\n})" ~ scale_y_continuous(limits = c(0.3, .45)),
      variable == "italic(MCC)" ~  scale_y_continuous(limits = c(0.165, .35))
    )
  ) +
  theme_bw() +
  scale_x_discrete(labels= gsub("_", "\n", levels(factor(sat_metric_df$enso)))) +
  xlab("") + ylab("analogue metrics") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = margin(t = -15),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.title = element_text(size = 9))
  

no_x <- theme(
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank()
)

((plt1 + no_x) + (plt2 + no_x) + (plt3 + no_x) + plt4 + 
    plot_layout(ncol = 1, axes = "collect_x", heights = c(1, 1, 1, 1.75))) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")") & 
  theme(plot.tag = element_text(size = 9),
        plot.tag.position  = c(0, 1))

ggsave(
  "output/03_precipitation-pattern-analogue/ANLGs_metrics.pdf",
  units = "in",
  width = 5.25,
  height = 4.75,
  dpi = 300,
  scale = 1.45
)
