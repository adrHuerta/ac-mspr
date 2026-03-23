rm(list = ls())

library(caret)
library(patchwork)
source("R/analogues/daily_frequency_plot.R")

# 1) raw data / individual WTs comes from:
# https://doi.org/10.1007/s00382-024-07578-4 and
# https://doi.org/10.1175/JCLI-D-21-0303.1

df <- read.csv("/scratch2/ahuerta/datasets/others/wt-tropical-sa.csv")
df$time <- as.Date(df$time)
df <- df[df$period == "1960-2024", ]
df <- df[, c(1, 2, 3)]
df <- reshape2::dcast(df, time ~ level, value.var = "cps")
colnames(df) <- c("ctime", "c200hpa", "c850hpa")

df <- data.frame(
  time = df$ctime,
  Type_850hPa = df$c850hpa,
  Type_200hPa = df$c200hpa
)


# 2) split into training (1980–2024) and test (1960–1980)

recent_start <- as.Date("1970-01-01")
df_train <- df[df$time >= recent_start, ]
df_test  <- df[df$time < recent_start, ]


# 3) one-hot encode WT categories

dummies <- dummyVars(~ Type_850hPa + Type_200hPa, data = df)
train_feat <- as.data.frame(predict(dummies, newdata = df_train))
test_feat  <- as.data.frame(predict(dummies, newdata = df_test))


# 4) hierarchical clustering on training data

dist_train <- proxy::dist(train_feat, method = "binary")
hc <- hclust(dist_train, method = "ward.D2")
k <- 9
train_clusters <- cutree(hc, k = k)
df_train$MetaWT <- factor(train_clusters)


# 5) cluster centroids

centroids <- aggregate(train_feat, by = list(cluster = train_clusters), FUN = mean)
rownames(centroids) <- centroids$cluster
centroids$cluster <- NULL

# 6) compute GLOBAL sigma (σ_global)

sigma_within <- sapply(1:k, function(i) {
  rows <- which(train_clusters == i)
  if (length(rows) > 1) {
    mean(proxy::dist(train_feat[rows, ], method = "binary"))
  } else {
    NA
  }
})
sigma_global <- mean(sigma_within, na.rm = TRUE)


# 7) HARD assignment for test set

assign_cluster <- function(x, centroids) {
  d <- proxy::dist(rbind(centroids, x), method = "binary")
  drow <- as.matrix(d)[nrow(d), 1:nrow(centroids)]
  idx <- which.min(drow)
  as.integer(rownames(centroids)[idx])
}
df_test$MetaWT <- factor(apply(test_feat, 1, assign_cluster, centroids = centroids))


# 8) hybrid gaussian × linear soft classification
# gaussian SOFT approach comes from: https://doi.org/10.1007/s00382-018-4506-7

alpha <- 2.5

get_probs_hybrid <- function(x, centroids, sigma = sigma_global, alpha = 1.5) {
  d <- proxy::dist(rbind(centroids, x), method = "binary")
  d <- as.numeric(as.matrix(d)[nrow(d), 1:nrow(centroids)])
  
  # hybrid weights: linear × Gaussian
  # w <- (1 - d) * exp(-(d^2) / (2 * sigma^2))
  w <- (1 - d) * exp(-(d^2) / (2 * sigma^2))
  w <- w^alpha
  probs <- w / sum(w)
  return(probs)
}

feat_all <- rbind(train_feat, test_feat)
probs_matrix <- t(apply(feat_all, 1, get_probs_hybrid, centroids = centroids, 
                        sigma = sigma_global, alpha = alpha))
colnames(probs_matrix) <- paste0("Prob_WT", 1:k)
# probs_matrix <- round(probs_matrix, 3)


# 9) combine outputs

df_final <- rbind(df_train, df_test)
df_final <- cbind(df_final, probs_matrix)
df_final <- df_final[order(df_final$time), ]
row.names(df_final) <- NULL


# 10) saving df_final (FINAL WTs)

# making same name on WT column and soft WT columns
df_final2 <- df_final
df_final2$MetaWT <- paste0("Prob_WT", df_final2$MetaWT)

# adding 02-29 (repeat what is found on 02-28)
whole_time <- seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day")
leap_days <- setdiff(whole_time, df_final2$time)
leap_days_df <- data.frame(time = leap_days)
leap_days_df <- cbind(leap_days_df, df_final2[df_final2$time %in% (leap_days-1), - 1])
df_final_with_leap_day <- rbind(df_final2, leap_days_df)
df_final_with_leap_day <- df_final_with_leap_day[order(df_final_with_leap_day$time),]
rownames(df_final_with_leap_day) <- NULL

write.csv(
  df_final_with_leap_day,
  "output/00_weather-types/WTs_tSA.csv",
  row.names = FALSE
)


## how similar (or different) is metaWT with the individual WTs? 

# plot
metawt_pl <- daily_frequency_plot(df_final, time_col = "time", discrete_col = "MetaWT")
wt850_pl <- daily_frequency_plot(df_final, time_col = "time", discrete_col = "Type_850hPa")
wt200_pl <- daily_frequency_plot(df_final, time_col = "time", discrete_col = "Type_200hPa")

(metawt_pl + wt850_pl + wt200_pl + plot_layout(ncol = 1)) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")") & 
  theme(plot.tag = element_text(size = 10),
        plot.tag.position  = c(0, 1))

ggsave(
  "output/00_weather-types/WTs_frqplots.pdf",
  units = "in",
  width = 4,
  height = 5,
  dpi = 300,
  scale = 1.65
)


# metrics
nmi_surface <- aricode::NMI(df_final$MetaWT, df$Type_850hPa)
nmi_altitude <- aricode::NMI(df_final$MetaWT, df$Type_200hPa)
ari_surface <- mclust::adjustedRandIndex(df_final$MetaWT, df$Type_850hPa)
ari_altitude <- mclust::adjustedRandIndex(df_final$MetaWT, df$Type_200hPa)
metrics_table <- t(data.frame(NMI = c(nmi_surface, nmi_altitude),
                            ARI = c(ari_surface, ari_altitude)))
colnames(metrics_table) <- c("surface", "altitude")
contingency_table <- cbind(
  table(df_final$MetaWT, df$Type_850hPa),
  table(df_final$MetaWT, df$Type_200hPa)
  )

write.csv(
  metrics_table,
  "output/00_weather-types/WTs_metrics.csv",
  row.names = TRUE
)

write.csv(
  contingency_table,
  "output/00_weather-types/WTs_contingency_table.csv",
  row.names = FALSE
)


## SOFT WTs analysis

df_final$Month <- as.integer(format(df$time, "%m"))
df_final$DOY <- as.integer(format(df$time, "%j"))

# Extract probability columns
prob_cols <- grep("Prob_WT", names(df_final))
prob_mat <- as.matrix(df_final[, prob_cols])

# count_WTs_P <- function(probs, threshold = 0.5) {
#   probs_sorted <- sort(probs, decreasing = TRUE)
#   cumsum_probs <- cumsum(probs_sorted)
#   sum(cumsum_probs <= threshold) + 1  # add 1 for first WT exceeding threshold
# }

shannon_entropy <- function(p) {
  p <- p / sum(p)    # normalize
  p <- p[p > 0]      # remove zeros
  -sum(p * log(p))   # natural log
}

# shannon_entropy
## how many equally probable weather types would give this same uncertainty
## how regime-dominated vs regime-mixed a day is
effective_contributors <- function(p) {
  H <- shannon_entropy(p)
  exp(H)  # convert log-entropy to real number
}

df_final$effective_contributors <- apply(prob_mat, 1, effective_contributors)

toplot <- df_final[, c("DOY", "effective_contributors")]
toplot <- aggregate(effective_contributors ~ DOY, data = df_final, mean)
toplot <- reshape2::melt(toplot, variable.name = "number", id.vars = "DOY")

month_max <- as.numeric(tapply(df_final$DOY, df_final$Month, max))

ewts_plt <-
  ggplot(data = toplot) +
  geom_line(aes(x = DOY, y = value)) + 
  geom_vline(xintercept = month_max[1:11], linetype="dashed", color="grey") + 
  scale_x_continuous(breaks = c(1, month_max), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(1, 6), breaks = seq(1, 6)) +
  labs(y = "Effective weather types", x = "Day of Year") + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.margin = margin(t = -5),
    axis.title=element_text(size = 8.5)
  )
  

(metawt_pl + ewts_plt + plot_layout(ncol = 1)) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")") & 
  theme(plot.tag = element_text(size = 10),
        plot.tag.position  = c(0, 1))

ggsave(
  "output/00_weather-types/WTs_frqplot_ewts.pdf",
  units = "in",
  width = 4,
  height = 4.25,
  dpi = 300,
  scale = 1.2
)

# asymmetric_nmi <- function(true_labels, pred_labels) {
#   
#   # contingency table
#   tab <- table(true_labels, pred_labels)
#   n <- sum(tab)
#   
#   # probabilities
#   pij <- tab / n
#   pi <- rowSums(pij)
#   pj <- colSums(pij)
#   
#   # mutual information
#   mi <- 0
#   for (i in seq_along(pi)) {
#     for (j in seq_along(pj)) {
#       if (pij[i,j] > 0) {
#         mi <- mi + pij[i,j] * log(pij[i,j] / (pi[i] * pj[j]))
#       }
#     }
#   }
#   
#   # entropy of ground truth
#   H_true <- -sum(pi * log(pi))
#   
#   # asymmetric NMI
#   nmi_a <- mi / H_true
#   return(nmi_a)
# }
