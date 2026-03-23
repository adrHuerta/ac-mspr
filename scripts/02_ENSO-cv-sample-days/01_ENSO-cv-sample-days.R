rm(list = ls())

library(ggplot2)
library(patchwork)

# E and C data from http://met.igp.gob.pe/datos/ecindex_ersstv5.txt

EC_index <- read.table("/scratch2/ahuerta/datasets/others/ecindex_ersstv5.txt", skip = 4, head = TRUE)
EC_index <- EC_index[EC_index$year >= 1960 & EC_index$year <= 2015, ]
EC_index$year_month <- paste(EC_index$year, formatC(EC_index$month, width = 2, format = "d", flag = "0"), sep = "-")

# E and C events based on https://www.nature.com/articles/s43017-020-0040-3

EC_index <- transform(
  EC_index,
  E_type = ifelse(E_index > 1, "E_Nino", ifelse(E_index < -0.75, "E_Nina", "E_Neutral")),
  C_type = ifelse(C_index > 1, "C_Nino", ifelse(C_index < -1, "C_Nina", "C_Neutral"))
)

# "interpolating" month to daily categories

daily_Dates <- seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day")
daily_Dates <- data.frame(year_month = format(daily_Dates, "%Y-%m"), day = format(daily_Dates, "%d"))
daily_Dates$dates <- as.Date(paste(daily_Dates$year_month, "-", daily_Dates$day, sep = ""))

daily_Dates$E <- NA
daily_Dates$E_type <- NA
daily_Dates$C <- NA
daily_Dates$C_type <- NA

for(xij in seq_len(nrow(EC_index))){
  
  e_ym_i <- EC_index[xij, ]$year_month
  e_ym_label <- EC_index[xij, ]$E_type
  e_ym_val <- EC_index[xij, ]$E_index
  
  daily_Dates[which(daily_Dates$year_month %in% e_ym_i), "E_type"] <- e_ym_label
  daily_Dates[which(daily_Dates$year_month %in% e_ym_i), "E"] <- e_ym_val
  
  c_ym_i <- EC_index[xij, ]$year_month
  c_ym_label <- EC_index[xij, ]$C_type
  c_ym_val <- EC_index[xij, ]$C_index
  
  daily_Dates[which(daily_Dates$year_month %in% c_ym_i), "C_type"] <- c_ym_label
  daily_Dates[which(daily_Dates$year_month %in% c_ym_i), "C"] <- c_ym_val
  
}

# defining the N sample values

Ndays = nrow(daily_Dates)
percent_sample = 5

# hybrid: proportional with a minimum per phase: ~ same size in both index

E_types_proportion <- table(daily_Dates$E_type) / length(daily_Dates$E_type)
# Or custom proportions for C: (I did this because E and C were different, lower in C)
# C_types_proportion <- table(daily_Dates$C_type) / length(daily_Dates$C_type)
C_types_proportion <- c("C_Neutral" = 0.70, "C_Nina" = 0.20, "C_Nino" = 0.10)

E_days_sample <- round((percent_sample * Ndays/ 100) * E_types_proportion)
C_days_sample <- round((percent_sample * Ndays/ 100) * C_types_proportion)


# 1) SAMPLE DAYS FOR E (stratified by E_type)
set.seed(123)

E_sample_idx <- integer(0)

for (etype in names(E_days_sample)) {
  # candidate rows for this E_type
  cand_idx <- which(daily_Dates$E_type == etype)
  
  n_to_sample <- min(E_days_sample[etype], length(cand_idx))
  if (n_to_sample > 0) {
    E_sample_idx <- c(
      E_sample_idx,
      sample(cand_idx, n_to_sample, replace = FALSE)
    )
  }
}

E_sample_idx  <- sort(unique(E_sample_idx))
E_sample_days <- daily_Dates[E_sample_idx,]

# 2) SAMPLE DAYS FOR C FROM REMAINING DAYS (no overlap with E)

remaining_idx  <- setdiff(seq_len(Ndays), E_sample_idx)
remaining_days <- daily_Dates[remaining_idx, ]

C_sample_idx_local <- integer(0)

for (ctype in names(C_days_sample)) {
  # candidate rows *in remaining_days* for this C_type
  cand_local <- which(remaining_days$C_type == ctype)
  
  n_to_sample <- min(C_days_sample[ctype], length(cand_local))
  if (n_to_sample > 0) {
    C_sample_idx_local <- c(
      C_sample_idx_local,
      sample(cand_local, n_to_sample, replace = FALSE)
    )
  }
}

C_sample_idx  <- sort(unique(remaining_idx[C_sample_idx_local]))
C_sample_days <- daily_Dates[C_sample_idx, ]


# 3) FINAL CV DAY SET = union(E_sample, C_sample)

CV_idx  <- sort(unique(c(E_sample_idx, C_sample_idx)))
CV_days <- daily_Dates[CV_idx, c("year_month", "day")]

E_CV <- E_sample_days[, c("dates", "E_type")]; colnames(E_CV) <-c("dates", "type")
C_CV <- C_sample_days[, c("dates", "C_type")]; colnames(C_CV) <-c("dates", "type")
EC_CV <- rbind(E_CV, C_CV)
EC_CV <- EC_CV[order(EC_CV$dates), ]
rownames(EC_CV) <- NULL

write.csv(
  EC_CV,
  "output/02_ENSO-cv-sample-days/CV_days.csv",
  row.names = FALSE
)

# 4) Plot

# create a monthly date for plotting EC_index time series
E_sample_days$E_type <- factor(E_sample_days$E_type, levels = c("E_Nino", "E_Neutral", "E_Nina"), labels = c("E_Niño", "E_Neutral", "E_Niña"))
C_sample_days$C_type <- factor(C_sample_days$C_type, levels = c("C_Nino", "C_Neutral", "C_Nina"), labels = c("C_Niño", "C_Neutral", "C_Niña"))

Eplot <- 
  ggplot(data = daily_Dates, aes(x = dates, y = E)) + 
  geom_line(colour = "black", linewidth = 0.3) + 
  theme_bw() +
  geom_hline(
    yintercept = c(1, -0.7),
    linetype    = "dashed",
    linewidth = 0.35,
    alpha       = 0.3,
  ) +
  geom_hline(
    yintercept = c(0),
    linewidth = 0.35,
    alpha       = 0.3,
  ) +
  geom_rug(
    data = E_sample_days,
    aes(x = dates, colour = E_type),
    sides = "tb",
    inherit.aes = FALSE,
    alpha  = 0.3,
    length = unit(0.05, "npc")
  ) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  xlab("") + ylab("") +
  scale_y_continuous(name = NULL, breaks = seq(-2, 4, 1)) +
  theme(
    legend.position = "bottom",
    legend.margin=margin(-10, 0, 0, 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = grid::unit(c(0,0,0,0), "mm")
  ) + 
  scale_x_date(expand = c(0, 0), date_labels = "%Y", breaks = "10 years") 

Cplot <- 
  ggplot(data = daily_Dates, aes(x = dates, y = C)) + 
  geom_line(colour = "black", linewidth = 0.3) + 
  theme_bw() +
  geom_hline(
    yintercept = c(1, -1),
    linetype    = "dashed",
    linewidth = 0.35,
    alpha       = 0.3,
  ) +
  geom_hline(
    yintercept = c(0),
    linewidth = 0.35,
    alpha       = 0.3,
  ) +
  geom_rug(
    data = C_sample_days,
    aes(x = dates, colour = C_type),
    sides = "tb",
    inherit.aes = FALSE,
    alpha  = 0.3,
    length = unit(0.05, "npc")
  ) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  xlab("") + ylab("") +
  scale_y_continuous(name = NULL) +
  theme(
    legend.position = "bottom",
    legend.margin=margin(-10, 0, 0, 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = grid::unit(c(0,0,0,0), "mm")
  ) + 
  scale_x_date(expand = c(0, 0), date_labels = "%Y", breaks = "10 years") 

(Eplot + Cplot + plot_layout(ncol = 1, heights = c(1, 1))) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")") & 
  theme(plot.tag = element_text(size = 10))

ggsave(
  "output/02_ENSO-cv-sample-days/EC_cv_sample_days.pdf",
  units = "in",
  width = 3.25,
  height = 3,
  dpi = 300,
  scale = 1.75
)
