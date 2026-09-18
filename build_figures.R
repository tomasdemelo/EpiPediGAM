# Rebuild manuscript figures.
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(ggsignif)
})

PROJ    <- getwd()
RES     <- if (dir.exists(file.path(PROJ, "Outputs", "Excel Files REMOTE"))) {
  file.path(PROJ, "Outputs", "Excel Files REMOTE")
} else if (dir.exists(file.path(PROJ, "Excel Files REMOTE"))) {
  file.path(PROJ, "Excel Files REMOTE")
} else {
  file.path(PROJ, "Excel Files v4-FULL")
}
SURR    <- if (dir.exists(file.path(PROJ, "Surrogate Null Outputs REMOTE"))) {
  file.path(PROJ, "Surrogate Null Outputs REMOTE")
} else {
  file.path(PROJ, "Surrogate Null Outputs v4-FULL")
}
AAFT    <- if (file.exists(file.path(SURR, "AAFT_null_selected.csv"))) {
  SURR
} else {
  file.path(PROJ, "Surrogate Null Outputs", "2-phasefixed-independent-AAFT-IAAFT-trend", "AAFT-N1000-seed1867")
}
OUT     <- if (dir.exists(file.path(PROJ, "Outputs", "Figures"))) file.path(PROJ, "Outputs", "Figures") else file.path(PROJ, "Figures")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# Color definitions for viruses and model families.
VIR <- c("Norovirus GII" = "green", "Enterovirus" = "red", "Influenza A" = "purple")
FAM <- c("Prevalence (beta)" = "#0072B2", "Incidence (Gaussian)" = "#D55E00")

theme_gam <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
        axis.ticks = element_line(colour = "black"),
        axis.title.x = element_text(size = 11.5),
        axis.title.y = element_text(size = 11.5),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 8.6, colour = "grey30"),
        plot.caption = element_text(hjust = 0, size = 8.5, colour = "grey30"),
        legend.position = "bottom", 
        legend.title = element_blank())

# Figure 2: viral trends and absences by site.
message("Figure 2 ...")
read_site <- function(nm) {
  f <- if (nm == "Ajax") "2022AjaxData.xlsx" else "2022PickeringData.xlsx"
  d <- suppressMessages(read_excel(file.path(PROJ, f), sheet = "Sheet1"))
  d$Date <- as.Date(d$Date)
  d <- d[d$Date >= as.Date("2022-09-01") & d$Date <= as.Date("2022-12-31"), ]
  data.frame(Site = nm, Date = d$Date,
             `Norovirus GII` = d$Noro7drolling, Enterovirus = d$EV7drolling,
             `Influenza A` = d$InfA7drolling, 
             Raw_Noro = d$Noro, 
             Raw_EV = d[[grep("^EV", names(d))[1]]], 
             Raw_InfA = d$InfA,
             Absence = d$percentElementary7drolling,
             check.names = FALSE)
}
site <- bind_rows(read_site("Ajax"), read_site("Pickering"))
site$Site <- factor(site$Site, levels = c("Ajax", "Pickering"))

viral <- site %>%
  pivot_longer(c("Norovirus GII", "Enterovirus", "Influenza A"),
               names_to = "Virus", values_to = "gc") %>%
  filter(!is.na(gc))
viral$Virus <- factor(viral$Virus, levels = names(VIR))

viral_raw <- site %>%
  pivot_longer(c("Raw_Noro", "Raw_EV", "Raw_InfA"),
               names_to = "Virus", values_to = "gc_raw") %>%
  mutate(Virus = case_when(
    Virus == "Raw_Noro" ~ "Norovirus GII",
    Virus == "Raw_EV" ~ "Enterovirus",
    Virus == "Raw_InfA" ~ "Influenza A"
  )) %>%
  filter(!is.na(gc_raw))
viral_raw$Virus <- factor(viral_raw$Virus, levels = names(VIR))

# Calculate first wastewater detection date.
aj  <- suppressMessages(read_excel(file.path(PROJ, "2022AjaxData.xlsx"), sheet = "Sheet1"))
aj$Date <- as.Date(aj$Date)
aj  <- aj[aj$Date >= as.Date("2022-09-01") & aj$Date <= as.Date("2022-12-31"), ]
clin <- data.frame(Site = factor("Ajax", levels = levels(site$Site)),
                   Date = min(aj$Date[!is.na(aj$FluCases) & aj$FluCases > 0]))
ww   <- viral %>% filter(Virus == "Influenza A") %>% group_by(Site) %>%
  summarise(Date = min(Date), .groups = "drop")

XSC <- scale_x_date(date_breaks = "1 week",
                    labels = function(x) ifelse(as.numeric(format(x, "%d")) <= 7, format(x, "%b %Y"), ""),
                    guide = guide_axis(angle = 45),
                    limits = as.Date(c("2022-08-29", "2022-12-27")), expand = c(0.015, 0))

# Dual-axis scaling factor.
SCALE_FACTOR <- 5

fig2 <- ggplot(viral, aes(x = Date)) +
  geom_area(data = filter(site, !is.na(Absence)), 
            aes(y = Absence / SCALE_FACTOR, fill = "Absences due to Illness"), 
            alpha = 0.2) +
  geom_line(data = filter(site, !is.na(Absence)), 
            aes(y = Absence / SCALE_FACTOR, colour = "Absences due to Illness"), 
            linewidth = 0.75) +
  geom_point(data = viral_raw, aes(y = log10(gc_raw), fill = Virus), 
             shape = 21, colour = "black", stroke = 0.3, size = 1.2, alpha = 0.6) +
  geom_line(aes(y = log10(gc), colour = Virus), linewidth = 0.75, alpha = 0.6) +
  scale_colour_manual(values = c(VIR, "Absences due to Illness" = "grey15")) +
  scale_fill_manual(values = c(VIR, "Absences due to Illness" = "grey75")) +
  guides(
    colour = guide_legend(override.aes = list(
      linetype = c("solid", "solid", "solid", "solid"),
      shape = c(21, 21, 21, NA),
      fill = c(VIR["Norovirus GII"], VIR["Enterovirus"], VIR["Influenza A"], "grey75"),
      colour = c(VIR["Norovirus GII"], VIR["Enterovirus"], VIR["Influenza A"], "grey15"),
      linewidth = c(0.75, 0.75, 0.75, 0.75),
      size = c(2, 2, 2, NA),
      alpha = c(1, 1, 1, 1)
    )),
    fill = "none"
  ) +
  XSC +
  facet_wrap(~Site, nrow = 1) +
  scale_y_continuous(
    name = expression("Viral signal (gc mL"^-1*", log"[10]*" scale)"),
    limits = c(0, 4),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * SCALE_FACTOR, name = "Elementary absence\ndue to illness (%)")
  ) +
  theme_gam + theme(axis.title.x = element_blank()) + labs(caption = "Viral lines and absenteeism are 7-day moving averages. Points show daily viral measurements.")

ggsave(file.path(OUT, "Figure2_viral_and_absences_v3.png"), fig2,
       width = 9, height = 4.5, dpi = 300, bg = "white")


# Figure 3.
# Restyled to match Figure 6 using transparent boxplots and points.
message("Figure 3 ...")
sig_func <- function(p) {
  if (is.na(p)) return(NA)
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return(NA)
}

fig3 <- ggplot(viral_raw, aes(x = Site, y = log10(gc_raw))) +
  geom_boxplot(aes(fill = Virus), alpha = 0.28, outlier.shape = NA, width = 0.6) +
  geom_point(aes(fill = Virus), shape = 21, colour = "black", stroke = 0.3, size = 2, alpha = 0.8,
             position = position_jitter(width = 0.15, seed = 42)) +
  geom_signif(comparisons = list(c("Ajax", "Pickering")),
              map_signif_level = sig_func, textsize = 4.5, vjust = 0.4) +
  scale_fill_manual(values = VIR) +
  facet_wrap(~Virus, nrow = 1) +
  scale_y_continuous(
    name = expression("Viral signal (gc mL"^-1*", log"[10]*" scale)"),
    limits = c(0, 4.5), 
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  theme_gam + theme(axis.title.x = element_blank(), legend.position = "none")

ggsave(file.path(OUT, "Figure3_viral_by_site_v3.png"), fig3,
       width = 7, height = 4.5, dpi = 300, bg = "white")

# Figure 4: deviance explained by window, age group, and site.
message("Figure 4 ...")
mf  <- list.files(RES, pattern = "_GAM-metrics\\.xlsx$", full.names = TRUE)
met <- do.call(rbind, lapply(mf, function(f) {
  d <- as.data.frame(suppressMessages(read_excel(f)))
  d$model <- sub("_GAM-metrics\\.xlsx$", "", basename(f)); d
}))
met <- met[abs(met$ACFResidual) <= 0.4, ]
met$Family <- factor(ifelse(grepl("percentchange", met$model),
                            "Incidence (Gaussian)", "Prevalence (beta)"),
                     levels = names(FAM))
# Label smoothing windows.
met$SMA  <- ifelse(grepl("1d", met$model), "1-day",
            ifelse(grepl("3d", met$model), "3-day",
            ifelse(grepl("5d", met$model), "5-day", "7-day")))
met$Age  <- ifelse(grepl("Elementary", met$model), "Elementary",
            ifelse(grepl("Secondary", met$model), "Secondary", "Total"))
met$City <- ifelse(grepl("Ajax", met$model), "Ajax", "Pickering")

strat <- bind_rows(
  transform(met[, c("DevianceExplained", "Family")], Panel = "Moving average window", Level = met$SMA),
  transform(met[, c("DevianceExplained", "Family")], Panel = "Age group",        Level = met$Age),
  transform(met[, c("DevianceExplained", "Family")], Panel = "Site",             Level = met$City))
strat$Panel <- factor(strat$Panel, levels = c("Moving average window", "Age group", "Site"))
strat$Level <- factor(strat$Level, levels = c("1-day", "3-day", "5-day", "7-day",
                                              "Elementary", "Secondary", "Total",
                                              "Ajax", "Pickering"))

set.seed(42)
fig4 <- ggplot(strat, aes(Level, DevianceExplained)) +
  geom_boxplot(aes(fill = Family), alpha = 0.28, outlier.shape = NA,
               position = position_dodge(0.78), width = 0.62) +
  geom_point(aes(fill = Family), shape = 21, colour = "black", stroke = 0.35,
             position = position_jitterdodge(jitter.width = 0.16, dodge.width = 0.78, seed = 42),
             size = 2.0, alpha = 0.85) +
  scale_fill_manual(values = FAM) +
  scale_y_continuous(limits = c(0.35, 1.0), breaks = seq(0.4, 1.0, 0.1),
                     expand = expansion(mult = c(0.03, 0.05))) +
  facet_wrap(~Panel, nrow = 1, scales = "free_x") +
  labs(x = NULL, y = "Deviance explained") +
  theme_gam + theme(axis.title.x = element_blank())
ggsave(file.path(OUT, "Figure4_deviance_explained_v3.png"), fig4,
       width = 8.8, height = 4.4, dpi = 300, bg = "white")

# Figure 5: top model observed versus fitted values.
message("Figure 5 ...")
top_mod <- met[which.max(met$DevianceExplained), ]
top_model_name <- top_mod$model

t_city <- top_mod$City
t_age  <- top_mod$Age
t_sma  <- top_mod$SMA # Selected smoothing window.
t_sma_num <- gsub("-day", "", t_sma)

is_change <- grepl("percentchange", top_model_name)
raw_col <- if(is_change) paste0("percentchange", t_age) else paste0("percent", t_age)
sm_col  <- paste0(raw_col, t_sma_num, "drolling")

top_data <- suppressMessages(read_excel(file.path(PROJ, paste0("2022", t_city, "Data.xlsx")), sheet = "Sheet1"))
top_data$Date <- as.Date(top_data$Date)
t_raw <- data.frame(Date = top_data$Date, 
                    RawAbs = top_data[[raw_col]],
                    SmoothAbs = top_data[[sm_col]])
t_raw <- t_raw[!is.na(t_raw$Date) & t_raw$Date >= as.Date("2022-09-01") & t_raw$Date <= as.Date("2022-12-31"), ]

fit_df <- suppressMessages(read_excel(file.path(RES, paste0(top_model_name, "_GAM-values.xlsx"))))
fit_df$Date <- as.Date(fit_df$Day)
fit_df$ci_lower <- as.numeric(fit_df$ci_lower)
fit_df$ci_upper <- as.numeric(fit_df$ci_upper)
fit_df$Residual <- fit_df$TotalAbsences - fit_df$fitted

acf_val <- round(top_mod$ACFResidual, 3)
ylab_str <- if(is_change) "Change in Absences (%)" else "Absences (%)"

pA <- ggplot() +
  geom_point(data = t_raw, aes(x = Date, y = RawAbs, colour = "Raw Daily"), 
             shape = 21, fill = "white", stroke = 0.3, alpha = 0.6) +
  geom_line(data = t_raw, aes(x = Date, y = SmoothAbs, colour = "Smoothed Data"), 
            linewidth = 0.8) +
  geom_ribbon(data = fit_df, aes(x = Date, ymin = ci_lower, ymax = ci_upper, colour = "Fitted 95% CI"), 
              fill = "#D55E00", alpha = 0.25, linetype = "blank") +
  geom_line(data = fit_df, aes(x = Date, y = fitted, colour = "Fitted Model"), 
            linewidth = 1.0) +
  scale_colour_manual(
    name = NULL,
    values = c("Raw Daily" = "black", "Smoothed Data" = "grey20", 
               "Fitted Model" = "#D55E00", "Fitted 95% CI" = NA),
    breaks = c("Raw Daily", "Smoothed Data", "Fitted Model", "Fitted 95% CI"),
    guide = guide_legend(
      override.aes = list(
        shape = c(21, NA, NA, 22),
        fill = c("white", NA, NA, "#D55E00"),
        linewidth = c(0.3, 0.8, 1.0, 0),
        linetype = c("blank", "solid", "solid", "blank"),
        alpha = c(1, 1, 1, 0.25),
        size = c(2, 0.5, 0.5, 4)
      )
    )
  ) +
  scale_y_continuous(limits = c(min(0, min(t_raw$RawAbs, na.rm = TRUE)), max(t_raw$RawAbs, na.rm = TRUE)), expand = c(0.02, 0)) +
  scale_x_date(limits = as.Date(c("2022-09-01", "2022-12-31")),
               date_breaks = "1 month", date_minor_breaks = "1 week",
               guide = guide_axis(minor.ticks = TRUE), expand = expansion(mult = c(0.02, 0.02))) +
  labs(title = paste0("Top model in-sample fit: ", t_city, " ", t_age),
       subtitle = paste0("n = ", nrow(fit_df), " (", format(min(fit_df$Date), "%d %b"), " \u2013 ", 
                         format(max(fit_df$Date), "%d %b"), "), +2-day offset. Both series are ", t_sma, " SMAs."),
       x = NULL, y = ylab_str) +
  theme_gam + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pB <- ggplot(fit_df, aes(x = Date, y = Residual)) +
  geom_segment(aes(xend = Date, yend = 0), colour = "grey40") +
  geom_point(shape = 21, fill = "grey70", colour = "black", size = 1.5, stroke = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  annotate("text", x = min(fit_df$Date) + 2, y = max(fit_df$Residual, na.rm = TRUE) * 0.9,
           label = paste0("ACF = ", acf_val), hjust = 0, size = 3.5, fontface = "italic") +
  scale_x_date(limits = as.Date(c("2022-09-01", "2022-12-31")),
               date_breaks = "1 month", date_labels = "%b %d, %Y", date_minor_breaks = "1 week",
               guide = guide_axis(minor.ticks = TRUE, angle = 45), expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = NULL, y = "Residuals") +
  theme_gam +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11.5),
        axis.ticks = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7))

fig5 <- pA / pB + plot_layout(heights = c(3, 1), guides = "collect") & theme(legend.position = "bottom", legend.title = element_blank())
ggsave(file.path(OUT, "Figure5_top_model_fit_v3.png"), fig5,
       width = 9, height = 6.2, dpi = 300, bg = "white")

# Figure 6: unique contribution to accuracy (drop-in-deviance).
message("Figure 6 ...")
drop_long <- tidyr::pivot_longer(met, 
                                 cols = c("EVDropDevPct", "NoroDropDevPct", "InfADropDevPct"), 
                                 names_to = "Predictor", values_to = "DropPct")

drop_long$Predictor <- ifelse(drop_long$Predictor == "EVDropDevPct", "Enterovirus",
                              ifelse(drop_long$Predictor == "NoroDropDevPct", "Norovirus GII", "Influenza A"))

set.seed(42)
fig6 <- ggplot(drop_long, aes(x = Predictor, y = DropPct)) +
  geom_boxplot(aes(fill = Predictor), alpha = 0.28, outlier.shape = NA, width = 0.6) +
  geom_point(aes(fill = Predictor), shape = 21, colour = "black", stroke = 0.3, size = 2, alpha = 0.8,
             position = position_jitter(width = 0.15, seed = 42)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  scale_fill_manual(values = VIR) +
  labs(title = "Unique Contribution to Accuracy (Drop-in-Deviance)",
       y = "Loss in deviance explained (%) when predictor removed",
       x = NULL) +
  theme_gam + theme(legend.position = "none")

ggsave(file.path(OUT, "Figure6_drop_in_deviance.png"), fig6, 
       width = 8, height = 5.5, dpi = 300, bg = "white")

# Figure 7: surrogate null distribution and ensemble test.
message("Figure 7 ...")
sel <- read.csv(file.path(AAFT, "AAFT_null_selected.csv"))
obs <- read.csv(file.path(AAFT, "AAFT_observed_base.csv"))
per <- read.csv(file.path(AAFT, "AAFT_null_per_realisation.csv"))
ens <- read.csv(file.path(AAFT, "AAFT_ensemble_statistic.csv"))

lab <- function(p) {
  q <- strsplit(p, "\\|")[[1]]
  paste0(q[1], " ", q[2], " ", q[3])
}
obs$lab <- vapply(obs$perm, lab, "")
sel$lab <- vapply(sel$perm, lab, "")
ord <- obs$lab[order(-obs$DevExact)]
sel$lab <- factor(sel$lab, levels = ord); obs$lab <- factor(obs$lab, levels = ord)
sel <- sel[!is.na(sel$lab), ]

pA <- ggplot(sel, aes(DevExact, lab)) +
  geom_violin(aes(fill = "Surrogate null", colour = "Surrogate null"), linewidth = 0.3, scale = "width", width = 0.9) +
  stat_summary(fun = median, geom = "point", shape = 124, size = 3.4, colour = "grey30") +
  geom_point(data = obs, aes(DevExact, lab, fill = "Observed model", colour = "Observed model"), shape = 21, stroke = 0.35, size = 2.6) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_fill_manual(
    name = NULL,
    values = c("Observed model" = "#0072B2", "Surrogate null" = "grey86"),
    breaks = c("Observed model", "Surrogate null")
  ) +
  scale_colour_manual(
    name = NULL,
    values = c("Observed model" = "black", "Surrogate null" = "grey75"),
    breaks = c("Observed model", "Surrogate null")
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        shape = c(21, 22),
        fill = c("#0072B2", "grey86"),
        colour = c("black", "grey65"),
        size = c(2.8, 3.5),
        stroke = c(0.35, 0.3)
      )
    ),
    colour = "none"
  ) +
  labs(tag = "A", x = "Deviance explained", y = NULL) +
  theme_gam +
  theme(plot.tag = element_text(face = "bold", size = 13))

scored <- per[per$scored %in% c(TRUE, "TRUE"), ]
pB <- ggplot(scored, aes(statistic)) +
  geom_histogram(bins = 46, fill = "grey86", colour = "white", linewidth = 0.25) +
  geom_vline(xintercept = ens$observed_statistic, colour = "#0072B2", linewidth = 0.9) +
  geom_vline(xintercept = median(scored$statistic), colour = "grey30",
             linetype = "22", linewidth = 0.6) +
  annotate("text", x = ens$observed_statistic, y = Inf, hjust = 1.08, vjust = -37,
           label = sprintf("observed %.3f\np = %.4f", ens$observed_statistic, ens$p_empirical),
           size = 3.1, colour = "#0072B2", fontface = "bold", lineheight = 1.05) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(tag = "B", x = "Mean percentile across retained models", y = "Count") +
  theme_gam +
  theme(plot.tag = element_text(face = "bold", size = 13),
        axis.title.y = element_text(vjust = -37.3))

fig7 <- (pA / pB) + plot_layout(heights = c(1, 0.85), guides = "collect") &
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave(file.path(OUT, "Figure7_surrogate_null_v3.png"), fig7,
       width = 8.2, height = 8.2, dpi = 300, bg = "white")

# Figure 8: noise degradation curves by model family.
message("Figure 8 ...")
curves <- do.call(rbind, lapply(seq_len(nrow(met)), function(i) {
  m <- met$model[i]; off <- as.character(met$Offset[i])
  mc <- file.path(RES, paste0(m, "_GAM-Monte-Carlo.xlsx"))
  do.call(rbind, lapply(c("prelog", "postlog"), function(s) {
    sn <- paste0(off, "_", s)
    if (!(sn %in% excel_sheets(mc))) return(NULL)
    d <- as.data.frame(suppressMessages(read_excel(mc, sheet = sn)))
    data.frame(model = m, Family = met$Family[i],
               Scheme = if (s == "prelog") "Pre-log (assay noise)" else "Post-log (downstream noise)",
               noise = d$noise_level, dev = d$mean_dev)
  }))
}))
agg <- curves %>% group_by(Family, Scheme, noise) %>%
  summarise(med = median(dev), lo = quantile(dev, .25), hi = quantile(dev, .75), .groups = "drop")

cross <- agg %>% group_by(Family, Scheme) %>%
  arrange(noise) %>%
  summarise(
    x = {
      idx <- which(med < 0.5)[1]
      if (is.na(idx)) {
        NA_real_
      } else if (idx == 1) {
        noise[1]
      } else {
        x0 <- noise[idx-1]; y0 <- med[idx-1]
        x1 <- noise[idx]; y1 <- med[idx]
        x0 + (0.5 - y0) * (x1 - x0) / (y1 - y0)
      }
    },
    .groups = "drop"
  ) %>% filter(!is.na(x))

fig8 <- ggplot(agg, aes(noise, med, colour = Scheme, fill = Scheme)) +
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey35") +
  geom_segment(data = cross, aes(x = x, xend = x, y = 0.5, yend = -Inf), 
               linetype = "dashed", linewidth = 0.4, show.legend = FALSE) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.95) + 
  geom_point(data = cross, aes(x = x, y = 0.5), shape = 21, size = 3.2,
             fill = "white", stroke = 0.9, colour = "grey20", show.legend = FALSE) +
  scale_colour_manual(values = c("Post-log (downstream noise)" = "#0072B2",
                                 "Pre-log (assay noise)" = "#E69F00")) +
  scale_fill_manual(values = c("Post-log (downstream noise)" = "#0072B2",
                               "Pre-log (assay noise)" = "#E69F00")) +
  scale_x_continuous(breaks = seq(0, 1, 0.05), 
                     labels = function(x) ifelse(round(x %% 0.2, 5) == 0, scales::percent(x, accuracy = 1), "")) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  facet_wrap(~Family, nrow = 1) +
  labs(x = "Simulated noise, relative to each predictor's standard deviation",
       y = "Deviance explained",
       caption = paste("Line, median across models within family; ribbon, interquartile range.",
                       "Circles mark where the median crosses 50%.\nDeviance explained is",
                       "family-relative, so the two panels are not on a comparable scale.")) +
  theme_gam
ggsave(file.path(OUT, "Figure8_noise_degradation_v3.png"), fig8,
       width = 9, height = 4.6, dpi = 300, bg = "white")

# Copy static supplementary figures if present.
if (dir.exists(file.path(PROJ, "Figures", "manuscript_current"))) {
  for (f in c("Figure1_sewershed_map.png", "SuppFigure5_partial_effects.png")) {
    src_f <- file.path(PROJ, "Figures", "manuscript_current", f)
    if (file.exists(src_f)) file.copy(src_f, file.path(OUT, f), overwrite = TRUE)
  }
}

cat("\n--- crossings on the faceted Figure 8 ---\n"); print(as.data.frame(cross))
cat("\nWritten to", OUT, "\n"); print(list.files(OUT))
