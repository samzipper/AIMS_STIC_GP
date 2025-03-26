## Figure7_TimeseriesPrcWet.R

## prep workspace
source(file.path("code", "paths+packages.R"))

## data prep - STIC data
# load stic data
df_stic <- 
  read_csv(file.path("data", "STIC_Daily_01-Tidy.csv")) |> 
  subset(sublocation == "HS")

# summarize to daily percent wet
df_stic_prcWet <- 
  df_stic |> 
  group_by(Date) |> 
  summarize(prc_wet_overall = mean(prc_wet, na.rm = T),
            n_stic = sum(is.finite(prc_wet))) |> 
  # trim first 2 days - suspicious data
  subset(Date >= ymd("2021-05-22"))

# set to NA during changeovers
df_stic_prcWet$prc_wet_overall[df_stic_prcWet$n_stic <= 25] <- NA

# inspect
ggplot(df_stic_prcWet, aes(x = Date, y = prc_wet_overall)) + geom_line()
ggplot(df_stic_prcWet, aes(x = Date, y = n_stic)) + geom_line()

## data prep - met data
df_met <- 
  read_csv(file.path("data", "MetData-Compiled.csv")) |> 
  mutate(P.ETo_mm = precip_mm - ETo_mm,
         P.ET_mm = precip_mm - ET_mm_ensemble)

## join and clean up
df_join <- 
  left_join(df_met, df_stic_prcWet, by = "Date") |> 
  mutate(precip_mm_cumsum = cumsum(precip_mm),
         ETo_mm_cumsum = cumsum(ETo_mm),
         ET_mm_cumsum = cumsum(ET_mm_ensemble),
         P.ETo_mm_cumsum = cumsum(P.ETo_mm),
         P.ET_mm_cumsum = cumsum(P.ET_mm))
table(df_join$precip_mm_source) # 2024 data is gridMET - before that is KNZ
table(df_join$ETo_mm_source)

# create exploratory plots
vars_plot <- c("prc_wet_overall", "P.ET_mm_cumsum")  # each will be plotted as a facet

df_join |> 
  dplyr::select(Date, all_of(vars_plot)) |> 
  pivot_longer(all_of(vars_plot)) |> 
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_wrap(~name, ncol = 1, scales = "free_y")

## compare delta prc_wet to different lags of hydrologic predictors
library(zoo)
df_join$prc_wet_change <- c(NA, diff(df_join$prc_wet_overall, lag = 1))

# loop
vars_test <- c("P.ET_mm", "precip_mm", "P.ETo_mm", "ET_mm_ensemble")
response_test <- c("prc_wet_change", "prc_wet_overall")
windows_test <- seq(1, 365, 1)
starting <- T
for (r in response_test){
  for (v in vars_test){
    for (w in windows_test){
      df_join[ "prc_wet_response"] <- df_join[ , r]
      df_join[ "climate_predictor"] <- rollsum(df_join[ , v], w, na.pad = TRUE, align = "right")
      r2 <- summary(lm(prc_wet_response ~ climate_predictor, data = df_join))$r.squared
      
      # compile
      df_rvw <- data.frame(response = r, variable = v, window = w, r2 = r2)
      
      if (starting){
        df_all <- df_rvw
        starting <- F
      } else {
        df_all <- bind_rows(df_all, df_rvw)
      }
      
    }
  }
}


# plot
ggplot(df_all, aes(x = window, y = r2)) + 
  geom_col() +
  facet_grid(response~variable) +
  scale_x_continuous(expand = c(0,0))

df_all[which.max(df_all$r2), ]
subset(df_all, variable == "P.ET_mm")[which.max(subset(df_all, variable == "P.ET_mm")$r2), ]

# extract best lags
subset(df_all, variable == "precip_mm")[which.max(subset(df_all, variable == "precip_mm")$r2), ]
precip_bestLag <- 27

subset(df_all, variable == "ET_mm_ensemble")[which.max(subset(df_all, variable == "ET_mm_ensemble")$r2), ]
ET_bestLag <- 290

# plot best relationship
df_join$precip_mm_bestLag <- rollsum(df_join$precip_mm, precip_bestLag, na.pad = TRUE, align = "right")
df_join$ET_mm_bestLag <- rollsum(df_join$ET_mm_ensemble, ET_bestLag, na.pad = TRUE, align = "right")

lm(prc_wet_overall ~ precip_mm_bestLag, data = df_join) |> summary()
lm(prc_wet_overall ~ ET_mm_bestLag, data = df_join) |> summary()
lm(prc_wet_overall ~ precip_mm_bestLag + ET_mm_bestLag, data = df_join) |> summary()

## FINAL PLOTS - VERSION 3 - 8 PANEL, DAILY AND LAGGED PRECIP + ET:
# R2 distribution
p_r2_precip <-
  subset(df_all, response == "prc_wet_overall" & variable == "precip_mm") |> 
  pivot_longer(variable) |> 
  ggplot(aes(x = window, y = r2)) + 
  geom_col(color = col.cat.blu, fill = col.cat.blu) +
  geom_vline(xintercept = precip_bestLag, color = col.cat.red) +
  scale_x_continuous(name = "Precipitation window [days]", limits = c(0, 365), expand = c(0,0)) +
  scale_y_continuous(name = "R\u00b2, wet STIC proportion ~\nsummed precipitation",
                     limits = c(0, 0.605), expand = c(0,0))

p_r2_ET <-
  subset(df_all, response == "prc_wet_overall" & variable == "ET_mm_ensemble") |> 
  pivot_longer(variable) |> 
  ggplot(aes(x = window, y = r2)) + 
  geom_col(color = col.cat.blu, fill = col.cat.blu) +
  geom_vline(xintercept = ET_bestLag, color = col.cat.red) +
  scale_x_continuous(name = "ET window [days]", limits = c(0, 365), expand = c(0,0)) +
  scale_y_continuous(name = "R\u00b2, wet STIC proportion ~\nsummed ET",
                     limits = c(0, 0.605), expand = c(0,0))

# predict prc_wet_overall
df_predict <- subset(df_join, 
                     is.finite(precip_mm_bestLag) &
                       is.finite(ET_mm_bestLag) &
                       is.finite(prc_wet_overall))
lm_P.ET <- lm(prc_wet_overall ~ precip_mm_bestLag + ET_mm_bestLag, 
              data = df_predict)
df_predict$prc_wet_predicted <- predict(lm_P.ET)
summary(lm_P.ET)
p_scatter <- 
  ggplot(df_predict, aes(y = prc_wet_overall, x = prc_wet_predicted)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.red) +
  scale_x_continuous(name = "Predicted wet STIC proportion",
                     limits = c(0,1), expand = c(0, 0),
                     breaks = seq(0, 1, 0.25),
                     labels = c("0", "", "0.5", "", "1")) +
  scale_y_continuous(name = "Wet STIC proportion",
                     limits = c(0,1), expand = c(0, 0),
                     breaks = seq(0, 1, 0.25),
                     labels = c("0", "", "0.5", "", "1"))

p_time_STIC <-
  ggplot(df_join, aes(x = Date, y = prc_wet_overall)) +
  geom_line() +
  scale_x_date(name = "Date", 
               limits = c(ymd("2021-05-01"), ymd("2024-05-21")), 
               expand = c(0,0), 
               breaks = seq(ymd("2021-05-01"), ymd("2024-05-01"), by = "1 month"),
               minor_breaks = NULL,
               labels = c("May\n2021", "", "", "", "Sep", "", "", "", 
                          "Jan\n2022", "", "", "", "May", "", "", "", "Sep", "", "", "", 
                          "Jan\n2023", "", "", "", "May", "", "", "", "Sep", "", "", "",
                          "Jan\n2024", "", "", "", "May")
  ) +
  scale_y_continuous(name = "Wet STIC\nproportion",
                     breaks = seq(0, 1, 0.25),
                     labels = c("0", "", "0.5", "", "1")) +
  theme(axis.title.x = element_blank()) +
  guides(x = guide_axis(minor.ticks = TRUE))

p_time_precip <-
  ggplot(df_join, aes(x = Date, y = precip_mm_bestLag)) +
  geom_line() +
  scale_x_date(name = "Date", 
               limits = c(ymd("2021-05-01"), ymd("2024-05-21")), 
               expand = c(0,0), 
               breaks = seq(ymd("2021-05-01"), ymd("2024-05-01"), by = "1 month"),
               minor_breaks = NULL,
               labels = c("May\n2021", "", "", "", "Sep", "", "", "", 
                          "Jan\n2022", "", "", "", "May", "", "", "", "Sep", "", "", "", 
                          "Jan\n2023", "", "", "", "May", "", "", "", "Sep", "", "", "",
                          "Jan\n2024", "", "", "", "May")
  ) +
  scale_y_continuous(name = paste0("Precipitation [mm],\n", precip_bestLag, "-day sum")) +
  theme(axis.title.x = element_blank()) +
  guides(x = guide_axis(minor.ticks = TRUE))

p_time_precip_daily <-
  ggplot(df_join, aes(x = Date, y = precip_mm)) +
  geom_line() +
  scale_x_date(name = "Date", 
               limits = c(ymd("2021-05-01"), ymd("2024-05-21")), 
               expand = c(0,0), 
               breaks = seq(ymd("2021-05-01"), ymd("2024-05-01"), by = "1 month"),
               minor_breaks = NULL,
               labels = c("May\n2021", "", "", "", "Sep", "", "", "", 
                          "Jan\n2022", "", "", "", "May", "", "", "", "Sep", "", "", "", 
                          "Jan\n2023", "", "", "", "May", "", "", "", "Sep", "", "", "",
                          "Jan\n2024", "", "", "", "May")
  ) +
  scale_y_continuous(name = "Precipitation [mm],\nDaily",
                     expand = expansion(mult = c(0, 0.05))) +
  theme(axis.title.x = element_blank()) +
  guides(x = guide_axis(minor.ticks = TRUE))

p_time_ET <-
  ggplot(df_join, aes(x = Date, y = ET_mm_bestLag)) +
  geom_line() +
  scale_x_date(name = "Date", 
               limits = c(ymd("2021-05-01"), ymd("2024-05-21")), 
               expand = c(0,0), 
               breaks = seq(ymd("2021-05-01"), ymd("2024-05-01"), by = "1 month"),
               minor_breaks = NULL,
               labels = c("May\n2021", "", "", "", "Sep", "", "", "", 
                          "Jan\n2022", "", "", "", "May", "", "", "", "Sep", "", "", "", 
                          "Jan\n2023", "", "", "", "May", "", "", "", "Sep", "", "", "",
                          "Jan\n2024", "", "", "", "May")
  ) +
  scale_y_continuous(name = paste0("ET [mm],\n", ET_bestLag, "-day sum")) +
  guides(x = guide_axis(minor.ticks = TRUE))

p_time_ET_daily <-
  ggplot(df_join, aes(x = Date, y = ET_mm_ensemble)) +
  geom_line() +
  scale_x_date(name = "Date", 
               limits = c(ymd("2021-05-01"), ymd("2024-05-21")), 
               expand = c(0,0), 
               breaks = seq(ymd("2021-05-01"), ymd("2024-05-01"), by = "1 month"),
               minor_breaks = NULL,
               labels = c("May\n2021", "", "", "", "Sep", "", "", "", 
                          "Jan\n2022", "", "", "", "May", "", "", "", "Sep", "", "", "", 
                          "Jan\n2023", "", "", "", "May", "", "", "", "Sep", "", "", "",
                          "Jan\n2024", "", "", "", "May")
  ) +
  scale_y_continuous(name = "ET [mm],\nDaily") +
  guides(x = guide_axis(minor.ticks = TRUE))

## ALTERNATE: SEPARATE INTO TWO PANELS

(p_time_STIC + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.04, 0.98)) +
    p_time_precip_daily + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.04, 0.98)) +
    p_time_ET_daily + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.005, 0.98))) +
  plot_layout(design = 
                "AAAAAA
                 BBBBBB
                 CCCCCC") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
ggsave(file.path("figures+tables", "Figure7_TimeseriesPrcWet-DailyData.png"),
       width = 190, height = 120, units = "mm")


(p_r2_precip + theme(plot.tag = element_text(hjust = 1, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.99, 0.98)) +
    p_r2_ET + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.01, 0.98)) +
    p_scatter + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.01, 0.98)) +
    p_time_precip + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.005, 0.98)) +
    p_time_ET + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.005, 0.98))) +
  plot_layout(design = 
                "AABBCC
                 DDDDDD
                 EEEEEE") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
ggsave(file.path("figures+tables", "Figure8_TimeseriesPrcWet-ModelFit.png"),
       width = 190, height = 155, units = "mm")


## ALTERNATE - ALL 8 PANELS IN ONE PLOT
(p_time_STIC + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.04, 0.98)) +
    p_time_precip_daily + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.005, 0.98)) +
    p_time_ET_daily + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.005, 0.98)) +
    p_r2_precip + theme(plot.tag = element_text(hjust = 1, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.99, 0.98)) +
    p_r2_ET + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.01, 0.98)) +
    p_scatter + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.01, 0.98)) +
    p_time_precip + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.005, 0.98)) +
    p_time_ET + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.005, 0.98))) +
  plot_layout(design = 
                "AAAAAA
                 BBBBBB
                 CCCCCC
                 DDEEFF
                 GGGGGG
                 HHHHHH") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
ggsave(file.path("figures+tables", "Figure7_TimeseriesPrcWet-8panelCombined.png"),
       width = 190, height = 240, units = "mm")