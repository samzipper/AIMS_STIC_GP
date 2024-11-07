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
vars_test <- c("P.ET_mm", "precip_mm", "P.ETo_mm")
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

## FINAL PLOTS:
# R2 distribution
precip_bestLag <- 27

p_r2 <-
  subset(df_all, response == "prc_wet_overall" & variable == "precip_mm") |> 
  pivot_longer(variable) |> 
  ggplot(aes(x = window, y = r2)) + 
  geom_col(color = col.cat.blu, fill = col.cat.blu) +
  geom_vline(xintercept = precip_bestLag, color = col.cat.red) +
  scale_x_continuous(name = "Precipitation window [days]", limits = c(0, 365), expand = c(0,0)) +
  scale_y_continuous(name = "R\u00b2, wet STIC proportion ~\nsummed precipitation",
                     limits = c(0, 0.605), expand = c(0,0))

# plot best relationship
df_join$precip_mm_bestLag <- rollsum(df_join$precip_mm, precip_bestLag, na.pad = TRUE, align = "right")

p_scatter <- 
  ggplot(df_join, aes(y = prc_wet_overall, x = precip_mm_bestLag)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.red) +
  scale_x_continuous(name = "Precipitation [mm], 27-day sum",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Wet STIC proportion",
                     expand = c(0, 0))

p_time_STIC <-
  ggplot(df_join, aes(x = Date, y = prc_wet_overall)) +
  geom_line() +
  scale_x_date(name = "Date [daily]", limits = c(ymd("2021-05-01"), ymd("2024-05-21")), expand = c(0,0), date_minor_breaks = "1 month") +
  scale_y_continuous(name = "Wet STIC\nproportion") +
  theme(axis.title.x = element_blank()) +
  guides(x = guide_axis(minor.ticks = TRUE))

p_time_precip <-
  ggplot(df_join, aes(x = Date, y = precip_mm_bestLag)) +
  geom_line() +
  scale_x_date(name = "Date [daily]", limits = c(ymd("2021-05-01"), ymd("2024-05-21")), expand = c(0,0), date_minor_breaks = "1 month") +
  scale_y_continuous(name = "Precipitation [mm],\n27-day sum") +
  guides(x = guide_axis(minor.ticks = TRUE))

(p_r2 + theme(plot.tag = element_text(hjust = 1, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.99, 0.98)) +
    p_scatter + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.01, 0.98)) +
    p_time_STIC + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.04, 0.98)) +
    p_time_precip + theme(plot.tag = element_text(hjust = 0, vjust = 1), plot.tag.location = "panel", plot.tag.position = c(0.005, 0.98))) +
  plot_layout(design = 
                "AABB
                 CCCC
                 DDDD",
              heights = c(1, 0.75, 0.75)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
ggsave(file.path("figures+tables", "Figure7_TimeseriesPrcWet.png"),
       width = 190, height = 140, units = "mm")

# p_time <-
#   df_join |> 
#   dplyr::select(Date, prc_wet_overall, precip_mm_bestLag) |> 
#   pivot_longer(-Date) |> 
#   ggplot(aes(x = Date, y = value)) +
#   geom_line() +
#   facet_wrap(~name, ncol = 1, scales = "free_y",
#              labeller = as_labeller(c("prc_wet_overall" = "(c) Wet STIC proportion",
#                                       "precip_mm_bestLag" = "(d) Precipitation [mm], 27-day sum"))) +
#   scale_x_date(name = "Date [daily]", expand = c(0,0)) +
#   scale_y_continuous(name = "Value of Variable") +
#   theme(strip.text = element_text(hjust = 0))