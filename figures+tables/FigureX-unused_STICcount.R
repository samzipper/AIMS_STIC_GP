## FigureX-unused_STICcount.R

# prep workspace
source(file.path("code", "paths+packages.R"))

# load tidied data
df_stic <- read_csv(file.path("data", "STIC_Daily_01-Tidy.csv"))

# summarize to daily
df_count <- 
  df_stic |> 
  group_by(Date) |> 
  summarize(n_stic = n(),
            n_fairOrBetter = sum(prc_FairOrBetter > 0.5)) |> 
  pivot_longer(starts_with("n_"), values_to = "count", names_to = "metric")

# plot
ggplot(df_count, aes(x = Date, y = count, color = metric)) +
  geom_line() +
  scale_color_manual(name = NULL, 
                     labels = c("n_fairOrBetter" = "Fair or Better Quality",
                                "n_stic" = "Total Active"),
                     values = c("n_fairOrBetter" = col.cat.blu,
                                "n_stic" = "black")) +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(name = "STIC Count", limits = c(0, 51), expand = c(0,0)) +
  theme(legend.position = c(0.01, 0.01),
        legend.justification = c(0, 0)) +
  guides(color = guide_legend(reverse = TRUE))
ggsave(file.path("figures+tables", "FigureX-unused_STICcount.png"),
       width = 95, height = 95, units = "mm")
