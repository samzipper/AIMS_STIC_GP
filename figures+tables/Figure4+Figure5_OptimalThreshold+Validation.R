## Figure4+5_OptimalThreshold+Validation.R

# prep workspace
source(file.path("code", "paths+packages.R"))

# read in validation data
df_validate <- read_csv(file.path("data", "STIC_KNZ-Validation.csv"))

## Figure 5: optimal threshold
df_classificationAccuracy <-
  tibble(condUncal_thres = seq(1e2, 1e5, 100),
         accuracy_prc = NA,
         wetAsDry_prc = NA,
         dryAsWet_prc = NA)
for (c in 1:length(df_classificationAccuracy$condUncal_thres)){
  cu <- df_classificationAccuracy$condUncal_thres[c]
  
  # overall accuracy
  df_classificationAccuracy$accuracy_prc[c] <- 
    (sum(df_validate$condUncal_STIC < cu & df_validate$wetdry_obs == "dry") +
       sum(df_validate$condUncal_STIC >= cu & df_validate$wetdry_obs == "wet")) / 
    sum(is.finite(df_validate$condUncal_STIC))
  
  # percent of wet observations misclassified as dry
  df_classificationAccuracy$wetAsDry_prc[c] <- 
    sum(df_validate$condUncal_STIC < cu & df_validate$wetdry_obs == "wet") / 
    sum(df_validate$wetdry_obs == "wet")
  
  # percent of dry observations misclassified as wet
  df_classificationAccuracy$dryAsWet_prc[c] <- 
    sum(df_validate$condUncal_STIC >= cu & df_validate$wetdry_obs == "dry") / 
    sum(df_validate$wetdry_obs == "dry")
}

df_classificationAccuracy$errorBalance <- 
  abs(df_classificationAccuracy$wetAsDry_prc - df_classificationAccuracy$dryAsWet_prc)

# choose optimal condUncal threshold
condUncal_thres <- df_classificationAccuracy$condUncal_thres[which.min(df_classificationAccuracy$errorBalance)]
df_classificationAccuracy |> 
  dplyr::select(-errorBalance) |> 
  pivot_longer(-condUncal_thres) |> 
  ggplot(aes(x = condUncal_thres, y = value, color = name)) +
  geom_vline(xintercept = condUncal_thres, color = col.gray) +
  geom_line() +
  scale_x_log10(name = "condUncal Threshold",
                limits = c(1e2, 50000),
                minor_breaks = c(seq(100, 1000, 100),
                                 seq(1000, 10000, 1000),
                                 seq(10000, 50000, 10000)),
                expand = c(0,0)) +
  scale_y_continuous(name = "Classification Accuracy", labels = scales::percent,
                     limits = c(0,1.005), expand = c(0,0)) +
  scale_color_manual(name = NULL,
                     values = c("accuracy_prc" = "black", 
                                "dryAsWet_prc" = col.cat.blu,
                                "wetAsDry_prc" = col.cat.red),
                     labels = c("accuracy_prc" = "Overall accuracy", 
                                "dryAsWet_prc" = "Dry misclassified as wet",
                                "wetAsDry_prc" = "Wet misclassified as dry")) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical") +
  guides(x = guide_axis(minor.ticks = TRUE),
         color = guide_legend())
ggsave(file.path("figures+tables", "Figure4_OptimalThreshold.png"),
       width = 95, height = 110, units = "mm")

## Figure 6: validation performance
# plot confusion matrix
df_confusion <- 
  df_validate |> 
  group_by(wetdry_obs, wetdry_STIC) |> 
  summarize(count = n())

accuracy <- 
  (df_confusion$count[df_confusion$wetdry_obs=="wet" & df_confusion$wetdry_STIC=="wet"] +
     df_confusion$count[df_confusion$wetdry_obs=="dry" & df_confusion$wetdry_STIC=="dry"])/sum(df_confusion$count)

p_accuracy <-
  ggplot(df_confusion, aes(x = wetdry_STIC, y = wetdry_obs, fill = count)) +
  geom_raster() +
  geom_text(aes(label = count)) +
  scale_x_discrete(name = "Classified STIC Data", expand = c(0,0), limits = rev,
                   labels = c("wet" = "Wet", "dry" = "Dry")) +
  scale_y_discrete(name = "Field Observation", expand = c(0,0),
                   labels = c("wet" = "Wet", "dry" = "Dry")) +
  scale_fill_gradient(name = "Count", low = "gray85", high = "#0082c8", 
                      limits = c(0, max(df_confusion$count))) +
  labs(title = paste0("(a) Overall accuracy = ", round(100*accuracy, 1), "%")) +
  theme(legend.position = "bottom")


# SpC comparison
SpC_min <- min(c(df_validate$SpC_obs, df_validate$SpC_STIC), na.rm = T)
SpC_max <- max(c(df_validate$SpC_obs, df_validate$SpC_STIC), na.rm = T)

# make a column to check if there is an "O" in STIC QAQC
df_validate$QAQC_O <- grepl("O", df_validate$QAQC_STIC, fixed = T) | grepl("C", df_validate$QAQC_STIC, fixed = T)

p_SpC <-
  ggplot(df_validate, aes(x = SpC_obs, y = SpC_STIC, color = QAQC_O, shape = QAQC_O)) +
  geom_abline(intercept = 0, slope = 1, color = "gray65") +
  geom_point() +
  scale_x_continuous(name = "Measured SpC [µS/cm]", 
                     limits = c(SpC_min, SpC_max),
                     expand = c(0,0)) +
  scale_y_continuous(name = "STIC SpC [µS/cm]",
                     limits = c(SpC_min, SpC_max),
                     expand = expansion(mult = c(0.0,0.01))) +
  scale_shape_manual(name = "SpC within\ncalibration\nrange?",
                     values = c("FALSE" = 16, "TRUE" = 1),
                     labels = c("FALSE" = "Yes", "TRUE" = "No")) +
  scale_color_manual(name = "SpC within\ncalibration\nrange?",
                     values = c("FALSE" = "black", "TRUE" = col.gray),
                     labels = c("FALSE" = "Yes", "TRUE" = "No")) +
  labs(title = "(b) SpC comparison") +
  theme(legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0))

(p_accuracy + p_SpC) +
  plot_layout(ncol = 1) +
  theme(plot.tag = element_text(face = "plain"))
ggsave(file.path("figures+tables", "Figure5_Validation.png"),
       width = 95, height = 190, units = "mm")

# get fit stats
lm(SpC_STIC ~ SpC_obs, df_validate) |> summary()
lm(SpC_STIC ~ SpC_obs, subset(df_validate, !QAQC_O)) |> summary()
