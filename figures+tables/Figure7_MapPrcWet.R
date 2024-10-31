## Figure7_MapPrcWet.R

## prep workspace
source(file.path("code", "paths+packages.R"))
library(readxl)
library(ggspatial)

## load data
# streamlines
sf_streams <- st_read(file.path("data", "Konza_StreamNetwork.shp"))

# stic data
df_stic <- read_csv(file.path("data", "STIC_Daily_01-Tidy.csv")) |> 
  subset(sublocation == "HS")

# site info - trim to just june (since all metadata used here is time-invariant)
df_sites <- read_xlsx(file.path("data", "ENVI_GP_approach3_20210603_20210812_V1.0.xlsx"),
                      sheet = "Final Data") |> 
  subset(date < ymd("2021-06-15"))

# convert sites to sf
sf_sites <- 
  st_as_sf(df_sites, coords = c("long", "lat"), crs = 4326)

## summarize to overall percent wet
df_stic_overall <- 
  df_stic |> 
  group_by(siteId) |> 
  summarize(mean_prc_wet = mean(prc_wet, na.rm = T),
            n_days = n())

df_stic_2021 <- 
  df_stic |> 
  subset(year(Date) == 2021) |> 
  group_by(siteId) |> 
  summarize(mean_prc_wet = mean(prc_wet, na.rm = T),
            n_days = n()) |> 
  subset(n_days > 225*0.8)

df_stic_2022 <- 
  df_stic |> 
  subset(year(Date) == 2022) |> 
  group_by(siteId) |> 
  summarize(mean_prc_wet = mean(prc_wet, na.rm = T),
            n_days = n()) |> 
  subset(n_days > 365*0.8)

df_stic_2023 <- 
  df_stic |> 
  subset(year(Date) == 2023) |> 
  group_by(siteId) |> 
  summarize(mean_prc_wet = mean(prc_wet, na.rm = T),
            n_days = n()) |> 
  subset(n_days > 365*0.8)

## map - overall
p_overall <-
  ggplot() + 
  geom_sf(data = sf_streams, color = col.cat.blu) +
  geom_sf(data = left_join(sf_sites, df_stic_overall, by = "siteId"), 
          aes(fill = mean_prc_wet), shape = 21, size = 2) +
  scale_fill_viridis_c(name = "% of Time Wet",
                       limits = c(0, 1),
                       labels = scales::percent,
                       option = "C",
                       direction = -1,
                       na.value = "white") +
  annotation_scale(location = "br") +
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.title.position = "top") +
  labs(title = "(a) Full Time Period")

p_2021 <-
  ggplot() + 
  geom_sf(data = sf_streams, color = col.cat.blu) +
  geom_sf(data = left_join(sf_sites, df_stic_2021, by = "siteId"), 
          aes(fill = mean_prc_wet), shape = 21, size = 2) +
  scale_fill_viridis_c(name = "% of Time Wet",
                       limits = c(0, 1),
                       labels = scales::percent,
                       option = "C",
                       direction = -1,
                       na.value = "white") +
  #annotation_scale() +
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.title.position = "top") +
  labs(title = "(b) 2021")

p_2022 <-
  ggplot() + 
  geom_sf(data = sf_streams, color = col.cat.blu) +
  geom_sf(data = left_join(sf_sites, df_stic_2022, by = "siteId"), 
          aes(fill = mean_prc_wet), shape = 21, size = 2) +
  scale_fill_viridis_c(name = "% of Time Wet",
                       limits = c(0, 1),
                       labels = scales::percent,
                       option = "C",
                       direction = -1,
                       na.value = "white") +
  #annotation_scale() +
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.title.position = "top") +
  labs(title = "(b) 2022")

p_2023 <-
  ggplot() + 
  geom_sf(data = sf_streams, color = col.cat.blu) +
  geom_sf(data = left_join(sf_sites, df_stic_2023, by = "siteId"), 
          aes(fill = mean_prc_wet), shape = 21, size = 2) +
  scale_fill_viridis_c(name = "% of Time Wet",
                       limits = c(0, 1),
                       labels = scales::percent,
                       option = "C",
                       direction = -1,
                       na.value = "white") +
  #annotation_scale() +
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.title.position = "top") +
  labs(title = "(b) 2023")

(p_overall + p_2021 + p_2022 + p_2023) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom",
        legend.key.width = unit(10, "mm"),
        legend.title = element_text(hjust = 0.5))
ggsave(file.path("figures+tables", "Figure7_MapPrcWet.png"),
       width = 150, height = 190, units = "mm")
