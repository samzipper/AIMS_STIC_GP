## Figure6_MapPrcWet.R

## prep workspace
source(file.path("code", "paths+packages.R"))
library(terra)
library(tidyterra)
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

# load other data - watershed boundary, DEM, limestone layers
sf_watershed <- st_read(file.path("data", "Konza_Watershed.shp"))
r_dem <- rast(file.path("data", "Konza_DEM_m.tif")) |> 
  mask(sf_watershed)
df_geology <- read_csv(file.path("data", "Konza_geology.csv")) |> 
  subset(type == "Ls") |> 
  subset(unit %in% c("Morrill Ls", "Crouse Ls"))

# in the DEM, set 1 for any value that is between the bottom_elevation_m and 
# top_elevation_m for any row in df_geology and 0 if it is not 
r_limestone <- r_dem
r_limestone[is.finite(r_limestone)] <- 0

for (i in 1:nrow(df_geology)) {
  r_limestone <- 
    r_limestone + 
    (r_dem >= df_geology$bottom_elevation_m[i] & 
        r_dem <= df_geology$top_elevation_m[i])
}
plot(r_limestone)

## map - overall
p_overall <-
  ggplot() + 
  geom_spatraster(data = r_dem, na.rm = T) +
  scale_fill_viridis_c(name = "Elevation [masl]", na.value = "transparent", option = "G") +
  geom_spatraster(data = r_limestone, aes(alpha = after_stat(value == 1)), fill = "white") +
  scale_alpha_manual(values = c(0, 1), guide = "none") +  # 50% transparent gray where r_limestone == 1
  #geom_sf(data = sf_watershed, color = col.gray, fill = NA) +
  geom_sf(data = sf_streams, color = col.gray) +
  geom_sf(data = left_join(sf_sites, df_stic_overall, by = "siteId"), 
          aes(color = mean_prc_wet), size = 2) +
  scale_color_viridis_c(name = "% of Time Wet",
                        limits = c(0, 1),
                        labels = c("0", "25", "50", "75", "100"),
                        option = "C",
                        direction = -1,
                        na.value = "white") +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         style = north_arrow_orienteering) +
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.title.position = "top") +
  labs(title = "(a) Full Time Period")

# just save p_overall
ggsave(file.path("figures+tables", "Figure6_MapPrcWet-Overall.png"),
       (p_overall + 
          theme(plot.title = element_blank(),
                # remove margin around plot and panel
                plot.margin = margin(0, 0, 0, 0),
                legend.position = "bottom",
                legend.key.width = unit(8, "mm"),
                legend.title = element_text(hjust = 0.5)) +
         guides(color = guide_colorbar(order = 1),
                fill = guide_colorbar(order = 2))),
       width = 95, height = 105, units = "mm")

ggsave(file.path("figures+tables", "Figure6_MapPrcWet-Overall.pdf"),
       (p_overall + 
          theme(plot.title = element_blank(),
                # remove margin around plot and panel
                plot.margin = margin(0, 0, 0, 0),
                legend.position = "bottom",
                legend.key.width = unit(8, "mm"),
                legend.title = element_text(hjust = 0.5)) +
          guides(color = guide_colorbar(order = 1),
                 fill = guide_colorbar(order = 2))),
       width = 95, height = 105, units = "mm", device = cairo_pdf)

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
ggsave(file.path("figures+tables", "Figure6_MapPrcWet-MultiYear.png"),
       width = 150, height = 190, units = "mm")
