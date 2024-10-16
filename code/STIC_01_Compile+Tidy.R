## STIC_01_Compile+Tidy.R
# This script is intended to compile the STIC data for the sublocations of interest (HS and SW [DUP] only),
# get rid of unnecessary columns, convert to local datetime, and otherwise tidy data.

source(file.path("code", "paths+packages.R"))

# load all STIC data - HS and DUP only
stic_file_list <- fs::dir_ls(path_data, regexp = "\\.csv$")
stic_hs_list <- stic_file_list[str_sub(stic_file_list, -11, -10) %in% c("HS", "SW")]
df_all_raw <- bind_rows(lapply(stic_hs_list, read_csv, col_types = "cTccccnncnccc"))

head(df_all_raw)
length(unique(df_all_raw$siteId))
unique(df_all_raw$sublocation)

# clean/tidy
df_out <-
  df_all_raw |> 
  # create local datetime and Date column - since OpenET and met data are daily at local time
  mutate(datetime_CST = with_tz(datetime, "America/Chicago"),
         Date = as.Date(datetime_CST)) |> 
  # get rid of unnecessary columns
  dplyr::select(datetime_CST, Date, siteId, sublocation, wetdry, qual_rating)

# summarize to daily
df_d <-
  df_out |> 
  group_by(Date, siteId, sublocation) |> 
  summarize(prc_wet = sum(wetdry == 1)/n(),
            prc_FairOrBetter = sum(qual_rating %in% c("excellent", "good", "fair"))/n())

ggplot(df_d, aes(x = Date, y = siteId, fill = prc_wet)) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1)

ggplot(df_d, aes(x = Date, y = siteId, fill = prc_FairOrBetter)) +
  geom_tile() +
  scale_fill_viridis_c(option = "E", direction = -1)

# save daily output
write_csv(df_d, file.path("data", "STIC_Daily_01-Tidy.csv"))
