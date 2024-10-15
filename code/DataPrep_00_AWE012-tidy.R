## DataPrep_00_AWE012-tidy.R
# This script is intended to load/clean AWE012 (daily meteorology) data.

source(file.path("code", "paths+packages.R"))

# load data
df_raw <- read_csv(file.path("data", "KNZ_AWE01", "AWE012.csv"))

# inspect data
summary(df_raw)
unique(df_raw$WATERSHED)

# create date column
df_raw$Date <- ymd(paste0(df_raw$RECYEAR, "-", df_raw$RECMONTH, "-", df_raw$RECDAY))

# get precip data
df_raw$precip_mm_KNZ <- as.numeric(df_raw$DPPT)

ggplot(df_raw, aes(x = Date, y= precip_mm_KNZ)) + geom_col()

# check for NAs
sum(is.na(df_raw$precip_mm_KNZ[df_raw$Date >= ymd("2021-05-01")]))
# 3 NA values - can gap fill with gridMET for those dates

# save output
df_raw |>
  dplyr::select(Date, precip_mm_KNZ) |> 
  subset(year(Date) >= 2021) |> 
  write_csv(file.path("data", "KNZ_AWE01", "AWE012-tidy.csv"))
