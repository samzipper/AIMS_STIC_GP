## DataPrep_00_AET01-tidy.R
# This script is intended to load/clean AET01 data.

source(file.path("code", "paths+packages.R"))

# load raw data
df_raw <- read_csv(file.path("data", "KNZ_AET01", "AET011.csv"))

# create date column
df_raw$Date <- ymd(paste0(df_raw$recyear, "-", df_raw$recmonth, "-", df_raw$recday))

# find NAs in DailyET
which(is.na(df_raw$DailyET))
df_raw$EstimET[which(is.na(df_raw$DailyET))]

ggplot(df_raw, aes(x = Date, y = DailyET)) + geom_line()

# gap-fill missing ETo data with EstimET where available
#  leave other gaps - can use gridMET ETo in these spots
df_raw$ETo_mm_KNZ <- df_raw$DailyET
df_raw$ETo_mm_KNZ[is.na(df_raw$ETo_mm_KNZ)] <- df_raw$EstimET[is.na(df_raw$ETo_mm_KNZ)]

which(is.na(df_raw$ETo_mm_KNZ))

# select output and save
df_raw |> 
  dplyr::select(Date, ETo_mm_KNZ) |> 
  subset(year(Date) >= 2021) |> 
  write_csv(file.path("data", "KNZ_AET01", "AET011-tidy.csv"))
