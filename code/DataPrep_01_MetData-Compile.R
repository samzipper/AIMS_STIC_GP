## DataPrep_01_MetData-Compile.R
# This script is intended to compile meteorological data including precip and ET
# for the period of interest.

source(file.path("code", "paths+packages.R"))

# load datasets
df_openet <- read_csv(file.path("data", "KNZ_OpenET", "KNZ_OpenET_Daily_Combined.csv"))
df_aet <- read_csv(file.path("data", "KNZ_AET01", "AET011-tidy.csv"))
df_awe <- read_csv(file.path("data", "KNZ_AWE01", "AWE012-tidy.csv"))

# join - need to convert openet Date column from dttm to date first
df_openet$Date <- as.Date(df_openet$Date)
df_all <-
  left_join(df_openet, df_aet, by = "Date") |> 
  left_join(df_awe, by = "Date")

# some basic comparisons
ggplot(df_all, aes(x = precip_mm_KNZ, y = precip_mm_gridmet)) + geom_point()
ggplot(df_all, aes(x = ETo_mm_KNZ, y = ETo_mm_gridmet)) + geom_point()

# created compiled precip and ETo columns - use site data first, if not available use gridMET
df_all$precip_mm <- df_all$precip_mm_KNZ
df_all$precip_mm_source <- "KNZ_AWE012"
i_missing_precip <- which(is.na(df_all$precip_mm))
df_all$precip_mm[i_missing_precip] <- df_all$precip_mm_gridmet[i_missing_precip]
df_all$precip_mm_source[i_missing_precip] <- "gridMET_OpenET"

df_all$ETo_mm <- df_all$ETo_mm_KNZ
df_all$ETo_mm_source <- "KNZ_AET01"
i_missing_ETo <- which(is.na(df_all$ETo_mm))
df_all$ETo_mm[i_missing_ETo] <- df_all$ETo_mm_gridmet[i_missing_ETo]
df_all$ETo_mm_source[i_missing_ETo] <- "gridMET_OpenET"

# inspect timeseries
ggplot(df_all, aes(x = Date, y = precip_mm)) + geom_col()
ggplot(df_all, aes(x = Date, y = ETo_mm)) + geom_line() # 2023 data weirdly low?
ggplot(df_all, aes(x = Date)) + 
  geom_line(aes(y = ETo_mm), color = "black") + 
  geom_line(aes(y = ET_mm_ensemble), color = "red")

# save output
df_all |> 
  dplyr::select(Date, precip_mm, precip_mm_source, ETo_mm, ETo_mm_source, ET_mm_ensemble) |> 
  write_csv(file.path("data", "MetData-Compiled.csv"))
