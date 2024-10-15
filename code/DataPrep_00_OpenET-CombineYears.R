## DataPrep_00_OpenET-CombineYears.R
# This script is intended to read in daily OpenET files for each year and 
# combine to produce a single output including all years.

source(file.path("code", "paths+packages.R"))

# list of OpenET files
f_names <- list.files(file.path("data", "KNZ_OpenET"), pattern = "*.csv")

# loop and load
for (f in f_names){
  df_f <- read_csv(file.path("data", "KNZ_OpenET", f))
  
  if (f == f_names[1]){
    df_all <- df_f
  } else {
    df_all <- bind_rows(df_all, df_f)
  }
}

# rename, organize for output
df_out <- 
  rename(df_all,
       all_of(c(Date = "DateTime", 
         ET_mm_ensemble = "Ensemble ET", 
         NDVI = "NDVI", 
         ETo_mm_gridmet = "Reference ET (ETo)", 
         precip_mm_gridmet = "Precip (gridMET)", 
         ET_mm_eemetric = "eeMetric ET", 
         ET_mm_ssebop = "SSEBop ET", 
         ET_mm_sims = "SIMS ET", 
         ET_mm_ptjpl = "PT-JPL ET", 
         ET_mm_disalexi = "DisALEXI ET", 
         ET_mm_geesebal = "geeSEBAL ET", 
         junk = "...12"))) |> 
  dplyr::select(Date, starts_with("ET_mm"), precip_mm_gridmet, ETo_mm_gridmet, NDVI)

# inspect
ggplot(df_out, aes(x = Date, y = ETo_mm_gridmet)) + geom_line()
ggplot(df_out, aes(x = Date, y = ET_mm_ensemble)) + geom_line()
ggplot(df_out, aes(x = Date, y = NDVI)) + geom_line()
ggplot(df_out, aes(x = Date, y = precip_mm_gridmet)) + geom_line()

# save output
write_csv(df_out, file.path("data", "KNZ_OpenET", "KNZ_OpenET_Daily_Combined.csv"))
