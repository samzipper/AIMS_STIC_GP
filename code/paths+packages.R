## paths+packages.R
# packages
library(tidyverse)
library(sf)
library(patchwork)
library(lubridate)

# path to AIMS STIC data QAQCed folder
path_data <- file.path("G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS", "QA QCed Data", "STIC", "GP", "AIMS_GP_KNZ_approach1_STIC")

## plotting controls
# ggplot theme
windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw(base_size=10, base_family="Arial") + 
    theme(
      text=element_text(color="black"),
      plot.title=element_text(size=rel(1)),
      axis.title=element_text(face="bold", size=rel(1)),
      axis.text=element_text(size=rel(1)),
      strip.text=element_text(size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      legend.text=element_text(size=rel(1)),
      panel.grid=element_blank(),
      plot.margin=unit(c(1,1,1,1), "mm"),
      strip.background=element_blank())
}

theme_set(theme_scz())

## color palettes
# categorical color palette from https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

## OpenET algorithm labels and colors
labs_algorithms <- c("disalexi" = "DisALEXI",
                     "ensemble" = "Ensemble",
                     "ptjpl" = "PT-JPL",
                     "eemetric" = "eeMETRIC",
                     "geesebal" = "geeSEBAL",
                     "sims" = "SIMS",
                     "ssebop" = "SSEBop")

pal_algorithms <- c("disalexi" = "#a6cee3", 
                    "eemetric" = "#1f78b4",
                    "ensemble" = "#e31a1c",
                    "geesebal" = "#b2df8a",
                    "ptjpl" = "#33a02c",
                    "sims" = "#fb9a99",
                    "ssebop" = "#fdbf6f",
                    "Reported" = "black")