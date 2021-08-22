library(lubridate) 

wd <- "C:\\Github\\BookRepo\\ForecastingPrincipal"
harMet_15Min <- read.csv(
  file=paste0(wd,"\\NEONDSMetTimeSeries\\NEON-DS-Met-Time-Series\\HARV\\FisherTower-Met\\hf001-10-15min-m.csv"),
  stringsAsFactors = FALSE)

class(harMet_15Min$datetime)
head(harMet_15Min$datetime)
View(harMet_15Min)

# convert column to date class
dateOnly_HARV <- as.Date(harMet_15Min$datetime)
# view data
head(dateOnly_HARV)

# R - Date-Time - The POSIX classes
tk_index(data_tbl)