# Filename: rollingMerge.R
# Purpose: Rolling merge of hourly weather and messages
# Created: 10/23/14
# Last update: 10/25/14
# by: Nicole Zeng

# NOTE: this should be used after "createWeatherCondtiions.R"

# packages
require(data.table)
require(plyr)
# datasets
load("msg.subset.wk.RData") # msg
load("hourly.weather.wk.RData") # wt
# remove any existing weather data from msg (from first time merge)
drop = c("Rain", "l.Rain", "Cloud", "l.Cloud", "Sunny", "Events", "Conditions", "Humidity", "wt.timestamp", "Rainfall")
msg[, (drop):=NULL]

# set wt keys and colomn order
setkey(wt, station, utc)

# set msg keys and column order
setnames(msg, "timestamp", "utc")
setnames(msg, "Station", "station")
setkey(msg, station, utc)

# merge with weather data now
msg.dt <- wt[msg, roll="nearest"]
