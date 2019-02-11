library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggmap)
library(grDevices)
library(shinyWidgets)
library(grid)
library(gridExtra)
library(gtable)
library(leaflet)
library(raster)
library(shinyjs)
library(mapview)
library(data.table)

app.data <- read.csv("./databases/all_data.csv", header = FALSE, quote = "", stringsAsFactors = F)
app.data <- data.table::setnames(app.data, old = colnames(app.data), new = gsub("\"", "", as.character(app.data[1,])))[c(2:397789,397791:nrow(app.data)),]
#Necessary to do this this way for R to read in the full data frame
#Doing so messes with the titles though, so this is the way to fix it
#Also removes problematic row (397790)
 
app.data[,c(2,3,5:9)] <- sapply(app.data[,c(2,3,5:9)], as.numeric)
app.data$Sensor.ID <- as.factor(gsub("\"", "", app.data$Sensor.ID))

app.data$Timestamp <- force_tz(ymd_hms(app.data$Timestamp), tz = "America/New_York") #Reads in timestamp as character, so converts to POSIXct

app.data <- mutate(app.data, Day = date(Timestamp), Time = strftime(Timestamp, format = "%H:%M"), Count = 1) 
#Creates two new columns for later subsetting of data by date and by time, as well as one column for measurement density count

mins <- unique(strftime(force_tz(as_datetime(ymd_hm("1970-01-01 00:00") : ymd_hm("1970-01-01 23:59")), tz = "America/New_York"), format = "%H:%M"))
mins.5 <- unique(strftime(floor_date(force_tz(as_datetime(ymd_hm("1970-01-01 00:00") : ymd_hm("1970-01-01 23:59")), tz = "America/New_York"), unit = "5 min"), format = "%H:%M"))
mins.5 <- c(mins.5, "23:59")
#Creates 2 lists of times from 0:00 to 23:59, one by the minute and one by 5 minute intervals, given as strings
#5 minute intervals will be used on slider
#1 minute interval list will be indexed by a "grep" function using the 5 minute interval choices

df.list.2 <- list("Temperature", "Humidity", "PM1", "PM2.5", "PM10")

titles.list <- list( "Avg. Temp. (\u00B0C)", "Avg. Humidity (%)", "Avg. PM1 Conc. (\u03BCg/m\u00B3)", "Avg. PM2.5 Conc. (\u03BCg/m\u00B3)", "Avg. PM10 Conc. (\u03BCg/m\u00B3)")

abbrs.list <- list("Temp.", "Humidity", "PM1", "PM2.5", "PM10")

titles.df <- data.frame(cbind(df.list.2, titles.list, abbrs.list))

f.titles <- function(y){
  
  if(y %in% df.list.2){  
    index <- which(titles.df[,1] == y)
    return(titles.df[index, 2])
  }
  
  else if(length(grep(".mdb", y)) > 0){
    index <- grep(gsub(".mdb", "", y), titles.df[,1])
    return(paste("log # of", titles.df[index, 3], "Data Points"))
  }
  
} #End f.titles

f.units <- function(z){
  index <- grep(paste(z), titles.df[,1])
  return(paste(titles.df[index, 3]))
}

our.sensors <- read.csv("./databases/LIMEA_AIRBEAM_SUMMARY.csv", header = TRUE, stringsAsFactors = FALSE)$AirBeamID[1:15]
our.sensors <- paste0("AirBeam:", our.sensors)

sensor.names <- names(table(app.data$Sensor.ID))