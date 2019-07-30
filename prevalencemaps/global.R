#File creation and library download
library(shiny)
library(leaflet)
library(shinyWidgets)
library(ggplot2)
library(shinydashboard)
library(tigris)
library(dplyr)
source("/srv/shiny-server/prevalencemaps/helpers.R")
library(reshape2)
library(rgdal)
library(rgeos)
library(sf)
library(feather)
library(survey)
library(broom)
library(jtools)
#write_feather(df, path) and read_feather(path) for quick reading of data

##getting the sf object to plot and match with brfss data
# location<-readOGR("C:/Users/llesz/Desktop/tl_2017_us_cbsa/tl_2017_us_cbsa.shp")
# location_min<-gSimplify(location, tol=0.01, topologyPreserve=TRUE)
# location_min = SpatialPolygonsDataFrame(location_min, data=location@data)
# location_min_sf <- st_as_sf(location_min, sf_column_name = )
# st_write(location_min_sf, "C:/Users/llesz/Documents/diseasemaps/databases/location_min_sf.shp")
location_min_sf<-st_read("C:/Users/llesz/Documents/diseasemaps/databases/location_min_sf.shp")

#getting names of MMSAs as options
weighted_asthma_prev <- read_feather("/srv/shiny-server/databases/prevalencemaps/weighted_current_asthma_w_counts_leaflet.feather")
mmsas <- filter(weighted_asthma_prev, YEAR == "2007-2017 (all years)")
matches <- match(mmsas$MMSA, location_min_sf$CBSAFP)
namatches<-matches[!is.na(matches)]
polynames <- as.character(location_min_sf[namatches,]$NAME)
polynames_final <- data.frame(polynames[!is.na(polynames)])
write_feather(polynames_final, "/srv/shiny-server/databases/prevalencemaps/mmsa_names.feather")
mmsa_names <- read_feather("/srv/shiny-server/databases/prevalencemaps/mmsa_names.feather")
mmsa_names$x <- as.character(mmsa_names$polynames..is.na.polynames..)
polynamesnumbers<-cbind(mmsa_names, location_min_sf[namatches,]$CBSAFP)
polynamesnumbers$CBSAFP<-polynamesnumbers$`location_min_sf[namatches, ]$CBSAFP`

weighted_current_asthma_prev <- read_feather("/srv/shiny-server/databases/prevalencemaps/weighted_current_asthma_w_counts_leaflet.feather")
weighted_current_asthma_prev$Asthma_percent <- weighted_current_asthma_prev$asthnow*100

weighted_current_vars <- read_feather("/srv/shiny-server/databases/prevalencemaps/weighted_current_variables_asthma_leaflet.feather")
colnames(weighted_current_vars) <- c("MMSA", "YEAR", "Asthma", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                     "Male", "Female", "White", "Asian/Pacific Islander",
                                     "Black", "Hispanic", "American Indian/Alaskan Native",
                                     "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grades 2 & 3 Obese",
                                     "Never smoked", "Former smoker", "Current smoker",
                                     "Less than high school", "High school", "Some college or more",
                                     "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

weighted_current_chd_prev <- read_feather("/srv/shiny-server/databases/prevalencemaps/weighted_current_chd_w_counts_leaflet.feather")
weighted_current_chd_prev$CHD_percent <- weighted_current_chd_prev$CHD*100

weighted_current_varschd <- read_feather("/srv/shiny-server/databases/prevalencemaps/weighted_current_variables_chd_leaflet.feather")
colnames(weighted_current_varschd) <- c("MMSA", "YEAR", "CHD", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                        "Male", "Female", "White", "Asian/Pacific Islander",
                                        "Black", "Hispanic", "American Indian/Alaskan Native",
                                        "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grades 2 & 3 Obese",
                                        "Never smoked", "Former smoker", "Current smoker",
                                        "Less than high school", "High school", "Some college or more",
                                        "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

weighted_current_flushot_prev <- read_feather("/srv/shiny-server/databases/prevalencemaps/weighted_current_Flushot_w_counts_leaflet.feather")
weighted_current_flushot_prev$Flushot_percent <- weighted_current_flushot_prev$Flushot*100

weighted_current_varsflushot <- read_feather("/srv/shiny-server/databases/prevalencemaps/weighted_current_variables_flushot_leaflet.feather")
colnames(weighted_current_varsflushot) <- c("MMSA", "YEAR", "Flushot", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                        "Male", "Female", "White", "Asian/Pacific Islander",
                                        "Black", "Hispanic", "American Indian/Alaskan Native",
                                        "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grades 2 & 3 Obese",
                                        "Never smoked", "Former smoker", "Current smoker",
                                        "Less than high school", "High school", "Some college or more",
                                        "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

current.all <- read_feather("/srv/shiny-server/databases/prevalencemaps/12.11.asthnow_final.feather")
colnames(current.all)[colnames(current.all)=="FLUSHOT"] <- "Flushot"
colnames(current.all)[colnames(current.all)=="CHD"] <- "CHD"
colnames(current.all)[colnames(current.all)=="ASTHNOW"] <- "Asthma"
colnames(current.all)[colnames(current.all)=="INCOME"] <- "Income"
colnames(current.all)[colnames(current.all)=="RACE"] <- "Race"
colnames(current.all)[colnames(current.all)=="SEX"] <- "Sex"
colnames(current.all)[colnames(current.all)=="SMOKER"] <- "Smoker"
colnames(current.all)[colnames(current.all)=="EDUCA"] <- "Education"
current.all$Asthma<-factor(current.all$Asthma, levels=c("0","1"), labels=c("No", "Yes"))
current.all$CHD<-factor(current.all$CHD, levels=c("0","1"), labels=c("No", "Yes"))
current.all$Income=factor(current.all$Income,levels(current.all$Income)[c(1,3,2)])
current.all$BMI=factor(current.all$BMI,levels(current.all$BMI)[c(1,5,2,3,4)])
current.all$Education=factor(current.all$Education,levels(current.all$Education)[c(2,3,1)])
current.all$Smoker=factor(current.all$Smoker,levels(current.all$Smoker)[c(1,3,2)])
levels(current.all$Income)<-c("More than $75,000","$25,000 to $75,000","Less than $25,000")
levels(current.all$BMI)<-c("Normal","Overweight","Grade 1 Obesity","Grade 2 Obesity","Grade 3 Obesity")
levels(current.all$Education)<-c("No High School","Some Higher Education/High School","Higher Education")
levels(current.all$Smoker)<-c("Never Smoked","Former Smoker","Current Smoker")
levels(current.all$Race)<-c("White","Asian/Pacific Islander","Black","Hispanic","Native American")
levels(current.all$Sex)<-c("Male","Female")
current.all2007_2017<-filter(current.all, YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015|YEAR==2016|YEAR==2017)
current.all2007_2010<-filter(current.all, YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010)
current.all2011_2017<-filter(current.all, YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015|YEAR==2016|YEAR==2017)

des2007_2017<-svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(MMSAWT), data=current.all2007_2017)
des2007_2010<-svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(MMSAWT), data=current.all2007_2010)
des2011_2017<-svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(MMSAWT), data=current.all2011_2017)
