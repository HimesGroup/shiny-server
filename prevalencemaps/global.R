.libPaths("/home/maya/R/x86_64-pc-linux-gnu-library/3.4/")
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
library(jtools)
#write_feather(df, path) and read_feather(path) for quick reading of data

##getting the sf object to plot and match with brfss data
location_min_sf<-st_read("/srv/shiny-server/databases/prevalencemaps/location_min_sf.shp")

mmsa_names<-read_feather("/srv/shiny-server/databases/prevalencemaps/mmsa_names.feather")
polynamesnumbers<-read_feather("/srv/shiny-server/databases/prevalencemaps/polynamesnumbers.feather")

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

weighted_current_varsflushot <- read_feather("/srv/shiny-server/databases/prevalencemaps/weighted_current_variables_Flushot_leaflet.feather")
colnames(weighted_current_varsflushot) <- c("MMSA", "YEAR", "Flushot", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                            "Male", "Female", "White", "Asian/Pacific Islander",
                                            "Black", "Hispanic", "American Indian/Alaskan Native",
                                            "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grades 2 & 3 Obese",
                                            "Never smoked", "Former smoker", "Current smoker",
                                            "Less than high school", "High school", "Some college or more",
                                            "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

current.all<-readRDS("/srv/shiny-server/databases/prevalencemaps/current_all.RDS")
current.all2007_2017<-readRDS("/srv/shiny-server/databases/prevalencemaps/current_all2007_2017.RDS")
current.all2007_2010<-readRDS("/srv/shiny-server/databases/prevalencemaps/current_all2007_2010.RDS")
current.all2011_2017<-readRDS("/srv/shiny-server/databases/prevalencemaps/current_all2011_2017.RDS")
