#.libPaths("/home/maya/R/x86_64-pc-linux-gnu-library/3.4/")
#File creation and library download
library(shiny)
library(leaflet)
library(shinyWidgets)
library(ggplot2)
library(shinydashboard)
library(tigris)
library(dplyr)
source("helpers.R")
library(reshape2)
library(rgdal)
library(rgeos)
library(sf)
library(feather)
library(survey)
library(jtools)
library(sjPlot)
library(bbplot)
#write_feather(df, path) and read_feather(path) for quick reading of data

#Getting the sf object to plot and match with BRFSS data
regions_min_sf<-st_read("../../main_database/prevalencemaps/region_min_sf.shp")
location_min_sf<-st_read("../../main_database/prevalencemaps/location_min_sf.shp")

#Loading the names of the MMSAs for matching between BRFSS data and census shapefile data
mmsa_names<-read_feather("../../main_database/prevalencemaps/mmsa_names.feather")
polynamesnumbers<-read_feather("../../main_database/prevalencemaps/polynamesnumbers.feather")

#Reading in data for geographical plotting
weighted_current_asthma_prev <- read_feather("../../main_database/prevalencemaps/weighted_current_asthma_w_counts_leaflet.feather")
weighted_current_asthma_prev$Asthma_percent <- weighted_current_asthma_prev$asthnow*100

#Reading in data for MMSA-stratifying graphical plotting
weighted_current_vars <- read_feather("../../main_database/prevalencemaps/weighted_current_variables_asthma_leaflet.feather")
colnames(weighted_current_vars) <- c("MMSA", "YEAR", "Asthma", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                     "Male", "Female", "White", "Asian/Pacific Islander",
                                     "Black", "Hispanic", "American Indian/Alaskan Native",
                                     "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grade 2 Obese", "Grade 3 Obese",
                                     "Never smoked", "Former smoker", "Current smoker",
                                     "Less than high school", "High school", "Some college or more",
                                     "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

weighted_current_chd_prev <- read_feather("../../main_database/prevalencemaps/weighted_current_chd_w_counts_leaflet.feather")
weighted_current_chd_prev$CHD_percent <- weighted_current_chd_prev$CHD*100

weighted_current_varschd <- read_feather("../../main_database/prevalencemaps/weighted_current_variables_chd_leaflet.feather")
colnames(weighted_current_varschd) <- c("MMSA", "YEAR", "CHD", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                        "Male", "Female", "White", "Asian/Pacific Islander",
                                        "Black", "Hispanic", "American Indian/Alaskan Native",
                                        "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grade 2 Obese", "Grade 3 Obese",
                                        "Never smoked", "Former smoker", "Current smoker",
                                        "Less than high school", "High school", "Some college or more",
                                        "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

weighted_current_flushot_prev <- read_feather("../../main_database/prevalencemaps/weighted_current_Flushot_w_counts_leaflet.feather")
weighted_current_flushot_prev$Flushot_percent <- weighted_current_flushot_prev$Flushot*100

weighted_current_varsflushot <- read_feather("../../main_database/prevalencemaps/weighted_current_variables_Flushot_leaflet.feather")
colnames(weighted_current_varsflushot) <- c("MMSA", "YEAR", "`Flushot Administration`", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                            "Male", "Female", "White", "Asian/Pacific Islander",
                                            "Black", "Hispanic", "American Indian/Alaskan Native",
                                            "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grade 2 Obese", "Grade 3 Obese",
                                            "Never smoked", "Former smoker", "Current smoker",
                                            "Less than high school", "High school", "Some college or more",
                                            "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

weighted_current_diabetes_prev <- read_feather("../../main_database/prevalencemaps/weighted_current_Diabetes_w_counts_leaflet.feather")
weighted_current_diabetes_prev$Diabetes_percent <- weighted_current_diabetes_prev$Diabetes*100

weighted_current_varsdiabetes <- read_feather("../../main_database/prevalencemaps/weighted_current_variables_Diabetes_leaflet.feather")
colnames(weighted_current_varsdiabetes) <- c("MMSA", "YEAR", "Diabetes", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                            "Male", "Female", "White", "Asian/Pacific Islander",
                                            "Black", "Hispanic", "American Indian/Alaskan Native",
                                            "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grade 2 Obese", "Grade 3 Obese",
                                            "Never smoked", "Former smoker", "Current smoker",
                                            "Less than high school", "High school", "Some college or more",
                                            "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

weighted_current_SRH_prev <- read_feather("../../main_database/prevalencemaps/weighted_current_SRH_w_counts_leaflet.feather")
weighted_current_SRH_prev$SRH_percent <- weighted_current_SRH_prev$SRH*100

weighted_current_varsSRH <- read_feather("../../main_database/prevalencemaps/weighted_current_variables_SRH_leaflet.feather")
colnames(weighted_current_varsSRH) <- c("MMSA", "YEAR", "`Good or Better Health`", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                        "Male", "Female", "White", "Asian/Pacific Islander",
                                        "Black", "Hispanic", "American Indian/Alaskan Native",
                                        "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grade 2 Obese", "Grade 3 Obese",
                                        "Never smoked", "Former smoker", "Current smoker",
                                        "Less than high school", "High school", "Some college or more",
                                        "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

weighted_current_HC_prev <- read_feather("../../main_database/prevalencemaps/weighted_current_HC_w_counts_leaflet.feather")
weighted_current_HC_prev$HC_percent <- weighted_current_HC_prev$HC*100

weighted_current_varsHC <- read_feather("../../main_database/prevalencemaps/weighted_current_variables_HC_leaflet.feather")
colnames(weighted_current_varsHC) <- c("MMSA", "YEAR", "`Has Health Insurance`", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                        "Male", "Female", "White", "Asian/Pacific Islander",
                                        "Black", "Hispanic", "American Indian/Alaskan Native",
                                        "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grade 2 Obese", "Grade 3 Obese",
                                        "Never smoked", "Former smoker", "Current smoker",
                                        "Less than high school", "High school", "Some college or more",
                                        "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

weighted_current_DEP_prev <- read_feather("../../main_database/prevalencemaps/weighted_current_DEP_w_counts_leaflet.feather")
weighted_current_DEP_prev$DEP_percent <- weighted_current_DEP_prev$DEP*100

weighted_current_varsDEP <- read_feather("../../main_database/prevalencemaps/weighted_current_variables_DEP_leaflet.feather")
colnames(weighted_current_varsDEP) <- c("MMSA", "YEAR", "`Depressive Disorder`", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                       "Male", "Female", "White", "Asian/Pacific Islander",
                                       "Black", "Hispanic", "American Indian/Alaskan Native",
                                       "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grade 2 Obese", "Grade 3 Obese",
                                       "Never smoked", "Former smoker", "Current smoker",
                                       "Less than high school", "High school", "Some college or more",
                                       "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

weighted_current_COPD_prev <- read_feather("../../main_database/prevalencemaps/weighted_current_COPD_w_counts_leaflet.feather")
weighted_current_COPD_prev$COPD_percent <- weighted_current_COPD_prev$COPD*100

weighted_current_varsCOPD <- read_feather("../../main_database/prevalencemaps/weighted_current_variables_COPD_leaflet.feather")
colnames(weighted_current_varsCOPD) <- c("MMSA", "YEAR", "COPD", "<$25,000", "$25,000-$75,000", ">$75,000", 
                                        "Male", "Female", "White", "Asian/Pacific Islander",
                                        "Black", "Hispanic", "American Indian/Alaskan Native",
                                        "Not overweight or obese", "Overweight", "Grade 1 Obese", "Grade 2 Obese", "Grade 3 Obese",
                                        "Never smoked", "Former smoker", "Current smoker",
                                        "Less than high school", "High school", "Some college or more",
                                        "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

weighted_current_YNSMOKE_prev <- read_feather("../../main_database/prevalencemaps/weighted_current_YNSMOKE_w_counts_leaflet.feather")
weighted_current_YNSMOKE_prev$YNSMOKE_percent <- weighted_current_YNSMOKE_prev$YNSMOKE*100

weighted_current_bmin_prev <- read_feather("../../main_database/prevalencemaps/weighted_current_bmin_w_counts_leaflet.feather")
weighted_current_bmin_prev$`Average BMI` <- weighted_current_bmin_prev$BMI_AVE

weighted_current_adi_prev <- read_feather("../../main_database/prevalencemaps/weighted_current_adi_w_counts_leaflet.feather")
weighted_current_adi_prev$`Average ADI` <- weighted_current_adi_prev$ADI_AVE

#Reading in data for use in bivariate, multivariate, and regional graphs.
current.all<-readRDS("../../main_database/prevalencemaps/current_all.RDS")
current.all <- current.all %>% dplyr::mutate(`ADI Quartile` = as.factor(paste0("Q",ntile(ADI, 4))))
