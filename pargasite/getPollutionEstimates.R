
pm_monthly_brick <- brick("pargasite_rasters/Monthly/10km_rasters/pm_monthly_brick_full_10km.tif")
ozone_monthly_brick <- brick("pargasite_rasters/Monthly/10km_rasters/ozone_monthly_brick_full_10km.tif")
no2_monthly_brick <- brick("pargasite_rasters/Monthly/10km_rasters/no2_monthly_brick_full_10km.tif")
so2_monthly_brick <- brick("pargasite_rasters/Monthly/10km_rasters/so2_monthly_brick_full_10km.tif")
co_monthly_brick <- brick("pargasite_rasters/Monthly/10km_rasters/co_monthly_brick_full_10km.tif")

pm_monthly_brick_pr <- brick("pargasite_rasters/Monthly/1km_rasters/pr_pm_monthly_brick_full_1km.tif")
ozone_monthly_brick_pr <- brick("pargasite_rasters/Monthly/1km_rasters/pr_ozone_monthly_brick_full_1km.tif")
no2_monthly_brick_pr <- brick("pargasite_rasters/Monthly/1km_rasters/pr_no2_monthly_brick_full_1km.tif")
so2_monthly_brick_pr <- brick("pargasite_rasters/Monthly/1km_rasters/pr_so2_monthly_brick_full_1km.tif")
co_monthly_brick_pr <- brick("pargasite_rasters/Monthly/1km_rasters/pr_co_monthly_brick_full_1km.tif")

getPollutionEstimates.df.app <- function(data, monthyear_start,
                                         monthyear_end, location) {
  
  month_year_start <- as.numeric(strsplit(monthyear_start, "-")[[1]])
  ind_start <- 12*(month_year_start[2]-1996) + month_year_start[1]
  
  month_year_end <- as.numeric(strsplit(monthyear_end, "-")[[1]])
  ind_end <- 12*(month_year_end[2]-1996) + month_year_end[1]
  
  if(location == "USA"){
    pollutant_bricks <- list(pm_monthly_brick, ozone_monthly_brick,
                             no2_monthly_brick, so2_monthly_brick, co_monthly_brick)
  } 
  else if (location == "PR"){
    pollutant_bricks <- list(pm_monthly_brick_pr, ozone_monthly_brick_pr,
                             no2_monthly_brick_pr, so2_monthly_brick_pr, co_monthly_brick_pr)
  }
  
  
  subset_bricks <- lapply(pollutant_bricks, function(pollutant_brick){
    return(raster::subset(pollutant_brick, c(ind_start:ind_end))) })
  
  data$pm_estimate <- rowMeans(raster::extract(subset_bricks[[1]], cbind(data$Longitude, data$Latitude)))
  data$ozone_estimate <- rowMeans(raster::extract(subset_bricks[[2]], cbind(data$Longitude, data$Latitude)))
  data$no2_estimate <- rowMeans(raster::extract(subset_bricks[[3]], cbind(data$Longitude, data$Latitude)))
  data$so2_estimate <- rowMeans(raster::extract(subset_bricks[[4]], cbind(data$Longitude, data$Latitude)))
  data$co_estimate <- rowMeans(raster::extract(subset_bricks[[5]], cbind(data$Longitude, data$Latitude)))
  
  return(data)
}
