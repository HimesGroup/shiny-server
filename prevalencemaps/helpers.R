percent_map <- function(by_mmsa, z) {
  
  mapNames<-location_min_sf$NAME #Makes list of names to assign to map
  matches <- match(location_min_sf$CBSAFP, by_mmsa$MMSA) #makes list of matches
  
  if (z=="Asthma") {
    valsAsthma<-as.numeric(by_mmsa$Asthma_percent[matches])
    nsAsthma <- format(by_mmsa$count[matches],big.mark=",")
  } else if (z=="CHD") {
    valsCHD<-as.numeric(by_mmsa$CHD_percent[matches])
    nsCHD <- format(by_mmsa$count,big.mark=",")[matches]
  } else {
    valsFlushot<-as.numeric(by_mmsa$Flushot_percent[matches])
    nsFlushot <- format(by_mmsa$count,big.mark=",")[matches]
  }
  
  
  if(z=="Asthma"){
    palAsthma <- colorBin("inferno", valsAsthma, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalAsthma <- colorBin("inferno", valsAsthma, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else if (z=="CHD") {
    palCHD <- colorBin("inferno", valsCHD, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalCHD <- colorBin("inferno", valsCHD, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else {
    palFlushot <- colorBin("inferno", valsFlushot, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalFlushot <- colorBin("inferno", valsFlushot, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  }
  
  percents_reformattedAsthma <- rep(0,945)
  percents_reformattedCHD <- rep(0,945)
  percents_reformattedFlushot <- rep(0,945)
  if(z=="Asthma"){
    for(i in c(1:945)){
      percents_reformattedAsthma[i] <- ifelse(!is.na(valsAsthma[i]), paste0(round(valsAsthma[i], 2), "%"), "No data")
    }
  } else if (z=="CHD") {
    for(i in c(1:945)){
      percents_reformattedCHD[i] <- ifelse(!is.na(valsCHD[i]), paste0(round(valsCHD[i], 2), "%"), "No data")
    }
  } else {
    for(i in c(1:945)){
      percents_reformattedFlushot[i] <- ifelse(!is.na(valsFlushot[i]), paste0(round(valsFlushot[i], 2), "%"), "No data")
    }
  }
  
  
  # names_reformatted <- rep(0, 945)
  # for(i in c(1:945)){
  #   names_reformatted[i] <- mapNames[i]
  # }
  
  if(z=="Asthma"){
    mmsa_popup_Asthma <- paste0("<strong>MMSA: </strong>", 
                                mapNames, 
                                "<br><strong>Weighted Asthma Prevalence: </strong>", 
                                percents_reformattedAsthma, "<br><strong>N: </strong>", nsAsthma)
    
  } else if (z=="CHD") {
    mmsa_popup_CHD <- paste0("<strong>MMSA: </strong>", 
                             mapNames, 
                             "<br><strong>Weighted CHD Prevalence: </strong>", 
                             percents_reformattedCHD, "<br><strong>N: </strong>", nsCHD)
  } else {
    mmsa_popup_Flushot <- paste0("<strong>MMSA: </strong>", 
                                 mapNames, 
                                 "<br><strong>Weighted Flushot Prevalence: </strong>", 
                                 percents_reformattedFlushot, "<br><strong>N: </strong>", nsFlushot)
  }
  
  if(z=="Asthma"){
    leaflet(location_min_sf) %>% 
      setView(lng=-94, lat=39, zoom=3) %>%
      addTiles(options = tileOptions(opacity = 1)) %>%
      addPolygons(color=~palAsthma(valsAsthma),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  popup=mmsa_popup_Asthma) %>%
      addPolylines(color = "black",
                   weight = 0.2) %>%
      addLegend("bottomleft", pal = revpalAsthma, 
                values = valsAsthma, 
                na.label = "No data",
                opacity = 1,
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)),
                title = "Prevalence")
  } else if (z=="CHD") {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      setView(lng=-94, lat=39, zoom=3) %>%
      addPolygons(color=~palCHD(valsCHD),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  popup=mmsa_popup_CHD) %>%
      addPolylines(color = "black",
                   weight = 0.2) %>%
      addLegend("bottomleft", pal = revpalCHD, 
                title = "Prevalence",
                values = valsCHD, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)))
  } else {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      setView(lng=-94, lat=39, zoom=3) %>%
      addPolygons(color=~palFlushot(valsFlushot),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  popup=mmsa_popup_Flushot) %>%
      addPolylines(color = "black",
                   weight = 0.2) %>%
      addLegend("bottomleft", pal = revpalFlushot, 
                title = "Prevalence",
                values = valsFlushot, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)))
    
  }
  
}
