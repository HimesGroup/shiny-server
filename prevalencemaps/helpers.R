percent_map <- function(by_mmsa, z) {
  
  mapNames<-location_min_sf$NAME #Makes list of names to assign to map
  matches <- match(location_min_sf$CBSAFP, by_mmsa$MMSA) #makes list of matches
  
  if (z=="Asthma") {
    valsAsthma<-as.numeric(by_mmsa$Asthma_percent[matches])
    nsAsthma <- format(by_mmsa$count[matches],big.mark=",")
  } else if (z=="CHD") {
    valsCHD<-as.numeric(by_mmsa$CHD_percent[matches])
    nsCHD <- format(by_mmsa$count,big.mark=",")[matches]
  } else if (z=="COPD") {
    valscopd<-as.numeric(by_mmsa$COPD_percent[matches])
    nscopd <- format(by_mmsa$count,big.mark=",")[matches]
  } else if (z=="`Has Health Insurance`") {
    valshc<-as.numeric(by_mmsa$HC_percent[matches])
    nshc <- format(by_mmsa$count,big.mark=",")[matches]
  } else if (z=="`Depressive Disorder`") {
    valsdep<-as.numeric(by_mmsa$DEP_percent[matches])
    nsdep <- format(by_mmsa$count,big.mark=",")[matches]
  } else if (z=="Smokes") {
    valssmoke<-as.numeric(by_mmsa$YNSMOKE_percent[matches])
    nssmoke <- format(by_mmsa$count,big.mark=",")[matches]
  } else if (z=="Diabetes") {
    valsDiabetes<-as.numeric(by_mmsa$Diabetes_percent[matches])
    nsDiabetes <- format(by_mmsa$count,big.mark=",")[matches]
  } else if (z=="`Good or Better Health`") {
    valsSRH<-as.numeric(by_mmsa$SRH_percent[matches])
    nsSRH <- format(by_mmsa$count,big.mark=",")[matches]
  } else if (z=="AVERAGE_BMI") {
    valsbmin<-as.numeric(by_mmsa$AVERAGE_BMI[matches])
    nsbmin <- format(by_mmsa$count,big.mark=",")[matches]
  } else if (z=="AVERAGE_ADI") {
    valsadi<-as.numeric(by_mmsa$AVERAGE_ADI[matches])
    nsadi <- format(by_mmsa$count,big.mark=",")[matches]
  } else {
    valsFlushot<-as.numeric(by_mmsa$Flushot_percent[matches])
    nsFlushot <- format(by_mmsa$count,big.mark=",")[matches]
  }
  
  
  if(z=="Asthma"){
    palAsthma <- colorBin("OrRd", valsAsthma, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalAsthma <- colorBin("OrRd", valsAsthma, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else if (z=="CHD") {
    palCHD <- colorBin("OrRd", valsCHD, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalCHD <- colorBin("OrRd", valsCHD, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else if (z=="COPD") {
    palCOPD <- colorBin("OrRd", valscopd, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalCOPD <- colorBin("OrRd", valscopd, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else if (z=="Diabetes") {
    palDiabetes <- colorBin("OrRd", valsDiabetes, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalDiabetes <- colorBin("OrRd", valsDiabetes, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else if (z=="AVERAGE_BMI") {
    palbmin <- colorBin("OrRd", valsbmin, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalbmin <- colorBin("OrRd", valsbmin, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else if (z=="AVERAGE_ADI") {
    paladi <- colorBin("OrRd", valsadi, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpaladi <- colorBin("OrRd", valsadi, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else if (z=="Smokes") {
    palsmoke <- colorBin("OrRd", valssmoke, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalsmoke <- colorBin("OrRd", valssmoke, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else if (z=="`Good or Better Health`") {
    palSRH <- colorBin("OrRd", valsSRH, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalSRH <- colorBin("OrRd", valsSRH, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else if (z=="`Depressive Disorder`") {
    paldep <- colorBin("OrRd", valsdep, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpaldep <- colorBin("OrRd", valsdep, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else if (z=="`Has Health Insurance`") {
    palhc <- colorBin("OrRd", valshc, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalhc <- colorBin("OrRd", valshc, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  } else {
    palFlushot <- colorBin("OrRd", valsFlushot, 6, pretty=FALSE, na.color="#ADADAD", reverse=FALSE)
    revpalFlushot <- colorBin("OrRd", valsFlushot, 6, pretty=FALSE, na.color="#ADADAD", reverse=TRUE)
  }
  
  percents_reformattedAsthma <- rep(0,945)
  percents_reformattedCHD <- rep(0,945)
  percents_reformattedCOPD <- rep(0,945)
  percents_reformattedFlushot <- rep(0,945)
  percents_reformattedDiabetes <- rep(0,945)
  percents_reformattedbmin <- rep(0,945)
  percents_reformattedSRH <- rep(0,945)
  percents_reformattedsmoke <- rep(0,945)
  percents_reformattedhc <- rep(0,945)
  percents_reformatteddep <- rep(0,945)
  percents_reformattedadi <- rep(0,945)
  if(z=="Asthma"){
    for(i in c(1:945)){
      percents_reformattedAsthma[i] <- ifelse(!is.na(valsAsthma[i]), paste0(round(valsAsthma[i], 2), "%"), "No data")
    }
  } else if (z=="CHD") {
    for(i in c(1:945)){
      percents_reformattedCHD[i] <- ifelse(!is.na(valsCHD[i]), paste0(round(valsCHD[i], 2), "%"), "No data")
    }
  } else if (z=="COPD") {
    for(i in c(1:945)){
      percents_reformattedCOPD[i] <- ifelse(!is.na(valscopd[i]), paste0(round(valscopd[i], 2), "%"), "No data")
    }
  } else if (z=="Smokes") {
    for(i in c(1:945)){
      percents_reformattedsmoke[i] <- ifelse(!is.na(valssmoke[i]), paste0(round(valssmoke[i], 2), "%"), "No data")
    }
  } else if (z=="`Has Health Insurance`") {
    for(i in c(1:945)){
      percents_reformattedhc[i] <- ifelse(!is.na(valshc[i]), paste0(round(valshc[i], 2), "%"), "No data")
    }
  } else if (z=="`Depressive Disorder`") {
    for(i in c(1:945)){
      percents_reformatteddep[i] <- ifelse(!is.na(valsdep[i]), paste0(round(valsdep[i], 2), "%"), "No data")
    }
  } else if (z=="Diabetes") {
    for(i in c(1:945)){
      percents_reformattedDiabetes[i] <- ifelse(!is.na(valsDiabetes[i]), paste0(round(valsDiabetes[i], 2), "%"), "No data")
    }
  } else if (z=="`Good or Better Health`") {
    for(i in c(1:945)){
      percents_reformattedSRH[i] <- ifelse(!is.na(valsSRH[i]), paste0(round(valsSRH[i], 2), "%"), "No data")
    }
  } else if (z=="AVERAGE_BMI") {
    for(i in c(1:945)){
      percents_reformattedbmin[i] <- ifelse(!is.na(valsbmin[i]), paste0(round(valsbmin[i], 2)), "No data")
    }
  } else if (z=="AVERAGE_ADI") {
    for(i in c(1:945)){
      percents_reformattedadi[i] <- ifelse(!is.na(valsadi[i]), paste0(round(valsadi[i], 2)), "No data")
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
  } else if (z=="COPD") {
    mmsa_popup_COPD <- paste0("<strong>MMSA: </strong>", 
                             mapNames, 
                             "<br><strong>Weighted COPD Prevalence: </strong>", 
                             percents_reformattedCOPD, "<br><strong>N: </strong>", nscopd)
  } else if (z=="Smokes") {
    mmsa_popup_smoke <- paste0("<strong>MMSA: </strong>", 
                              mapNames, 
                              "<br><strong>Weighted Smoking Prevalence: </strong>", 
                              percents_reformattedsmoke, "<br><strong>N: </strong>", nssmoke)
  } else if (z=="Diabetes") {
    mmsa_popup_Diabetes <- paste0("<strong>MMSA: </strong>", 
                             mapNames, 
                             "<br><strong>Weighted Diabetes Prevalence: </strong>", 
                             percents_reformattedDiabetes, "<br><strong>N: </strong>", nsDiabetes)
  } else if (z=="`Good or Better Health`") {
    mmsa_popup_SRH <- paste0("<strong>MMSA: </strong>", 
                                  mapNames, 
                                  "<br><strong>Weighted Good or Better Health: </strong>", 
                                  percents_reformattedSRH, "<br><strong>N: </strong>", nsSRH)
  } else if (z=="`Has Health Insurance`") {
    mmsa_popup_hc <- paste0("<strong>MMSA: </strong>", 
                             mapNames, 
                             "<br><strong>Weighted Prevalence of Health Insurance: </strong>", 
                             percents_reformattedhc, "<br><strong>N: </strong>", nshc)
  } else if (z=="`Depressive Disorder`") {
    mmsa_popup_dep <- paste0("<strong>MMSA: </strong>", 
                             mapNames, 
                             "<br><strong>Weighted Prevalence of Depressive Disorder: </strong>", 
                             percents_reformatteddep, "<br><strong>N: </strong>", nsdep)
  } else if (z=="AVERAGE_BMI") {
    mmsa_popup_bmin <- paste0("<strong>MMSA: </strong>", 
                             mapNames, 
                             "<br><strong>Weighted BMI Average: </strong>", 
                             percents_reformattedbmin, "<br><strong>N: </strong>", nsbmin)
  } else if (z=="AVERAGE_ADI") {
    mmsa_popup_adi <- paste0("<strong>MMSA: </strong>", 
                              mapNames, 
                              "<br><strong>Weighted ADI Average: </strong>", 
                              percents_reformattedadi, "<br><strong>N: </strong>", nsadi)
  } else {
    mmsa_popup_Flushot <- paste0("<strong>MMSA: </strong>", 
                                 mapNames, 
                                 "<br><strong>Weighted Flushot Administration Prevalence: </strong>", 
                                 percents_reformattedFlushot, "<br><strong>N: </strong>", nsFlushot)
  }
  
  if(z=="Asthma"){
    leaflet(location_min_sf) %>% 
      setView(lng=-94, lat=39, zoom=3.5) %>%
      addTiles(options = tileOptions(opacity = 1)) %>%
      addMapPane("below", zIndex = 410) %>%
      addMapPane("above", zIndex = 420) %>%
      addPolygons(color=~palAsthma(valsAsthma),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  options = pathOptions(pane = "above"),
                  popup=mmsa_popup_Asthma) %>%
      addPolylines(color = "black",
                   weight = 0.2,
                   options = pathOptions(pane = "above")) %>%
      addPolylines(data=regions_min_sf,
                   color = "black",
                   weight = 0.5,
                   options = pathOptions(pane = "below")) %>%
      addPolygons(data=regions_min_sf,
                  weight = 1,
                  fillOpacity = 0,
                  smoothFactor = 0.5,
                  stroke = TRUE,
                  options = pathOptions(pane = "below")) %>%
      addLegend("bottomleft", pal = revpalAsthma, 
                values = valsAsthma, 
                na.label = "No data",
                opacity = 1,
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)),
                title = "Prevalence")
  } else if (z=="CHD") {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      addMapPane("below", zIndex = 410) %>%
      addMapPane("above", zIndex = 420) %>%
      setView(lng=-94, lat=39, zoom=3.5) %>%
      addPolygons(color=~palCHD(valsCHD),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  options = pathOptions(pane = "above"),
                  popup=mmsa_popup_CHD) %>%
      addPolylines(color = "black",
                   weight = 0.2,
                   options = pathOptions(pane = "above")) %>%
      addPolylines(data=regions_min_sf,
                   color = "black",
                   weight = 0.5,
                   options = pathOptions(pane = "below")) %>%
      addPolygons(data=regions_min_sf,
                  weight = 1,
                  fillOpacity = 0,
                  smoothFactor = 0.5,
                  stroke = TRUE,
                  options = pathOptions(pane = "below")) %>%
      addLegend("bottomleft", pal = revpalCHD, 
                title = "Prevalence",
                values = valsCHD, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)))
  } else if (z=="COPD") {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      addMapPane("below", zIndex = 410) %>%
      addMapPane("above", zIndex = 420) %>%
      setView(lng=-94, lat=39, zoom=3.5) %>%
      addPolygons(color=~palCOPD(valscopd),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  options = pathOptions(pane = "above"),
                  popup=mmsa_popup_COPD) %>%
      addPolylines(color = "black",
                   weight = 0.2,
                   options = pathOptions(pane = "above")) %>%
      addPolylines(data=regions_min_sf,
                   color = "black",
                   weight = 0.5,
                   options = pathOptions(pane = "below")) %>%
      addPolygons(data=regions_min_sf,
                  weight = 1,
                  fillOpacity = 0,
                  smoothFactor = 0.5,
                  stroke = TRUE,
                  options = pathOptions(pane = "below")) %>%
      addLegend("bottomleft", pal = revpalCOPD, 
                title = "Prevalence",
                values = valscopd, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)))
  } else if (z=="Smokes") {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      addMapPane("below", zIndex = 410) %>%
      addMapPane("above", zIndex = 420) %>%
      setView(lng=-94, lat=39, zoom=3.5) %>%
      addPolygons(color=~palsmoke(valssmoke),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  options = pathOptions(pane = "above"),
                  popup=mmsa_popup_smoke) %>%
      addPolylines(color = "black",
                   weight = 0.2,
                   options = pathOptions(pane = "above")) %>%
      addPolylines(data=regions_min_sf,
                   color = "black",
                   weight = 0.5,
                   options = pathOptions(pane = "below")) %>%
      addPolygons(data=regions_min_sf,
                  weight = 1,
                  fillOpacity = 0,
                  smoothFactor = 0.5,
                  stroke = TRUE,
                  options = pathOptions(pane = "below")) %>%
      addLegend("bottomleft", pal = revpalsmoke, 
                title = "Prevalence",
                values = valssmoke, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)))
  } else if (z=="Diabetes") {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      addMapPane("below", zIndex = 410) %>%
      addMapPane("above", zIndex = 420) %>%
      setView(lng=-94, lat=39, zoom=3.5) %>%
      addPolygons(color=~palDiabetes(valsDiabetes),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  options = pathOptions(pane = "above"),
                  popup=mmsa_popup_Diabetes) %>%
      addPolylines(color = "black",
                   weight = 0.2,
                   options = pathOptions(pane = "above")) %>%
      addPolylines(data=regions_min_sf,
                   color = "black",
                   weight = 0.5,
                   options = pathOptions(pane = "below")) %>%
      addPolygons(data=regions_min_sf,
                  weight = 1,
                  fillOpacity = 0,
                  smoothFactor = 0.5,
                  stroke = TRUE,
                  options = pathOptions(pane = "below")) %>%
      addLegend("bottomleft", pal = revpalDiabetes, 
                title = "Prevalence",
                values = valsDiabetes, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)))
  } else if (z=="`Good or Better Health`") {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      addMapPane("below", zIndex = 410) %>%
      addMapPane("above", zIndex = 420) %>%
      setView(lng=-94, lat=39, zoom=3.5) %>%
      addPolygons(color=~palSRH(valsSRH),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  options = pathOptions(pane = "above"),
                  popup=mmsa_popup_SRH) %>%
      addPolylines(color = "black",
                   weight = 0.2,
                   options = pathOptions(pane = "above")) %>%
      addPolylines(data=regions_min_sf,
                   color = "black",
                   weight = 0.5,
                   options = pathOptions(pane = "below")) %>%
      addPolygons(data=regions_min_sf,
                  weight = 1,
                  fillOpacity = 0,
                  smoothFactor = 0.5,
                  stroke = TRUE,
                  options = pathOptions(pane = "below")) %>%
      addLegend("bottomleft", pal = revpalSRH, 
                title = "Prevalence",
                values = valsSRH, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)))
  } else if (z=="`Depressive Disorder`") {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      addMapPane("below", zIndex = 410) %>%
      addMapPane("above", zIndex = 420) %>%
      setView(lng=-94, lat=39, zoom=3.5) %>%
      addPolygons(color=~paldep(valsdep),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  options = pathOptions(pane = "above"),
                  popup=mmsa_popup_dep) %>%
      addPolylines(color = "black",
                   weight = 0.2,
                   options = pathOptions(pane = "above")) %>%
      addPolylines(data=regions_min_sf,
                   color = "black",
                   weight = 0.5,
                   options = pathOptions(pane = "below")) %>%
      addPolygons(data=regions_min_sf,
                  weight = 1,
                  fillOpacity = 0,
                  smoothFactor = 0.5,
                  stroke = TRUE,
                  options = pathOptions(pane = "below")) %>%
      addLegend("bottomleft", pal = revpaldep, 
                title = "Prevalence",
                values = valsdep, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)))
  } else if (z=="`Has Health Insurance`") {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      addMapPane("below", zIndex = 410) %>%
      addMapPane("above", zIndex = 420) %>%
      setView(lng=-94, lat=39, zoom=3.5) %>%
      addPolygons(color=~palhc(valshc),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  options = pathOptions(pane = "above"),
                  popup=mmsa_popup_hc) %>%
      addPolylines(color = "black",
                   weight = 0.2,
                   options = pathOptions(pane = "above")) %>%
      addPolylines(data=regions_min_sf,
                   color = "black",
                   weight = 0.5,
                   options = pathOptions(pane = "below")) %>%
      addPolygons(data=regions_min_sf,
                  weight = 1,
                  fillOpacity = 0,
                  smoothFactor = 0.5,
                  stroke = TRUE,
                  options = pathOptions(pane = "below")) %>%
      addLegend("bottomleft", pal = revpalhc, 
                title = "Prevalence",
                values = valshc, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)))
  } else if (z=="AVERAGE_BMI") {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      addMapPane("below", zIndex = 410) %>%
      addMapPane("above", zIndex = 420) %>%
      setView(lng=-94, lat=39, zoom=3.5) %>%
      addPolygons(color=~palbmin(valsbmin),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  options = pathOptions(pane = "above"),
                  popup=mmsa_popup_bmin) %>%
      addPolylines(color = "black",
                   weight = 0.2,
                   options = pathOptions(pane = "above")) %>%
      addPolylines(data=regions_min_sf,
                   color = "black",
                   weight = 0.5,
                   options = pathOptions(pane = "below")) %>%
      addPolygons(data=regions_min_sf,
                  weight = 1,
                  fillOpacity = 0,
                  smoothFactor = 0.5,
                  stroke = TRUE,
                  options = pathOptions(pane = "below")) %>%
      addLegend("bottomleft", pal = revpalbmin, 
                title = "Prevalence",
                values = valsbmin, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1,transform = function(x) sort(x, decreasing = TRUE)))
  } else if (z=="AVERAGE_ADI") {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      addMapPane("below", zIndex = 410) %>%
      addMapPane("above", zIndex = 420) %>%
      setView(lng=-94, lat=39, zoom=3.5) %>%
      addPolygons(color=~paladi(valsadi),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  options = pathOptions(pane = "above"),
                  popup=mmsa_popup_adi) %>%
      addPolylines(color = "black",
                   weight = 0.2,
                   options = pathOptions(pane = "above")) %>%
      addPolylines(data=regions_min_sf,
                   color = "black",
                   weight = 0.5,
                   options = pathOptions(pane = "below")) %>%
      addPolygons(data=regions_min_sf,
                  weight = 1,
                  fillOpacity = 0,
                  smoothFactor = 0.5,
                  stroke = TRUE,
                  options = pathOptions(pane = "below")) %>%
      addLegend("bottomleft", pal = revpaladi, 
                title = "Prevalence",
                values = valsadi, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1,transform = function(x) sort(x, decreasing = TRUE)))
  } else {
    leaflet(location_min_sf) %>% 
      addTiles(options = tileOptions(opacity = 1)) %>%
      addMapPane("below", zIndex = 410) %>%
      addMapPane("above", zIndex = 420) %>%
      setView(lng=-94, lat=39, zoom=3.5) %>%
      addPolygons(color=~palFlushot(valsFlushot),
                  weight = 1,
                  fillOpacity = 0.6,
                  smoothFactor = 0.5,
                  stroke = FALSE,
                  options = pathOptions(pane = "above"),
                  popup=mmsa_popup_Flushot) %>%
      addPolylines(color = "black",
                   weight = 0.2,
                   options = pathOptions(pane = "above")) %>%
      addPolylines(data=regions_min_sf,
                   color = "black",
                   weight = 0.5,
                   options = pathOptions(pane = "below")) %>%
      addPolygons(data=regions_min_sf,
                  weight = 1,
                  fillOpacity = 0,
                  smoothFactor = 0.5,
                  stroke = TRUE,
                  options = pathOptions(pane = "below")) %>%
      addLegend("bottomleft", pal = revpalFlushot, 
                title = "Prevalence",
                values = valsFlushot, 
                opacity = 1,
                na.label = "No data",
                labFormat = labelFormat(digits = 1, suffix = "%",transform = function(x) sort(x, decreasing = TRUE)))
    
  }
  
}
