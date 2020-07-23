.libPaths("/home/maya/R/x86_64-pc-linux-gnu-library/3.4/")
#source("/srv/shiny-server/prevalencemaps/global.R")
function(input, output, clientData, session) {
  
  observe({
    c_dis <- input$control_disease
    eval(parse(text = "c_dis"))
    
    # Select input =============================================
    s_options <- list()
    s_options[[paste("2011")]] <-
      paste0(c_dis, "2011")
    s_options[[paste("2012")]] <-
      paste0(c_dis, "2012")
    s_options[[paste("2013")]] <-
      paste0(c_dis, "2013")
    s_options[[paste("2014")]] <-
      paste0(c_dis, "2014")
    s_options[[paste("2015")]] <-
      paste0(c_dis, "2015")
    s_options[[paste("2016")]] <-
      paste0(c_dis, "2016")
    s_options[[paste("2017")]] <-
      paste0(c_dis, "2017")
    s_options[[paste("2011-2017 (all years)")]] <-
      paste0(c_dis, "2011-2017 (all years)")
    
    updateSelectInput(session, "inSelect",
                      label = paste("Year:"),
                      choices = s_options,
                      selected = paste0(c_dis, "2011-2017 (all years)")
    )
    
    c_disg <- input$control_diseaseg
    eval(parse(text = "c_disg"))
    
    #Calling in data for use in the map
    brfss_year <- reactive({
      switch(paste(c_dis,input$var,sep=" "),
             "Asthma 2011" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2012" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2013" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2014" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2015" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2016" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2017" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2011-2017 (all years)" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "CHD 2011" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2012" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2013" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2014" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2015" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2016" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2017" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2011-2017 (all years)" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "`Flushot Administration` 2011" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "`Flushot Administration` 2012" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "`Flushot Administration` 2013" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "`Flushot Administration` 2014" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "`Flushot Administration` 2015" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "`Flushot Administration` 2016" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "`Flushot Administration` 2017" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "`Flushot Administration` 2011-2017 (all years)" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Diabetes 2011" = filter(weighted_current_diabetes_prev, YEAR==paste(input$var)),
             "Diabetes 2012" = filter(weighted_current_diabetes_prev, YEAR==paste(input$var)),
             "Diabetes 2013" = filter(weighted_current_diabetes_prev, YEAR==paste(input$var)),
             "Diabetes 2014" = filter(weighted_current_diabetes_prev, YEAR==paste(input$var)),
             "Diabetes 2015" = filter(weighted_current_diabetes_prev, YEAR==paste(input$var)),
             "Diabetes 2016" = filter(weighted_current_diabetes_prev, YEAR==paste(input$var)),
             "Diabetes 2017" = filter(weighted_current_diabetes_prev, YEAR==paste(input$var)),
             "Diabetes 2011-2017 (all years)" = filter(weighted_current_diabetes_prev, YEAR==paste(input$var)),
             "`Good or Better Health` 2011" = filter(weighted_current_SRH_prev, YEAR==paste(input$var)),
             "`Good or Better Health` 2012" = filter(weighted_current_SRH_prev, YEAR==paste(input$var)),
             "`Good or Better Health` 2013" = filter(weighted_current_SRH_prev, YEAR==paste(input$var)),
             "`Good or Better Health` 2014" = filter(weighted_current_SRH_prev, YEAR==paste(input$var)),
             "`Good or Better Health` 2015" = filter(weighted_current_SRH_prev, YEAR==paste(input$var)),
             "`Good or Better Health` 2016" = filter(weighted_current_SRH_prev, YEAR==paste(input$var)),
             "`Good or Better Health` 2017" = filter(weighted_current_SRH_prev, YEAR==paste(input$var)),
             "`Good or Better Health` 2011-2017 (all years)" = filter(weighted_current_SRH_prev, YEAR==paste(input$var)),
             "`Average BMI` 2011" = filter(weighted_current_bmin_prev, YEAR==paste(input$var)),
             "`Average BMI` 2012" = filter(weighted_current_bmin_prev, YEAR==paste(input$var)),
             "`Average BMI` 2013" = filter(weighted_current_bmin_prev, YEAR==paste(input$var)),
             "`Average BMI` 2014" = filter(weighted_current_bmin_prev, YEAR==paste(input$var)),
             "`Average BMI` 2015" = filter(weighted_current_bmin_prev, YEAR==paste(input$var)),
             "`Average BMI` 2016" = filter(weighted_current_bmin_prev, YEAR==paste(input$var)),
             "`Average BMI` 2017" = filter(weighted_current_bmin_prev, YEAR==paste(input$var)),
             "`Average BMI` 2011-2017 (all years)" = filter(weighted_current_bmin_prev, YEAR==paste(input$var)),
             "`Average ADI` 2011" = filter(weighted_current_adi_prev, YEAR==paste(input$var)),
             "`Average ADI` 2012" = filter(weighted_current_adi_prev, YEAR==paste(input$var)),
             "`Average ADI` 2013" = filter(weighted_current_adi_prev, YEAR==paste(input$var)),
             "`Average ADI` 2014" = filter(weighted_current_adi_prev, YEAR==paste(input$var)),
             "`Average ADI` 2015" = filter(weighted_current_adi_prev, YEAR==paste(input$var)),
             "`Average ADI` 2016" = filter(weighted_current_adi_prev, YEAR==paste(input$var)),
             "`Average ADI` 2017" = filter(weighted_current_adi_prev, YEAR==paste(input$var)),
             "`Average ADI` 2011-2017 (all years)" = filter(weighted_current_adi_prev, YEAR==paste(input$var)),
             "`Has Health Insurance` 2011" = filter(weighted_current_HC_prev, YEAR==paste(input$var)),
             "`Has Health Insurance` 2012" = filter(weighted_current_HC_prev, YEAR==paste(input$var)),
             "`Has Health Insurance` 2013" = filter(weighted_current_HC_prev, YEAR==paste(input$var)),
             "`Has Health Insurance` 2014" = filter(weighted_current_HC_prev, YEAR==paste(input$var)),
             "`Has Health Insurance` 2015" = filter(weighted_current_HC_prev, YEAR==paste(input$var)),
             "`Has Health Insurance` 2016" = filter(weighted_current_HC_prev, YEAR==paste(input$var)),
             "`Has Health Insurance` 2017" = filter(weighted_current_HC_prev, YEAR==paste(input$var)),
             "`Has Health Insurance` 2011-2017 (all years)" = filter(weighted_current_HC_prev, YEAR==paste(input$var)),
             "`Depressive Disorder` 2011" = filter(weighted_current_DEP_prev, YEAR==paste(input$var)),
             "`Depressive Disorder` 2012" = filter(weighted_current_DEP_prev, YEAR==paste(input$var)),
             "`Depressive Disorder` 2013" = filter(weighted_current_DEP_prev, YEAR==paste(input$var)),
             "`Depressive Disorder` 2014" = filter(weighted_current_DEP_prev, YEAR==paste(input$var)),
             "`Depressive Disorder` 2015" = filter(weighted_current_DEP_prev, YEAR==paste(input$var)),
             "`Depressive Disorder` 2016" = filter(weighted_current_DEP_prev, YEAR==paste(input$var)),
             "`Depressive Disorder` 2017" = filter(weighted_current_DEP_prev, YEAR==paste(input$var)),
             "`Depressive Disorder` 2011-2017 (all years)" = filter(weighted_current_DEP_prev, YEAR==paste(input$var)),
             "COPD 2011" = filter(weighted_current_COPD_prev, YEAR==paste(input$var)),
             "COPD 2012" = filter(weighted_current_COPD_prev, YEAR==paste(input$var)),
             "COPD 2013" = filter(weighted_current_COPD_prev, YEAR==paste(input$var)),
             "COPD 2014" = filter(weighted_current_COPD_prev, YEAR==paste(input$var)),
             "COPD 2015" = filter(weighted_current_COPD_prev, YEAR==paste(input$var)),
             "COPD 2016" = filter(weighted_current_COPD_prev, YEAR==paste(input$var)),
             "COPD 2017" = filter(weighted_current_COPD_prev, YEAR==paste(input$var)),
             "COPD 2011-2017 (all years)" = filter(weighted_current_COPD_prev, YEAR==paste(input$var)),
             "Smoking 2011" = filter(weighted_current_YNSMOKE_prev, YEAR==paste(input$var)),
             "Smoking 2012" = filter(weighted_current_YNSMOKE_prev, YEAR==paste(input$var)),
             "Smoking 2013" = filter(weighted_current_YNSMOKE_prev, YEAR==paste(input$var)),
             "Smoking 2014" = filter(weighted_current_YNSMOKE_prev, YEAR==paste(input$var)),
             "Smoking 2015" = filter(weighted_current_YNSMOKE_prev, YEAR==paste(input$var)),
             "Smoking 2016" = filter(weighted_current_YNSMOKE_prev, YEAR==paste(input$var)),
             "Smoking 2017" = filter(weighted_current_YNSMOKE_prev, YEAR==paste(input$var)),
             "Smoking 2011-2017 (all years)" = filter(weighted_current_YNSMOKE_prev, YEAR==paste(input$var))
      )
    })
    
    #Calling in data for use in the MMSA graphs; not all variables here match with variables used in the map, due to redundancy (i.e BMI already displayed)
    full_dat <- reactive({
      switch(paste(c_disg,input$varg,sep=" "),
             "Asthma 2011" = filter(weighted_current_vars, YEAR==paste(input$varg)),
             "Asthma 2012" = filter(weighted_current_vars, YEAR==paste(input$varg)),
             "Asthma 2013" = filter(weighted_current_vars, YEAR==paste(input$varg)),
             "Asthma 2014" = filter(weighted_current_vars, YEAR==paste(input$varg)),
             "Asthma 2015" = filter(weighted_current_vars, YEAR==paste(input$varg)),
             "Asthma 2016" = filter(weighted_current_vars, YEAR==paste(input$varg)),
             "Asthma 2017" = filter(weighted_current_vars, YEAR==paste(input$varg)),
             "Asthma 2011-2017 (all years)" = filter(weighted_current_vars, YEAR==paste(input$varg)),
             "CHD 2011" = filter(weighted_current_varschd, YEAR==paste(input$varg)),
             "CHD 2012" = filter(weighted_current_varschd, YEAR==paste(input$varg)),
             "CHD 2013" = filter(weighted_current_varschd, YEAR==paste(input$varg)),
             "CHD 2014" = filter(weighted_current_varschd, YEAR==paste(input$varg)),
             "CHD 2015" = filter(weighted_current_varschd, YEAR==paste(input$varg)),
             "CHD 2016" = filter(weighted_current_varschd, YEAR==paste(input$varg)),
             "CHD 2017" = filter(weighted_current_varschd, YEAR==paste(input$varg)),
             "CHD 2011-2017 (all years)" = filter(weighted_current_varschd, YEAR==paste(input$varg)),
             "`Flushot Administration` 2011" = filter(weighted_current_varsflushot, YEAR==paste(input$varg)),
             "`Flushot Administration` 2012" = filter(weighted_current_varsflushot, YEAR==paste(input$varg)),
             "`Flushot Administration` 2013" = filter(weighted_current_varsflushot, YEAR==paste(input$varg)),
             "`Flushot Administration` 2014" = filter(weighted_current_varsflushot, YEAR==paste(input$varg)),
             "`Flushot Administration` 2015" = filter(weighted_current_varsflushot, YEAR==paste(input$varg)),
             "`Flushot Administration` 2016" = filter(weighted_current_varsflushot, YEAR==paste(input$varg)),
             "`Flushot Administration` 2017" = filter(weighted_current_varsflushot, YEAR==paste(input$varg)),
             "`Flushot Administration` 2011-2017 (all years)" = filter(weighted_current_varsflushot, YEAR==paste(input$varg)),
             "Diabetes 2007" = filter(weighted_current_varsdiabetes, YEAR==paste(input$varg)),
             "Diabetes 2011" = filter(weighted_current_varsdiabetes, YEAR==paste(input$varg)),
             "Diabetes 2012" = filter(weighted_current_varsdiabetes, YEAR==paste(input$varg)),
             "Diabetes 2013" = filter(weighted_current_varsdiabetes, YEAR==paste(input$varg)),
             "Diabetes 2014" = filter(weighted_current_varsdiabetes, YEAR==paste(input$varg)),
             "Diabetes 2015" = filter(weighted_current_varsdiabetes, YEAR==paste(input$varg)),
             "Diabetes 2016" = filter(weighted_current_varsdiabetes, YEAR==paste(input$varg)),
             "Diabetes 2017" = filter(weighted_current_varsdiabetes, YEAR==paste(input$varg)),
             "Diabetes 2011-2017 (all years)" = filter(weighted_current_varsdiabetes, YEAR==paste(input$varg)),
             "`Good or Better Health` 2011" = filter(weighted_current_varsSRH, YEAR==paste(input$varg)),
             "`Good or Better Health` 2012" = filter(weighted_current_varsSRH, YEAR==paste(input$varg)),
             "`Good or Better Health` 2013" = filter(weighted_current_varsSRH, YEAR==paste(input$varg)),
             "`Good or Better Health` 2014" = filter(weighted_current_varsSRH, YEAR==paste(input$varg)),
             "`Good or Better Health` 2015" = filter(weighted_current_varsSRH, YEAR==paste(input$varg)),
             "`Good or Better Health` 2016" = filter(weighted_current_varsSRH, YEAR==paste(input$varg)),
             "`Good or Better Health` 2017" = filter(weighted_current_varsSRH, YEAR==paste(input$varg)),
             "`Good or Better Health` 2011-2017 (all years)" = filter(weighted_current_varsSRH, YEAR==paste(input$varg)),
             "`Has Health Insurance` 2011" = filter(weighted_current_varsHC, YEAR==paste(input$varg)),
             "`Has Health Insurance` 2012" = filter(weighted_current_varsHC, YEAR==paste(input$varg)),
             "`Has Health Insurance` 2013" = filter(weighted_current_varsHC, YEAR==paste(input$varg)),
             "`Has Health Insurance` 2014" = filter(weighted_current_varsHC, YEAR==paste(input$varg)),
             "`Has Health Insurance` 2015" = filter(weighted_current_varsHC, YEAR==paste(input$varg)),
             "`Has Health Insurance` 2016" = filter(weighted_current_varsHC, YEAR==paste(input$varg)),
             "`Has Health Insurance` 2017" = filter(weighted_current_varsHC, YEAR==paste(input$varg)),
             "`Has Health Insurance` 2011-2017 (all years)" = filter(weighted_current_varsHC, YEAR==paste(input$varg)),
             "`Depressive Disorder` 2011" = filter(weighted_current_varsDEP, YEAR==paste(input$varg)),
             "`Depressive Disorder` 2012" = filter(weighted_current_varsDEP, YEAR==paste(input$varg)),
             "`Depressive Disorder` 2013" = filter(weighted_current_varsDEP, YEAR==paste(input$varg)),
             "`Depressive Disorder` 2014" = filter(weighted_current_varsDEP, YEAR==paste(input$varg)),
             "`Depressive Disorder` 2015" = filter(weighted_current_varsDEP, YEAR==paste(input$varg)),
             "`Depressive Disorder` 2016" = filter(weighted_current_varsDEP, YEAR==paste(input$varg)),
             "`Depressive Disorder` 2017" = filter(weighted_current_varsDEP, YEAR==paste(input$varg)),
             "`Depressive Disorder` 2011-2017 (all years)" = filter(weighted_current_varsDEP, YEAR==paste(input$varg)),
             "COPD 2011" = filter(weighted_current_varsCOPD, YEAR==paste(input$varg)),
             "COPD 2012" = filter(weighted_current_varsCOPD, YEAR==paste(input$varg)),
             "COPD 2013" = filter(weighted_current_varsCOPD, YEAR==paste(input$varg)),
             "COPD 2014" = filter(weighted_current_varsCOPD, YEAR==paste(input$varg)),
             "COPD 2015" = filter(weighted_current_varsCOPD, YEAR==paste(input$varg)),
             "COPD 2016" = filter(weighted_current_varsCOPD, YEAR==paste(input$varg)),
             "COPD 2017" = filter(weighted_current_varsCOPD, YEAR==paste(input$varg)),
             "COPD 2011-2017 (all years)" = filter(weighted_current_varsCOPD, YEAR==paste(input$varg))
      )
    })
    
    #making the map
    output$map <- renderLeaflet({
      percent_map(brfss_year(), c_dis)
    })
    gc()
    observe({
      input$reset_button
      leafletProxy("map") %>% setView(lat = 39, lng = -94, zoom = 4)
    })
    gc()
    proxy <- leafletProxy("map")
    observe({
      if(input$mmsa_input!=""){
        #Find polygon selected in map MMSA dropdown, and find a point on which to focus for adjusted zoom/center
        selected_polygon <- subset(location_min_sf, location_min_sf$NAME==input$mmsa_input)
        polygon_lat <- paste(selected_polygon$INTPTLAT)
        polygon_lon <- paste(selected_polygon$INTPTLON)
        
        #Add a black highlight of the selected MMSA
        proxy %>% addPolylines(stroke=TRUE, color="black", weight=3,data=selected_polygon,group="highlighted_polygon") %>%
          setView(lng=polygon_lon,lat=polygon_lat,zoom=5)
      }
    })
    
    #Multivariate graph formation
    output$multigraph <- renderPlot({ df <- current.all %>% filter(!is.na(BMI))
    
    ggplot(df, aes_string(x=paste(input$factors), fill=paste(input$control_disease3), weights="MMSAWT")) + 
      geom_bar(position="fill", width=0.5) + coord_flip() + 
      #facet_grid(paste(input$multivariable), scales = "free") + 
      facet_wrap(paste(input$multivariable), nrow=length(unique(df[[paste(input$multivariable)]]))) + #ncol=length(unique(df[[paste(input$multivariable)]]))
      scale_x_discrete(gsub("`","",paste(input$factors))) +
      ggtitle("Multivariate Interactions of Variables (2011-2017)") +
      scale_fill_manual(name=gsub("`","",paste(input$control_disease3)), values=c("#FDD49E","#FC8D59")) + 
      xlab(gsub("`","",paste(input$multivariable))) + theme_bw() + 
      theme(axis.text.x=element_text(hjust=1,angle=20,size=12), #
            axis.text.y=element_text(size=14),
            axis.title = element_blank(),
            plot.title = element_text(size = 18, face="bold"),
            strip.background = element_rect(fill="#f5f5f5"),
            strip.text = element_text(size=14),
            legend.text = element_text(size=14),
            legend.title = element_text(size=15,face="bold"))
    })
    gc()
    
    #Bivariate graph formation
    f.c_dis2<-as.factor(input$control_disease2)
    output$graph <- renderPlot ({ current.all %>% 
        filter(!is.na(BMI)) %>%
        ggplot(aes_string(x=paste(f.c_dis2), fill=(paste(input$variable)), weights = "MMSAWT")) + 
        scale_x_discrete(gsub("`","",paste(input$control_disease2)), labels = c("0"="No","1"="Yes")) +
        ggtitle(paste(gsub("`","",input$control_disease2), "Prevalence (2011-2017) Across", gsub("`","",input$variable))) +
        scale_fill_brewer(name=gsub("`","",paste(input$variable)), palette = "OrRd") + ylab("") +
        theme(axis.text.x=element_text(hjust=1)) + 
        geom_bar(position="fill", width=0.5) + theme_bw() + 
        theme(axis.text.x=element_text(size=15), #hjust=1,angle=20,
              axis.text.y=element_text(size=12),
              axis.title.x = element_blank(),
              plot.title = element_text(size = 18, face="bold"),
              legend.text = element_text(size=13),
              legend.title = element_text(size=15, face="bold"))
    })
    
    gc()
    
    
    #Regionality graph formation
    
    #label maker
    addline_format <- function(x,...){
      gsub('\\s','\n',x)
    }
    
    output$regiongraph <- renderPlot ({ 
      df <- current.all %>% 
        filter(!is.na(BMI)) %>%
        filter(!is.na(Region)) 
      
      ggplot(df, aes_string(x=paste(input$control_disease4), fill=(paste(input$variable2)), weights = "MMSAWT")) + 
        theme_bw() + 
        facet_wrap("Region", nrow=5) + coord_flip() +
        ggtitle(paste(gsub("`","",input$control_disease4), "Prevalence (2011-2017) Across U.S Regions")) +
        scale_fill_brewer(name=(gsub("`","",paste(input$variable2))), palette = "OrRd") +
        geom_bar(position="fill",width=0.50) + 
        scale_x_discrete(gsub("`","",paste(input$control_disease4))) + 
        #,breaks=unique(df[[input$control_disease4]]), labels=addline_format(unique(df[[input$control_disease4]]))) + 
        theme(axis.text.x=element_text(hjust=1,angle=20,size=12),
              axis.text.y=element_text(size=14),
              axis.title = element_blank(),
              plot.title = element_text(size = 18, face="bold"),
              strip.background = element_rect(fill="#f5f5f5"),
              strip.text = element_text(size=14),
              legend.text = element_text(size=14),
              legend.title = element_text(size=15,face="bold")) 
    })
    gc()
    
    #Making the map popups and labelling information
    mmsa.click <- reactive ({
      as.character(mmsa_names[match(input$mmsa_input, 
                                    mmsa_names$x), "x"]) })
    
    mmsas.click <- reactive ({
      mmsamatches<-which(apply(mmsa_names, 1, function(x) all(x == mmsa.click())))
      if(!is.na(mmsa.click()))
        return(polynamesnumbers[mmsamatches,3])
      return(mmsas.click<-NA)
    })
    
    
    disease_percent_raw <- reactive ({ if( length(mmsas.click())==1 & !is.na(mmsas.click()) ) 
      return(brfss_year()[which(brfss_year()$MMSA == as.numeric(paste(mmsas.click()[[1]]))), 5]) 
      return("") })
    
    disease_percent <- reactive ({ if(paste(disease_percent_raw())!="numeric(0)" & input$control_disease == "Asthma") 
      return(paste0(as.character(round(disease_percent_raw(), 2)), "%"))
      else if(paste(disease_percent_raw())!="numeric(0)" & input$control_disease == "CHD") 
        return(paste0(as.character(round(disease_percent_raw(), 2)), "%"))
      else if(paste(disease_percent_raw())!="numeric(0)" & input$control_disease == "COPD") 
        return(paste0(as.character(round(disease_percent_raw(), 2)), "%"))
      else if(paste(disease_percent_raw())!="numeric(0)" & input$control_disease == "Diabetes") 
        return(paste0(as.character(round(disease_percent_raw(), 2)), "%"))
      else if(paste(disease_percent_raw())!="numeric(0)" & input$control_disease == "`Flushot Administration`") 
        return(paste0(as.character(round(disease_percent_raw(), 2)), "%"))
      else if(paste(disease_percent_raw())!="numeric(0)" & input$control_disease == "`Has Health Insurance`")
        return(paste0(as.character(round(disease_percent_raw(), 2)), "%"))
      else if(paste(disease_percent_raw())!="numeric(0)" & input$control_disease == "`Depressive Disorder`")
        return(paste0(as.character(round(disease_percent_raw(), 2)), "%"))
      else if(paste(disease_percent_raw())!="numeric(0)" & input$control_disease == "Smokes")
        return(paste0(as.character(round(disease_percent_raw(), 2)), "%"))
      else if(paste(disease_percent_raw())!="numeric(0)" & input$control_disease == "`Good or Better Health`") 
        return(paste0(as.character(round(disease_percent_raw(), 2)), "%"))
      else if (paste(disease_percent_raw())!="numeric(0)" & input$control_disease == "AVERAGE_BMI")
        return(paste0(as.character(round(disease_percent_raw(), 2))))
      else if (paste(disease_percent_raw())!="numeric(0)" & input$control_disease == "AVERAGE_ADI")
        return(paste0(as.character(round(disease_percent_raw(), 2))))
      else return ("") })
    
    
    sample_size <- reactive({ ifelse(!is.null(mmsas.click()) & !is.na(mmsas.click()), paste0(" (N = ", formatC(brfss_year()[match(as.numeric(paste(mmsas.click()[[1]])), brfss_year()$MMSA), "count"], format = "d", big.mark=","), ")"), "") })
    
    output$mapinfo <- renderText({ if (length(mmsa.click()) == 1 & (disease_percent()) != "")  
      paste0("Weighted ", gsub("`","",input$control_disease), " Prevalence in ", input$mmsa_input, " in ", input$var,": ", disease_percent(), sample_size()) 
      else if (!is.na(mmsa.click())) paste0( mmsa.click() , ": No data for this MMSA/year") 
      else paste0("") })
    
    output$mapyearinfo <- renderText({paste0(gsub("`","",input$control_disease), " Prevalence in ", input$var) })
    
    #Making MMSA graphs and making conditions for data display
    mmsa.clickg <- reactive ({
      as.character(mmsa_names[match(input$mmsa_inputg, 
                                    mmsa_names$x), "x"]) })
    
    mmsas.clickg <- reactive ({
      mmsamatches<-which(apply(mmsa_names, 1, function(x) all(x == mmsa.clickg())))
      if(!is.na(mmsa.clickg()))
        return(polynamesnumbers[mmsamatches,3])
      return(mmsas.clickg<-NA)
    })
    
    
    disease_percent_rawg <- reactive ({ if( length(mmsas.clickg())==1 & !is.na(mmsas.clickg()) ) 
      return(brfss_year()[which(brfss_year()$MMSA == as.numeric(paste(mmsas.clickg()[[1]]))), 5]) 
      return("") })
    
    disease_percentg <- reactive ({ if(paste(disease_percent_rawg())!="numeric(0)" & input$control_diseaseg == "Asthma") 
      return(paste0(as.character(round(disease_percent_rawg(), 2)), "%"))
      else if(paste(disease_percent_rawg())!="numeric(0)" & input$control_diseaseg == "CHD") 
        return(paste0(as.character(round(disease_percent_rawg(), 2)), "%"))
      else if(paste(disease_percent_rawg())!="numeric(0)" & input$control_diseaseg == "COPD") 
        return(paste0(as.character(round(disease_percent_rawg(), 2)), "%"))
      else if(paste(disease_percent_rawg())!="numeric(0)" & input$control_diseaseg == "Diabetes") 
        return(paste0(as.character(round(disease_percent_rawg(), 2)), "%"))
      else if(paste(disease_percent_rawg())!="numeric(0)" & input$control_diseaseg == "`Flushot Administration`") 
        return(paste0(as.character(round(disease_percent_rawg(), 2)), "%"))
      else if(paste(disease_percent_rawg())!="numeric(0)" & input$control_diseaseg == "`Has Health Insurance`")
        return(paste0(as.character(round(disease_percent_rawg(), 2)), "%"))
      else if(paste(disease_percent_rawg())!="numeric(0)" & input$control_diseaseg == "`Depressive Disorder`")
        return(paste0(as.character(round(disease_percent_rawg(), 2)), "%"))
      else if(paste(disease_percent_rawg())!="numeric(0)" & input$control_diseaseg == "`Good or Better Health`") 
        return(paste0(as.character(round(disease_percent_rawg(), 2)), "%"))
      else return ("") })
    
    output$graphinfo <- renderText({ if(length(mmsa.clickg()) == 1 & disease_percentg()!= "")
      paste("Data for", gsub("`","",input$control_diseaseg), "in", input$mmsa_inputg, "in", input$varg, ": ")
      else if (!is.na(mmsa.clickg())) paste0(mmsa.clickg(), ": No data for this MMSA/year")
      else paste0("") })
    
    
    
    mmsa_yearly_dat <- reactive ({ if(!is.na(as.numeric(paste(mmsas.clickg()[[1]])))) return(full_dat()[which(full_dat()$MMSA == as.numeric(paste(mmsas.clickg()[[1]]))),]) else return(data.frame(0)) })
    
    
    
    bmi <- reactive({ reshape2::melt(select(mmsa_yearly_dat(), 3, 14, 15, 16, 17, 18), id.vars=c_disg) })
    race <- reactive({ reshape2::melt(select(mmsa_yearly_dat(), 3, 9, 10, 11, 12, 13), id.vars=c_disg) })
    income <- reactive({ reshape2::melt(select(mmsa_yearly_dat(), 3, 4, 5, 6), id.vars=c_disg) })
    smoking <- reactive({ reshape2::melt(select(mmsa_yearly_dat(), 3, 19, 20, 21), id.vars=c_disg) })
    age_cat <- reactive({ reshape2::melt(select(mmsa_yearly_dat(), 3, 25:30), id.vars=c_disg) })
    gender <- reactive({ reshape2::melt(select(mmsa_yearly_dat(), 3, 7, 8), id.vars=c_disg) })
    
    
    
    
    output$bmi_plot <- renderPlot({ ggplot(bmi(), aes(x = factor(bmi()[,1]), y = value, fill = factor(variable))) + 
        geom_bar(stat="identity", position="fill",width=0.50) +
        scale_x_discrete(labels=c(paste("No",gsub("`","",c_disg)), paste(gsub("`","",c_disg)))) +
        scale_fill_brewer(name="variable", palette = "Blues") + bbc_style() +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        geom_vline(xintercept = 0.5, size = 1, colour="#333333") +
        theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15, face = "bold")) +
        guides(fill = guide_legend(title = "Body Mass Index (BMI)", title.position = "top", title.hjust = 0.5, nrow=2)) +
        theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank(), axis.text = element_text(size=14),
              panel.grid.major.x = element_line(color="#cbcbcb"), panel.grid.major.y=element_blank()) + coord_flip() 
    })
    
    output$race_plot <- renderPlot({ ggplot(race(), aes(x = factor(race()[,1]), y = value, fill = factor(variable))) + 
        geom_bar(stat="identity", position="fill",width=0.50) + 
        scale_x_discrete(labels=c(paste("No",gsub("`","",c_disg)), paste(gsub("`","",c_disg)))) +
        scale_fill_brewer(name="variable", palette = "Greens") + bbc_style() +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        geom_vline(xintercept = 0.5, size = 1, colour="#333333") +
        theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15, face = "bold")) +
        guides(fill = guide_legend(title = "Race/Ethnicity", title.position = "top", title.hjust = 0.5, nrow=3)) +
        theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank(), axis.text = element_text(size=14),
              panel.grid.major.x = element_line(color="#cbcbcb"), panel.grid.major.y=element_blank()) + coord_flip() 
    })
    
    output$income_plot <- renderPlot({ ggplot(income(), aes(x = factor(income()[,1]), y = value, fill = factor(variable))) + 
        geom_bar(stat="identity", position="fill",width=0.50) + 
        scale_x_discrete(labels=c(paste("No",gsub("`","",c_disg)), paste(gsub("`","",c_disg)))) +
        scale_fill_brewer(name="variable", palette = "Oranges") + bbc_style() +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        geom_vline(xintercept = 0.5, size = 1, colour="#333333") +
        theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15, face = "bold")) +
        guides(fill = guide_legend(title = "Income", title.position = "top", title.hjust = 0.5)) +
        theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank(), axis.text = element_text(size=14),
              panel.grid.major.x = element_line(color="#cbcbcb"), panel.grid.major.y=element_blank()) + coord_flip() 
    })
    
    output$smoking_plot <- renderPlot({ ggplot(smoking(), aes(x = factor(smoking()[,1]), y = value, fill = factor(variable))) + 
        geom_bar(stat="identity", position="fill",width=0.50) + 
        scale_x_discrete(labels=c(paste("No",gsub("`","",c_disg)), paste(gsub("`","",c_disg)))) +
        scale_fill_brewer(name="variable", palette = "Purples") + bbc_style() +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        geom_vline(xintercept = 0.5, size = 1, colour="#333333") +
        theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15, face = "bold")) +
        guides(fill = guide_legend(title = "Smoking Status", title.position = "top", title.hjust = 0.5)) +
        theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank(), axis.text = element_text(size=14),
              panel.grid.major.x = element_line(color="#cbcbcb"), panel.grid.major.y=element_blank()) + coord_flip() 
    })
    
    output$age_plot <- renderPlot({ ggplot(age_cat(), aes(x = factor(age_cat()[,1]), y = value, fill = factor(variable))) + 
        geom_bar(stat="identity", position="fill",width=0.50) + 
        scale_x_discrete(labels=c(paste("No",gsub("`","",c_disg)), paste(gsub("`","",c_disg)))) +
        scale_fill_brewer(name="variable", palette = "Reds") + bbc_style() +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        geom_vline(xintercept = 0.5, size = 1, colour="#333333") +
        theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15, face = "bold")) +
        guides(fill = guide_legend(title = "Age", title.position = "top", title.hjust = 0.5)) +
        theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank(), axis.text = element_text(size=14),
              panel.grid.major.x = element_line(color="#cbcbcb"), panel.grid.major.y=element_blank()) + coord_flip() 
    })
    
    output$gender_plot <- renderPlot({ ggplot(gender(), aes(x = factor(gender()[,1]), y = value, fill = factor(variable))) + 
        geom_bar(stat="identity", position="fill",width=0.50) + 
        scale_x_discrete(labels=c(paste("No",gsub("`","",c_disg)), paste(gsub("`","",c_disg)))) +
        scale_fill_manual(name="variable", values=c("#fff3f5","#ff5b77")) + bbc_style() +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        geom_vline(xintercept = 0.5, size = 1, colour="#333333") +
        theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15, face = "bold")) +
        guides(fill = guide_legend(title = "Sex", title.position = "top", title.hjust = 0.5)) +
        theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank(), axis.text = element_text(size=14),
              panel.grid.major.x = element_line(color="#cbcbcb"), panel.grid.major.y=element_blank()) + coord_flip() 
    })
    
  })}
