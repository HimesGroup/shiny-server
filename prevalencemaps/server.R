.libPaths("/home/maya/R/x86_64-pc-linux-gnu-library/3.4/")
source("/srv/shiny-server/prevalencemaps/global.R")

function(input, output, clientData, session) {
  
  observe({
    c_dis <- input$control_disease
    eval(parse(text = "c_dis"))
    
    # Select input =============================================
    s_options <- list()
    s_options[[paste("2007")]] <-
      paste0(c_dis, "2007")
    s_options[[paste("2008")]] <-
      paste0(c_dis, "2008")
    s_options[[paste("2009")]] <-
      paste0(c_dis, "2009")
    s_options[[paste("2010")]] <-
      paste0(c_dis, "2010")
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
    s_options[[paste("2007-2017 (all years)")]] <-
      paste0(c_dis, "2007-2017 (all years)")
    s_options[[paste("2007-2010")]] <-
      paste0(c_dis, "2007-2010")
    s_options[[paste("2011-2017")]] <-
      paste0(c_dis, "2011-2017")
    
    updateSelectInput(session, "inSelect",
                      label = paste("Year:"),
                      choices = s_options,
                      selected = paste0(c_dis, "2007-2017 (all years)")
    )
    
    brfss_year <- reactive({
      switch(paste(input$control_disease,input$var,sep=" "),
             "Asthma 2007" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2008" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2009" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2010" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2011" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2012" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2013" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2014" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2015" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2016" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2017" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2007-2017 (all years)" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2007-2010" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "Asthma 2011-2017" = filter(weighted_current_asthma_prev, YEAR==paste(input$var)),
             "CHD 2007" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2008" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2009" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2010" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2011" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2012" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2013" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2014" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2015" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2016" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2017" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2007-2017 (all years)" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2007-2010" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "CHD 2011-2017" = filter(weighted_current_chd_prev, YEAR==paste(input$var)),
             "Flushot 2007" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2008" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2009" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2010" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2011" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2012" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2013" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2014" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2015" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2016" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2017" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2007-2017 (all years)" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2007-2010" = filter(weighted_current_flushot_prev, YEAR==paste(input$var)),
             "Flushot 2011-2017" = filter(weighted_current_flushot_prev, YEAR==paste(input$var))
      )
    })
    
    full_dat <- reactive({
      switch(paste(input$control_disease,input$var, sep=" "),
             "Asthma 2007" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2008" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2009" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2010" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2011" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2012" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2013" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2014" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2015" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2016" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2017" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2007-2017 (all years)" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2007-2010" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "Asthma 2011-2017" = filter(weighted_current_vars, YEAR==paste(input$var)),
             "CHD 2007" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2008" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2009" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2010" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2011" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2012" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2013" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2014" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2015" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2016" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2017" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2007-2017 (all years)" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2007-2010" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "CHD 2011-2017" = filter(weighted_current_varschd, YEAR==paste(input$var)),
             "Flushot 2007" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2008" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2009" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2010" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2011" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2012" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2013" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2014" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2015" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2016" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2017" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2007-2017 (all years)" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2007-2010" = filter(weighted_current_varsflushot, YEAR==paste(input$var)),
             "Flushot 2011-2017" = filter(weighted_current_varsflushot, YEAR==paste(input$var))
      )
    })
    
    
    
    #making the map
    output$map <- renderLeaflet({
      percent_map(brfss_year(), c_dis)
    })
    gc()
    observe({
      input$reset_button
      leafletProxy("map") %>% setView(lat = 39, lng = -94, zoom = 3)
    })
    
    #making the bivariate graph
    current.all.year <- reactive ({ if(input$variableyear1=="2007_2017"){
      current.all2007_2017
    } else if (input$variableyear1=="2007_2010"){
      current.all2007_2010
    } else {
      current.all2011_2017
    }
    })
    
    #making the multivariate graph
    current.all.year2 <- reactive ({ if(input$variableyear2=="2007_2017"){
      current.all2007_2017
    } else if (input$variableyear2=="2007_2010"){
      current.all2007_2010
    } else {
      current.all2011_2017
    }
    })
    
    #making the regional graph
    current.all.year3 <- reactive ({ if(input$variableyear3=="2007_2017"){
      current.all2007_2017
    } else if (input$variableyear3=="2007_2010"){
      current.all2007_2010
    } else {
      current.all2011_2017
    }
    })
    
    #Multivariate
    output$multigraph <- renderPlot({ current.all.year2() %>% 
      filter(!is.na(BMI)) %>%
      ggplot(aes_string(x=paste(input$control_disease3), fill=paste(input$factors), weights="MMSAWT")) + 
      geom_bar(position="fill") +
      facet_grid(rows=paste(input$multivariable)) +
      scale_x_discrete(paste(input$control_disease3), labels = c("0"="No","1"="Yes")) +
      ggtitle("Multivariate Interactions") +
      scale_fill_discrete(name=paste(input$factors)) + 
      theme(axis.text.x=element_text(hjust=1)) + ylab("") +
      theme(axis.ticks.x = element_blank())
    })
    gc()
     # mm <- reactive ({ 
      #  fit <- svyglm(as.formula(paste(input$control_disease3,"~",input$factors,"*",
       #             input$multivariable)), design=des.year2(), family=binomial(), 
        #            data=current.all.year2())
      #  k<-summ(fit, exp=TRUE)
      #  e<-k$coeftable
      #  mm<-e[-1,-3]
      #  mm
      #})
      
      #output$summarymulti <- renderTable( 
      #  mm(),
      #  striped=TRUE, rownames=TRUE, colnames=TRUE
      #)
      
    #Bivariate
    f.c_dis2<-as.factor(input$control_disease2)
    output$graph <- renderPlot ({ current.all.year() %>% 
      filter(!is.na(BMI)) %>%
      ggplot(aes_string(x=paste(f.c_dis2), fill=(paste(input$variable)), weights = "MMSAWT")) + 
      scale_x_discrete(paste(input$control_disease2), labels = c("0"="No","1"="Yes")) +
      ggtitle(paste(input$control_disease2, "Prevalence Across", input$variable)) +
      scale_fill_discrete(name=paste(input$variable)) + 
      theme(axis.text.x=element_text(hjust=1)) + ylab("") +
      geom_bar( position="fill") +
      theme(axis.text.x=element_text(hjust=1))
    })
  gc()
 #   b<-reactive({
 #       jz <- summ(svyglm(as.formula(paste(input$control_disease2,"~",input$variable)), 
 #                     design=des.year(), family=binomial(), 
 #                     data=current.all.year()), exp=TRUE)
 #       truth<-jz$coeftable[-1,-3]
 #       truth
  #  })
    
 #   output$summary <- renderTable( 
 #     b(),
 #     striped=TRUE, rownames=TRUE, colnames=TRUE
 #   )
    
    #Regionality
    output$regiongraph <- renderPlot ({ current.all.year3() %>% 
        filter(!is.na(BMI)) %>%
        filter(!is.na(Region)) %>%
        ggplot(aes_string(x=paste(input$control_disease4), fill=(paste(input$variable2)), weights = "MMSAWT")) + 
        scale_x_discrete(paste(input$control_disease4)) +
        facet_grid(rows="Region") +
        ggtitle(paste(input$control_disease4, "Prevalence Across U.S Regions")) +
        scale_fill_discrete(name=paste(input$variable2)) + 
        theme(axis.text.x=element_text(hjust=1)) + ylab("") +
        geom_bar(position="fill") +
        theme(axis.text.x=element_text(hjust=1))
    })
   gc()
    #making the plots
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
      
      disease_percent <- reactive ({ if(paste(disease_percent_raw())!="numeric(0)") return(paste0(as.character(round(disease_percent_raw(), 2)), "%"))
        return ("") })
      

    sample_size <- reactive({ ifelse(!is.null(mmsas.click()) & !is.na(mmsas.click()), paste0(" (N = ", formatC(brfss_year()[match(mmsas.click(), brfss_year()$MMSA), "count"], format = "d", big.mark=","), ")"), "") })

      output$mapinfo <- renderText({ if(length(mmsa.click()) == 1 & (disease_percent()) != "")  
        paste0("Weighted ", c_dis, " Prevalence in ", input$mmsa_input, " in ", input$var,": ", disease_percent(), sample_size()) 
       else if (!is.na(mmsa.click())) paste0( mmsa.click() , ": No data for this MMSA/year") 
       else  
        paste0("") })
      
      output$mapyearinfo <- renderText({paste0(input$control_disease, " Prevalence in ", input$var) })
        
        output$graphinfo <- renderText({ if(length(mmsa.click()) == 1 & disease_percent()!= "")
        paste("Data for", input$mmsa_input, "in", input$var, ": ")
        else if (!is.na(mmsa.click())) paste0(mmsa.click(), ": No data for this MMSA/year")
          else
            paste0("") })
      
  
    mmsa_yearly_dat <- reactive ({ if(!is.na(as.numeric(paste(mmsas.click()[[1]])))) return(full_dat()[which(full_dat()$MMSA == as.numeric(paste(mmsas.click()[[1]]))),]) else return(data.frame(0)) })

      bmi <- reactive({ melt(select(mmsa_yearly_dat(), 3, 14, 15, 16, 17), id.vars=c_dis) })
      race <- reactive({ melt(select(mmsa_yearly_dat(), 3, 9, 10, 11, 12, 13), id.vars=c_dis) })
      income <- reactive({ melt(select(mmsa_yearly_dat(), 3, 4, 5, 6), id.vars=c_dis) })
      smoking <- reactive({ melt(select(mmsa_yearly_dat(), 3, 18, 19, 20), id.vars=c_dis) })
      age_cat <- reactive({ melt(select(mmsa_yearly_dat(), 3, 24:29), id.vars=c_dis) })
      gender <- reactive({ melt(select(mmsa_yearly_dat(), 3, 7, 8), id.vars=c_dis) })
    
 
      
      
      output$bmi_plot <- renderPlot({ ggplot(bmi(), aes(x = factor(bmi()[,1]), y = value, fill = factor(variable))) + 
          geom_bar(stat="identity", position="fill") +
          scale_x_discrete(labels=c(paste("No",c_dis), paste(c_dis))) +
          theme(legend.position = "top", legend.text = element_text(size = 10), legend.title = element_text(size = 14, face = "bold")) +
          guides(fill = guide_legend(title = "Body Mass Index (BMI)", title.position = "top", title.hjust = 0.5, nrow=2)) +
          theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank()) +
          coord_flip() })
      
      output$race_plot <- renderPlot({ ggplot(race(), aes(x = factor(race()[,1]), y = value, fill = factor(variable))) + 
          geom_bar(stat="identity", position="fill") +
          scale_x_discrete(labels=c(paste("No",c_dis), paste(c_dis))) +
          theme(legend.position = "top", legend.text = element_text(size = 10), legend.title = element_text(size = 14, face = "bold")) +
          guides(fill = guide_legend(title = "Race/Ethnicity", title.position = "top", title.hjust = 0.5, nrow=3)) +
          theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank()) +
          coord_flip() })
      
      output$income_plot <- renderPlot({ ggplot(income(), aes(x = factor(income()[,1]), y = value, fill = factor(variable))) + 
          geom_bar(stat="identity", position="fill") +
          scale_x_discrete(labels=c(paste("No",c_dis), paste(c_dis))) +
          theme(legend.position = "top", legend.text = element_text(size = 10), legend.title = element_text(size = 14, face = "bold")) +
          guides(fill = guide_legend(title = "Income", title.position = "top", title.hjust = 0.5)) +
          theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank()) +
          coord_flip() })
      
      output$smoking_plot <- renderPlot({ ggplot(smoking(), aes(x = factor(smoking()[,1]), y = value, fill = factor(variable))) + 
          geom_bar(stat="identity", position="fill") +
          scale_x_discrete(labels=c(paste("No",c_dis), paste(c_dis))) +
          theme(legend.position = "top", legend.text = element_text(size = 10), legend.title = element_text(size = 14, face = "bold")) +
          guides(fill = guide_legend(title = "Smoking Status", title.position = "top", title.hjust = 0.5)) +
          theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank()) +
          coord_flip() })
      
      output$age_plot <- renderPlot({ ggplot(age_cat(), aes(x = factor(age_cat()[,1]), y = value, fill = factor(variable))) + 
          geom_bar(stat="identity", position="fill") +
          scale_x_discrete(labels=c(paste("No",c_dis), paste(c_dis))) +
          theme(legend.position = "top", legend.text = element_text(size = 10), legend.title = element_text(size = 14, face = "bold")) +
          guides(fill = guide_legend(title = "Age", title.position = "top", title.hjust = 0.5)) +
          theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank()) +
          coord_flip() })
      
      output$gender_plot <- renderPlot({ ggplot(gender(), aes(x = factor(gender()[,1]), y = value, fill = factor(variable))) + 
          geom_bar(stat="identity", position="fill") +
          scale_x_discrete(labels=c(paste("No",c_dis), paste(c_dis))) +
          theme(legend.position = "top", legend.text = element_text(size = 10), legend.title = element_text(size = 14, face = "bold")) +
          guides(fill = guide_legend(title = "Sex", title.position = "top", title.hjust = 0.5)) +
          theme(axis.line = element_blank(), axis.title.y=element_blank(), axis.title.x = element_blank()) +
          coord_flip() })
      
  })}
