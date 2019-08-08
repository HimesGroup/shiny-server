.libPaths("/home/maya/R/x86_64-pc-linux-gnu-library/3.4/") 
library(shiny)
library(shinyWidgets)
library(shinydashboard)
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
    menuItem("Introduction", tabName = "Introduction", icon = icon("square")),
    menuItem("Map of MMSA Data", tabName = "Map", icon = icon("square")),
    menuItem("MMSA-Specific Graphs", tabName = "MMSAGraph", icon = icon("square")),
    menuItem("US Bivariate Graphs", tabName = "Bivariate", icon = icon("square")),
    menuItem("US Multivariate Graphs", tabName = "Multivariate", icon = icon("square")),
    menuItem("US Regional Graphs", tabName = "Regional", icon = icon("square"))
  )
)

body<-dashboardBody(
  tabItems(
    tabItem(tabName = "Introduction",
                   includeHTML("/srv/shiny-server/databases/prevalencemaps/introprevalencemaps.html")
    ),
    tabItem(tabName = "Map",
            fluidRow(
              column(4,
                     selectInput("control_disease", "Condition of Interest:", 
                                 choices=c("Asthma" = "Asthma","CHD" = "CHD", "Flushot"= "Flushot"))
                     ),
              column(4,
                     selectInput("var", "Year:",
                                 c("2007" = "2007", "2008" = "2008", "2009" = "2009",
                                   "2010" = "2010", "2011" = "2011", "2012" = "2012",
                                   "2013" = "2013", "2014" = "2014", "2015" = "2015",
                                   "2016" = "2016", "2017" = "2017",
                                   "2007-2017 (all years)" = "2007-2017 (all years)",
                                   "2007-2010" = "2007-2010",
                                   "2011-2017" = "2011-2017"), selected = "2007-2017 (all years)")),
              column(4,
                     selectInput("mmsa_input", "MMSA:",
                                 choices = mmsa_names$x))
            ),
            fluidRow(        
              column(12,
                     box(width=NULL,
                         h5(textOutput("mapyearinfo"), align="center"),
                         leafletOutput("map"),
                         actionButton("reset_button", "Reset view"),
                         h5(textOutput("mapinfo"), align="center")
                     ))
            )),
    tabItem(tabName = "MMSAGraph",
            fluidRow(
              column(12,
                     h5(textOutput("graphinfo"), align="center"))
            ),
            fluidRow(
              column(4,
                     box(width=NULL,
                         plotOutput("income_plot", height = 140),
                         plotOutput("smoking_plot", height = 140)))
              ,
              column(4,
                     box(width=NULL,
                         plotOutput("race_plot", height = 140),
                         plotOutput("bmi_plot", height = 140))
              ),
              column(4,
                     box(width=NULL,
                         plotOutput("gender_plot", height = 140),
                         plotOutput("age_plot", height = 140))
              ))),
    tabItem(tabName = "Bivariate",
            fluidRow(
              box(width=5, height = 350, h2("Bivariate Relationships"),
                  selectInput("control_disease2",
                              "Condition of Interest:",
                              choices=c("Asthma" = "Asthma","CHD" = "CHD", "Flushot"= "Flushot")
                  ),
                  selectInput("variable", "Variable of Interest:",
                              choices = c("BMI"="BMI","Smoker"="Smoker","Education"="Education","Income"="Income",
                                          "Race"="Race")),
                  selectInput("variableyear1", "Years:",
                              choices=c("2007-2017"="2007_2017","2007-2010"="2007_2010","2011-2017"="2011_2017"))
                  ),
              box(width=7,
                  plotOutput("graph", height = 350))
            )),
            #fluidRow(
            #  box(width=12,h4("Regression Output with Odds Ratios"),
            #      tableOutput("summary"))
            #)
    tabItem(tabName = "Multivariate",        
    fluidRow(
              box(width=5,height = 400,h2("Multivariable Relationships"),
                  selectInput("control_disease3",
                              "Condition of Interest:",
                              choices=c("Asthma" = "Asthma","CHD" = "CHD", "Flushot"= "Flushot")),
                  selectInput("factors",
                              "Other Variables:",
                              choices=c("BMI"="BMI","Smoker"="Smoker","Education"="Education","Income"="Income",
                                        "Race"="Race","Flushot"="Flushot","Asthma"="Asthma","CHD"="CHD")),
                  selectInput("multivariable", "Stratifying Factor:",
                              choices = c("Income"="Income","Race"="Race","Sex"="Sex")),
                  selectInput("variableyear2", "Years:",
                              choices=c("2007-2017"="2007_2017","2007-2010"="2007_2010","2011-2017"="2011_2017"))
                  ),
              box(width=7,
                  plotOutput("multigraph", height=400))
              )),
  #  fluidRow(
  #    box(width=12,
  #        h4("Regression Output with Odds Ratios"),
  #        tableOutput("summarymulti"))
  #  )
            
    tabItem(tabName = "Regional",
            fluidRow(
              box(width=5,h2("Relationships Across Regions"),
                  selectInput("control_disease4",
                              "Condition of Interest:",
                              choices=c("Asthma" = "Asthma", "CHD" = "CHD", "Flushot"= "Flushot")),
                  selectInput("variable2",
                              "Variable of Interest:",
                              choices=c("BMI"="BMI","Smoker"="Smoker","Education"="Education","Income"="Income",
                                        "Race"="Race")),
                  selectInput("variableyear3", "Years:",
                              choices=c("2007-2017"="2007_2017","2007-2010"="2007_2010","2011-2017"="2011_2017"))
              
                  ),
                box(width=7,
                    plotOutput("regiongraph"))
            ))
    )
)
  


  shinyUI(
    dashboardPage(skin="black",dashboardHeader(title="Prevalence Maps"),
                  dashboardSidebar(sidebar),
                  dashboardBody(body),
                  tags$script(src="gomap.js")
                  )
                
                )
  #add after body if want to add css and js code 
  #,
  # tags$head(
  #   theme = "styles.css"
  # ),
  # tags$script(src="hoge.js")
