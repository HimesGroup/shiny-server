.libPaths("/home/avantika/R/x86_64-pc-linux-gnu-library/3.4/") 
library(shiny)
library(shinyWidgets)
library(shinydashboard)

#Setting up the visual interface for the app
shinyUI(fluidPage(title="Prevalence Maps",
  titlePanel(h1("Prevalence Maps", align = "left")),
  tabsetPanel(
    tabPanel("Map of MMSA Data",
             #Setting controls for how the dropdown options work
            tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}"))),
            br(),
            #Setting up the layout of the page for display of data
            fluidRow(
              column(4,
                     selectInput("control_disease", "Condition of Interest:", 
                                 choices=c("Asthma" = "Asthma", "COPD" = "COPD", "CHD" = "CHD", "Diabetes"="Diabetes", 
                                           "Average BMI" = "`Average BMI`", "Average ADI" = "`Average ADI`",
                                           "Flushot Administration"= "`Flushot Administration`",
                                           "Good or Better Health" = "`Good or Better Health`",
                                           "Smoking" = "Smoking",
                                           "Depressive Disorder" = "`Depressive Disorder`",
                                           "Has Health Insurance" = "`Has Health Insurance`"))
                     ),
              column(4,
                     selectInput("var", "Year:",
                                 c("2011" = "2011", "2012" = "2012",
                                   "2013" = "2013", "2014" = "2014", "2015" = "2015",
                                   "2016" = "2016", "2017" = "2017",
                                   "2011-2017 (all years)" = "2011-2017 (all years)"
                                   ), selected = "2011-2017 (all years)")),
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
    tabPanel("About",
             includeHTML("/srv/shiny-server/databases/prevalencemaps/introprevalencemaps.html")
    ),
    tabPanel("MMSA-Specific Graphs",
            br(),
            fluidRow(
              tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}"))),
              #br(),
                column(4,
                       selectInput("control_diseaseg", "Condition of Interest:", 
                                   choices=c("Asthma" = "Asthma", "COPD" = "COPD", "CHD" = "CHD", "Diabetes"="Diabetes", 
                                             "Flushot Administration"= "`Flushot Administration`",
                                             "Good or Better Health" = "`Good or Better Health`",
                                             "Depressive Disorder" = "`Depressive Disorder`",
                                             "Has Health Insurance" = "`Has Health Insurance`"))
                ),
                column(4,
                       selectInput("varg", "Year:",
                                   c("2011" = "2011", "2012" = "2012",
                                     "2013" = "2013", "2014" = "2014", "2015" = "2015",
                                     "2016" = "2016", "2017" = "2017",
                                     "2011-2017 (all years)" = "2011-2017 (all years)"
                                   ), selected = "2011-2017 (all years)")),
                column(4,
                       selectInput("mmsa_inputg", "MMSA:",
                                   choices = mmsa_names$x))),
              hr(),
              column(12,
                     h4(textOutput("graphinfo"), align="center")),br(),
              column(12, fluidRow(
                      column(4,
                             box(width=NULL,
                                 plotOutput("income_plot", height = 240),br(), #140
                                 plotOutput("smoking_plot", height = 240))
                             ),
                      column(4,
                             box(width=NULL,
                                 plotOutput("race_plot", height = 240),br(),
                                 plotOutput("bmi_plot", height = 240))
                      ),
                      column(4,
                             box(width=NULL,
                                 plotOutput("gender_plot", height = 240),br(),
                                 plotOutput("age_plot", height = 240))),
              br()))),
    tabPanel("US Bivariate Graphs",
            fluidRow(
              box(width=3, height = 350, h2("Bivariate Relationships"),
                  selectInput("control_disease2",
                              "Condition of Interest:",
                              choices=c("Asthma" = "Asthma", "COPD" = "COPD", "CHD" = "CHD", "Diabetes"="Diabetes", 
                                        "Flushot Administration"= "`Flushot Administration`",
                                        "Good or Better Health" = "`Good or Better Health`",
                                        "Depressive Disorder" = "`Depressive Disorder`",
                                        "Has Health Insurance" = "`Has Health Insurance`")
                  ),
                  selectInput("variable", "Variable of Interest:",
                              choices = c("BMI"="BMI","Smokers"="Smokers","Education"="Education","Income"="Income",
                                          "Race"="Race", "ADI Quartile" = "`ADI Quartile`"))
                  ), br(),
              box(width=6,
                  plotOutput("graph", height = 500))
            )),
    tabPanel("US Multivariate Graphs",        
    fluidRow(
      br(),        
      box(width=3,height = 500,h2("Multivariable Relationships"),
                  selectInput("control_disease3",
                              "Condition of Interest:",
                              choices=c("Asthma" = "Asthma", "COPD" = "COPD", "CHD" = "CHD", "Diabetes"="Diabetes", 
                                        "Flushot Administration"= "`Flushot Administration`",
                                        "Good or Better Health" = "`Good or Better Health`",
                                        "Depressive Disorder" = "`Depressive Disorder`",
                                        "Has Health Insurance" = "`Has Health Insurance`")),
                  selectInput("factors",
                              "Other Variables:",
                              choices=c("Smokers"="Smokers","BMI"="BMI","Education"="Education","Income"="Income",
                                        "Race"="Race","Flushot"="`Flushot Administration`", "ADI Quartile" = "`ADI Quartile`")),
                  selectInput("multivariable", "Stratifying Factor:",
                              choices = c("BMI"="BMI","Education"="Education",
                                          "Income"="Income","Race"="Race","Sex"="Sex", "ADI Quartile" = "`ADI Quartile`"))
                 ),br(),
              box(width=7,
                  plotOutput("multigraph", height=800),align="center",br()) #, width = 1350
             )
     ),
    tabPanel("US Regional Graphs",
           br(),
           fluidRow(
              box(width=3,h2("Relationships Across Regions"),
                  selectInput("control_disease4",
                              "Condition of Interest:",
                              choices=c("Asthma" = "Asthma", "COPD" = "COPD", "CHD" = "CHD", "Diabetes"="Diabetes", 
                                        "Flushot Administration"= "`Flushot Administration`",
                                        "Good or Better Health" = "`Good or Better Health`",
                                        "Depressive Disorder" = "`Depressive Disorder`",
                                        "Has Health Insurance" = "`Has Health Insurance`")),
                  selectInput("variable2",
                              "Variable of Interest:",
                              choices=c("BMI"="BMI","Smokers"="Smokers","Education"="Education","Income"="Income",
                                        "Race"="Race",  "ADI Quartile" = "`ADI Quartile`"))
                  ),
                box(width=7,
                    plotOutput("regiongraph", height=800),align="center",br())
            ))
    )
  )
)