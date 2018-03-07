library(shiny)
library(datasets)

ui <- shinyUI(fluidPage(
  br(),
  
  # Application title
  #titlePanel(h2("Lung Cell Transcriptome Explorer", align="center")),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      p("Expression results for individual gene transcripts using publicly available RNA-Seq datasets."),
      br(),
      
      selectInput("tissue", label = "Tissue:",
                  choices = list("Airway Smooth Muscle 1" = "ASM1", "Airway Smooth Muscle 2" = "ASM2", "Airway Epithelium" = "AE"), 
                  selected="AE"),
      
      textInput(inputId="gene",label="Type the official gene symbol:", value= "GAPDH"),
      br(),
      
      p("Abundance of each transcript is expressed in Transcripts per Million Reads Mapped (TPM) as computed by",
        a("kallisto.", href="https://pachterlab.github.io/kallisto/", target="_blank"),
        "Differential expression results were obtained with",
        a("sleuth.", href="http://pachterlab.github.io/sleuth/", target="_blank"),
        "The table includes transcripts for which at least 47% of samples had 5 or more reads.",
        "The plot includes transcripts in the table with average TPM > 1 across all conditions."),
      
      br(),
      #textInput("debugcode", "Debug:", ""),
      
      br(),
      br(),
      br(),
      fluidRow(column(3, imageOutput("logo")),
      "Created with ",
      a("RStudio's Shiny", href="http://www.rstudio.com/shiny", target="_blank")
    )),
    
    mainPanel(p(""),
              #h5("Results For Each Experimental Condition", align="center"),
              #h3(textOutput("gene", container=span)),
              plotOutput("GeneBoxPlot"),
              br(),br(),br(),br(),br(),
              uiOutput("studyText"),
              downloadButton('downloadPic', 'Download Figure'),
              br(),
              br(),
              htmlOutput("table_title", align="center"),
#               h5(paste0("Differential Expression Results for ",textOutput("gene")), align="center"),
              dataTableOutput("diffResults"),
              br()
    )
  )
))
