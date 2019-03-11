library(shiny)
library(datasets)

ui <- shinyUI(fluidPage(
  br(),
  
  # Application title
  #titlePanel(h2("Lung Cell Transcriptome Explorer", align="center")),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      p("Expression results for individual genes using publicly available RNA-Seq datasets."),
      br(),
      
      selectInput("tissue", label = "Tissue:",
                  choices = list("Airway Smooth Muscle 1" = "ASM1", "Airway Smooth Muscle 2" = "ASM2","Airway Smooth Muscle 3" = "ASM3","Small Airway Epithelial"="SAE"), 
                  selected="ASM1"),
      
      #textInput(inputId="gene",label="Type the official gene symbol:", value= "GAPDH"),
      uiOutput("genesAvail"), tags$head(tags$style(type="text/css", "#curr_gene {width: 190px}")),
      br(),
      
      p("Read counts of each gene is obtained by HTSeq and normalized by",
        a("DESeq2.", href="https://www.ncbi.nlm.nih.gov/pubmed/25516281", target="_blank"),
        "Differential expression results were obtained by DESeq2. Genes with a total read count <10 were filtered out before differential analysis."),
        # a("sleuth.", href="http://pachterlab.github.io/sleuth/", target="_blank"),
        # "The table includes transcripts for which at least 47% of samples had 5 or more reads.",
        # "The plot includes transcripts in the table with average TPM > 1 across all conditions."),
      
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
