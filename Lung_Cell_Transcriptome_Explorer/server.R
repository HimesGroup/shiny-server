library(shiny)
library(datasets)
library(ggplot2)
library(data.table)
library(magrittr)
library(dplyr)

#Enable Browser Mode on Errors for Debugging Purposes
#options(error = browser)

#Load data files - gene names and dataset info
sras <- fread("../databases/dataset_info.csv") %>% tbl_df
all_genes <- fread("../databases/gene_names.csv") %>% tbl_df
unfiltered_genes <- fread("../databases/sleuth_unfiltered_genes.csv") %>% tbl_df

#sleuth output: fold changes, pvalues, conditions - for data table beneath plot
de <- list()
de[["SRP005411"]] <- fread("../databases/SRP005411_full_sleuth.txt", sep = " ") %>% tbl_df
de[["SRP043162"]] <- fread("../databases/SRP043162_full_sleuth.txt", sep = " ") %>% tbl_df
de[["SRP033351"]] <- fread("../databases/SRP033351_full_sleuth.txt", sep = " ") %>% tbl_df

#kallisto results - by-transcript TPMs, used for the plots
tpms <- list()
tpms[["SRP005411"]] <- fread("../databases/SRP005411_full_kallisto.txt", sep = " ") %>% tbl_df
tpms[["SRP043162"]] <- fread("../databases/SRP043162_full_kallisto.txt", sep = " ") %>% tbl_df
tpms[["SRP033351"]] <- fread("../databases/SRP033351_full_kallisto.txt", sep = " ") %>% tbl_df

# make a list of gene symbols in all datasets for checking whether gene symbol entered is valid - used later on
sleuth_filtered_genes <- unique(c(de$SRP005411$ext_gene, de$SRP043162$ext_gene, de$SRP033351$ext_gene))

server <- shinyServer(function(input, output, session) {
    
    updateSelectizeInput(session, "gene", choices=all_genes$name, selected="GAPDH", server=TRUE)
    
    curr_gene <- reactive({gsub(" ", "", toupper(input$gene), fixed = TRUE)})
    
    #used to make situation-specific error messages
    in_all <- reactive({if (!(curr_gene() %in% all_genes$name)) {FALSE} else {TRUE}})  # wrong symbol was input
    in_unfiltered <- reactive({if ((curr_gene() %in% all_genes$name) & !(curr_gene() %in% unfiltered_genes$x)) {FALSE} else {TRUE}}) # gene not in database
    in_sleuth_filtered <- reactive({if ((curr_gene() %in% unfiltered_genes$x) & !(curr_gene() %in% sleuth_filtered_genes)) {FALSE} else {TRUE}}) # didn't pass sleuth filter
    
    output$gene <- renderPrint({
      curr_gene()
    })
    
    output$studyText <- renderUI({
        if (!is.null(input$debugcode) && (input$debugcode == "studyText")) {
            browser()
        }
        
        sras %>% filter(Tissue == input$tissue) %$% 
            p("Data used is available in the SRA under accession ",
              a(paste0(SRA_ID, ","), href=paste0("http://www.ncbi.nlm.nih.gov/sra/?term=", SRA_ID)),
              "and corresponds to ",
              Description,
              if (PMID != "-") {
                  HTML(paste0("More details were published <a href=http://www.ncbi.nlm.nih.gov/pubmed/?term=", PMID, ">here.</a>"))
              }
            )
    })
    
    getGeneBoxPlot <-reactive({
      
      validate(need(curr_gene() != "", "Please enter a gene id")) # no gene symbol was input
      validate(need(in_all() != FALSE, "Please enter a valid gene id.")) # invalid gene symbol was input
      validate(need(in_unfiltered() != FALSE, "This gene is not in the reference database.")) # gene not in database
      validate(need(in_sleuth_filtered() != FALSE, "No transcripts for this gene passed the sleuth filter (at least 53% of the replicates must have 5+ reads).")) # gene did not pass sleuth filter
      
      # This may be because less than , or all of the gene .
        x <- sras %>% 
            filter(Tissue == input$tissue) %$% 
            SRA_ID
        
        curr_data <- tpms[[x]] %>%
            filter(ext_gene == curr_gene()) 
        
        if (nrow(curr_data) > 0) {
            
            curr_data$average_tpm <- vector(length=nrow(curr_data))
            
            for (i in 1:nrow(curr_data)) {
                curr_data$average_tpm[i] <- mean(curr_data[which(curr_data$target_id == curr_data$target_id[i]),]$tpm)
            }
            
            curr_data <- curr_data %>%
                filter(average_tpm > 1)
        }    
        
        in_filtered <- reactive({if (nrow(curr_data) == 0) {FALSE} else {TRUE}}) # didn't pass sleuth filter
        validate(need(in_filtered() != FALSE, "All transcripts for this gene had very low expression (average TPM < 1).")) # gene did not pass our filter
        
        if (nrow(curr_data) > 0) { # this iteration of curr_data has already been filtered to only have average_tpm > 1
            gene_plot <- ggplot(curr_data, aes(x = condition, y = tpm, fill=condition)) + 
                geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18") + 
                stat_boxplot(geom ='errorbar', color="grey18") + 
                geom_jitter(size=0.8, width=0.2) +
                facet_wrap(~target_id) + 
                guides(fill=FALSE) + 
                theme_bw() +  
                labs(title=curr_gene()) + 
                labs(x="condition") + labs(y="TPM") + 
                theme(text = element_text(size=9), 
                      strip.text.x = element_text(size = 10), 
                      axis.text.x = element_text(angle = 90, hjust = 1, size=12),
                      axis.text.y = element_text(size=9),
                      title = element_text(size=12),
                      axis.title.x = element_text(size=12),
                      axis.title.y = element_text(size=12))
            if (nrow(curr_data) > 0) {gene_plot}
        }
    })    
    
    output$GeneBoxPlot <- renderPlot(width = 650, height = 500, {
        if (!is.null(input$debugcode) && (input$debugcode == "geneBoxPlot")) {
            browser()
        }
        gbp <- getGeneBoxPlot()
        gbp
    })
    
    output$table_title <- renderUI({
      title_string <- paste0("Differential Expression Results for ", curr_gene())
      HTML(title_string)
    })
    
    output$diffResults <- renderDataTable({
        if (!is.null(input$debugcode) && (input$debugcode == "diffResults")) {
            browser()
        }
        
        sras %>% filter(Tissue == input$tissue) %$% de[[SRA_ID]] %>% subset(ext_gene %in% curr_gene()) %>%  
            mutate(b=round(b, digits=2), pval=format(pval, scientific=TRUE, digits=3), qval=format(qval, scientific=TRUE, digits=3)) %>% 
            arrange(-b, qval) %>% 
            dplyr::select(target_id, Comparison, b, pval, qval) %>% #rearrange columns in desired order
            dplyr::rename(`Transcript`=target_id, `Beta value`=b, `P-value`=pval, `Q-value`=qval)
        
        
    }, options=list(paging=FALSE, searching=FALSE)
    )
    
    output$downloadPic <- downloadHandler(
        filename = function() {paste(input$tissue, "_", curr_gene(), "_", Sys.Date(), '.png', sep='')},
        content = function(file) {
            png(file, width=10, height=6, units="in", res=600)
            print(getGeneBoxPlot())
            dev.off()
        },
        contentType = 'image/png'
    )
    
})
