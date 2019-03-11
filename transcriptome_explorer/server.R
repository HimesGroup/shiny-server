library(shiny)
library(datasets)
library(ggplot2)
library(data.table)
library(magrittr)
library(dplyr)
library(feather)

#Enable Browser Mode on Errors for Debugging Purposes
#options(error = browser)

#Load data files - gene names and dataset info
# "lcte" appended to beginning of filename stands for "lung cell transcriptome explorer"
sras <- read_feather("/srv/shiny-server/databases/asthmagenes_deseq2/lcte_dataset_info_asm.feather") %>% tbl_df
all_genes <- read_feather("/srv/shiny-server/databases/lcte_gene_names.feather") %>% tbl_df
unfiltered_genes <- read_feather("/srv/shiny-server/databases/lcte_sleuth_unfiltered_genes.feather") %>% tbl_df
#all_genes <- read_feather("/srv/shiny-server/databases/Gene_names.feather")

#deseq2 results : log2FC, padj and conditions- for datatable 
de <- list()
de[["SRP033351"]] <- read_feather("/srv/shiny-server/databases/asthmagenes_deseq2/SRP033351/SRP033351_de.feather") %>% tbl_df
de[["SRP043162"]] <- read_feather("/srv/shiny-server/databases/asthmagenes_deseq2/SRP043162/SRP043162_de.feather") %>% tbl_df
de[["SRP098649"]] <- read_feather("/srv/shiny-server/databases/asthmagenes_deseq2/SRP098649/SRP098649_de.feather") %>% tbl_df
de[["SRP005411"]] <- read_feather("/srv/shiny-server/databases/asthmagenes_deseq2/SRP005411/SRP005411_de.feather") %>% tbl_df

#Deseq2 count results - by gene for plots
tpms <- list()
tpms[["SRP033351"]] <- read_feather("/srv/shiny-server/databases/asthmagenes_deseq2/SRP033351/SRP033351_pheno+counts.feather") %>% tbl_df
tpms[["SRP043162"]] <- read_feather("/srv/shiny-server/databases/asthmagenes_deseq2/SRP043162/SRP043162_pheno+counts.feather") %>% tbl_df
tpms[["SRP098649"]] <- read_feather("/srv/shiny-server/databases/asthmagenes_deseq2/SRP098649/SRP098649_pheno+counts.feather") %>% tbl_df
tpms[["SRP005411"]] <- read_feather("/srv/shiny-server/databases/asthmagenes_deseq2/SRP005411/SRP005411_pheno+counts.feather") %>% tbl_df


# make a list of gene symbols in all datasets for checking whether gene symbol entered is valid - used later on
deseq2_filtered_genes <- unique(c(de$SRP005411$gene_symbol, de$SRP043162$gene_symbol, de$SRP033351$gene_symbol, de$SRP005411$gene_symbol))

server <- shinyServer(function(input, output, session) {
    
    #updateSelectizeInput(session, "gene", choices=all_genes$name, selected="GAPDH", server=TRUE)
    genes <- reactive({selectizeInput("gene", "Official Gene Symbol:", all_genes, selected="GAPDH", width="185px", options = list(create = TRUE))})
    output$genesAvail <- renderUI({genes()})
    
    #gene <- reactive({toString(input$gene)})
    curr_gene <- reactive({gsub(" ", "", toupper(toString(input$gene)), fixed = TRUE)})
    
    #used to make situation-specific error messages
    in_all <- reactive({if (!(curr_gene() %in% all_genes$name)) {FALSE} else {TRUE}})  # wrong symbol was input
    in_unfiltered <- reactive({if ((curr_gene() %in% all_genes$name) & !(curr_gene() %in% unfiltered_genes$x)) {FALSE} else {TRUE}}) # gene not in database
    in_deseq2_filtered <- reactive({if ((curr_gene() %in% unfiltered_genes$x) & !(curr_gene() %in% deseq2_filtered_genes)) {FALSE} else {TRUE}}) # didn't pass sleuth filter
    
    output$gene <- renderPrint({
      curr_gene()
    })
    
    # more information about the dataset selected
    output$studyText <- renderUI({
        if (!is.null(input$debugcode) && (input$debugcode == "studyText")) {
            browser()
        }
        
        sras %>% filter(Tissue == input$tissue) %$% 
            p("Data used is available in the SRA under accession ",
              a(paste0(SRA_ID, ","), href=paste0("http://www.ncbi.nlm.nih.gov/sra/?term=", SRA_ID), target="_blank"),
              "and corresponds to ",
              Description,
              "More details were published ",
              a("here.", href=paste0("http://www.ncbi.nlm.nih.gov/pubmed/?term=", PMID), target="_blank"))
    })
    
    #generate facetted boxplot for the gene selected, using kallisto TPMs
    getGeneBoxPlot <-reactive({
      
      validate(need(curr_gene() != "", "Please enter a gene id")) # no gene symbol was input
      validate(need(in_all() != FALSE, "Please enter a valid gene id.")) # invalid gene symbol was input
      validate(need(in_unfiltered() != FALSE, "This gene is not in the reference database.")) # gene not in database
      validate(need(in_deseq2_filtered() != FALSE, "Gene did not pass our DESeq2 filter (total counts should be greater than 10).")) # gene did not pass sleuth filter
      
      # This may be because less than , or all of the gene .
        x <- sras %>% 
            filter(Tissue == input$tissue) %$% 
            SRA_ID
        
        curr_data <- tpms[[x]] %>% filter(gene_symbol == curr_gene()) 
        
        # if (nrow(curr_data) > 0) {
        #     
        #     # curr_data$average_tpm <- vector(length=nrow(curr_data))
        #     # 
        #     # for (i in 1:nrow(curr_data)) {
        #     #     curr_data$average_tpm[i] <- mean(curr_data[which(curr_data$target_id == curr_data$target_id[i]),]$tpm)
        #     # }
        #     # 
        #     # curr_data <- curr_data %>%
        #     #     filter(average_tpm > 1)
        #   
        #    curr_data
        #}    
        
        in_filtered <- reactive({if (nrow(curr_data) == 0) {FALSE} else {TRUE}}) # didn't pass sleuth filter
        #validate(need(in_filtered() != FALSE, "All transcripts for this gene had very low expression (average TPM < 1).")) # gene did not pass our filter
        
        if (nrow(curr_data) > 0) { # this iteration of curr_data has already been filtered to only have average_tpm > 1
            gene_plot <- ggplot(curr_data, aes(x = Status, y = value, fill=Status)) + 
                geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18") + 
                stat_boxplot(geom ='errorbar', color="grey18") + 
                geom_jitter(aes(shape=Donor),size=1, width=0.2) +
                scale_shape_manual(values=seq(0,length(curr_data$Donor))) + 
                facet_wrap(~Gene) + 
                guides(fill=FALSE) + 
                theme_bw() +  
                labs(title=curr_gene()) + 
                labs(x="condition") + labs(y="Normalized Read Count") + 
                theme(text = element_text(size=9), 
                      strip.text.x = element_text(size = 10), 
                      axis.text.x = element_text(angle = 90, hjust = 1, size=12),
                      axis.text.y = element_text(size=9),
                      title = element_text(size=12),
                      axis.title.x = element_text(size=12),
                      legend.text=element_text(size=9),
                      #axis.title.x = element_blank(),
                      axis.title.y = element_text(size=12))
            if (nrow(curr_data) > 0) {gene_plot}
        }
    })    
    
    #output boxplot
    output$GeneBoxPlot <- renderPlot(width = 650, height = 500, {
        if (!is.null(input$debugcode) && (input$debugcode == "geneBoxPlot")) {
            browser()
        }
        gbp <- getGeneBoxPlot()
        gbp
    })
    
    #output accompanying sleuth results
    output$table_title <- renderUI({
      title_string <- paste0("Differential Expression Results for ", curr_gene())
      HTML(title_string)
    })
    
    output$diffResults <- renderDataTable({
        if (!is.null(input$debugcode) && (input$debugcode == "diffResults")) {
            browser()
        }
        
        sras %>% filter(Tissue == input$tissue) %$% de[[SRA_ID]] %>% subset(gene_symbol %in% curr_gene()) %>%  
            arrange(-log2FoldChange, padj) %>%   
            mutate(log2FoldChange=round(log2FoldChange, digits=2), padj=format(padj, scientific=TRUE, digits=3)) %>% 
            dplyr::select(Gene, Comparison,log2FoldChange, padj) %>% #rearrange columns in desired order
            dplyr::rename(`Gene`= Gene, `LogFC`= log2FoldChange, `Q-value`= padj)
        #removed p-values from the table, since we had originally saved two of the datasets without p-values to save space
        
    }, options=list(paging=FALSE, searching=FALSE)
    )
    
    #R logo for user interface 
    output$logo <- renderImage({ 
      return(list(
        src = "/srv/shiny-server/databases/www/bigorb.png",
        height=38.7*1.5,
        width=42.7*1.5,
        filetype = "image/png",
        alt = "bigorb"))}, deleteFile = FALSE)
    
    #download gene boxplot
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
