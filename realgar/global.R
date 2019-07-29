.libPaths("/home/maya/R/x86_64-pc-linux-gnu-library/3.4/") # else have problems with Gviz package not being found

library(shiny)
library(feather)
library(dplyr)
library(data.table)
library(ggplot2)
library(cowplot)
library(forestplot) 
library(lattice)
library(stringr)
library(viridis) 
library(DT) 
source("/srv/shiny-server/realgar/utilities/meta.R")
source("/srv/shiny-server/realgar/utilities/comb_pval.R")
source("/srv/shiny-server/realgar/utilities/name_convert.R")


##############################################
## Choices for tissue, asthma and treatment ##
##############################################

#Tissue types
tissue_choices <-c("Airway smooth muscle"="ASM", "Bronchial epithelium"="BE","Lens epithelium" = "LEC","BEAS-2B" = "BEAS-2B",
                   "Nasal epithelium"="NE","Small airway epithelium"="SAE","Whole lung"="Lung","Skeletal muscle myotubes"="myotubes",
                   "CD4"="CD4", "CD8"="CD8", "MCF10A-Myc" = "MCF10A-Myc","CD3" = "CD3",
                   "Lymphoblastoid cell" = "LCL","Macrophage" = "MACRO",  
                   "Peripheral blood mononuclear cell"="PBMC","White blood cell"="WBC","Whole blood"="Blood",
                   "Lymphoblastic leukemia cell" = "chALL","Osteosarcoma U2OS cell" = "U2OS")

#Disease types
asthma_choices <- c("Allergic asthma"="allergic_asthma", "Asthma"="asthma",
                    "Fatal asthma"="fatal_asthma", "Mild to moderate asthma"="mild_to_moderate_asthma","Severe asthma"="severe_asthma",
                    "Mild asthma with rhinitis"="rhinitis_mild_asthma","Severe asthma with rhinitis"="rhinitis_severe_asthma",
                    "Non-allergic asthma"="non_allergic_asthma")

#Treatment choices
treatment_choices <- c("β2-agonist"="BA", 
                       "Phosphodiesterase inhibitor"="PDE",
                       "Smoking"="smoking", "Vitamin D"="vitD","Glucocorticoid" = "GC")

#GWAS options
gwas_choices <- c("EVE"="snp_eve_subs","Ferreira"="snp_fer_subs","GABRIEL"="snp_gabriel_subs","GRASP"="snp_subs","TAGC"="snp_TAGC_subs")


####################
## READ IN FILES ##
####################

# load descriptions of all gene expression and GWAS datasets
Alldata_Info <- read_feather("/mnt/volume_nyc1_01/data/Microarray_data_infosheet_latest_R.feather")

#then split off into gene expression and GWAS dataset info - else forest plot text columns get messed up
GWAS_Dataset_Info <- Alldata_Info[which(Alldata_Info$App == "GWAS"),]
Dataset_Info <- Alldata_Info[which(!(Alldata_Info$App == "GWAS")),]

#load and name GEO microarray and RNA-Seq datasets
for (i in na.omit(Dataset_Info$Unique_ID)) {assign(i, read_feather(paste0("/srv/shiny-server/databases/results_feather_files/", i, ".feather")))}

Dataset_Info$PMID <- as.character(Dataset_Info$PMID) #else next line does not work
Dataset_Info[is.na(Dataset_Info$PMID), "PMID"] <- ""
Dataset_Info$Report <- as.character(c("QC"))

##BA_PDE dataset
BA_PDE_Info <- Dataset_Info %>% dplyr::filter(Asthma == "BA_PDE")

#load info for gene tracks: gene locations, TFBS, SNPs, etc.
tfbs <- read_feather("/srv/shiny-server/databases/tfbs_for_app.feather") #TFBS data from ENCODE - matched to gene ids using bedtools
snp <- read_feather("/srv/shiny-server/databases/grasp_output_for_app.feather") #SNP data from GRASP - matched to gene ids using bedtools
snp_eve <- read_feather("/srv/shiny-server/databases/eve_data_realgar.feather") #SNP data from EVE - was already in hg19 - matched to gene ids using bedtools 
snp_gabriel <- read_feather("/srv/shiny-server/databases/gabriel_data_realgar.feather") #SNP data from GABRIEL - lifted over from hg17 to hg19 - matched to gene ids using bedtools 
snp_fer <- read_feather("/srv/shiny-server/databases/allerg_GWAS_data_realgar.feather") #SNP data from Ferreira - already in hg19 - matched to gene ids using bedtools
snp_TAGC <- read_feather("/srv/shiny-server/databases/TAGC_data_realgar.feather") #SNP data from TAGC - already in hg19 - matched to gene ids using bedtools
gene_locations <- fread("/srv/shiny-server/databases/gene_positions.txt", header = TRUE, stringsAsFactors = FALSE) #gene location & transcript data from GENCODE
chrom_bands <- read_feather("/srv/shiny-server/databases/chrom_bands.feather") #chromosome band info for ideogram - makes ideogram load 25 seconds faster
all_genes <- read_feather("/srv/shiny-server/databases/Gene_names.feather")
#unlike all other files, gene_locations is faster with fread than with readRDS (2s load, vs 4s)


####################
## GWAS SNP data ##
####################

#compute -log10 for SNPs -- used for SNP colors
snp <- dplyr::mutate(snp, neg_log_p = -log10(p))
snp_eve <- dplyr::mutate(snp_eve, neg_log_meta_p = -log10(meta_P),
                         neg_log_meta_p_aa = -log10(meta_P_AA),
                         neg_log_meta_p_ea = -log10(meta_P_EA),
                         neg_log_meta_p_lat = -log10(meta_P_LAT))
snp_gabriel <- dplyr::mutate(snp_gabriel, neg_log_p = -log10(P_ran))
snp_fer <- dplyr::mutate(snp_fer, neg_log_p = -log10(PVALUE))
snp_TAGC <- dplyr::mutate(snp_TAGC, neg_log_p_multi = -log10(p_ran_multi),
                          neg_log_p_euro = -log10(p_ran_euro))

#color tfbs based on binding score - used in tracks
#create color scheme based on encode binding score & snp p-values
tfbs$color <- inferno(50)[as.numeric(cut(tfbs$score,breaks = 50))]

breaks <- c(seq(0,8,by=0.001), Inf) # this sets max universally at 8 (else highest one OF THE SUBSET would be the max)

snp$color <- inferno(8002)[as.numeric(cut(snp$neg_log_p, breaks = breaks))]
snp_gabriel$color <- inferno(8002)[as.numeric(cut(snp_gabriel$neg_log_p, breaks = breaks))]
snp_fer$color <- inferno(8002)[as.numeric(cut(snp_fer$neg_log_p, breaks = breaks))]

snp_TAGC$color_p_ran_multi <- inferno(8002)[as.numeric(cut(snp_TAGC$p_ran_multi,breaks = breaks))]
snp_TAGC$color_p_ran_euro <- inferno(8002)[as.numeric(cut(snp_TAGC$p_ran_euro,breaks = breaks))]

snp_eve$color_meta_P <- inferno(8002)[as.numeric(cut(snp_eve$neg_log_meta_p,breaks = breaks))]
snp_eve$color_meta_P_AA <- inferno(8002)[as.numeric(cut(snp_eve$neg_log_meta_p_aa,breaks = breaks))]
snp_eve$color_meta_P_EA <- inferno(8002)[as.numeric(cut(snp_eve$neg_log_meta_p_ea,breaks = breaks))]
snp_eve$color_meta_P_LAT <- inferno(8002)[as.numeric(cut(snp_eve$neg_log_meta_p_lat,breaks = breaks))]

output.table <- data.frame() # initiate output table - used later in output.tableforplot()
heatmap_colors <-  inferno # heatmap colors - used in p-value plot

# make a list of gene symbols in all datasets for checking whether gene symbol entered is valid - used for GeneSymbol later on
genes_avail <- vector()
for (i in ls()[grep("GSE", ls())]) {
  gene_names <- as.character(levels(get(i)$Gene))
  genes_avail <- unique(c(genes_avail, gene_names))
}

##########################
## FORESTPLOT FUNCTION ##
##########################

# Function: "forestplot_func" 
# Forest plots
forestplot_func <- function(dat, ptitle,curr_gene) {
  
  validate(need(nrow(dat) != 0, "No entries available for your selections. Please choose other options.")) #Generate a error message when no data available for selected options.
  
  # create an empty dataset for forestplot text
  tabletext <- data.frame(matrix(nrow=1, ncol=4))
  
  if ("asthma"%in%dat$App) {
    text_temp <- dat[,c("GEO_ID","Long_tissue_name","Asthma","Q Value")]
  } else {
    text_temp <- dat[,c("GEO_ID","Long_tissue_name","Treatment","Q Value")]
  }
  
  # assign column names same as the original data
  names(tabletext) <- names(text_temp) <- c("GEO_ID","Long_tissue_name","Condition","Q Value")
  tabletext[1,] <- c("GEO ID", "Tissue", "Condition", "Q-Value")
  
  #Chnage to character strings
  text_temp$GEO_ID <- as.character(text_temp$GEO_ID)
  text_temp$Long_tissue_name <- as.character(text_temp$Long_tissue_name)
  
  # assign variables to meta-analysis result row
  if (nrow(dat)>1) {
    text_temp[nrow(dat),c("GEO_ID")] <- " "
    text_temp[nrow(dat),c("Long_tissue_name")] <- " "
    text_temp[nrow(dat),3] <- "Effect size-based integration =   " # "Asthma" or "Treatment" is in column 3
  }
  
  #Change to factor
  text_temp <- text_temp %>% mutate_all(as.factor)
  
  #List of alphabets - if forestplot gives overlap/height scaling error : add more combinations to list alphabets
  mix = paste(letters,LETTERS)
  alph1 = append(letters, LETTERS)
  alphabets <- append(alph1,mix)
  
  #Text table
  # add text for individual study result
  tablevector <- as.vector(as.matrix(rbind(tabletext,text_temp)))
  levels<-alphabets[1:(nrow(text_temp)+1)]
  x<-levels
  for (j in 1:(ncol(text_temp)-1)){x<-append(x,levels)}
  #Assign position for columns
  table <- data.frame(V0 = factor(x, rev(levels)), V05 = rep(c(0,0.2,0.6,1.0),each=nrow(text_temp)+1),V1=tablevector) #c(1,1.5,2.7,3.7) #0,0.2,0.7,1.1
  
  # remove double quote
  options(useFancyQuotes = FALSE)
  
  
  ##G1 plot1 - datatable
  g1 <- ggplot(table, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
    geom_text(size = 4.4, hjust=0, vjust=0.5,
              fontface = ifelse(table$V1 %in% c("Effect size-based integration =   ","GEO ID","Tissue","Condition","Q-Value")|table$V1 == table$V1[nrow(table)], 2, 1)) + 
    theme_bw() +
    geom_segment(y=nrow(dat)+1.5,yend=nrow(dat)+1.5,x=0,xend=1.1) + #x=1, xend=4.0 #x=0,xend=1.2
    geom_segment(y=nrow(dat)+0.5,yend=nrow(dat)+0.5,x=0,xend=1.1) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "lines"),
          plot.title = element_text(size=20, hjust=0.5, vjust=0.5))+
    labs(x="",y="") + #+ xlim(-0.5, 4) 
    coord_cartesian(xlim=c(0, 1.1)) #1,4.5 #3.8 #0, 1.2
  
  #Forest plot
  # table with fold changes for plot
  tableplot <- rbind(c(NA,NA,NA,NA),dat[,c("Fold Change","Lower_bound_CI","Upper_bound_CI")])	
  no_of_values <- nrow(dat)+1
  tableplot <- data.frame(sapply(tableplot, as.numeric))
  tableplot$group <-table$V0[1:no_of_values]
  
  #X-ticks
  xticks = seq(from = min(0.9, min(dat$Lower_bound_CI)), to = max(max(dat$Upper_bound_CI),1.2), length.out = 5)
  
  #Colors
  library(viridis)
  breaks <- c(seq(0,8,by=0.001), Inf) # this sets max universally at 8 (else highest one OF THE SUBSET would be the max)
  b_clrs <- l_clrs <- inferno(8002)[as.numeric(cut(dat$neglogofP, breaks = breaks))] #8002 is length(breaks) - ensures there are enough colors
  colors <- append(NA,b_clrs)
  tableplot$neglogofP2 <- append(NA,dat$neglogofP)
  
  #Point
  point<-c(NA)
  if (nrow(dat)>1){
    for (i in 2:(nrow(dat))){point<-append(point,15)}
    point <- append(point,18)
  }else{ point<-append(point,15)}
  
  #Box size
  if (nrow(dat)>1) {
    total <- as.numeric(dat$Total)
    boxsize=c(0,2*log10(total)) # 0 for the header line
  } else {boxsize=2} # boxsize = 2 default
  
  
  #Theme
  theme_set(theme_bw())
  
  theme_update(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill="#BDB6B0"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size=16),
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    axis.text=element_text(size=10)
  )
  
  ##G2 Plot 2 - forestplot
  g2 <- ggplot(tableplot,aes(Fold.Change,group)) + geom_point(aes(fill=neglogofP2),size=boxsize, shape=point, colour = colors, na.rm=TRUE)+
    geom_errorbarh(aes(xmax = tableplot$Upper_bound_CI, xmin = tableplot$Lower_bound_CI), height = 0.15, colour=colors ,na.rm=TRUE) +
    geom_vline(xintercept = 1, linetype = "longdash") + scale_x_continuous(breaks = xticks) + labs(x= "Fold Change",y="",fill="-log(Qvalue)") +
    #guides(fill=FALSE)
    scale_fill_gradientn(colours=inferno(8002),limits=c(0,8),breaks = seq(0,8)) + guides(fill = guide_colourbar(barheight = 10)) 
  
  #Legend size
  if (nrow(dat) > 5) {g2 <- g2 + theme(legend.text = element_text(size=10),legend.title = element_text(size=14))}
  else {g2 <- g2 + theme(legend.text = element_text(size=8),legend.title = element_text(size=12))}
  
  #Add title if specified  
  title <- ggdraw() + draw_label(paste0(ptitle, curr_gene),fontface = 'bold')
  
  #Bind two plots together and then bind this with the title
  fplot <- plot_grid(g1,g2,ncol=2,rel_widths = c(2.1,1),align = "h") #rel_widths #align="h",axis="tblr",
  plot_grid(title,fplot,ncol = 1, rel_heights = c(0.1, 1),align = "v")
  
}

#1 PX = 0.0104166653543 in.
getHeight_func <- function(dat){
  height=25 #27 optimum
  height<- 150 + height*(abs(nrow(dat)-2)) #Title: 3x + pcomb object (5px) #110
  return(height)
}
