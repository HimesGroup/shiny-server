
.libPaths("/home/maya/R/x86_64-pc-linux-gnu-library/3.4/") # else have problems with Gviz package not being found
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(feather, quietly = T)
library(dplyr, quietly = T)
library(data.table, quietly = T)
library(ggplot2, quietly = T)
library(cowplot, quietly = T)
library(lattice, quietly = T)
library(stringr, quietly = T)
library(viridis, quietly = T) 
library(DT, quietly = T) 
source("utilities/sql_queries.R")
source("utilities/meta.R")
source("utilities/comb_pval.R")
source("utilities/name_convert.R")
source("utilities/forestplots.R")



##############################################
## Choices for tissue, asthma and treatment ##
##############################################

#Tissue types
tissue_choices <-c("Airway smooth muscle"="ASM", "Bronchial epithelium"="BE","Lens epithelium" = "LEC","BEAS-2B" = "BEAS-2B",
                   "Nasal epithelium"="NE","Small airway epithelium"="SAE","Whole lung"="Lung","Skeletal muscle myotubes"="myotubes",
                   "CD4"="CD4", "CD8"="CD8", "MCF10A-Myc" = "MCF10A-Myc","CD3" = "CD3", "A549" = "A549", 
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

#Gene list
all_genes <- read_feather("/srv/shiny-server/databases/gene_list.feather")
gene_list <- as.vector(all_genes$V1)
rm(all_genes)

#Gene choices
genec <- read_feather("/srv/shiny-server/databases/Sig_gene_list.feather")
gene_choices <- as.vector(genec$V1)
rm(genec)

####################
## READ IN FILES ##
####################

# load descriptions of all gene expression and GWAS datasets
Alldata_Info <- read_feather("/srv/shiny-server/databases/Microarray_data_infosheet_latest_R.feather")

#then split off into gene expression and GWAS dataset info - else forest plot text columns get messed up
GWAS_Dataset_Info <- Alldata_Info[which(Alldata_Info$App == "GWAS"),]
Dataset_Info <- Alldata_Info[which(!(Alldata_Info$App == "GWAS")),]

#Remove big data ---
rm(Alldata_Info)

Dataset_Info$PMID <- as.character(Dataset_Info$PMID) #else next line does not work
Dataset_Info[is.na(Dataset_Info$PMID), "PMID"] <- ""
Dataset_Info$Report <- as.character(c("QC"))

##BA_PDE dataset ---
BA_PDE_Info <- Dataset_Info %>% dplyr::filter(Asthma == "BA_PDE")

####################
## GWAS SNP data ##
####################

#load info for gene tracks: gene locations, TFBS, SNPs, etc.

#from feather files ---
chrom_bands <- read_feather("/srv/shiny-server/databases/chrom_bands.feather") #chromosome band info for ideogram - makes ideogram load 25 seconds faster


