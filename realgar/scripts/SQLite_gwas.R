#Install package
require("RSQLite")
library(DBI)
library(dplyr)
library(dbplyr)
library(feather)
library(viridis)

#Read in files
#load info for gene tracks: gene locations, TFBS, SNPs, etc.
tfbs <- read_feather("/srv/shiny-server/databases/tfbs_for_app.feather") #TFBS data from ENCODE - matched to gene ids using bedtools
snp <- read_feather("/srv/shiny-server/databases/grasp_output_for_app.feather") #SNP data from GRASP - matched to gene ids using bedtools
snp_eve <- read_feather("/srv/shiny-server/databases/eve_data_realgar.feather") #SNP data from EVE - was already in hg19 - matched to gene ids using bedtools 
snp_gabriel <- read_feather("/srv/shiny-server/databases/gabriel_data_realgar.feather") #SNP data from GABRIEL - lifted over from hg17 to hg19 - matched to gene ids using bedtools 
snp_fer <- read_feather("/srv/shiny-server/databases/allerg_GWAS_data_realgar.feather") #SNP data from Ferreira - already in hg19 - matched to gene ids using bedtools
snp_TAGC <- read_feather("/srv/shiny-server/databases/TAGC_data_realgar.feather") #SNP data from TAGC - already in hg19 - matched to gene ids using bedtools
gene_locations <- read_feather("/srv/shiny-server/databases/gene_positions.feather")

breaks <- c(seq(0,8,by=0.001), Inf) # this sets max universally at 8 (else highest one OF THE SUBSET would be the max)

#SNP
snp <- dplyr::mutate(snp, neg_log_p = -log10(p))
snp$color <- inferno(8002)[as.numeric(cut(snp$neg_log_p, breaks = breaks))]


#SNP_EVE
snp_eve <- dplyr::mutate(snp_eve, neg_log_meta_p = -log10(meta_P),
                         neg_log_meta_p_aa = -log10(meta_P_AA),
                         neg_log_meta_p_ea = -log10(meta_P_EA),
                         neg_log_meta_p_lat = -log10(meta_P_LAT))
snp_eve$color_meta_P <- inferno(8002)[as.numeric(cut(snp_eve$neg_log_meta_p,breaks = breaks))]
snp_eve$color_meta_P_AA <- inferno(8002)[as.numeric(cut(snp_eve$neg_log_meta_p_aa,breaks = breaks))]
snp_eve$color_meta_P_EA <- inferno(8002)[as.numeric(cut(snp_eve$neg_log_meta_p_ea,breaks = breaks))]
snp_eve$color_meta_P_LAT <- inferno(8002)[as.numeric(cut(snp_eve$neg_log_meta_p_lat,breaks = breaks))]

#SNP gabriel
snp_gabriel <- dplyr::mutate(snp_gabriel, neg_log_p = -log10(P_ran))
snp_gabriel$color <- inferno(8002)[as.numeric(cut(snp_gabriel$neg_log_p, breaks = breaks))]

#SNP fer
snp_fer <- dplyr::mutate(snp_fer, neg_log_p = -log10(PVALUE))
snp_fer$color <- inferno(8002)[as.numeric(cut(snp_fer$neg_log_p, breaks = breaks))]

#SNP TAGC
snp_TAGC <- dplyr::mutate(snp_TAGC, neg_log_p_multi = -log10(p_ran_multi),
                          neg_log_p_euro = -log10(p_ran_euro))
snp_TAGC$color_p_ran_multi <- inferno(8002)[as.numeric(cut(snp_TAGC$p_ran_multi,breaks = breaks))]
snp_TAGC$color_p_ran_euro <- inferno(8002)[as.numeric(cut(snp_TAGC$p_ran_euro,breaks = breaks))]

#color tfbs based on binding score - used in tracks
#create color scheme based on encode binding score & snp p-values
tfbs$color <- inferno(50)[as.numeric(cut(tfbs$score,breaks = 50))]


#Put it in database
db = dbConnect(SQLite(), dbname="/mnt/volume_nyc1_01/data/realgar-gwas.sqlite")

#Check table
dbListTables(db)

#Add in database
dbWriteTable(conn=db, name="snp", snp, row.names=F)
dbWriteTable(conn=db, name="snp_eve", snp_eve, row.names=F)
dbWriteTable(conn=db, name="snp_gabriel", snp_gabriel, row.names=F)
dbWriteTable(conn=db, name="snp_fer", snp_fer, row.names=F)
dbWriteTable(conn=db, name="snp_TAGC", snp_TAGC, row.names=F)
dbWriteTable(conn=db, name="tfbs", tfbs, row.names=F)
dbWriteTable(conn=db, name="gene_locations", gene_locations, row.names=F)

#Check table
dbListTables(db)

#Disconnect
dbDisconnect(db)

