##SQLITE database for REALGAR

#Install package
require("RSQLite")
library(DBI)
library(dplyr)
library(dbplyr)
library(feather)

#### SET UP Database #####
#path = "/mnt/volume_nyc1_01/data/"
# load descriptions of all gene expression and GWAS datasets
Alldata_Info <- read_feather("/mnt/volume_nyc1_01/data/Microarray_data_infosheet_latest_R.feather")

#then split off into gene expression and GWAS dataset info - else forest plot text columns get messed up
GWAS_Dataset_Info <- Alldata_Info[which(Alldata_Info$App == "GWAS"),]
Dataset_Info <- Alldata_Info[which(!(Alldata_Info$App == "GWAS")),]

##Get dataframe with only Total and App columns with Unique ID
Total_Info <- Dataset_Info %>% dplyr::select(Unique_ID,Total,App)

## Read in datafiles for transcriptomics studies
#load and name GEO microarray and RNA-Seq datasets
#/mnt/volume_nyc1_01/data/
db = dbConnect(SQLite(), dbname="realgar-omics.sqlite") #realgar-db.sqlite

# List tables in your database
dbListTables(db)

# List files
f = as.vector(na.omit(Dataset_Info$Unique_ID))
path = "/mnt/volume_nyc1_01/data/results_feather_files/"

#Data filter function
data_filter <- function(x){
  x <- x %>%
    dplyr::group_by(Gene) %>%
    dplyr::filter(PValue==min(PValue)) %>%
    dplyr::mutate(lower = logFC - 2*SD, 
                  upper = logFC + 2*SD,
                  Fold_Change=2^(logFC), 
                  neglogofP=(-log10(adjPVal)), #note that this is taking -log10 of adjusted p-value
                  Lower_bound_CI = 2^(lower), 
                  Upper_bound_CI = 2^(upper)) 
}



# Read feather files
# Format for database
# Write to database #takes around 10 minutes
for (i in f){
  d = read_feather(paste0(path, i,".feather"))
  names(d) <- gsub("[.]","",names(d))
  data <- cbind(Unique_ID = i,d)
  data <- data_filter(data)
  final_data <- merge(data,Total_Info,by="Unique_ID")
  rm(data)
  dbWriteTable(conn=db, name="REALGAR", final_data, append=T, row.names=F)
}

#Check table
dbListTables(db)

# List columns in a table
dbListFields(db, "REALGAR")

#Disconnect
dbDisconnect(db)

