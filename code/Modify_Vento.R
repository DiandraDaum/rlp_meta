#modify Vento
#Code to modify Human-2018-Vento-Tormo-LR-pairs.csv and 
#interaction_input_Vento_Cellphonedbv5.csv to extract ligand and receptors gene symbols

#BiocManager::install("org.Hs.eg.db")
library('org.Hs.eg.db')
# Read the .csv file
#data <- read.csv("/Users/diandra/rlp_meta/data/Cellphone/Human-2018-Vento-Tormo-LR-pairs.csv")
data <- read.csv("/Users/diandra/rlp_meta/data/Cellphone/interaction_input_Vento_Cellphonedbv5.csv", stringsAsFactors = FALSE)

data$partner_a <- ifelse(is.na(mapIds(org.Hs.eg.db, keys=data$partner_a, column="SYMBOL", keytype="UNIPROT", multiVals="first")), 
                         data$partner_a, 
                         mapIds(org.Hs.eg.db, keys=data$partner_a, column="SYMBOL", keytype="UNIPROT", multiVals="first"))

data$partner_b <- ifelse(is.na(mapIds(org.Hs.eg.db, keys=data$partner_b, column="SYMBOL", keytype="UNIPROT", multiVals="first")), 
                         data$partner_b, 
                         mapIds(org.Hs.eg.db, keys=data$partner_b, column="SYMBOL", keytype="UNIPROT", multiVals="first"))

# Write the modified data back to the same file
write.csv(data, "/Users/diandra/rlp_meta/data/new_files/interaction_input_Vento_Cellphonedbv5_new.csv", row.names = FALSE)



#create ligand and receptor columns
data <- read.csv("/Users/diandra/rlp_meta/data/new_files/interaction_input_Vento_Cellphonedbv5_new.csv", stringsAsFactors = FALSE)

data$ligand <- data$partner_a
data$receptor<- data$partner_b
write.csv(data, "/Users/diandra/rlp_meta/data/new_files/interaction_input_Vento_Cellphonedbv5_new.csv", row.names = FALSE)


#keep only directionality: ligand-receptor
file <- read.csv("/Users/diandra/rlp_meta/data/new_files/interaction_input_Vento_Cellphonedbv5_new.csv", stringsAsFactors = FALSE)
                 
# Remove rows where directionality is not "Ligand-Receptor"
file_subset <- subset(file, directionality == "Ligand-Receptor")  

# Check if protein_name_a is empty, and if so, set ligand to NA
file_subset$ligand <- ifelse(file_subset$protein_name_a == "", NA, file_subset$ligand)

#re-write filtered output file
write.csv(file_subset, "/Users/diandra/rlp_meta/data/new_files/interaction_input_Vento_Cellphonedbv5_new.csv", row.names = FALSE)           
                 
                 
                 
                 
