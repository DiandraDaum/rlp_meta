#convert Omni
#Code to modify the OmniPathPPIs.tsv file to get the gene symbol from the uniprot ID

#from source target to ligand receptor
#BiocManager::install("org.Hs.eg.db")
library('org.Hs.eg.db')
# Read the.tsv file
data <- read.delim("/Users/diandra/rlp_meta/data/LewisLabUCSD/Human-2021-OmniPath-Turei/OmniPathPPIs.tsv", header = TRUE, sep = "\t")
# Rename the columns
colnames(data) <- c("ligand", "receptor", "is_directed", "is_stimulation", "is_inhibition", 
                    "consensus_direction", "consensus_stimulation", "consensus_inhibition", "dip_url")

#ensembl<-mapIds(org.Mm.eg.db, keys=rownames(df), column=’ENSEMBL’, keytype=’SYMBOL’, multiVals=”first”)
#entrez<-mapIds(org.Mm.eg.db, keys=rownames(df), column=’ENTREZID’, keytype=’SYMBOL’, multiVals=”first”)
#entrez<-mapIds(org.Mm.eg.db, keys=rownames(df), column=’UNIPROT’, keytype=’SYMBOL’, multiVals=”first”)

# Create a copy of the original Uniprot IDs
data$ligand_original <- data$ligand
data$receptor_original <- data$receptor

# Convert Uniprot IDs to gene symbols
data$ligand <- mapIds(org.Hs.eg.db, keys=data$ligand, column="SYMBOL", keytype="UNIPROT", multiVals="first")
data$receptor <- mapIds(org.Hs.eg.db, keys=data$receptor, column="SYMBOL", keytype="UNIPROT", multiVals="first")

# Replace NA values with BIRC5 only when the original Uniprot ID is O15392
data$ligand[is.na(data$ligand) & data$ligand_original == "O15392"] <- "BIRC5"
data$receptor[is.na(data$receptor) & data$receptor_original == "O15392"] <- "BIRC5"
# Replace NA values with MIR17HG only when the original Uniprot ID is Q75NE6
data$ligand[is.na(data$ligand) & data$ligand_original == "Q75NE6"] <- "MIR17HG"
data$receptor[is.na(data$receptor) & data$receptor_original == "Q75NE6"] <- "MIR17HG"
# Replace NA values with TRBV7-9 only when the original Uniprot ID is P04435
data$ligand[is.na(data$ligand) & data$ligand_original == "P04435"] <- "TRBV7-9"
data$receptor[is.na(data$receptor) & data$receptor_original == "P04435"] <- "TRBV7-9"
# Replace NA values with PIK3R2 only when the original Uniprot ID is Q96EV4
data$ligand[is.na(data$ligand) & data$ligand_original == "Q96EV4"] <- "PIK3R2"
data$receptor[is.na(data$receptor) & data$receptor_original == "Q96EV4"] <- "PIK3R2"
# Replace NA values with DKFZp686J04131 only when the original Uniprot ID is Q68D39
data$ligand[is.na(data$ligand) & data$ligand_original == "Q68D39"] <- "DKFZp686J04131"
data$receptor[is.na(data$receptor) & data$receptor_original == "Q68D39"] <- "DKFZp686J04131"
# Replace NA values with MT-RNR2 only when the original Uniprot ID is Q8IVG9
data$ligand[is.na(data$ligand) & data$ligand_original == "Q8IVG9"] <- "MT-RNR2"
data$receptor[is.na(data$receptor) & data$receptor_original == "Q8IVG9"] <- "MT-RNR2"
# Replace NA values with CARD17P only when the original Uniprot ID is Q5XLA6
data$ligand[is.na(data$ligand) & data$ligand_original == "Q5XLA6"] <- "CARD17P"
data$receptor[is.na(data$receptor) & data$receptor_original == "Q5XLA6"] <- "CARD17P"
# Replace NA values with HLA-DMA only when the original Uniprot ID is P28067
data$ligand[is.na(data$ligand) & data$ligand_original == "P28067"] <- "HLA-DMA"
data$receptor[is.na(data$receptor) & data$receptor_original == "P28067"] <- "HLA-DMA"
# Replace NA values with CARD16 only when the original Uniprot ID is Q5EG05
data$ligand[is.na(data$ligand) & data$ligand_original == "Q5EG05"] <- "CARD16"
data$receptor[is.na(data$receptor) & data$receptor_original == "Q5EG05"] <- "CARD16"
# Replace NA values with ATP2B4 only when the original Uniprot ID is P23634
data$ligand[is.na(data$ligand) & data$ligand_original == "P23634"] <- "ATP2B4"
data$receptor[is.na(data$receptor) & data$receptor_original == "P23634"] <- "ATP2B4"
# Replace NA values with CHK2 only when the original Uniprot ID is Q683Z8
data$ligand[is.na(data$ligand) & data$ligand_original == "Q683Z8"] <- "CHK2"
data$receptor[is.na(data$receptor) & data$receptor_original == "Q683Z8"] <- "CHK2"
# Replace NA values with BCR/ABL fusion only when the original Uniprot ID is A9UF07
data$ligand[is.na(data$ligand) & data$ligand_original == "A9UF07"] <- "BCR/ABL fusion"
data$receptor[is.na(data$receptor) & data$receptor_original == "A9UF07"] <- "BCR/ABL fusion"
# Replace NA values with RET/PTC2 fusion only when the original Uniprot ID is Q15300
data$ligand[is.na(data$ligand) & data$ligand_original == "Q15300"] <- "RET/PTC2"
data$receptor[is.na(data$receptor) & data$receptor_original == "Q15300"] <- "RET/PTC2"
# Replace NA values with PDPK2P fusion only when the original Uniprot ID is Q6A1A2
data$ligand[is.na(data$ligand) & data$ligand_original == "Q6A1A2"] <- "PDPK2P"
data$receptor[is.na(data$receptor) & data$receptor_original == "Q6A1A2"] <- "PDPK2P"

# Write the modified data back to the same file
write.table(data, "/Users/diandra/rlp_meta/data/new_files/OmniPathPPIs_new.tsv", sep = "\t", quote = FALSE, row.names = FALSE)


