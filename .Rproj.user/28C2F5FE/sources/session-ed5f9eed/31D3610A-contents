#modify Vento
# Read the .csv file
#data <- read.csv("/Users/diandra/rlp_meta/data/Cellphone/Human-2018-Vento-Tormo-LR-pairs.csv")
data <- read.csv("/Users/diandra/rlp_meta/data/Cellphone/interaction_input_Vento_Cellphonedbv5.csv", stringsAsFactors = FALSE)

# Create the ligand column
data$ligand <- ifelse(is.na(data$protein_name_a) | data$protein_name_a == "", 
                      data$partner_a, 
                      sub("_.*", "", data$protein_name_a))

# Create the receptor column
data$receptor <- ifelse(is.na(data$protein_name_b) | data$protein_name_b == "", 
                        data$partner_b, 
                        sub("_.*", "", data$protein_name_b))

# Check for swapped values and swap them if necessary
swapped_rows <- grepl("receptor", data$ligand, ignore.case = TRUE) | grepl("ligand", data$receptor, ignore.case = TRUE)
swapped_rows[is.na(swapped_rows)] <- FALSE  # Replace NA with FALSE
if (any(swapped_rows)) {
  cat("Swapped values found. Swapping...\n")
}
data[swapped_rows, c("ligand", "receptor")] <- data[swapped_rows, c("receptor", "ligand")]

# Write the updated data to a new .csv file
#write.csv(data, "/Users/diandra/rlp_meta/data/new_files/Human-2018-Vento-Tormo-LR-pairs_new.csv", row.names = FALSE)
write.csv(data, "/Users/diandra/rlp_meta/data/new_files/interaction_input_Vento_Cellphonedbv5_new.csv", row.names = FALSE)




#check vento again
# Load the necessary libraries
library(org.Hs.eg.db)
library(readr)

# Load your data
data <- read.csv("~/Downloads/DB/LewisLabUCSD/Human-2018-Vento-Tormo-LR-pairs.csv", stringsAsFactors = FALSE)

#Extract the Uniprot ID part from the string
data$partner_a_uniprot <- sub("_HUMAN", "", data$protein_name_a)
data$partner_b_uniprot <- sub("_HUMAN", "", data$protein_name_b)

# Convert Uniprot IDs to gene symbols using the uniprot package
ids <- unique(c(data$partner_a_uniprot, data$partner_b_uniprot))
conv <- id_mapping(ids, from = "UNIPROTKB", to = "GENENAME")

# Merge the converted IDs back into the data
data <- merge(data, conv, by.x = c("partner_a_uniprot", "partner_b_uniprot"), by.y = c("from", "from"))

# Rename the columns
data <- data %>% 
  rename(partner_a_symbol = to, partner_b_symbol = to)

# Save the updated data to the same file
write.csv(data, "~/Downloads/DB/LewisLabUCSD/Human-2018-Vento-Tormo-LR-pairs_new.csv", row.names = FALSE)

