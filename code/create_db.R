#author: Diandra
#try read datasets from https://github.com/LewisLabUCSD/Ligand-Receptor-Pairs


#try swapping --> IT WORKS!
#remember to convert Ramilowski from .txt to .xlsx
#also ximerakis works better from .txt to .xlsx
#also toronto from txt to xlsx
#new version with more file origins
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(writexl)

# Set the folder path and output file path
folder_path <- "/Users/diandra/rlp_meta/data/input_files_for_db"
#folder_path <- "/Users/diandra/rlp_meta/data/Cellchat/try"

output_file_path <- "/Users/diandra/rlp_meta/results/alldbfull.csv" #csv output
output_file_path2 <- "/Users/diandra/rlp_meta/results/alldbfull.xlsx" #xlsx output
#output_file_path <- "/Users/diandra/rlp_meta/data/Cellchat/try1.csv" #csv output
#output_file_path2 <- "/Users/diandra/rlp_meta/data/Cellchat/try1.xlsx" #xlsx output


# Define the possible column names for ligand and receptor
ligand_cols <- c("LIGAND", "ligand", "Ligand (Symbol)", "From", "Ligand.ApprovedSymbol", "ligand.symbol", "Ligand", "Ligand gene symbol", "ligand_gene_symbol", "source_genesymbol", "from", "Gene1_Symbol", "source_gene")
receptor_cols <- c("RECEPTOR(S)", "receptor", "Receptor (Symbols)", "To", "Receptor.ApprovedSymbol", "receptor.receptor", "Receptor", "Receptor gene symbol", "receptor_gene_symbol", "target_genesymbol", "to", "Gene_name", "Gene2_Symbol", "target_gene")

# Initialize an empty data frame to store the results
results <- data.frame(receptor = character(), ligand = character(), file = character())

# Loop through all files in the folder
files <- dir(folder_path, pattern = "*.csv|*.txt|*.tsv|*.xlsx")
for (file in files) {
  # Read the file
  file_path <- file.path(folder_path, file)
  if (grepl("\\.csv", file)) {
    df <- read_csv(file_path, show_col_types = FALSE)
  } else if (grepl("\\.txt", file)) {
    df <- read_table(file_path, show_col_types = FALSE)
  } else if (grepl("\\.tsv", file)) {
    df <- read_tsv(file_path, show_col_types = FALSE)
  } else if (grepl("\\.xlsx", file)) {
    df <- read_xlsx(file_path)
  } else {
    # If the file extension is not recognized, skip it
    cat("The file", file, "cannot be read because it is not.csv or.txt or.tsv or.xlsx.\n")
    next
  }
  
  # Find the ligand and receptor columns
  ligand_col <- names(df)[names(df) %in% ligand_cols][1]
  print(ligand_col)
  receptor_col <- names(df)[names(df) %in% receptor_cols][1]
  print(receptor_col)
  
  # Check that both columns are found
  if (!is.null(ligand_col) &&!is.null(receptor_col)) {
    # Extract the ligand-receptor pairs
    pairs <- df[, c(ligand_col, receptor_col)]
    colnames(pairs) <- c("ligand", "receptor")
    
    # Check for swapped values and swap them if necessary
    #swapped_rows <- str_detect(pairs$ligand, "receptor") | str_detect(pairs$receptor, "ligand")
    swapped_rows <- str_detect(pairs$ligand, "receptor") | str_detect(pairs$receptor, "ligand") | str_detect(pairs$ligand, "Receptor") | str_detect(pairs$receptor, "Ligand") | str_detect(pairs$ligand, "RECEPTOR") | str_detect(pairs$receptor, "LIGAND")
    #swapped_rows <- str_detect(pairs$ligand, "\\b(receptor|ligand)\\b") | str_detect(pairs$receptor, "\\b(receptor|ligand)\\b")
    swapped_rows[is.na(swapped_rows)] <- FALSE  # Replace NA with FALSE
    if (any(swapped_rows)) {
      cat("Swapped values found in file", file, ". Swapping...\n")
    }
    pairs[swapped_rows, ] <- pairs[swapped_rows, c("receptor", "ligand")]
    
    pairs$file <- file
    results <- rbind(results, pairs)
  } else {
    # If either column is not found, skip this file
    cat("Skipping file", file, "because ligand or receptor column not found.\n")
  }
}

# Remove "-" and "???" and "xxx" values, and values that start with "?", check :?
results_filtered <- results %>% 
  filter(ligand != receptor) %>%
  filter(!(ligand == "-" | ligand == "???" | ligand == "xxx" | ligand == "????" | 
             receptor == "-" | receptor == "???" | receptor == "????" | receptor == "xxx"))
if (nrow(results)!= nrow(results_filtered)) {
  warning("Rows with '-' or '???' or 'xxx' or values starting with '?' in ligand or receptor columns were removed.")
}
results <- results_filtered

# Identify and print the redundant pairs (only forward) that were removed
n_before_distinct <- nrow(results)
results_sorted <- results %>% 
  mutate(ligand = toupper(ligand), receptor = toupper(receptor)) %>% 
  filter(!(str_detect(ligand, "COMPLEX") | str_detect(receptor, "COMPLEX")))

duplicates <- results_sorted[duplicated(results_sorted[, c("ligand", "receptor")]) | duplicated(results_sorted[, c("receptor", "ligand")], fromLast = TRUE), ]
if (nrow(duplicates) > 0) {
  message("The following redundant pairs were removed:")
  print(duplicates[, c("ligand", "receptor")])
}

# Remove duplicates and keep one copy, then aggregate file names
results_distinct <- results_sorted %>% 
  mutate(interaction = paste(ligand, receptor, sep = "_")) %>% 
  group_by(interaction, ligand, receptor) %>% 
  summarise(file = paste(unique(file), collapse = "; "), 
            count = n())

n_after_distinct <- nrow(results_distinct)

if (n_before_distinct!= n_after_distinct) {
  warning(paste("Duplicate rows were removed: ", n_before_distinct - n_after_distinct, " rows removed."))
}

# Rename the receptor column to receptor(s)
names(results_distinct)[names(results_distinct) == "receptor"] <- "receptor(s)"

# Write the results to the output file
write.csv(results_distinct, output_file_path, row.names = FALSE)
write_xlsx(results_distinct, output_file_path2)




#-------------------------------------------------------------------------------
#check genesymbols with progress bar
# Load the necessary libraries
library(readxl)
library(org.Hs.eg.db)
library(writexl)
library(progress)

# Load your data (replace 'your_data.csv' with your actual file name)
data <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")

# Create the ligand_valid and receptor_valid columns
data$ligand_valid <- FALSE
data$receptor_valid <- FALSE

# Create a progress bar
pb <- progress_bar$new(format = "[:bar] :percent eta: :eta", total = nrow(data))

# Vectorize the is_gene_symbol function
for (i in 1:nrow(data)) {
  data$ligand_valid[i] <- data$ligand[i] %in% keys(org.Hs.eg.db, "SYMBOL")
  data$receptor_valid[i] <- data$`receptor(s)`[i] %in% keys(org.Hs.eg.db, "SYMBOL")
  pb$tick()
}

# Now you can use the data with the new columns
# For example, you can filter the data to only include rows where both ligand and receptor are valid
valid_data <- data[data$ligand_valid & data$receptor_valid, ]

# Remove the rows that do not have valid gene symbols
data <- data[data$ligand_valid & data$receptor_valid, ]

# Remove the validation columns
data <- data[,!(names(data) %in% c("ligand_valid", "receptor_valid"))]

# Save the output to a new xlsx file
write_xlsx(data, "/Users/diandra/rlp_meta/results/alldbfull.xlsx")


#-------------------------------------------------------------------------------
#keep best interactions
#best receptor for each ligand
library(dplyr)
library(readxl)

# Read the output file
results_ligand <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")

# Group by ligand and keep the interaction with the highest count
results_top_count <- results_ligand %>% 
  group_by(ligand) %>% 
  slice_max(count, n = 1) %>% 
  ungroup()%>% 
  arrange(desc(count)) 

# Write the results to a new output file
write_xlsx(results_top_count, "/Users/diandra/rlp_meta/results/alldb_top_count_ligand.xlsx")


#-------------------------------------------------------------------------------
#keep best interactions
#best receptor for each ligand
library(dplyr)
library(readxl)

# Read the output file
results_receptor <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")

# Group by ligand and keep the interaction with the highest count
results_top_count <- results_receptor %>% 
  group_by('receptor(s)') %>% 
  slice_max(count, n = 1) %>% 
  ungroup()%>% 
  arrange(desc(count)) 

# Write the results to a new output file
write_xlsx(results_top_count, "/Users/diandra/rlp_meta/results/alldb_top_count_receptor.xlsx")
