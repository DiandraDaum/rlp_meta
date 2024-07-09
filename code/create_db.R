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

output_file_path <- "/Users/diandra/rlp_meta/results/alldbfull.csv" #csv output
output_file_path2 <- "/Users/diandra/rlp_meta/results/alldbfull.xlsx" #xlsx output

# Define the possible column names for ligand and receptor
ligand_cols <- c("LIGAND", "ligand", "Ligand (Symbol)", "From", "Ligand.ApprovedSymbol", "ligand.symbol", "Ligand",  "AliasA", "Ligand gene symbol", "HMDB_ID", "ligand_gene_symbol", "source_genesymbol", "from", "Gene1_Symbol")
receptor_cols <- c("RECEPTOR(S)", "receptor", "Receptor (Symbols)", "To", "Receptor.ApprovedSymbol", "receptor.receptor", "Receptor", "AliasB", "Receptor gene symbol", "receptor_gene_symbol", "target_genesymbol", "to", "Gene_name", "Gene2_Symbol")

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
    swapped_rows <- str_detect(pairs$ligand, "receptor") | str_detect(pairs$receptor, "ligand")
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

# Remove "-" and "???" and "xxx" values, and values that start with "?"
results_filtered <- results %>% 
  filter(!(ligand == "-" | ligand == "???" | ligand == "xxx" | ligand == "????" | 
             receptor == "-" | receptor == "???" | receptor == "????" | receptor == "xxx"))
if (nrow(results)!= nrow(results_filtered)) {
  warning("Rows with '-' or '???' or 'xxx' or values starting with '?' in ligand or receptor columns were removed.")
}
results <- results_filtered

# Identify and print the redundant pairs that were removed
# Identify and print the redundant pairs that were removed
n_before_distinct <- nrow(results)
results_sorted <- results %>% 
  mutate(ligand = toupper(ligand), receptor = toupper(receptor)) %>% 
  mutate(temp = pmin(ligand, receptor), temp2 = pmax(ligand, receptor)) %>% 
  mutate(ligand = temp, receptor = temp2) %>% 
  select(-temp, -temp2)

duplicates <- results_sorted[duplicated(results_sorted[, c("ligand", "receptor")]) | duplicated(results_sorted[, c("ligand", "receptor")], fromLast = TRUE), ]
if (nrow(duplicates) > 0) {
  message("The following redundant pairs were removed:")
  print(duplicates[, c("ligand", "receptor")])
}

# Remove duplicates and keep one copy, then aggregate file names
results_distinct <- results_sorted %>% 
  group_by(ligand, receptor) %>% 
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

