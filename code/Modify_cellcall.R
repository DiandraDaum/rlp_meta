#Modify cellcall to keep only the lr pairs

# Load the readr and writexl packages
library(readr)
library(writexl)

# Read in the txt file from folder CellCall
folder_path <- "/Users/diandra/rlp_meta/data/CellCall"

files <- dir(folder_path, pattern = "*.csv|*.txt|*.tsv|*.xlsx")

ligand_receptor_list <- list()

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
  
  # Extract the Ligand_Symbol and Receptor_Symbol columns
  ligand_receptor <- df[, c("Ligand_Symbol", "Receptor_Symbol")]
  
  # Add the extracted data to the list
  ligand_receptor_list <- c(ligand_receptor_list, list(ligand_receptor))
}

# Combine the list of data frames into a single data frame
ligand_receptor_df <- do.call(rbind, ligand_receptor_list)

# Write the extracted data to a new xlsx file
write_xlsx(ligand_receptor_df, "/Users/diandra/rlp_meta/data/new_files/CellCall-2021_new.xlsx")



#remove duplicates-------------------------------------------------------------
# from 23,119 entries to 1,448 entries
library(readxl)
library(writexl)
df <- read_xlsx("/Users/diandra/rlp_meta/data/new_files/CellCall-2021_new.xlsx")

df_duplicates <- df[duplicated(paste(df$Ligand_Symbol, df$Receptor_Symbol)), ]
df_unique <- df[!duplicated(paste(df$Ligand_Symbol, df$Receptor_Symbol)), ]
#get  22,628 entries (considers duplicates all the -- pairs) --> 41,755 entries

# Print the removed rows
print("Removed rows:")
print(df_duplicates)

# Save the unique rows to a new file
write_xlsx(df_unique, "/Users/diandra/rlp_meta/data/new_files/CellCall-2021_new.xlsx")


