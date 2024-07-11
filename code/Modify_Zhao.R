#modify Zhao.tsv
#code to make Zhao.tsv run into the next code: split ligand and receptor pair
# Load the readr and tidyr libraries
library(readr)
library(tidyr)

# Specify the input file path
input_file <- "/Users/diandra/rlp_meta/data/LewisLabUCSD/Human-2023-Zhao-LR-pairs.tsv"

# Load the TSV file into a data frame
df <- read_tsv(input_file)

# Check if the "interaction_name" column exists
if ("interaction_name" %in% names(df)) {
  # Split the values in the "interaction_name" column into two new columns
  df <- df %>% 
    separate(interaction_name, into = c("ligand", "receptor"), sep = "_")
}

# Get the input file name without the extension
file_name <- gsub("\\.tsv", "", basename(input_file))

# Specify the new output directory
new_dir <- "/Users/diandra/rlp_meta/data/new_files"

# Save the updated data frame to a new TSV file with "new" in the end
output_file <- file.path(new_dir, paste0(file_name, "_new.tsv"))
write_tsv(df, output_file)


df <- read_tsv("/Users/diandra/rlp_meta/data/new_files/Human-2023-Zhao-LR-pairs_new.tsv")

df_removed <- df %>% 
  filter(interaction_type!= "ligand-receptor")

df_filtered <- df %>% 
  filter(interaction_type == "ligand-receptor")

# Print the removed lines
print("Removed lines:")
print(df_removed)

write_tsv(df_filtered, "/Users/diandra/rlp_meta/data/new_files/Human-2023-Zhao-LR-pairs_new.tsv" )

