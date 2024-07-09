#modify Zhao.tsv
#code to make Zhao.tsv run into the next code: split ligand and receptor pair
# Load the readr and tidyr libraries
library(readr)
library(tidyr)

# Specify the input file path
input_file <- "/Users/diandra/Downloads/LewisLabUCSD/Human-2023-Zhao-LR-pairs.tsv"

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

# Get the input file directory
dir <- dirname(input_file)

# Save the updated data frame to a new TSV file with "new" in the end
output_file <- file.path(dir, paste0(file_name, "_new.tsv"))
write_tsv(df, output_file)


