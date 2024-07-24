
#convert uniprot ids to protein names

# Load the required library
library(org.Hs.eg.db)
library('org.Hs.eg.db')
#BiocManager::install("biomaRt")
library(biomaRt)
BiocManager::install("ipdDb")
library(ipdDb)

#modify ahern_2022_matrix-------------------------------------------------------
# Read the CSV file
m <- read.csv("~/covid_data/ms_covid19_and_controls/ahern_2022_matrix.csv")
colnames(m)[1] <- "Protein"
# Convert the first column from Uniprot IDs to Ensembl gene names
m$Protein <- mapIds(org.Hs.eg.db, keys=m$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
# Write the updated matrix to a new CSV file
write.csv(m, file="~/covid_data/ms_covid19_and_controls/Clean_matrix/ahern_2022_matrix_new.csv", row.names=FALSE)

#modify byeon_2022_matrix-------------------------------------------------------
# Read the CSV file
m <- read.csv("~/covid_data/ms_covid19_and_controls/byeon_2022_matrix.csv")
colnames(m)[1] <- "Protein"
# Convert the first column from Uniprot IDs to Ensembl gene names using org.Hs.eg.db
m$Protein <- mapIds(org.Hs.eg.db, keys=m$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
# Identify proteins that are still NA after the first conversion
#na_proteins <- m$Protein[is.na(m$Protein)]
# Create a Mart object
#ensembl <- useMart("ensembl", dataset="hsapiens_gene_ensembl")
# List available filters (uncomment to check)
filters <- listFilters(ensembl)
# Use biomaRt to convert the remaining NA proteins
#proteins_converted <- getBM(filters=c(uniprot_swissprot=na_proteins), attributes=c("hgnc_symbol"), mart=ensembl)
# Update the m$Protein column with the new conversions
#m$Protein[is.na(m$Protein)] <- proteins_converted$hgnc_symbol[match(na_proteins, proteins_converted$uniprot_swissprot)]
# Write the updated matrix to a new CSV file
write.csv(m, file="~/covid_data/ms_covid19_and_controls/Clean_matrix/byeon_2022_matrix_gene_symbols.csv", row.names=FALSE)




#clean from uniprot id to ensembl-----------------------------------------------
library(readr)
library(xlsx)
library(org.Hs.eg.db)

folder_path <- "~/covid_data/ms_covid19_and_controls/To_clean"
output_folder_path <- "~/covid_data/ms_covid19_and_controls/Clean_matrix"
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
  
  # Convert the first column from Uniprot IDs to Ensembl gene names
  colnames(df)[1] <- "Protein"
  
  # Check if the first column contains ";" and split if necessary
  if (any(grepl(";", df$Protein))) {
    df$Protein <- sapply(strsplit(as.character(df$Protein), ";"), `[`, 1)
  }
  
  # Map Uniprot IDs to Ensembl gene names
  tryCatch(
    expr = {
      protein_map <- mapIds(org.Hs.eg.db, keys=df$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
      df$Protein <- as.character(protein_map)
    },
    error = function(e) {
      cat("Warning: File", file, "cannot be converted because it contains invalid Uniprot IDs.\n")
      return
    }
  )
  
  if (exists("protein_map")) {
    # Write the updated matrix to a new CSV file
    file_base <- sub("\\.[^.]+$", "", file)
    file_ext <- sub(".*\\.", ".", file)
    output_file_name <- paste0(file_base, "_new", file_ext)
    output_file_path <- file.path(output_folder_path, output_file_name)
    write.csv(df, file=output_file_path, row.names=FALSE)
    
    cat("Processed file:", file, "\n")
  }
}

