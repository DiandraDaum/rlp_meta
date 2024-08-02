
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

#modify biomart-------------------------------------------------------
# Read the CSV file
m <- read.csv("~/covid_data/ms_covid19_and_controls/byeon_2022_matrix.csv")
colnames(m)[1] <- "Protein"
# Convert the first column from Uniprot IDs to Ensembl gene names using org.Hs.eg.db
#m$Protein <- mapIds(org.Hs.eg.db, keys=m$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
# Identify proteins that are still NA after the first conversion
#na_proteins <- m$Protein[is.na(m$Protein)]
# Create a Mart object
mart <- useMart("ensembl", dataset="hsapiens_gene_ensembl")
mart <- useDataset(dataset="hsapiens_gene_ensembl", mart=mart)

# Use biomaRt to convert the remaining NA proteins
#proteins_converted <- getBM(attributes=c("hgnc_symbol","ensembl_gene_id","entrezgene_id"), filters="hgnc_symbol", mart=mart, values=na_proteins, uniqueRows=TRUE, bmHeader = T)
m$Protein <- getBM(attributes=c("hgnc_symbol","ensembl_gene_id","entrezgene_id"), filters="hgnc_symbol", mart=mart, values=m$Protein, uniqueRows=TRUE, bmHeader = T)
# Update the m$Protein column with the new conversions
#m$Protein[is.na(m$Protein)] <- proteins_converted$hgnc_symbol[match(na_proteins, proteins_converted$uniprot_swissprot)]
# Write the updated matrix to a new CSV file
write.csv(m, file="~/covid_data/ms_covid19_and_controls/Clean_matrix/byeon_2022_matrix_gene_symbols.csv", row.names=FALSE)

#modify suvarna-----------------------------------------------------------------
# assume 'df' is your matrix
df <- read.csv("~/covid_data/ms_covid19_and_controls/suvarna_2021_frontiers_in_physiology_matrix.csv")

# define a function to clean the gene ids
clean_gene_id <- function(gene_id) {
  # if the value contains ';' or ':', return NA to remove the row
  if (grepl(";|:", gene_id)) {
    return(NA)
  }
  # if the value starts with 'CON__', extract the part after '_'
  else if (grepl("^CON__", gene_id)) {
    return(gsub("CON__", "", gene_id))
  }
  # if the value starts with 'p|', extract the part between '|' and '|'
  else if (grepl("^sp\\|", gene_id)) {
    return(gsub("sp\\|(.*?)\\|.*", "\\1", gene_id))
  }
  # if the value starts with 'REV__sp|', extract the part between '|' and '|'
  else if (grepl("^REV__sp\\|", gene_id)) {
    return(gsub("REV__sp\\|(.*?)\\|.*", "\\1", gene_id))
  }
  # otherwise, return the original value
  else {
    return(gene_id)
  }
}

# apply the cleaning function to the first column
df[, 1] <- sapply(df[, 1], clean_gene_id)
# remove rows with NA values
df <- df[!is.na(df[, 1]), ]
colnames(df)[1] <- "Protein"
# save the cleaned matrix
write.csv(df, "~/covid_data/ms_covid19_and_controls/To_clean/suvarna_2021_frontiers_in_physiology_matrix_new.csv", row.names = FALSE)




#clean from uniprot id to ensembl-----------------------------------------------
library(readr)
library(xlsx)
library(org.Hs.eg.db)

#folder_path <- "~/covid_data/ms_covid19_and_controls/To_clean"
folder_path <- "~/covid_data/olink_cancer/Clean_olink/try"
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

