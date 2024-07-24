#modify ximerakis by adding a label (ligand or receptor) to the alias_A or alias_B

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(writexl)

data1 <- read_xlsx("/Users/diandra/rlp_meta/data/BaderDB/Human-2019-Ximerakis-BaderLab-2017.xlsx")
data2 <- read.table("/Users/diandra/rlp_meta/data/BaderDB/protein_type_ximerakis.txt", header = TRUE, sep = "\t", encoding = "UTF-8")


# Create new columns in data1
data1$alias_A_type <- NA
data1$alias_B_type <- NA

# Assign the curated_protein_type values to the new columns
data1$alias_A_type <- data2$curated_protein_type[match(data1$AliasA, data2$hgnc_symbol)]
data1$alias_B_type <- data2$curated_protein_type[match(data1$AliasB, data2$hgnc_symbol)]

#write output
write_xlsx(data1, "/Users/diandra/rlp_meta/data/new_files/Human-2019-Ximerakis-BaderLab-2017_new.xlsx")



data1 <- read_xlsx("/Users/diandra/rlp_meta/data/new_files/Human-2019-Ximerakis-BaderLab-2017_new.xlsx")
# Create new columns for ligand and receptor
data1$ligand <- NA
data1$receptor <- NA

# Define a function to determine ligand/receptor assignment
assign_ligand_receptor <- function(alias_A, alias_B, alias_A_type, alias_B_type) {
  if (alias_A_type == alias_B_type) {
    return(c("-", "-"))
  } else if (alias_A_type == "ECM" || alias_B_type == "ECM") {
    return(c("-", "-"))
  } else if (grepl("Receptor", alias_A_type) && !grepl("Receptor", alias_B_type)) {
    return(c(alias_B, alias_A))
  } else if (grepl("Receptor", alias_B_type) && !grepl("Receptor", alias_A_type)) {
    return(c(alias_A, alias_B))
  } else if (grepl("ECM/Receptor", alias_A_type) && !grepl("Receptor", alias_B_type)) {
    return(c(alias_A, alias_B))
  } else if (grepl("ECM/Receptor", alias_B_type) && !grepl("Receptor", alias_A_type)) {
    return(c(alias_B, alias_A))
  } else if (grepl("ECM/Receptor/Ligand", alias_A_type) && !grepl("Receptor", alias_B_type)) {
    return(c(alias_A, alias_B))
  } else if (grepl("ECM/Receptor/Ligand", alias_B_type) && !grepl("Receptor", alias_A_type)) {
    return(c(alias_B, alias_A))
  } else if (grepl("Ligand", alias_A_type) && !grepl("Ligand", alias_B_type)) {
    return(c(alias_A, alias_B))
  } else if (grepl("Ligand", alias_B_type) && !grepl("Ligand", alias_A_type)) {
    return(c(alias_B, alias_A))
  } else if (grepl("ECM/Ligand", alias_A_type) && !grepl("Ligand", alias_B_type)) {
    return(c(alias_A, alias_B))
  } else if (grepl("ECM/Ligand", alias_B_type) && !grepl("Ligand", alias_A_type)) {
    return(c(alias_B, alias_A))
  } else {
    # If none of the above conditions are met, copy alias_A to ligand and alias_B to receptor
    return(c(alias_A, alias_B))
  }
}
# Apply the function to each row of data1
data1[, c("ligand", "receptor")] <- t(apply(data1, 1, function(x) {
  assign_ligand_receptor(x["AliasA"], x["AliasB"], x["alias_A_type"], x["alias_B_type"])
}))

write_xlsx(data1, "/Users/diandra/rlp_meta/data/new_files/Human-2019-Ximerakis-BaderLab-2017_new.xlsx")

#DUPLICATES-------------------------------------------------------------------------------
#REMOVE DUPLICATES!
# Load the xlsx file
library(readxl)
library(writexl)
df <- read_xlsx("/Users/diandra/rlp_meta/data/new_files/Human-2019-Ximerakis-BaderLab-2017_new.xlsx")

# Check for duplicates in ligand-receptor pairs
#df_duplicates <- df[duplicated(paste(df$AliasA, df$AliasB)), ]
#df_unique <- df[!duplicated(paste(df$AliasA, df$AliasB)), ]
#get 43,408 entries --> 41,755 entries in alldbfull
df_duplicates <- df[duplicated(paste(df$ligand, df$receptor)), ]
df_unique <- df[!duplicated(paste(df$ligand, df$receptor)), ]
#get  22,628 entries (considers duplicates all the -- pairs) --> 41,755 entries

# Print the removed rows
print("Removed rows:")
print(df_duplicates)

# Save the unique rows to a new file
write_xlsx(df_unique, "/Users/diandra/rlp_meta/data/new_files/Human-2019-Ximerakis-BaderLab-2017_new2.xlsx")

#ONLY LR -----------------------------------------------------------------------
#decide not to do this anymore
library(readxl)
library(writexl)
df <- read_xlsx("/Users/diandra/rlp_meta/data/new_files/Human-2019-Ximerakis-BaderLab-2017_new2.xlsx")

df <- df[(df$alias_A_type == "Ligand" & df$alias_B_type == "Receptor") | 
           (df$alias_A_type == "Receptor" & df$alias_B_type == "Ligand"), ]

write_xlsx(df, "/Users/diandra/rlp_meta/data/new_files/Human-2019-Ximerakis-BaderLab-2017_new2.xlsx")


