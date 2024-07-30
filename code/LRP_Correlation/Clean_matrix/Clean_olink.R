#clean olink data

#clean pancancer_olink_data_biostudies_v2------------------------------------------------------

# Read the matrix from the .txt file
# Read the file, skip the first row, and set the first row as column names
matrix_data <- read.table("~/covid_data/olink_cancer/pancancer_olink_data_biostudies_v2.txt", header = TRUE, row.names = NULL)

# Write the matrix to a .csv file
write.csv(matrix_data, "~/covid_data/olink_cancer/pancancer_olink_data_biostudies_v2.csv", row.names = FALSE)
# Normalized Protein eXpression (NPX) (log2 scale)
# Load the necessary libraries
library(org.Hs.eg.db)

# Read the CSV file
matrix_data <- read.csv("~/covid_data/olink_cancer/pancancer_olink_data_biostudies_v2.csv")

# Convert the Uniprot IDs to gene symbols
matrix_data$UniProt <- mapIds(org.Hs.eg.db, keys=matrix_data$UniProt, column="SYMBOL", keytype="UNIPROT", multiVals="first")
matrix_data$UniProt[is.na(matrix_data$UniProt)] <- matrix_data$UniProt[is.na(matrix_data$UniProt)]

# Keep only the GeneSymbol and NPX columns
matrix_data <- matrix_data[, c("UniProt", "NPX")]

# Load the necessary libraries
library(tidyr)
library(dplyr)

matrix_data <- matrix_data %>%
  drop_na() %>%  # Remove rows with missing values
  group_by(UniProt) %>%
  mutate(NPX_id = paste0("NPX.", row_number()))

# Pivot the data to wide format
matrix_data_wide <- matrix_data %>%
  pivot_wider(id_cols = UniProt, names_from = NPX_id, values_from = NPX)

# Write the updated matrix to a new CSV file
write.csv(matrix_data_wide, file="~/covid_data/olink_cancer/Clean_olink/pancancer_olink_data_biostudies_v2_new.csv", row.names=FALSE)


#modify next-gen covid19--------------------------------------------------------
library(dplyr)
library(readxl)
library(writexl)
library(tibble)

# Load the xlsx file
df <- read_xlsx("~/covid_data/olink_cardiovascular/Zhong_next_gen_covid19.xlsx")

# Remove the "Visit" column
df <- df[, -2]

# Remove the first row (which is now the first column)
df <- df[-1, ]

# Transpose the data (switch columns and rows)
df <- as.data.frame(t(df))
colnames(df)[1] <- "Protein"
# Remove the first row (which is now the first column)
df <- df[-1, ]

# Write the updated matrix to a new CSV file
write.csv(df, file="~/covid_data/olink_cancer/Clean_olink/Zhong_next_gen_covid19_new.csv", row.names=FALSE)



