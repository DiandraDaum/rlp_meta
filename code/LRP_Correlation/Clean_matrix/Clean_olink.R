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



#modify Longitudinal proteomic analysis of severe COVID-19----------------------
library(dplyr)
library(readxl)
library(writexl)
library(tibble)

# Load the xlsx file
a <- read_xlsx("~/covid_data/olink_cardiovascular/2ALongitudinal_proteomic_analysis_of_severeCOVID-19.xlsx")
c <- read_xlsx("~/covid_data/olink_cardiovascular/2CLongitudinal_proteomic_analysis_of_severeCOVID-19.xlsx")
# Rename the columns using the second row
colnames(a) <- a[1, ]
# Remove the first row
a <- a[-1, ]
# Rename the columns using the second row
colnames(c) <- c[1, ]
# Remove the first row
c <- c[-1, ]

# Create a lookup table for Olink IDs to UniProt IDs
olink_to_uniprot <- setNames(a$UniProt, a$OlinkID)

# Replace Olink IDs with UniProt IDs in column headers of c
colnames(c)[-1] <- olink_to_uniprot[colnames(c)[-1]]

# Transpose c, keeping the UniProt IDs as a column
c_transposed <- t(c)

# Replace the column names with the first row
colnames(c_transposed) <- c_transposed[1, ]

# Remove the first row
c_transposed <- c_transposed[-1, ]

# Add the UniProt IDs as a column
c_transposed <- data.frame(UniProt = rownames(c_transposed), c_transposed)
colnames(c_transposed)[1] <- "Protein"

library(org.Hs.eg.db)
# Convert the Uniprot IDs to gene symbols
c_transposed$Protein <- mapIds(org.Hs.eg.db, keys=c_transposed$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
c_transposed$Protein[is.na(c_transposed$Protein)] <- c_transposed$Protein[is.na(c_transposed$Protein)]


write.csv(a, file="~/covid_data/olink_cardiovascular/2ALongitudinal_proteomic_analysis_of_severeCOVID-19.csv", row.names=FALSE)
write.csv(c_transposed, file="~/covid_data/olink_cancer/Clean_olink/2CLongitudinal_proteomic_analysis_of_severeCOVID-19_new.csv", row.names=FALSE)

#convert multgi-platform--------------------------------------------------------
library(dplyr)
library(readxl)
library(writexl)
library(tibble)

# Load the xlsx file
df <- read_xlsx("~/covid_data/olink_cancer/Multi-platform_Ovarian_Cancer_Plasma.xlsx")

#convert to csv
write.csv(df, file="~/covid_data/olink_cancer/Clean_olink/Multi-platform_Ovarian_Cancer_Plasma_new.csv", row.names=FALSE)





#clean petrera------------------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(tibble)

# Load the xlsx file
df <- read_xlsx("~/covid_data/olink_cardiovascular/Petrera20.xlsx", skip = 4)
# Get the column names that meet the condition
col_names <- which(str_detect(names(df), "ONCOLOGY|CARDIOVASCULAR"))

# Select the first column and the columns that meet the condition
df <- df[, c(1, col_names)]

# Remove rows with specific values in the first column
df <- df %>%
  filter(!is.na(.[[1]])) %>%
  filter(.[[1]] != "Assay") %>%
  filter(.[[1]] != "OlinkID")

# Remove the last four rows
df <- head(df, -4)

# Set the first row as column names
colnames(df) <- df[1, ]
df <- df[-1, ]

# Transpose df, keeping the column 1 as a column
df_transposed <- t(df)

# Replace the column names with the first row
colnames(df_transposed) <- df_transposed[1, ]

# Remove the first row
df_transposed <- df_transposed[-1, ]

# Add the original column headers as a column
df_transposed <- data.frame(Column1 = rownames(df_transposed), df_transposed)
colnames(df_transposed)[1] <- "Protein"
df_transposed <- head(df_transposed, -8)

# Remove rows where the value in the "Protein" column is NA, NULL, or has more than 8 characters
df_transposed <- df_transposed %>%
  filter(!is.na(Protein), 
         !is.null(Protein), 
         nchar(as.character(Protein)) <= 8)

#convert to csv
write.csv(df_transposed, file="~/covid_data/olink_cancer/Clean_olink/Petrera20_new.csv", row.names=FALSE)

#open again
m <- read.csv("~/covid_data/olink_cancer/Clean_olink/Petrera20_new.csv")
# Remove the last two rows
#m <- head(m, -8)
# Create a copy of the original data frame
m_original <- m

# Perform the filter operation
m <- m %>%
  filter(!is.na(m$Protein)) %>%
  filter(!is.null(m$Protein))%>%
  filter(!str_detect(Protein, ","))

# Find the removed rows
removed_rows <- setdiff(m_original$Protein, m$Protein)

# Print the removed rows
print(removed_rows)
setdiff(m_original$Protein, m$Protein) #NA in row 109
#m <- m_original
# Filter out rows with ;, NA, or NULL in the Protein column
#m <- m %>% filter(!str_detect(Protein, ",")) %>% filter(!is.na(Protein))
#m <- m %>% filter(!str_detect(m$Protein, ","))#%>% filter(!is.na(m$Protein))
         
library(org.Hs.eg.db)
protein_map <- mapIds(org.Hs.eg.db, keys=m$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
m$Protein <- as.character(protein_map)
m <- as.data.frame(m)
#m <- as.matrix(m)
#m$Protein <- as.character(m$Protein)
# Convert the Uniprot IDs to gene symbols
#new_id <-  mapIds(org.Hs.eg.db, keys=m$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
#remove NULL values
#m <-m[!unlist(lapply(new_id, is.null)), ]
#m$Protein <- as.character(unlist(new_id))
#save
write.csv(m, file="~/covid_data/olink_cancer/Clean_olink/Petrera20_new.csv", row.names=FALSE)
write_xlsx(m, "~/covid_data/olink_cancer/Clean_olink/Petrera20_new.xlsx")

