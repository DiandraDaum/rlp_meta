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
df <- head(df, -3)

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
         !str_detect(Protein, "NA"),
         nchar(as.character(Protein)) <= 8)

#convert to csv
write.csv(df_transposed, file="~/covid_data/olink_cancer/Clean_olink/Petrera20_new.csv", row.names=FALSE)
write_xlsx(df_transposed, "~/covid_data/olink_cancer/Clean_olink/Petrera20_new.xlsx")


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
#removed_rows <- setdiff(m_original$Protein, m$Protein)
# Print the removed rows
#print(removed_rows)
#setdiff(m_original$Protein, m$Protein) #NA in row 109
#m <- m_original
# Filter out rows with ;, NA, or NULL in the Protein column
#m <- m %>% filter(!str_detect(Protein, ",")) %>% filter(!is.na(Protein))
#m <- m %>% filter(!str_detect(m$Protein, ","))#%>% filter(!is.na(m$Protein))
         
library(org.Hs.eg.db)
protein_map <- mapIds(org.Hs.eg.db, keys=m$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
m$Protein <- as.character(protein_map)
# Rename duplicate protein names
m$Protein <- make.unique(as.character(m$Protein), sep = ".")
#m <- as.data.frame(m)
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

#DEBUG --> IL6 found 2 times!---------------------------------------------------
m_file_path <- file.path("~/covid_data/olink_cancer/Clean_olink/Petrera20_new.csv") # replace with the actual file name
m <- read.csv(m_file_path)
print(str(m))
print(sapply(m, is.numeric))
for (i in 1:nrow(m)) {
  tryCatch(
    expr = {
      li_row <- m[i, ]
      as.numeric(li_row[, -1])
    },
    error = function(e) {
      print(paste("Error occurred at row", i))
      print(li_row)
      stop()
    }
  )
}
m <- read.csv(m_file_path, na.strings = c("", "HERE"))


m_file_path <- file.path(m_folder_path, m)

# Read the file
# Read the file
#m <- as.data.frame(read.csv(m_file_path)) # Convert to data frame
m <- read.csv(m_file_path)
colnames(m)[1] <- "Protein"
# Remove rows with NA values in the Protein column
m <- m[!is.na(m$Protein), ]


#try corr_lrps_meta loop
#top2 lrps with at least a count of 2
database <- read_xlsx("~/rlp_meta/results/alldb_top3_ligand.xlsx")
# Extracted lrps lists from database
l <- as.list(database$ligand) # ligands list
r <- as.list(database$`receptor(s)`) # receptors list
# Initialize empty lists to store lim and rim
lim <- list()
rim <- list()
file_info_list <- list()
all_lim_values <- list()
all_rim_values <- list()
# Loop over each ligand-receptor pair
for (i in 1:length(l)) {
  lii = l[i]
  rii = r[i]
  
  # Test li and ri in m
  if (lii %in% m$Protein & rii %in% m$Protein) {
    li_row <- m[m$Protein == lii, ]
    ri_row <- m[m$Protein == rii, ]
    lim <- c(lim, list(li_row))
    rim <- c(rim, list(ri_row))
    # Create a new row for file_info with correct column names
    new_row <- data.frame(ligand = lii, receptor = rii, file = basename(m_file_path), stringsAsFactors = FALSE)
    # Append the new row to file_info_list
    file_info_list <- c(file_info_list, list(new_row))
  }
}
print(lim)
# Check if lim or rim is empty
if (length(lim) > 0) {
  # Create lim matrix
  lim_values <- do.call(rbind, lapply(lim, function(x) as.numeric(x[, -1])))
  rownames(lim_values) <- sapply(lim, function(x) x$Protein)
  colnames(lim_values) <- 1:ncol(lim_values)
  
  # Add lim values to the list
  all_lim_values <- c(all_lim_values, list(lim_values))
}

if (length(rim) > 0) {
  # Create rim matrix
  rim_values <- do.call(rbind, lapply(rim, function(x) as.numeric(x[, -1])))
  rownames(rim_values) <- sapply(rim, function(x) x$Protein)
  colnames(rim_values) <- 1:ncol(rim_values)
  
  # Add rim values to the list
  all_rim_values <- c(all_rim_values, list(rim_values))
}


#clean Kozlova------------------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(tibble)

# Load the xlsx file
df <- read_xlsx("~/covid_data/olink_cancer/Kozlova_OlinkONC_2024_lymphoma.xlsx")
# Remove rows with specific values in the first column
df <- df %>%
  filter(.[[1]] != "Assay") %>%
  filter(.[[1]] != "Olink ID")%>%
  filter(.[[1]] != "LOD")

# Rename the columns using the second row
colnames(df) <- df[1, ]
# Remove the first row
df <- df[-1, ]
# Transpose df, keeping the column 1 as a column
df_transposed <- t(df)
# Replace the column names with the first row
colnames(df_transposed) <- df_transposed[1, ]
# Remove the first row
df_transposed <- df_transposed[-1, ]

# Add the UniProt IDs as a column
df_transposed <- data.frame(UniProt = rownames(df_transposed), df_transposed)
colnames(df_transposed)[1] <- "Protein"
library(org.Hs.eg.db)
protein_map <- mapIds(org.Hs.eg.db, keys=df_transposed$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
df_transposed$Protein <- as.character(protein_map)
df_transposed$Protein[is.na(df_transposed$Protein)] <- df_transposed$Protein[is.na(df_transposed$Protein)]
# Rename duplicate protein names
df_transposed$Protein <- make.unique(as.character(df_transposed$Protein), sep = ".")
df_transposed <- df_transposed %>%
  filter(!is.na(df_transposed$Protein)) %>%
  filter(!is.null(df_transposed$Protein))%>%
  filter(!str_detect(Protein, ","))
write.csv(df_transposed, file="~/covid_data/olink_cancer/Clean_olink/Kozlova_OlinkONC_2024_lymphoma.csv", row.names=FALSE)




#clean Li-----------------------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(tibble)

# Load the xlsx file
df <- read_xlsx("~/covid_data/olink_cancer/Li_OlinkONC_2024_Targeted_proteomics_cervical_cancer.xlsx", skip = 4)
# Remove rows with specific values in the first column
df <- df %>%
  filter(!is.na(.[[2]])) %>%
  filter(.[[1]] != "OlinkID")
# Remove the last three rows
df <- head(df, -3)
# Transpose df, keeping the column 1 as a column
df_transposed <- t(df)
# Replace the column names with the first row
colnames(df_transposed) <- df_transposed[1, ]
# Remove the first row
df_transposed <- df_transposed[-1, ]

# Add the UniProt IDs as a column
df_transposed <- data.frame(UniProt = rownames(df_transposed), df_transposed)
colnames(df_transposed)[1] <- "Protein"
library(org.Hs.eg.db)
protein_map <- mapIds(org.Hs.eg.db, keys=df_transposed$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
df_transposed$Protein <- as.character(protein_map)
df_transposed$Protein[is.na(df_transposed$Protein)] <- df_transposed$Protein[is.na(df_transposed$Protein)]
# Rename duplicate protein names
df_transposed$Protein <- make.unique(as.character(df_transposed$Protein), sep = ".")
df_transposed <- df_transposed %>%
  filter(!is.na(df_transposed$Protein)) %>%
  filter(!is.null(df_transposed$Protein))%>%
  filter(!str_detect(Protein, ","))
# Remove the last four rows
df_transposed <- head(df_transposed, -4)
write.csv(df_transposed, file="~/covid_data/olink_cancer/Clean_olink/Li_OlinkONC_2024_Targeted_proteomics_cervical_cancer_new.csv", row.names=FALSE)

#Clean coffey-------------------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(tibble)

# Load the xlsx file
df <- read_xlsx("~/covid_data/olink_cancer/Coffey_OLlinkONC_2024_Phase_1_elotuzumab.xlsx")
# Rename the columns using the second row
colnames(df) <- df[1, ]
# Remove the first row
df <- df[-1, ]
# Rename the columns using the second row
colnames(df) <- df[1, ]
# Remove the first row
df <- df[-1, ]
# Remove the second column
df <- df[ , -2]
# Transpose df, keeping the column 1 as a column
df_transposed <- t(df)
# Replace the column names with the first row
colnames(df_transposed) <- df_transposed[1, ]
# Remove the first row
df_transposed <- df_transposed[-1, ]

# Add the UniProt IDs as a column
df_transposed <- data.frame(UniProt = rownames(df_transposed), df_transposed)
colnames(df_transposed)[1] <- "Protein"
df_transposed$Protein <- make.unique(as.character(df_transposed$Protein), sep = ".")
df_transposed <- df_transposed %>%
  filter(!is.na(df_transposed$Protein)) %>%
  filter(!is.null(df_transposed$Protein))%>%
  filter(!str_detect(Protein, ","))
write.csv(df_transposed, file="~/covid_data/olink_cancer/Clean_olink/Coffey_OLlinkONC_2024_Phase_1_elotuzumab_new.csv", row.names=FALSE)

#clean BLANCO-------------------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(tibble)

# Load the xlsx file
df <- read_xlsx("~/covid_data/olink_cancer/Blanco-Heredia_OlinkONC_2024_Converging.xlsx")
# Rename the columns using the second row
colnames(df) <- df[1, ]
# Remove the first row
df <- df[-1, ]
# Rename the columns using the second row
colnames(df) <- df[1, ]
# Remove the first row
df <- df[-1, ]
# Remove the third column
df <- df[ , -3]
df <- head(df, -21)
# Remove the second column
df <- df[ , -2]
# Transpose df, keeping the column 1 as a column
df_transposed <- t(df)
# Replace the column names with the first row
colnames(df_transposed) <- df_transposed[1, ]
# Remove the first row
df_transposed <- df_transposed[-1, ]

# Add the UniProt IDs as a column
df_transposed <- data.frame(UniProt = rownames(df_transposed), df_transposed)
colnames(df_transposed)[1] <- "Protein"
df_transposed$Protein <- make.unique(as.character(df_transposed$Protein), sep = ".")
# Replace "-" with NA in all columns except the first one
# Replace "-" with NA in all columns except the first one
df_transposed[, 2:ncol(df_transposed)] <- lapply(df_transposed[, 2:ncol(df_transposed)], function(x) ifelse(x == "-", NA, x))
df_transposed <- df_transposed %>%
  filter(!is.na(df_transposed$Protein)) %>%
  filter(!is.null(df_transposed$Protein))%>%
  filter(!str_detect(Protein, ","))
write.csv(df_transposed, file="~/covid_data/olink_cancer/Clean_olink/Blanco-Heredia_OlinkONC_2024_Converging_new.csv", row.names=FALSE)


#clean Gao----------------------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(tibble)

# Load the xlsx file
df <- read_xlsx("~/covid_data/olink_cancer/Gao_OlinkONC_2024_Prognostic_SUPP3.xlsx")
# Remove the first row
df <- df[-2, ]
df <- df[-1, ]
#df <- df[-1, ]
# Rename the columns using the second row
colnames(df) <- df[1, ]
# Remove the first row
df <- df[-1, ]
# Rename the columns using the second row
colnames(df) <- df[1, ]
# Remove the first row
df <- df[-1, ]
df <- df[-1, ]
df <- df[-1, ]
#remove last columns
df <- head(df, -5)

# Transpose df, keeping the column 1 as a column
df_transposed <- t(df)
# Replace the column names with the first row
colnames(df_transposed) <- df_transposed[1, ]
# Remove the first row
df_transposed <- df_transposed[-1, ]

# Add the UniProt IDs as a column
df_transposed <- data.frame(UniProt = rownames(df_transposed), df_transposed)
colnames(df_transposed)[1] <- "Protein"
df_transposed$Protein <- make.unique(as.character(df_transposed$Protein), sep = ".")
df_transposed <- df_transposed %>%
  filter(!is.na(df_transposed$Protein)) %>%
  filter(!is.null(df_transposed$Protein))%>%
  filter(!str_detect(Protein, ","))
#remove last columns
df_transposed <- head(df_transposed, -1)
library(org.Hs.eg.db)
protein_map <- mapIds(org.Hs.eg.db, keys=df_transposed$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
df_transposed$Protein <- as.character(protein_map)
df_transposed$Protein[is.na(df_transposed$Protein)] <- df_transposed$Protein[is.na(df_transposed$Protein)]
# Rename duplicate protein names
df_transposed$Protein <- make.unique(as.character(df_transposed$Protein), sep = ".")
df_transposed <- df_transposed %>%
  filter(!is.na(df_transposed$Protein)) %>%
  filter(!is.null(df_transposed$Protein))%>%
  filter(!str_detect(Protein, ","))
write.csv(df_transposed, file="~/covid_data/olink_cancer/Clean_olink/Gao_OlinkONC_2024_Prognostic_SUPP3_new.csv", row.names=FALSE)


#clean Minas--------------------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(tibble)

# Load the xlsx file
df <- read_xlsx("~/covid_data/olink_cancer/Minas_OlinkONC_2023_prostate cancer.xlsx")
# Remove the column
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
df <- df[ , -2]
# Transpose df, keeping the column 1 as a column
df_transposed <- t(df)
# Replace the column names with the first row
colnames(df_transposed) <- df_transposed[1, ]
# Remove the first row
df_transposed <- df_transposed[-1, ]

# Add the UniProt IDs as a column
df_transposed <- data.frame(UniProt = rownames(df_transposed), df_transposed)
colnames(df_transposed)[1] <- "Protein"
df_transposed$Protein <- make.unique(as.character(df_transposed$Protein), sep = ".")
df_transposed <- df_transposed %>%
  filter(!is.na(df_transposed$Protein)) %>%
  filter(!is.null(df_transposed$Protein))%>%
  filter(!str_detect(Protein, ","))
df_transposed$Protein <- toupper(df_transposed$Protein)
write.csv(df_transposed, file="~/covid_data/olink_cancer/Clean_olink/Minas_OlinkONC_2023_prostate cancer_new.csv", row.names=FALSE)


#clean Alsawaf------------------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(tibble)

# Load the xlsx file
df <- read_csv("~/covid_data/olink_cancer/Al_Sawaf_OlinkONC_2023_TRACERx.csv")
# Remove the columns
df <- df[ , -1]
# Get the column names that meet the condition
col_names <- which(str_detect(names(df), "SampleID|UniProt|Panel|NPX"))
# Select the first column and the columns that meet the condition
df <- df[, c(1, col_names)]
# Remove the columns
df <- df[ , -1]
df <- df[grep("Oncology|Cardiovascular", df$Panel, ignore.case = TRUE), ]
df <- df %>% select(-c(Panel, Panel_Lot_Nr))
library(tidyr)
df_matrix <- pivot_wider(df, 
                         id_cols = UniProt, 
                         names_from = SampleID, 
                         values_from = NPX)
colnames(df_matrix)[1] <- "Protein"
library(org.Hs.eg.db)
protein_map <- mapIds(org.Hs.eg.db, keys=df_matrix$Protein, column="SYMBOL", keytype="UNIPROT", multiVals="first")
df_matrix$Protein <- as.character(protein_map)
df_matrix$Protein[is.na(df_matrix$Protein)] <- df_matrix$Protein[is.na(df_matrix$Protein)]
# Rename duplicate protein names
df_matrix$Protein <- make.unique(as.character(df_matrix$Protein), sep = ".")
df_matrix <- df_matrix %>%
  filter(!is.na(df_matrix$Protein)) %>%
  filter(!is.null(df_matrix$Protein))%>%
  filter(!str_detect(Protein, ","))
write.csv(df_matrix, file="~/covid_data/olink_cancer/Clean_olink/Al_Sawaf_OlinkONC_2023_TRACERx.csv", row.names=FALSE)

