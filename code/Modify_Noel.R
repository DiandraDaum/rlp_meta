#modify Noel
#Code to modify the original Human-2020-Noël-LR-pairs.xlsx and the 
#Human-2020-Noël-LR-pairs.xlsx files to show all the ligand1/2/3/4 and receptor 1/2/3/4/5 pairs.

# Load necessary libraries
library(readxl)
library(tidyr)
library(dplyr)
library(writexl)

# Load the xlsx file
data <- read_excel("/Users/diandra/rlp_meta/data/ICELLNET/Human-2020-Noël-LR-pairs.xlsx")

# Pivot the data to create new rows for each combination of ligand and receptor
data_split <- data %>%
  tidyr::pivot_longer(
    cols = c("Ligand 1", "Ligand 2"),
    names_to = "Ligand_Type",
    values_to = "Ligand"
  ) %>%
  tidyr::pivot_longer(
    cols = c("Receptor 1", "Receptor 2", "Receptor 3"),
    names_to = "Receptor_Type",
    values_to = "Receptor"
  ) %>%
  dplyr::select(-Ligand_Type, -Receptor_Type) %>%
  dplyr::filter(!is.na(Ligand) &!is.na(Receptor))

# Write the modified data to a new Excel file
write_xlsx(data_split, "/Users/diandra/rlp_meta/data/new_files/Human-2020-Noël-LR-pair_new.xlsx")


#modify Noelv2
# Load necessary libraries
library(readxl)
library(tidyr)
library(dplyr)
library(writexl)

# Load the xlsx file
data <- read_excel("/Users/diandra/rlp_meta/data/ICELLNET/ICELLNETdb_v2.xlsx")

# Pivot the data to create new rows for each combination of ligand and receptor
data_split <- data %>%
  tidyr::pivot_longer(
    cols = c("Ligand 1", "Ligand 2", "Ligand 3", "Ligand 4"),
    names_to = "Ligand_Type",
    values_to = "Ligand"
  ) %>%
  tidyr::pivot_longer(
    cols = c("Receptor 1", "Receptor 2", "Receptor 3", "Receptor 4", "Receptor 5"),
    names_to = "Receptor_Type",
    values_to = "Receptor"
  ) %>%
  dplyr::select(-Ligand_Type, -Receptor_Type) %>%
  dplyr::filter(!is.na(Ligand) &!is.na(Receptor))

# Write the modified data to a new Excel file
write_xlsx(data_split, "/Users/diandra/rlp_meta/data/new_files/ICELLNETdb_v2_new.xlsx")


