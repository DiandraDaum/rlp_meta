#check similarity between different files/versions


#check Human-2019-Ximerakis-BaderLab-2017.xlsx and receptor_ligand_interactions_mitab_v1.0_April2017.xlsx
# Load the necessary packages
library(readxl)
library(VennDiagram)

# Read the Excel files
Ximerakis <- read_excel("/Users/diandra/rlp_meta/data/input_files_for_db/Human-2019-Ximerakis-BaderLab-2017.xlsx")
mitab <- read_excel("/Users/diandra/rlp_meta/data/input_files_for_db/receptor_ligand_interactions_mitab_v1.0_April2017.xlsx")

# Extract the unique identifiers from each data frame
Ximerakis_identifiers <- unique(Ximerakis$AliasA)
mitab_identifiers <- unique(mitab$AliasA)

# Calculate the sizes of the sets and their intersection
only_Ximerakis <- length(setdiff(Ximerakis_identifiers, mitab_identifiers))
only_mitab <- length(setdiff(mitab_identifiers, Ximerakis_identifiers))
both <- length(intersect(Ximerakis_identifiers, mitab_identifiers))

# Create a Venn diagram
# Create a new plot window
dev.new()
draw.pairwise.venn(
  area1 = only_Ximerakis + both,
  area2 = only_mitab + both,
  cross.area = both,
  category = c("Ximerakis", "mitab"),
  fill = "skyblue",
  alpha = 0.5,
  main = "Venn Diagram: Ximerakis vs mitab"
)
#identical, remove mitab

library(readxl)
library(VennDiagram)

#check Human-2018-Vento-Tormo-LR-pairs.csv and interaction_input_Vento_Cellphonedbv5.csv
# Read the csv files
Vento2018 <- read_csv("/Users/diandra/rlp_meta/data/input_files_for_db/Human-2018-Vento-Tormo-LR-pairs_new.csv")
Ventov5 <- read_csv("/Users/diandra/rlp_meta/data/input_files_for_db/interaction_input_Vento_Cellphonedbv5_new.csv")

# Extract the unique identifiers from each data frame
Vento2018_identifiers <- unique(Vento2018$partner_a)
Ventov5_identifiers <- unique(Ventov5$partner_a)

# Calculate the sizes of the sets and their intersection
only_Vento2018 <- length(setdiff(Vento2018_identifiers, Ventov5_identifiers))
only_Ventov5 <- length(setdiff(Ventov5_identifiers, Vento2018_identifiers))
both <- length(intersect(Vento2018_identifiers, Ventov5_identifiers))

# Get the identifiers that are unique to each file
unique_to_Vento2018 <- setdiff(Vento2018_identifiers, Ventov5_identifiers)
unique_to_Ventov5 <- setdiff(Ventov5_identifiers, Vento2018_identifiers)

# Subset the original data frames to get the corresponding data
Vento2018_unique_data <- Vento2018[Vento2018$partner_a %in% unique_to_Vento2018, ]
Ventov5_unique_data <- Ventov5[Ventov5$partner_a %in% unique_to_Ventov5, ]

# Print the unique data
print(Vento2018_unique_data)
print(Ventov5_unique_data)


# Create a Venn diagram
# Create a new plot window
dev.new()
draw.pairwise.venn(
  area1 = only_Vento2018 + both,
  area2 = only_Ventov5 + both,
  cross.area = both,
  category = c("Vento2018", "Ventov5"),
  fill = "skyblue",
  alpha = 0.5,
  main = "Venn Diagram: Vento2018 vs Ventov5"
)
#output: 240 shared, 521 ventov5, 155 vento2018, keep both



#check Human-2020-Cabello-Aguilar-LR-pairs.csv and LRdb_122019_SingleCellSignalIR.xlsx
Cabello2020 <- read_csv("/Users/diandra/rlp_meta/data/input_files_for_db/Human-2020-Cabello-Aguilar-LR-pairs.csv")
Singlecell19 <- read_excel("/Users/diandra/rlp_meta/data/input_files_for_db/LRdb_122019_SingleCellSignalIR.xlsx")
# Extract the unique identifiers from each data frame
Cabello2020_identifiers <- unique(Cabello2020$ligand)
Singlecell19_identifiers <- unique(Singlecell19$ligand)

# Calculate the sizes of the sets and their intersection
only_Cabello2020 <- length(setdiff(Cabello2020_identifiers, Singlecell19_identifiers))
only_Singlecell19 <- length(setdiff(Singlecell19_identifiers, Cabello2020_identifiers))
both <- length(intersect(Cabello2020_identifiers, Singlecell19_identifiers))

# Create a Venn diagram
# Create a new plot window
dev.new()
draw.pairwise.venn(
  area1 = only_Cabello2020 + both,
  area2 = only_Singlecell19 + both,
  cross.area = both,
  category = c("Cabello2020", "Singlecell19"),
  fill = "skyblue",
  alpha = 0.5,
  main = "Venn Diagram: Cabello2020 vs Singlecell19"
)
#output:identical (807), remove Cabello



#check Human-2020-Jin-LR-pairs.csv and CellChatDB.humanv2-2023-Jin-LR-pairs.xlsx
Jin20 <- read_csv("/Users/diandra/rlp_meta/data/input_files_for_db/Human-2020-Jin-LR-pairs.csv")
cellchat23 <- read_xlsx("/Users/diandra/rlp_meta/data/input_files_for_db/CellChatDB.humanv2-2023-Jin-LR-pairs.xlsx")
# Extract the unique identifiers from each data frame
Jin20_identifiers <- unique(Jin20$ligand)
cellchat23_identifiers <- unique(cellchat23$ligand)

# Calculate the sizes of the sets and their intersection
only_Jin20 <- length(setdiff(Jin20_identifiers, cellchat23_identifiers))
only_cellchat23 <- length(setdiff(cellchat23_identifiers, Jin20_identifiers))
both <- length(intersect(Jin20_identifiers, cellchat23_identifiers))


# Get the identifiers that are unique to each file
unique_to_Jin20 <- setdiff(Jin20_identifiers, cellchat23_identifiers)
unique_to_cellchat23 <- setdiff(cellchat23_identifiers, Jin20_identifiers)

# Subset the original data frames to get the corresponding data
Jin20_unique_data <- Jin20[Jin20$ligand %in% unique_to_Jin20, ]
cellchat23_unique_data <- cellchat23[cellchat23$ligand %in% unique_to_cellchat23, ]

# Print the unique data
print(Jin20_unique_data)
print(cellchat23_unique_data)

# Create a Venn diagram
# Create a new plot window
dev.new()
draw.pairwise.venn(
  area1 = only_Jin20 + both,
  area2 = only_cellchat23 + both,
  cross.area = both,
  category = c("Jin20", "cellchat23"),
  fill = "skyblue",
  alpha = 0.5,
  main = "Venn Diagram: Jin20 vs cellchat23"
)

#output: cellchat23 288, both 512, Jin20 66, keep both





#check Human-2020-Noël-LR-pairs.xlsx and ICELLNETdb_v2.xlsx
file1 <- read_excel("/Users/diandra/rlp_meta/data/input_files_for_db/Human-2020-Noël-LR-pairs_new.xlsx")
file2 <- read_excel("/Users/diandra/rlp_meta/data/input_files_for_db/ICELLNETdb_v2_new.xlsx")

# Extract the unique identifiers from each data frame
file1_identifiers <- unique(file1$Ligand)
file2_identifiers <- unique(file2$Ligand)

# Calculate the sizes of the sets and their intersection
only_file1 <- length(setdiff(file1_identifiers, file2_identifiers))
only_file2 <- length(setdiff(file2_identifiers, file1_identifiers))
both <- length(intersect(file1_identifiers, file2_identifiers))


# Get the identifiers that are unique to each file
unique_to_file1 <- setdiff(file1_identifiers, file2_identifiers)
unique_to_file2 <- setdiff(file2_identifiers, file1_identifiers)

# Subset the original data frames to get the corresponding data
file1_unique_data <- file1[file1$Ligand %in% unique_to_file1, ]
file2_unique_data <- file2[file2$Ligand %in% unique_to_file2, ]

# Print the unique data
print(file1_unique_data)
print(file2_unique_data)

# Create a Venn diagram
# Create a new plot window
dev.new()
draw.pairwise.venn(
  area1 = only_file1 + both,
  area2 = only_file2 + both,
  cross.area = both,
  category = c("file1", "file2"),
  fill = "skyblue",
  alpha = 0.5,
  main = "Venn Diagram: file1 vs file2"
)
#file 2 418 contains file 1 222, remove ICELLNETv2



#check Human-2020-Shao-LR-pairs.txt and Celltalk_human_lr_pair.txt
file1 <- read_table("/Users/diandra/rlp_meta/data/input_files_for_db/Human-2020-Shao-LR-pairs.txt")
file2 <- read_table("/Users/diandra/rlp_meta/data/input_files_for_db/Celltalk_human_lr_pair.txt")
# Extract the unique identifiers from each data frame
file1_identifiers <- unique(file1$lr_pair)
file2_identifiers <- unique(file2$lr_pair)

# Calculate the sizes of the sets and their intersection
only_file1 <- length(setdiff(file1_identifiers, file2_identifiers))
only_file2 <- length(setdiff(file2_identifiers, file1_identifiers))
both <- length(intersect(file1_identifiers, file2_identifiers))


# Get the identifiers that are unique to each file
unique_to_file1 <- setdiff(file1_identifiers, file2_identifiers)
unique_to_file2 <- setdiff(file2_identifiers, file1_identifiers)

# Subset the original data frames to get the corresponding data
file1_unique_data <- file1[file1$lr_pair %in% unique_to_file1, ]
file2_unique_data <- file2[file2$lr_pair %in% unique_to_file2, ]

# Print the unique data
print(file1_unique_data)
print(file2_unique_data)

# Create a Venn diagram
# Create a new plot window
dev.new()
draw.pairwise.venn(
  area1 = only_file1 + both,
  area2 = only_file2 + both,
  cross.area = both,
  category = c("file1", "file2"),
  fill = "skyblue",
  alpha = 0.5,
  main = "Venn Diagram: file1 vs file2"
)
#output:3398 identity, remove celltalk


#check Human-2022-Zheng-MetSensor-pairs.tsv and met_sen_October-25-2022_14-52-47.tsv
file1 <- read_tsv("/Users/diandra/rlp_meta/data/input_files_for_db/Human-2022-Zheng-MetSensor-pairs.tsv")
file2 <- read_tsv("/Users/diandra/rlp_meta/data/input_files_for_db/met_sen_October-25-2022_14-52-47.tsv")
# Extract the unique identifiers from each data frame
file1_identifiers <- unique(file1$HMDB_ID)
file2_identifiers <- unique(file2$HMDB_ID)

# Calculate the sizes of the sets and their intersection
only_file1 <- length(setdiff(file1_identifiers, file2_identifiers))
only_file2 <- length(setdiff(file2_identifiers, file1_identifiers))
both <- length(intersect(file1_identifiers, file2_identifiers))


# Get the identifiers that are unique to each file
unique_to_file1 <- setdiff(file1_identifiers, file2_identifiers)
unique_to_file2 <- setdiff(file2_identifiers, file1_identifiers)

# Subset the original data frames to get the corresponding data
file1_unique_data <- file1[file1$HMDB_ID %in% unique_to_file1, ]
file2_unique_data <- file2[file2$HMDB_ID %in% unique_to_file2, ]

# Print the unique data
print(file1_unique_data)
print(file2_unique_data)

# Create a Venn diagram
# Create a new plot window
dev.new()
draw.pairwise.venn(
  area1 = only_file1 + both,
  area2 = only_file2 + both,
  cross.area = both,
  category = c("file1", "file2"),
  fill = "skyblue",
  alpha = 0.5,
  main = "Venn Diagram: file1 vs file2"
)
#file 1 440, file 2 432, remove met_sen




