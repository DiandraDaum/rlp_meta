#load CellChat database.rda

load("/Users/diandra/rlp_meta/data/Cellchat/CellChatDB.human.rda")
library(openxlsx)

# Assuming CellChatDB.human is a list with 4 elements
data_list <- CellChatDB.human

# Create a workbook object
wb <- createWorkbook()

# Add each data frame to a separate sheet
for (i in seq_along(data_list)) {
  addWorksheet(wb, sheetName = paste0("Sheet", i))
  writeData(wb, sheet = paste0("Sheet", i), data_list[[i]])
}

# Save the workbook to an xlsx file
saveWorkbook(wb, "/Users/diandra/rlp_meta/data/Cellchat/CellChatDB.humanv2-2023-Jin-LR-pairs.xlsx", overwrite = TRUE)

# Load the readxl package to read the xlsx file
library(readxl)

# Load the xlsx file
df <- read_excel("/Users/diandra/rlp_meta/data/Cellchat/CellChatDB.humanv2-2023-Jin-LR-pairs.xlsx")

# Rename the columns
colnames(df)[colnames(df) == "ligand"] <- "ligand.ligand"
colnames(df)[colnames(df) == "receptor"] <- "receptor.receptor"

# Write the modified dataframe back to the xlsx file
library(writexl)
write_xlsx(df, "/Users/diandra/rlp_meta/data/new_files/CellChatDB.humanv2-2023-Jin-LR-pairs_new.xlsx")







#load CellChat PPI.human.rda
library(openxlsx)
load("/Users/diandra/rlp_meta/data/Cellchat/PPI.human.rda")
# Check the name of the loaded object
ls()

# Convert the dgCMatrix object to a matrix
PPI.human_matrix <- as.matrix(PPI.human)

# Convert the matrix to a data frame
df <- as.data.frame(PPI.human_matrix)
# Save the workbook to an xlsx file
write.xlsx(df, "/Users/diandra/rlp_meta/data/Cellchat/PPI.humanv2-2023-Jin-LR-pairs.xlsx", row.names = FALSE)




#try dense matrix
# Load CellChat PPI.human.rda
load("/Users/diandra/rlp_meta/data/Cellchat/PPI.human.rda")

# Check the name of the loaded object
ls()

# Let's assume the object is named "PPI.human"
library(openxlsx)
library(Matrix)

# Convert the sparse matrix to a dense matrix
PPI.human_dense <- as.matrix(PPI.human)

# Write the dense matrix to an xlsx file
write.xlsx(PPI.human_dense, "/Users/diandra/rlp_meta/data/Cellchat/PPI.humanv2-2023-Jin-LR-pairs.xlsx", rowNames = FALSE)



