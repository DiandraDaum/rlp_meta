#load CellChat database

install.packages("openxlsx")
library(openxlsx)
library(writexl)


# Assuming CellChatDB.human is a list with 4 elements
data_list <- load("/Users/diandra/Downloads/DB/Cellchat/CellChatDB.human.rda")

# Create a list to store the data frames
data_frames <- lapply(data_list, function(x) {
  if (is.data.frame(x)) {
    return(x)
  } else {
    # If the element is not a data frame, try to convert it to one
    return(as.data.frame(x))
  }
})

# Write the data frames to separate sheets in the xlsx file
write_xlsx(data_frames, "/Users/diandra/Downloads/DB/Cellchat/CellChatDB.human.xlsx")


load("/Users/diandra/Downloads/DB/Cellchat/CellChatDB.human.rda")
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
saveWorkbook(wb, "/Users/diandra/Downloads/DB/Cellchat/CellChatDB.human.xlsx", overwrite = TRUE)
