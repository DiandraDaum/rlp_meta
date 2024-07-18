#Jaccard plots with results data
#see overlap of the used datasets

## Script to generate the comparison data presented in the Manuscript
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(vegan) # for vegdist function
library(RColorBrewer)
library(gridExtra)

# Read the xlsx output file
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")

# Extract the file names from the "file" column
file_names <- results_distinct$file %>%
  str_split("; ") %>%
  unlist() %>%
  unique() %>%
  sort()

# Create a binary matrix to store the interaction data
binary_matrix <- matrix(0, nrow = length(file_names), ncol = nrow(results_distinct))
colnames(binary_matrix) <- 1:nrow(results_distinct)
rownames(binary_matrix) <- file_names

# Populate the binary matrix with interaction data
for (i in 1:nrow(results_distinct)) {
  files <- results_distinct$file[i] %>%
    str_split("; ") %>%
    unlist()
  for (j in 1:length(files)) {
    binary_matrix[which(file_names == files[j]), i] <- 1
  }
}

# Calculate the Jaccard index
jaccard_matrix <- matrix(0, nrow = length(file_names), ncol = length(file_names))
colnames(jaccard_matrix) <- file_names
rownames(jaccard_matrix) <- file_names

for (i in 1:nrow(jaccard_matrix)) {
  for (j in 1:ncol(jaccard_matrix)) {
    intersection <- sum(binary_matrix[i, ] & binary_matrix[j, ])
    union <- sum(binary_matrix[i, ] | binary_matrix[j, ])
    jaccard_matrix[i, j] <- intersection / union
  }
}

# Convert the Jaccard matrix to a data frame
jaccard_df <- as.data.frame(as.matrix(jaccard_matrix))
colnames(jaccard_df) <- file_names
rownames(jaccard_df) <- file_names

# Assign a number to each file
file_numbers <- 1:length(file_names)
file_legend <- data.frame(File = file_names, Number = file_numbers)

jaccard_df_long <- jaccard_df %>%
  tibble::rownames_to_column("Var1") %>%
  tidyr::pivot_longer(cols = -Var1, 
                      names_to = "Var2", 
                      values_to = "value") %>%
  mutate(Var1 = factor(Var1, levels = file_names, labels = file_numbers),
         Var2 = factor(Var2, levels = file_names, labels = file_numbers))

# Create the heatmap
heatmap_plot <- ggplot(jaccard_df_long, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) + 
  geom_tile() + 
  geom_text() + 
  scale_fill_gradient(low = "royalblue2", high = "yellow", name = "Jaccard Index") + 
  theme_minimal() + 
  labs(x = "File", y = "File") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  theme(panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", colour = "black")) + 
  theme(legend.position = "right")

# Print the heatmap
print(heatmap_plot)

# Print the file legend
cat("File Legend:\n")
for (i in 1:nrow(file_legend)) {
  cat(paste0(file_legend$Number[i], ": ", file_legend$File[i], "\n"))
}

# Save the plot as a PDF
ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot.pdf", plot = heatmap_plot, width = 8, height = 6, units = "in", dpi = 300)

#list of alphabetical order files numbers:
#1: CCIDB_Human_new.xlsx
#2: CellCall-2021_new.xlsx
#3: CellChatDB.humanv2-2023-Jin-LR-pairs_new.xlsx
#4: Cellinker_Homos_2021.txt
#5: Human-2010-Kirouac-LR-pairs_new.xlsx
#6: Human-2014-Qiao-LR-pairs.xlsx
#7: Human-2015-Choi-LR-pairs.txt
#8: Human-2015-Ramilowski-LR-pairs.xlsx
#9: Human-2017-Pavlicev-LR-pairs.xlsx
#10: Human-2019-Wang-LR-pairs.csv
#11: Human-2019-Ximerakis-BaderLab-2017_new2.xlsx
#12: Human-2020-Hou-LR-pairs.xlsx
#13: Human-2020-Shao-LR-pairs.txt
#14: Human-2022-Dimitrov-LR-pairs.csv
#15: Human-2023-Zhao-LR-pairs_new.tsv
#16: ICELLNETdb_v2_new.xlsx
#17: Interaction_input_Vento_Cellphonedbv5_new.csv
#18: LRdb_122019_SingleCellSignalIR.xlsx
#19: NicheNet-LR-pairs.csv
#20: OmniPathPPIs_new.tsv
