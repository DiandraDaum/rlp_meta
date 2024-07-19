#Heatmaps from results data

# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(readxl)
library(stringr)
#library(ComplexHeatmap)
library(jpeg)
library(magick)
# Load the necessary libraries
library(heatmaply)


# Read the xlsx output file
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldb_count2.xlsx")

# Extract the top 1000 interactions based on the "count" column
top_interactions <- results_distinct[order(results_distinct$count, decreasing = TRUE), ][1:10, ]

# Extract the file names from the "file" column
file_names <- top_interactions$file %>%
  str_split("; ") %>%
  unlist() %>%
  unique() %>%
  sort()

# Create a binary matrix to store the interaction data
binary_matrix <- matrix(0, nrow = length(file_names), ncol = nrow(top_interactions))
colnames(binary_matrix) <- top_interactions$interaction
rownames(binary_matrix) <- file_names

# Populate the binary matrix with interaction data
for (i in 1:nrow(top_interactions)) {
  files <- top_interactions$file[i] %>%
    str_split("; ") %>%
    unlist()
  for (j in 1:length(files)) {
    binary_matrix[which(file_names == files[j]), i] <- 1
  }
}

# Create a heatmap using heatmaply
heatmap_plot <- heatmaply(t(binary_matrix), 
                          col = c("lightblue", "pink"), 
                          main = "Top 10 Interactions", 
                          xlab = "Files", 
                          ylab = "Interactions", 
                          border = "black", 
                          cluster_rows = TRUE, 
                          cluster_cols = TRUE)

# Save the plot as a PDF
pdf("/Users/diandra/rlp_meta/results/plots/Top_Interactions_heatmap.pdf", width = 8, height = 6)
heatmap_plot
dev.off()

