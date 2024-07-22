#Heatmaps from results data

# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(readxl)
library(stringr)
library(heatmaply)

# Read the xlsx output file
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count2.xlsx")

# Extract the top 1000 interactions based on the "count" column
top_interactions <- results_distinct[order(results_distinct$count, decreasing = TRUE), ][1:20, ]

# Extract the file names from the "file" column
file_names <- top_interactions$file %>%
  str_split("; ") %>%
  unlist() %>%
  unique() %>%
  sort()

# Create a color palette for the file names
file_colors <- rainbow(length(file_names))

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

# Remove row names from binary_matrix
rownames(binary_matrix) <- NULL

# Create a heatmap with a binary legend
heatmap_plot <- heatmaply(t(binary_matrix), 
                          col = c("lightblue", "pink"), 
                          breaks = c(0, 1), 
                          main = "Top 20 Interactions", 
                          xlab = "Files", 
                          ylab = "Interactions", 
                          grid_cols = NULL, 
                          grid_rows = NULL, 
                          dendrogram = "both", 
                          k_row = 3, 
                          k_col = 3, 
                          border = "black",
                          ColSideColors = file_colors,
                          col_names = FALSE,
                          row_names=FALSE,
                          use_raster = TRUE,
                          raster_device = "pdf",
                          legend = list(
                              col = file_colors,
                              lab = file_names
                            )
                          )

# Draw the heatmap
heatmap_plot
# Print the list of file names and their corresponding colors
print(paste("File", 1:length(file_names), ":", file_names, " Color:", file_colors))
# Save the plot as a PDF
#ggsave("/Users/diandra/rlp_meta/results/plots/Top_Interactions_heatmap.pdf", plot = heatmap_plot, width = 8, height = 6, units = "in", dpi = 300)
#create pdf file of the heatmap
pdf("/Users/diandra/rlp_meta/results/plots/Top_Interactions_heatmap.pdf", width = 8, height = 6)
dev.off

