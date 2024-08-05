#Heatmaps from results data
#code to create a heatmap of presence/absence of ligand-receptor pair interaction 
#between the files that were used to build the input file

# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(readxl)
library(stringr)
library(heatmaply)

#top 20 interactions------------------------------------------------------------
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
rownames(binary_matrix) <- sub("([0-9]{2}).*$", "\\1", file_names)
#file_pairs$file_num_truncated <- str_sub(file_pairs$file_num, 1, 10)
#file_pairs$file_num_truncated <- sub("([0-9]{2}).*$", "\\1", file_pairs$file)
#rownames(binary_matrix) <- substr(rownames(binary_matrix),"([0-9]{2}).*$", "\\1")
  
#print(rownames(binary_matrix))
# Create a heatmap with a binary legend
heatmap_plot <- heatmaply(t(binary_matrix), 
                          col = c("white", "black"), #breaks = c(0, 1), 
                          main = "Top 20 Interactions", 
                          xlab = "Files", 
                          ylab = "Interactions", 
                          grid_cols = NULL, 
                          grid_rows = NULL, 
                          dendrogram = "both", 
                          k_row = 3, 
                          k_col = 3, 
                          border = "black", #ColSideColors = FALSE,
                          col_names = FALSE,
                          row_names=FALSE,
                          use_raster = TRUE,
                          raster_device = "pdf",
                          #legend = list(col = file_colors, lab = file_names)
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

#top 1000 interactions----------------------------------------------------------
# Read the xlsx output file
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count2.xlsx")

# Extract the top 1000 interactions based on the "count" column
top_interactions <- results_distinct[order(results_distinct$count, decreasing = TRUE), ][1:1000, ]

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
rownames(binary_matrix) <- sub("([0-9]{2}).*$", "\\1", file_names)

library(plotly)
# Create a heatmap with a binary legend
heatmap_plot <- heatmaply(t(binary_matrix), 
                          col = c("lightblue", "pink"), 
                          breaks = c(0, 1), 
                          main = "Top 1000 Interactions", 
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
                          yticklabels = FALSE,  
                          use_raster = TRUE,
                          raster_device = "pdf",
                          legend = list(
                            col = file_colors,
                            lab = file_names
                          )
)

# Draw the heatmap
heatmap_plot
# Save the plot as a PDF
heatmap_plot %>%
  to_image(format = "pdf", out_file = "Top1000_Interactions_heatmap.pdf", width = 800, height = 600)

#complexheatmap-----------------------------------------------------------------
# Load necessary libraries
library(dplyr)
library(readxl)
library(ComplexHeatmap)
library(stringr)
library(grid)
library(viridis)

# Load the input file
input_file <- read_xlsx("~/rlp_meta/results/alldb_top3_ligand.xlsx")

# Extract the top 1000 interactions based on the "count" column
top_interactions <- input_file[order(input_file$count, decreasing = TRUE), ][1:1000, ]

# Extract the file names from the "file" column
file_names <- top_interactions$file %>%
  str_split("; ") %>%
  unlist() %>%
  unique() %>%
  sort()

# Create a matrix of presence/absence of the top 10 interactions in the files
interaction_matrix <- matrix(0, nrow = length(file_names), ncol = nrow(top_interactions))
rownames(interaction_matrix) <- file_names
colnames(interaction_matrix) <- top_interactions$interaction

# Populate the matrix with 1s where an interaction is present in a file
for (i in 1:nrow(top_interactions)) {
  files <- unlist(str_split(top_interactions$file[i], "; "))
  for (j in 1:length(files)) {
    interaction_matrix[which(rownames(interaction_matrix) == files[j]), i] <- 1
  }
}

rownames(interaction_matrix) <- sub("([0-9]{2}).*$", "\\1", rownames(interaction_matrix))

# Perform hierarchical clustering on the file names using the centroid method
file_clusters <- hclust(dist(interaction_matrix), method = "centroid")

#invert column/rows
interaction_matrix <- t(interaction_matrix)

# Open a PDF file
pdf("/Users/diandra/rlp_meta/results/plots/Complexheatmap_top_db3.pdf", width = 10, height = 10)
# Create the heatmap using complexheatmap
Heatmap(interaction_matrix, 
        cluster_columns = file_clusters, 
        cluster_rows = FALSE, 
        row_title = "Interactions", 
        column_title = "Files",
        col = c("#fde725", "#440154"), #viridis based
        rect_gp = gpar(col = "grey8", lwd = 0.2), 
        show_row_names = TRUE, 
        show_column_names = TRUE
        )
#draw(ha)

# Close the PDF file
dev.off()
library(shiny)
shiny_env = new.env()
htShiny(ha)
ht_shiny(ha)






















