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

# Read the xlsx output file
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")

# Extract the file names from the "file" column
file_names <- results_distinct$file %>%
  str_split("; ") %>%
  unlist() %>%
  unique()

# Create a binary matrix to store the interaction data
binary_matrix <- matrix(0, nrow = nrow(results_distinct), ncol = length(file_names))
colnames(binary_matrix) <- file_names

# Populate the binary matrix with interaction data
for (i in 1:nrow(results_distinct)) {
  files <- results_distinct$file[i] %>%
    str_split("; ") %>%
    unlist()
  binary_matrix[i, match(files, file_names)] <- 1
}

# Calculate the Jaccard index
jaccard_matrix <- 1 - vegdist(binary_matrix, method = "jaccard")

# Convert the Jaccard matrix to a data frame
jaccard_df <- as.data.frame(as.matrix(jaccard_matrix))
colnames(jaccard_df) <- file_names
#rownames(jaccard_df) <- file_names

# Convert the data frame to long format for ggplot
jaccard_df_long <- reshape2::melt(jaccard_df)

# Create the heatmap
heatmap_plot <- ggplot(jaccard_df_long, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "blue", high = "yellow") + 
  theme_minimal() + 
  labs(x = "File", y = "File", fill = "Jaccard Index") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Print the plot
print(heatmap_plot)

# Save the plot as a PDF
pdf("/Users/diandra/rlp_meta/results/plots/heatmap_plot.pdf", width = 8, height = 6)
print(heatmap_plot)
dev.off()