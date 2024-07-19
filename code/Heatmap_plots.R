#Heatmaps from results data

#load libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(readxl)
library(stringr)

# Read the xlsx output file
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")

# Extract the top 1000 interactions based on the "count" column
top_interactions <- results_distinct[order(results_distinct$count, decreasing = TRUE), ][1:1000, ]

# Extract the file names from the "file" column
file_names <- top_interactions$file %>%
  str_split("; ") %>%
  unlist() %>%
  unique() %>%
  sort()

# Create a binary matrix to store the interaction data
binary_matrix <- matrix(0, nrow = length(file_names), ncol = nrow(top_interactions))
colnames(binary_matrix) <- 1:nrow(top_interactions)
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

# Convert the binary matrix to a data frame
binary_df <- as.data.frame(t(binary_matrix))
colnames(binary_df) <- file_names

# Assign a number to each file
file_numbers <- 1:length(file_names)
file_legend <- data.frame(File = file_names, Number = file_numbers)

# Reorder factor levels
binary_df_long <- binary_df %>%
  mutate(Interaction = row_number()) %>%
  pivot_longer(-Interaction, names_to = "File", values_to = "Presence") %>%
  mutate(Presence = as.factor(Presence)) %>%
  mutate(File = forcats::fct_infreq(File))

# Create the heatmap
heatmap_plot <- ggplot(binary_df_long, aes(x = Interaction, y = File, fill = Presence)) + 
  geom_tile() + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"), name = "Presence") + 
  theme_minimal() + 
  labs(x = "Interaction", y = "File") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  theme(panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", colour = "black")) + 
  theme(legend.position = "right")

# Print the heatmap
print(heatmap_plot)

# Save the plot as a PDF
ggsave("/Users/diandra/rlp_meta/results/plots/Top_Interactions_plot.pdf", plot = heatmap_plot, width = 8, height = 6, units = "in", dpi = 300)
