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

#jaccard matrix names-----------------------------------------------------------------
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

# Create the heatmap
jaccard_df_long <- jaccard_df %>%
  tibble::rownames_to_column("Var1") %>%
  tidyr::pivot_longer(cols = -Var1, 
                      names_to = "Var2", 
                      values_to = "value")

# Create a function to shorten file names
shorten_file_names <- function(x) {
  if (nchar(x) > 10) {
    return(paste0(substr(x, 1, 10), "..."))
  } else {
    return(x)
  }
}

# Apply the function to Var1 and Var2
jaccard_df_long$Var1 <- sapply(jaccard_df_long$Var1, shorten_file_names)
jaccard_df_long$Var2 <- sapply(jaccard_df_long$Var2, shorten_file_names)

# Create the heatmap
jaccard_plot <- ggplot(jaccard_df_long, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) + 
  geom_tile() + 
  geom_text(size = 2.5) + 
  scale_fill_gradient(low = "royalblue2", high = "yellow", name = "Jaccard Index") + 
  theme_minimal() + 
  labs(x = "File", y = "File") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  theme(panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", colour = "black")) + 
  theme(legend.position = "right")

# Print the heatmap
print(jaccard_plot)

# Save the plot as a PDF
ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_new.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)



#% sharing----------------------------------------------------------------------
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

# Calculate the percentage of shared interaction
percent_shared_matrix <- matrix(0, nrow = length(file_names), ncol = length(file_names))
colnames(percent_shared_matrix) <- file_names
rownames(percent_shared_matrix) <- file_names

for (i in 1:nrow(percent_shared_matrix)) {
  for (j in 1:ncol(percent_shared_matrix)) {
    intersection <- sum(binary_matrix[i, ] & binary_matrix[j, ])
    total <- sum(binary_matrix[i, ] | binary_matrix[j, ])
    percent_shared_matrix[i, j] <- (intersection / total) * 100
  }
}

# Add a final column for the mean shared interaction
mean_shared_interaction <- rowMeans(percent_shared_matrix)
percent_shared_matrix <- cbind(percent_shared_matrix, Mean_Sharing = mean_shared_interaction)

# Convert the percentage shared matrix to a data frame
percent_shared_df <- as.data.frame(t(percent_shared_matrix))
colnames(percent_shared_df) <- file_names

# Assign a number to each file
file_numbers <- 1:length(file_names)
file_legend <- data.frame(File = file_names, Number = file_numbers)

percent_shared_df_long <- percent_shared_df %>%
  tibble::rownames_to_column("Var1") %>%
  tidyr::pivot_longer(cols = -Var1, 
                      names_to = "Var2", 
                      values_to = "value") %>%
  mutate(Var1 = factor(Var1, levels = file_names, labels = file_numbers),
         Var2 = factor(Var2, levels = c(file_names, "Mean_Sharing"), labels = c(file_numbers, "Mean_Sharing")))

# Create the heatmap
heatmap_plot <- ggplot(percent_shared_df_long, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) + 
  geom_tile() + 
  geom_text(size = 2) + 
  scale_fill_gradient(low = "royalblue2", high = "yellow", name = "% Shared lrps") + 
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
ggsave("/Users/diandra/rlp_meta/results/plots/Percent_Shared_plot.pdf", plot = heatmap_plot, width = 8, height = 6, units = "in", dpi = 300)



#sharing names------------------------------------------------------------------
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

# Calculate the percentage of shared interaction
percent_shared_matrix <- matrix(0, nrow = length(file_names), ncol = length(file_names))
colnames(percent_shared_matrix) <- file_names
rownames(percent_shared_matrix) <- file_names

for (i in 1:nrow(percent_shared_matrix)) {
  for (j in 1:ncol(percent_shared_matrix)) {
    intersection <- sum(binary_matrix[i, ] & binary_matrix[j, ])
    total <- sum(binary_matrix[i, ] | binary_matrix[j, ])
    percent_shared_matrix[i, j] <- (intersection / total) * 100
  }
}

# Add a final column for the mean shared interaction
mean_shared_interaction <- rowMeans(percent_shared_matrix)
percent_shared_matrix <- cbind(percent_shared_matrix, Mean_Sharing = mean_shared_interaction)

# Convert the percentage shared matrix to a data frame
percent_shared_df <- as.data.frame(t(percent_shared_matrix))
colnames(percent_shared_df) <- file_names

# Create the heatmap
shorten_file_names <- Vectorize(function(x) {
  if (nchar(x) > 12) {
    return(paste0(substr(x, 1, 12), "..."))
  } else {
    return(x)
  }
})

# Calculate mean sharing values for each file
mean_sharing_values <- rowMeans(percent_shared_matrix)
mean_sharing_df <- data.frame(Var1 = shorten_file_names(file_names), 
                              Var2 = shorten_file_names("Mean_Sharing"), 
                              value = mean_sharing_values)

percent_shared_df_long <- percent_shared_df %>%
  tibble::rownames_to_column("Var1") %>%
  tidyr::pivot_longer(cols = -Var1, 
                      names_to = "Var2", 
                      values_to = "value") %>%
  mutate(Var1 = sapply(Var1, shorten_file_names),
         Var2 = sapply(Var2, shorten_file_names)) %>%
  bind_rows(mean_sharing_df) %>%
  arrange(Var2)

heatmap_plot <- ggplot(percent_shared_df_long, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) + 
  geom_tile() + 
  geom_text(size = 2) + 
  scale_fill_gradient(low = "royalblue2", high = "yellow", name = "% Shared lrps") + 
  theme_minimal() + 
  labs(x = "File", y = "File") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  theme(panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", colour = "black")) + 
  theme(legend.position = "right")

# Print the heatmap
print(heatmap_plot)


# Save the plot as a PDF
ggsave("/Users/diandra/rlp_meta/results/plots/Percent_Shared_plot_new.pdf", plot = heatmap_plot, width = 8, height = 6, units = "in", dpi = 300)
