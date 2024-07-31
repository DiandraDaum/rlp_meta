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
#results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count2.xlsx")
#results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count3.xlsx")

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
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  theme(panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", colour = "black")) + 
  theme(legend.position = "right")

# Print the heatmap
print(jaccard_plot)

# Save the plot as a PDF
ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_new.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_new2.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_new3.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)




#try alldbfull clustering centroid---------------------------------------------------------------
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
#results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count2.xlsx")
#results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count3.xlsx")

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

# Calculate the distance between each pair of files
distance_matrix <- 1 - jaccard_matrix

# Perform hierarchical clustering
#hc <- hclust(as.dist(distance_matrix), method = "ward.D2")
#hc <- hclust(as.dist(distance_matrix), method = "mcquitty") #WPGMA
#hc <- hclust(as.dist(distance_matrix), method = "ward.D")
#hc <- hclust(as.dist(distance_matrix), method = "single")
#hc <- hclust(as.dist(distance_matrix), method = "complete")
#hc <- hclust(as.dist(distance_matrix), method = "average") #UPGMA
#hc <- hclust(as.dist(distance_matrix), method = "median") #WPGMC:good
hc <- hclust(as.dist(distance_matrix), method = "centroid") #UPGMC:probaly the best

# Reorder the rows and columns of the Jaccard matrix based on the clustering
jaccard_matrix <- jaccard_matrix[hc$order, hc$order]

# Convert the Jaccard matrix to a data frame
jaccard_df <- as.data.frame(as.matrix(jaccard_matrix))
colnames(jaccard_df) <- file_names[hc$order]
rownames(jaccard_df) <- file_names[hc$order]

# Create the jaccard_df_long data frame with clustering information
jaccard_df_long <- jaccard_df %>%
  rownames_to_column("Var1") %>%
  gather(Var2, value, -Var1) %>%
  mutate(Var1 = factor(Var1, levels = file_names[hc$order]),
         Var2 = factor(Var2, levels = file_names[hc$order]))

#cut the file names in a constant way
jaccard_df_long$Var1 <- sub("([0-9]{2}).*$", "\\1", jaccard_df_long$Var1)
jaccard_df_long$Var2 <- sub("([0-9]{2}).*$", "\\1", jaccard_df_long$Var2)

# Set the correct order for Var1 and Var2
jaccard_df_long$Var1 <- factor(jaccard_df_long$Var1, levels = unique(jaccard_df_long$Var1)[order(match(unique(jaccard_df_long$Var1), file_names[hc$order]))])
jaccard_df_long$Var2 <- factor(jaccard_df_long$Var2, levels = unique(jaccard_df_long$Var2)[order(match(unique(jaccard_df_long$Var2), file_names[hc$order]))])

# Create the heatmap
jaccard_plot <- ggplot(jaccard_df_long, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) + 
  geom_tile() + 
  geom_text(size = 2.5) + 
  scale_fill_gradient(low = "royalblue2", high = "yellow", name = "Jaccard Index") + 
  theme_minimal() + 
  labs(x = "File", y = "File") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  theme(panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", colour = "black")) + 
  theme(legend.position = "right")

# Print the heatmap
print(jaccard_plot)

# Save the plot as a PDF
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_clustered_ward.D2.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_clustered_mcquitty.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_clustered_ward.D.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_clustered_single.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_clustered_complete.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_clustered_average.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_clustered_median.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_clustered_centroid.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)

#try alldbfull_count2 clustering centroid---------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(vegan) # for vegdist function
library(RColorBrewer)
library(gridExtra)

# Read the xlsx output file
#results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count2.xlsx")
#results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count3.xlsx")

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

# Calculate the distance between each pair of files
distance_matrix <- 1 - jaccard_matrix

# Perform hierarchical clustering
#hc <- hclust(as.dist(distance_matrix), method = "ward.D2")
hc <- hclust(as.dist(distance_matrix), method = "centroid")

# Reorder the rows and columns of the Jaccard matrix based on the clustering
jaccard_matrix <- jaccard_matrix[hc$order, hc$order]

# Convert the Jaccard matrix to a data frame
jaccard_df2 <- as.data.frame(as.matrix(jaccard_matrix))
colnames(jaccard_df2) <- file_names[hc$order]
rownames(jaccard_df2) <- file_names[hc$order]

# Create the jaccard_df_long data frame with clustering information
jaccard_df_long <- jaccard_df2 %>%
  rownames_to_column("Var1") %>%
  gather(Var2, value, -Var1) %>%
  mutate(Var1 = factor(Var1, levels = file_names[hc$order]),
         Var2 = factor(Var2, levels = file_names[hc$order]))

#cut the file names in a constant way
jaccard_df_long$Var1 <- sub("([0-9]{2}).*$", "\\1", jaccard_df_long$Var1)
jaccard_df_long$Var2 <- sub("([0-9]{2}).*$", "\\1", jaccard_df_long$Var2)

# Set the correct order for Var1 and Var2
jaccard_df_long$Var1 <- factor(jaccard_df_long$Var1, levels = unique(jaccard_df_long$Var1)[order(match(unique(jaccard_df_long$Var1), file_names[hc$order]))])
jaccard_df_long$Var2 <- factor(jaccard_df_long$Var2, levels = unique(jaccard_df_long$Var2)[order(match(unique(jaccard_df_long$Var2), file_names[hc$order]))])

# Create the heatmap
jaccard_plot2 <- ggplot(jaccard_df_long, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) + 
  geom_tile() + 
  geom_text(size = 2.5) + 
  scale_fill_gradient(low = "royalblue2", high = "yellow", name = "Jaccard Index") + 
  theme_minimal() + 
  labs(x = "File", y = "File") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  theme(panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", colour = "black")) + 
  theme(legend.position = "right")

# Print the heatmap
print(jaccard_plot2)

# Save the plot as a PDF
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_clustered_ward.D2.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot2_clustered_centroid.pdf", plot = jaccard_plot2, width = 8, height = 6, units = "in", dpi = 300)
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot3_clustered_ward.D2.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)


#try alldbfull_count3 clustering centroid---------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(vegan) # for vegdist function
library(RColorBrewer)
library(gridExtra)

# Read the xlsx output file
#results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")
#results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count2.xlsx")
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count3.xlsx")

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

# Calculate the distance between each pair of files
distance_matrix <- 1 - jaccard_matrix

# Perform hierarchical clustering
#hc <- hclust(as.dist(distance_matrix), method = "ward.D2")
hc <- hclust(as.dist(distance_matrix), method = "centroid")

# Reorder the rows and columns of the Jaccard matrix based on the clustering
jaccard_matrix <- jaccard_matrix[hc$order, hc$order]

# Convert the Jaccard matrix to a data frame
jaccard_df3 <- as.data.frame(as.matrix(jaccard_matrix))
colnames(jaccard_df3) <- file_names[hc$order]
rownames(jaccard_df3) <- file_names[hc$order]

# Create the jaccard_df_long data frame with clustering information
jaccard_df_long <- jaccard_df3 %>%
  rownames_to_column("Var1") %>%
  gather(Var2, value, -Var1) %>%
  mutate(Var1 = factor(Var1, levels = file_names[hc$order]),
         Var2 = factor(Var2, levels = file_names[hc$order]))

#cut the file names in a constant way
jaccard_df_long$Var1 <- sub("([0-9]{2}).*$", "\\1", jaccard_df_long$Var1)
jaccard_df_long$Var2 <- sub("([0-9]{2}).*$", "\\1", jaccard_df_long$Var2)

# Set the correct order for Var1 and Var2
jaccard_df_long$Var1 <- factor(jaccard_df_long$Var1, levels = unique(jaccard_df_long$Var1)[order(match(unique(jaccard_df_long$Var1), file_names[hc$order]))])
jaccard_df_long$Var2 <- factor(jaccard_df_long$Var2, levels = unique(jaccard_df_long$Var2)[order(match(unique(jaccard_df_long$Var2), file_names[hc$order]))])

# Create the heatmap
jaccard_plot3 <- ggplot(jaccard_df_long, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) + 
  geom_tile() + 
  geom_text(size = 2.5) + 
  scale_fill_gradient(low = "royalblue2", high = "yellow", name = "Jaccard Index") + 
  theme_minimal() + 
  labs(x = "File", y = "File") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  theme(panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", colour = "black")) + 
  theme(legend.position = "right")

# Print the heatmap
print(jaccard_plot3)

# Save the plot as a PDF
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot_clustered_ward.D2.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
#ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot2_clustered_ward.D2.pdf", plot = jaccard_plot, width = 8, height = 6, units = "in", dpi = 300)
ggsave("/Users/diandra/rlp_meta/results/plots/Jaccard_plot3_clustered_centroid.pdf", plot = jaccard_plot3, width = 8, height = 6, units = "in", dpi = 300)

#mean difference jaccard plots x3-----------------------------------------------
# Calculate the mean difference between each pair of matrixes
mean_diff_1_2 <- mean(abs(as.matrix(jaccard_df) - as.matrix(jaccard_df2)), na.rm = TRUE)
mean_diff_1_3 <- mean(abs(as.matrix(jaccard_df) - as.matrix(jaccard_df3)), na.rm = TRUE)
mean_diff_2_3 <- mean(abs(as.matrix(jaccard_df2) - as.matrix(jaccard_df3)), na.rm = TRUE)

# Round the results to keep decimals only
mean_diff_1_2 <- round(mean_diff_1_2, 2)
mean_diff_1_3 <- round(mean_diff_1_3, 2)
mean_diff_2_3 <- round(mean_diff_2_3, 2)

# Print the results
print(paste("Mean difference between Jaccard matrix 1 and 2: ", mean_diff_1_2))
#[1] "Mean difference between Jaccard matrix 1 and 2:  0.07"
print(paste("Mean difference between Jaccard matrix 2 and 3: ", mean_diff_2_3))
#[1] "Mean difference between Jaccard matrix 2 and 3:  0.04"
print(paste("Mean difference between Jaccard matrix 1 and 3: ", mean_diff_1_3))
#[1] "Mean difference between Jaccard matrix 1 and 3:  0.09"


#% sharing names------------------------------------------------------------------
#code to obtain a % matrix of sharing interaction pairs between the files that were use to create the input file:
# Read the xlsx output file
#results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")
#results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count2.xlsx")
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count3.xlsx")

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

# Create a data frame for mean sharing values
# Create a data frame for mean sharing values
mean_sharing_df <- data.frame(Var2 = "Mean_Sharing", 
                              value = mean_sharing_values, 
                              stringsAsFactors = FALSE)

# Bind the mean sharing data frame to the long format data frame
percent_shared_df_long <- percent_shared_df %>%
  tibble::rownames_to_column("Var1") %>%
  tidyr::pivot_longer(cols = -Var1, 
                      names_to = "Var2", 
                      values_to = "value") %>%
  mutate(Var1 = sapply(Var1, shorten_file_names),
         Var2 = sapply(Var2, shorten_file_names)) %>%
  group_by(Var1) %>%
  mutate(mean_sharing = mean_sharing_df$value[match(Var1, unique(Var1))]) %>%
  select(Var1, Var2, value, everything())

percent_shared_df_long$Var1 <- factor(percent_shared_df_long$Var1, 
                                      levels = c(setdiff(unique(percent_shared_df_long$Var1), "Mean_Sharing"), "Mean_Sharing"))
#x = reorder(Var1, as.numeric(factor(percent_shared_df_long$Var1)))
#crate matrix: still to fix mean_sharing from the middle to the last column
heatmap_plot <- ggplot(percent_shared_df_long, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) + 
  geom_tile() + 
  geom_text(size = 2) + 
  scale_fill_gradient(low = "royalblue2", high = "yellow", name = "% Shared lrps") + 
  theme_minimal() + 
  labs(x = "File", y = "File") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  theme(panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", colour = "black")) + 
  theme(legend.position = "right")

# Print the heatmap
print(heatmap_plot)

# Save the plot as a PDF
#ggsave("/Users/diandra/rlp_meta/results/plots/Percent_Shared_plot_new.pdf", plot = heatmap_plot, width = 8, height = 6, units = "in", dpi = 300)
#ggsave("/Users/diandra/rlp_meta/results/plots/Percent_Shared_plot_new2.pdf", plot = heatmap_plot, width = 8, height = 6, units = "in", dpi = 300)
ggsave("/Users/diandra/rlp_meta/results/plots/Percent_Shared_plot_new3.pdf", plot = heatmap_plot, width = 8, height = 6, units = "in", dpi = 300)

