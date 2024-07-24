#Upset plots with results data
#code to create an upset plot to see the overlap between the files used to create the input file
# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(UpSetR) #https://doi.org/10.1093/bioinformatics/btx364

# Read the xlsx output file
#results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull_count2.xlsx")

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

# Convert the binary matrix to a data frame
binary_df <- as.data.frame(binary_matrix)

# Cut the file names
colnames(binary_df) <- substr(colnames(binary_df), 1, 10)

# Create the upset plot
upset_plot <- upset(binary_df, 
                    main.bar.color = "lightblue", 
                    sets.bar.color = "lightblue", 
                    order.by = "freq", 
                    decreasing = T, 
                    nsets = ncol(binary_df), #from 5 to 20
)
                    

# Print the plot
print(upset_plot)

# Save the plot as a PDF
#pdf("/Users/diandra/rlp_meta/results/plots/upset_plot.pdf", width = 8, height = 6)
pdf("/Users/diandra/rlp_meta/results/plots/upset_plot2.pdf", width = 8, height = 6)
print(upset_plot)
dev.off()

