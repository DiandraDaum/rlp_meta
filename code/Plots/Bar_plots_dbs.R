#create histograms plots with results data

#histograms lrp in each dataset
#input_files_for_db
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

# Read the xlsx output file
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")

# Count the total number of pairs for each file
file_pairs <- results_distinct %>%
  mutate(file = stringr::str_split(file, "; ")) %>%
  tidyr::unnest(file) %>%
  group_by(file) %>%
  summarise(total_pairs = n())

# Calculate the total number of pairs in the input file
total_pairs_in_file <- nrow(results_distinct)

# Assign a number and color to each file
file_pairs <- file_pairs %>%
  mutate(file_num = reorder(file, total_pairs)) %>%
  mutate(file_color = as.numeric(factor(file)))

#cut file names
#file_pairs$file_num_truncated <- str_sub(file_pairs$file_num, 1, 10)
file_pairs$file_num_truncated <- sub("([0-9]{2}).*$", "\\1", file_pairs$file)

# Reorder the file_num_truncated variable
file_pairs <- file_pairs %>%
  mutate(file_num_truncated = reorder(file_num_truncated, total_pairs))

# Create the plot
p <- ggplot(file_pairs, aes(x = file_num_truncated, y = total_pairs, fill = factor(file_num_truncated))) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = comma(total_pairs)), vjust = -0.5, size = 3) + 
  labs(x = "DB files", y = "Number of lrps") + 
  ggtitle("Ligand-receptor pairs in each dataset") + 
  theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) + 
  scale_y_continuous(labels = comma, breaks = seq(0, max(file_pairs$total_pairs) + 2000, by = 2000), limits = c(0, max(file_pairs$total_pairs) + 2000)) + 
  annotate("text", x = 1, y = max(file_pairs$total_pairs)+ 2000, label = paste0("Total lrps: ", comma(total_pairs_in_file)), hjust = 0, vjust = 1)

# Print the plot to the Plots pane in RStudio
print(p)

# Save the plot as a PDF
ggsave("/Users/diandra/rlp_meta/results/plots/ligand_receptor_pairs.pdf", width = 8, height = 6, units = "in")

#at least 2 counts--------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

# Read the xlsx output file
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")

# Filter rows with count >= 2
results_distinct_filtered <- results_distinct %>%
  filter(count >= 2)

# Count the total number of pairs for each file
file_pairs <- results_distinct_filtered %>%
  mutate(file = stringr::str_split(file, "; ")) %>%
  tidyr::unnest(file) %>%
  group_by(file) %>%
  summarise(total_pairs = n())

# Calculate the total number of pairs in the filtered input file
total_pairs_in_file <- nrow(results_distinct_filtered)

# Assign a number and color to each file
file_pairs <- file_pairs %>%
  mutate(file_num = reorder(file, total_pairs)) %>%
  mutate(file_color = as.numeric(factor(file)))

#cut file names
file_pairs$file_num_truncated <- sub("([0-9]{2}).*$", "\\1", file_pairs$file)

# Reorder the file_num_truncated variable
file_pairs <- file_pairs %>%
  mutate(file_num_truncated = reorder(file_num_truncated, total_pairs))

# Create the plot
p <- ggplot(file_pairs, aes(x = file_num_truncated, y = total_pairs, fill = factor(file_num_truncated))) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = comma(total_pairs)), vjust = -0.5, size = 3) + 
  labs(x = "DB files", y = "Total Number of lrps") + 
  ggtitle("Ligand-receptor pairs in each dataset (count >= 2)") + 
  theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) + 
  scale_y_continuous(labels = comma, breaks = seq(0, max(file_pairs$total_pairs) + 1000, by = 1000), limits = c(0, max(file_pairs$total_pairs) + 1000)) + 
  annotate("text", x = 1, y = max(file_pairs$total_pairs)+ 1000, label = paste0("Total lrps: ", comma(total_pairs_in_file)), hjust = 0, vjust = 1)

# Print the plot to the Plots pane in RStudio
print(p)

# Save the plot as a PDF
ggsave("/Users/diandra/rlp_meta/results/plots/ligand_receptor_pairs2.pdf", width = 8, height = 6, units = "in")

#at least 3 counts--------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

# Read the xlsx output file
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldbfull.xlsx")

# Filter rows with count >= 2
results_distinct_filtered <- results_distinct %>%
  filter(count >= 3)

# Count the total number of pairs for each file
file_pairs <- results_distinct_filtered %>%
  mutate(file = stringr::str_split(file, "; ")) %>%
  tidyr::unnest(file) %>%
  group_by(file) %>%
  summarise(total_pairs = n())

# Calculate the total number of pairs in the filtered input file
total_pairs_in_file <- nrow(results_distinct_filtered)

# Assign a number and color to each file
file_pairs <- file_pairs %>%
  mutate(file_num = reorder(file, total_pairs)) %>%
  mutate(file_color = as.numeric(factor(file)))

#cut file names
file_pairs$file_num_truncated <- sub("([0-9]{2}).*$", "\\1", file_pairs$file)

# Reorder the file_num_truncated variable
file_pairs <- file_pairs %>%
  mutate(file_num_truncated = reorder(file_num_truncated, total_pairs))

# Create the plot
p <- ggplot(file_pairs, aes(x = file_num_truncated, y = total_pairs, fill = factor(file_num_truncated))) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = comma(total_pairs)), vjust = -0.5, size = 3) + 
  labs(x = "DB files", y = "Total Number of lrps") + 
  ggtitle("Ligand-receptor pairs in each dataset (count >= 3)") + 
  theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) + 
  scale_y_continuous(labels = comma, breaks = seq(0, max(file_pairs$total_pairs) + 1000, by = 500), limits = c(0, max(file_pairs$total_pairs) + 1000)) + 
  annotate("text", x = 1, y = max(file_pairs$total_pairs)+ 1000, label = paste0("Total lrps: ", comma(total_pairs_in_file)), hjust = 0, vjust = 1)

# Print the plot to the Plots pane in RStudio
print(p)

# Save the plot as a PDF
ggsave("/Users/diandra/rlp_meta/results/plots/ligand_receptor_pairs3.pdf", width = 8, height = 6, units = "in")

#input top_ligand2--------------------------------------------------------------
#input:alldb_at_least_2_counts_ligand.xlsx
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

# Read the xlsx output file
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldb_at_least_2_counts_ligand.xlsx")

# Count the total number of pairs for each file
file_pairs <- results_distinct %>%
  mutate(file = stringr::str_split(file, "; ")) %>%
  tidyr::unnest(file) %>%
  group_by(file) %>%
  summarise(total_pairs = n())

# Calculate the total number of pairs in the input file
total_pairs_in_file <- nrow(results_distinct)

# Assign a number and color to each file
file_pairs <- file_pairs %>%
  mutate(file_num = reorder(file, total_pairs)) %>%
  mutate(file_color = as.numeric(factor(file)))

#cut file names
file_pairs$file_num_truncated <- sub("([0-9]{2}).*$", "\\1", file_pairs$file)

# Reorder the file_num_truncated variable
file_pairs <- file_pairs %>%
  mutate(file_num_truncated = reorder(file_num_truncated, total_pairs))

# Create the plot
p <- ggplot(file_pairs, aes(x = file_num_truncated, y = total_pairs, fill = factor(file_num_truncated))) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = comma(total_pairs)), vjust = -0.5, size = 3) + 
  labs(x = "DB files", y = "Total Number of lrps") + 
  ggtitle("Ligand-receptor pairs in each dataset (top2_ligand)") + 
  theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) + 
  scale_y_continuous(labels = comma, breaks = seq(0, max(file_pairs$total_pairs) + 500, by = 500), limits = c(0, max(file_pairs$total_pairs) + 500)) + 
  annotate("text", x = 1, y = max(file_pairs$total_pairs)+ 500, label = paste0("Total lrps: ", comma(total_pairs_in_file)), hjust = 0, vjust = 1)

# Print the plot to the Plots pane in RStudio
print(p)

# Save the plot as a PDF
ggsave("/Users/diandra/rlp_meta/results/plots/lrps_min2_ligand.pdf", width = 8, height = 6, units = "in")

#at least 3 ligand--------------------------------------------------------------
#input:alldb_at_least_3_counts_ligand.xlsx
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

# Read the xlsx output file
results_distinct <- read_xlsx("/Users/diandra/rlp_meta/results/alldb_at_least_3_counts_ligand.xlsx")

# Count the total number of pairs for each file
file_pairs <- results_distinct %>%
  mutate(file = stringr::str_split(file, "; ")) %>%
  tidyr::unnest(file) %>%
  group_by(file) %>%
  summarise(total_pairs = n())

# Calculate the total number of pairs in the input file
total_pairs_in_file <- nrow(results_distinct)

# Assign a number and color to each file
file_pairs <- file_pairs %>%
  mutate(file_num = reorder(file, total_pairs)) %>%
  mutate(file_color = as.numeric(factor(file)))

#cut file names
file_pairs$file_num_truncated <- sub("([0-9]{2}).*$", "\\1", file_pairs$file)

# Reorder the file_num_truncated variable
file_pairs <- file_pairs %>%
  mutate(file_num_truncated = reorder(file_num_truncated, total_pairs))

# Create the plot
p <- ggplot(file_pairs, aes(x = file_num_truncated, y = total_pairs, fill = factor(file_num_truncated))) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = comma(total_pairs)), vjust = -0.5, size = 3) + 
  labs(x = "DB files", y = "Total Number of lrps") + 
  ggtitle("Ligand-receptor pairs in each dataset (top3_ligand)") + 
  theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) + 
  scale_y_continuous(labels = comma, breaks = seq(0, max(file_pairs$total_pairs) + 500, by = 500), limits = c(0, max(file_pairs$total_pairs) + 500)) + 
  annotate("text", x = 1, y = max(file_pairs$total_pairs)+ 500, label = paste0("Total lrps: ", comma(total_pairs_in_file)), hjust = 0, vjust = 1)

# Print the plot to the Plots pane in RStudio
print(p)

# Save the plot as a PDF
ggsave("/Users/diandra/rlp_meta/results/plots/lrps_min3_ligand.pdf", width = 8, height = 6, units = "in")

