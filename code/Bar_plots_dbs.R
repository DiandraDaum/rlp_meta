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
  mutate(file_num = as.numeric(factor(file))) %>%
  arrange(file_num) %>%
  mutate(file_color = as.numeric(factor(file)))

# Create and save a bar plot
p<- ggplot(file_pairs, aes(x = file_num, y = total_pairs, fill = factor(file_num))) + 
    geom_bar(stat = "identity") + 
    geom_text(aes(label = comma(total_pairs)), vjust = -0.5, size = 3) + 
    labs(x = "DB files", y = "Total Number of lrps") + 
    ggtitle("Ligand-receptor pairs in each dataset") + 
    theme_classic() + 
    scale_fill_discrete(name = "File Name", labels = paste0(file_pairs$file, " (", file_pairs$file_num, ")")) + 
    scale_y_continuous(labels = comma, breaks = seq(0, total_pairs_in_file, by = 2000), limits = c(0, total_pairs_in_file)) + 
    annotate("text", x = mean(file_pairs$file_num) + 0.5, y = total_pairs_in_file, label = paste0("Total lrps: ", comma(total_pairs_in_file)), hjust = 0, vjust = 1)

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
  mutate(file_num = as.numeric(factor(file))) %>%
  arrange(file_num) %>%
  mutate(file_color = as.numeric(factor(file)))

# Create and save a bar plot
p <- ggplot(file_pairs, aes(x = file_num, y = total_pairs, fill = factor(file_num))) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = comma(total_pairs)), vjust = -0.5, size = 3) + 
  labs(x = "DB files", y = "Total Number of lrps") + 
  ggtitle("Ligand-receptor pairs in each dataset (count >= 2)") + 
  theme_classic() + 
  scale_fill_discrete(name = "File Name", labels = paste0(file_pairs$file, " (", file_pairs$file_num, ")")) + 
  scale_y_continuous(labels = comma, breaks = seq(0, total_pairs_in_file, by = 1000), limits = c(0, total_pairs_in_file)) + 
  annotate("text", x = mean(file_pairs$file_num) + 0.5, y = total_pairs_in_file, label = paste0("Total lrps: ", comma(total_pairs_in_file)), hjust = 0, vjust = 1)

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
  mutate(file_num = as.numeric(factor(file))) %>%
  arrange(file_num) %>%
  mutate(file_color = as.numeric(factor(file)))

# Create and save a bar plot
p <- ggplot(file_pairs, aes(x = file_num, y = total_pairs, fill = factor(file_num))) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = comma(total_pairs)), vjust = -0.5, size = 3) + 
  labs(x = "DB files", y = "Total Number of lrps") + 
  ggtitle("Ligand-receptor pairs in each dataset (count >= 3)") + 
  theme_classic() + 
  scale_fill_discrete(name = "File Name", labels = paste0(file_pairs$file, " (", file_pairs$file_num, ")")) + 
  scale_y_continuous(labels = comma, breaks = seq(0, total_pairs_in_file, by = 500), limits = c(0, total_pairs_in_file)) + 
  annotate("text", x = mean(file_pairs$file_num) + 0.5, y = total_pairs_in_file, label = paste0("Total lrps: ", comma(total_pairs_in_file)), hjust = 0, vjust = 1)

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
  mutate(file_num = as.numeric(factor(file))) %>%
  arrange(file_num) %>%
  mutate(file_color = as.numeric(factor(file)))

# Create and save a bar plot
p<- ggplot(file_pairs, aes(x = file_num, y = total_pairs, fill = factor(file_num))) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = comma(total_pairs)), vjust = -0.5, size = 3) + 
  labs(x = "DB files", y = "Total Number of lrps") + 
  ggtitle("Best receptor for each ligand (min count 2)") + 
  theme_classic() + 
  scale_fill_discrete(name = "File Name", labels = paste0(file_pairs$file, " (", file_pairs$file_num, ")")) + 
  scale_y_continuous(labels = comma, breaks = seq(0, total_pairs_in_file, by = 500), limits = c(0, total_pairs_in_file)) + 
  annotate("text", x = mean(file_pairs$file_num) + 0.5, y = total_pairs_in_file, label = paste0("Total lrps: ", comma(total_pairs_in_file)), hjust = 0, vjust = 1)

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
  mutate(file_num = as.numeric(factor(file))) %>%
  arrange(file_num) %>%
  mutate(file_color = as.numeric(factor(file)))

# Create and save a bar plot
p<- ggplot(file_pairs, aes(x = file_num, y = total_pairs, fill = factor(file_num))) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = comma(total_pairs)), vjust = -0.5, size = 3) + 
  labs(x = "DB files", y = "Total Number of lrps") + 
  ggtitle("Best receptor for each ligand (min count 3)") + 
  theme_classic() + 
  scale_fill_discrete(name = "File Name", labels = paste0(file_pairs$file, " (", file_pairs$file_num, ")")) + 
  scale_y_continuous(labels = comma, breaks = seq(0, total_pairs_in_file, by = 250), limits = c(0, total_pairs_in_file)) + 
  annotate("text", x = mean(file_pairs$file_num) + 0.5, y = total_pairs_in_file, label = paste0("Total lrps: ", comma(total_pairs_in_file)), hjust = 0, vjust = 1)

# Print the plot to the Plots pane in RStudio
print(p)

# Save the plot as a PDF
ggsave("/Users/diandra/rlp_meta/results/plots/lrps_min3_ligand.pdf", width = 8, height = 6, units = "in")

