#lrps correlation for all the files in clean_matrix

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(writexl)
library(DescTools)
library(fmsb)
library(ggplot2)
library(stats)

# Load data
#top2 lrps with at least a count of 2
database <- read_xlsx("~/rlp_meta/results/alldb_top2_ligand.xlsx")

# Extracted lrps lists from database
l <- as.list(database$ligand) # ligands list
r <- as.list(database$`receptor(s)`) # receptors list

# Create lists to store results
spearman_results <- data.frame(rlp = character(), spearman_corr = character(), 
                               n_li = character(), n_ri = character(), na_li = character(), na_ri = character())
#pearson_results <- data.frame(rlp = character(), pearson_corr = character(), n_li = character(), n_ri = character(), na_li = character(), na_ri = character())
# save spearman_results as csv
output_file_path <- "/Users/diandra/rlp_meta/results/spearman_results_meta.csv" #csv output


# Get a list of files in the folder
m_folder_path <- "~/covid_data/ms_covid19_and_controls/Clean_matrix/try"



# Initialize lists to store lim and rim values
# Loop over each file in the folder
# Create a new matrix to store the file information
file_info_list <- list()
all_lim_values <- list()
all_rim_values <- list()
for (m in dir(m_folder_path, pattern = "*.csv")) {
  m_file_path <- file.path(m_folder_path, m)
  
  # Read the file
  m <- read.csv(m_file_path)
  colnames(m)[1] <- "Protein"
  # Remove rows with NA values in the Protein column
  m <- m[!is.na(m$Protein), ]
  
  # Initialize empty lists to store lim and rim
  lim <- list()
  rim <- list()
  # Loop over each ligand-receptor pair
  for (i in 1:length(l)) {
    lii = l[i]
    rii = r[i]
    
    # Test li and ri in m
    if (lii %in% m$Protein & rii %in% m$Protein) {
      li_row <- m[m$Protein == lii, ]
      ri_row <- m[m$Protein == rii, ]
      lim <- c(lim, list(li_row))
      rim <- c(rim, list(ri_row))
      # Create a new row for file_info with correct column names
      new_row <- data.frame(ligand = lii, receptor = rii, file = basename(m_file_path), stringsAsFactors = FALSE)
      # Append the new row to file_info_list
      file_info_list <- c(file_info_list, list(new_row))
    }
  }
  
  # Check if lim or rim is empty
  if (length(lim) > 0) {
    # Create lim matrix
    lim_values <- do.call(rbind, lapply(lim, function(x) as.numeric(x[, -1])))
    rownames(lim_values) <- sapply(lim, function(x) x$Protein)
    colnames(lim_values) <- 1:ncol(lim_values)
    
    # Add lim values to the list
    all_lim_values <- c(all_lim_values, list(lim_values))
  }
  
  if (length(rim) > 0) {
    # Create rim matrix
    rim_values <- do.call(rbind, lapply(rim, function(x) as.numeric(x[, -1])))
    rownames(rim_values) <- sapply(rim, function(x) x$Protein)
    colnames(rim_values) <- 1:ncol(rim_values)
    
    # Add rim values to the list
    all_rim_values <- c(all_rim_values, list(rim_values))
  }
}
# Combine files for the same ligand-receptor pair
file_info_matrix <- do.call(rbind, lapply(file_info_list, unlist))
colnames(file_info_matrix) <- c("ligand", "receptor", "file")
file_info_matrix <- cbind(interaction = paste(file_info_matrix[, 1], file_info_matrix[, 2], sep = "_"), file_info_matrix)


# Find the maximum number of columns for lim values
max_cols_lim <- max(sapply(all_lim_values, ncol))
# Pad the lim matrices with NA values to match the maximum number of columns
all_lim_values <- lapply(all_lim_values, function(x) {
  if (ncol(x) < max_cols_lim) {
    cbind(x, matrix(NA, nrow = nrow(x), ncol = max_cols_lim - ncol(x)))
  } else {
    x
  }
})
# Combine all lim values into a single matrix
lim_values <- do.call(rbind, all_lim_values)
# Remove the.number suffix from the row names
#rownames(lim_values) <- sub("\\.[0-9]+$", "", rownames(lim_values))

# Find the maximum number of columns for rim values
max_cols_rim <- max(sapply(all_rim_values, ncol))
# Pad the rim matrices with NA values to match the maximum number of columns
all_rim_values <- lapply(all_rim_values, function(x) {
  if (ncol(x) < max_cols_rim) {
    cbind(x, matrix(NA, nrow = nrow(x), ncol = max_cols_rim - ncol(x)))
  } else {
    x
  }
})
# Combine all rim values into a single matrix
rim_values <- do.call(rbind, all_rim_values)
# Remove the.number suffix from the row names
#rownames(rim_values) <- sub("\\.[0-9]+$", "", rownames(rim_values))

#n length = sample size
#Minimal optimal sample size  (6?)
na_li=length(lim_values)
na_ri=length(rim_values)


#SPEARMAN correlation-----------------------------------------------------------
#correlation tests:cor.test(x, y, method = “spearman”)
#r=0; there is no relation between the variable. r=+1; perfectly positively correlated.
#r=-1; perfectly negatively correlated. r= 0 to 0.30; negligible correlation.
#r=0.30 to 0.50; moderate correlation. r=0.50 to 1 highly correlated.
#Parameters: x, y: numeric vectors with the same length
#rs takes a value between -1 (negative association) and 1 (positive association).
#rs = 0 means there is no association.
#If association is monotonically increasing then rs = 1.
#If association is monotonically decreasing then rs = -1.
#It can be used when association is nonlinear.It can be applied for ordinal variables.
#spearman_corr <- cor.test(lim_values, rim_values, method = 'spearman')
spearman_corrs <- matrix(NA, nrow(lim_values), 5)
rownames(spearman_corrs) <- paste(rownames(lim_values), rownames(rim_values), sep = "_")
colnames(spearman_corrs) <- c("Tot_correlation", "p-value", "CI_lower", "CI_upper", "Tot_sample_size")

for (i in 1:nrow(lim_values)) {
  li_values <- lim_values[i,, drop = FALSE]
  ri_values <- rim_values[i,, drop = FALSE]
  
  # Calculate total correlation
  ok_tot_li <-!is.na(li_values)
  ok_tot_ri <-!is.na(ri_values)
  ok_tot <- ok_tot_li & ok_tot_ri
  if (sum(ok_tot) >= 6) {
    corr_test <- cor.test(li_values[ok_tot], ri_values[ok_tot], method = "spearman")
    corr_test2 <- spearman.ci.sas(li_values[ok_tot], ri_values[ok_tot], adj.bias=TRUE, conf.level=0.95)
    spearman_corr <- corr_test$estimate
    p_value <- corr_test$p.value
    tot_sample_size <- min(length (li_values[ok_tot]), length (ri_values[ok_tot]))
    ci_lower <- corr_test2$rho.ll
    ci_upper <- corr_test2$rho.ul
  } else {
    spearman_corr <- NA
    p_value <- NA
    tot_sample_size <- 0
    ci_lower <- NA
    ci_upper <- NA
  }
  
  # Store the results
  spearman_corrs[i, 1] <- spearman_corr
  spearman_corrs[i, 2] <- p_value
  spearman_corrs[i, 3] <- ci_lower
  spearman_corrs[i, 4] <- ci_upper
  spearman_corrs[i, 5] <- tot_sample_size
}

#results------------------------------------------------------------------------
#Store as result list in the beginning
# Create spearman_results
spearman_results <- data.frame(lrp = rownames(spearman_corrs), 
                               spearman_corr = spearman_corrs[, 1], 
                               p_value = spearman_corrs[, 2], 
                               adjusted_p_value = p.adjust(spearman_corrs[, 2], method = "fdr", n = length(spearman_corrs[, 2])),
                               CI_lower = spearman_corrs[, 3], 
                               CI_upper = spearman_corrs[, 4], 
                               Tot_sample_size = spearman_corrs[, 5],
                               file=file_info_matrix[,4])
rownames(spearman_results) <- 1:nrow(spearman_results)
# Remove rows with missing values
removed_rows <- spearman_results %>%
  filter(lrp < min(lrp, na.rm = TRUE) | lrp > max(lrp, na.rm = TRUE) |
           spearman_corr < min(spearman_corr, na.rm = TRUE) | spearman_corr > max(spearman_corr, na.rm = TRUE) |
           is.na(lrp) | is.na(spearman_corr) | is.na(adjusted_p_value))
# Print the removed rows
print(removed_rows)
spearman_results <- spearman_results %>%
  filter(!is.na(lrp) &!is.na(spearman_corr) &!is.na(adjusted_p_value))
write.csv(spearman_results, output_file_path, row.names = FALSE)

#spearman plots--------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
#BiocManager::install("ggrepel")
library(ggrepel)

#plot only spearman significant adjusted p-values 
filtered_results <- subset(spearman_results, adjusted_p_value <= 0.05)
# Remove duplicates, keeping the row with the highest adjusted_p_value
filtered_results <- filtered_results[!duplicated(filtered_results$lrp, fromLast = TRUE) |
                                       duplicated(filtered_results$lrp) & 
                                       filtered_results$adjusted_p_value == 
                                       ave(filtered_results$adjusted_p_value, 
                                           filtered_results$lrp, FUN = max), ]
filtered_results <- filtered_results[order(filtered_results$adjusted_p_value, decreasing = TRUE), ]
filtered_results <- filtered_results[!duplicated(filtered_results$lrp, fromLast = TRUE), ]

# Calculate the number of significant and non-significant results
n_significant <- sum(spearman_results$adjusted_p_value <= 0.05, na.rm = TRUE)
n_not_significant <- sum(spearman_results$adjusted_p_value > 0.05, na.rm = TRUE)

# Create the volcano plot
ggplot(spearman_results, aes(x = spearman_corr, y = -log10(adjusted_p_value))) +
  geom_point(aes(color = interaction(factor(adjusted_p_value <= 0.05), factor(spearman_corr > 0)))) +
  scale_color_manual(values = c("TRUE.TRUE" = "orange2", "TRUE.FALSE" = "dodgerblue3", "FALSE.TRUE" = "orange", "FALSE.FALSE" = "dodgerblue1"),
                     breaks = c("TRUE.TRUE", "TRUE.FALSE", "FALSE.TRUE", "FALSE.FALSE"),
                     labels = c(paste("<=0.05, + Corr (n = ", sum(spearman_results$adjusted_p_value <= 0.05 & spearman_results$spearman_corr > 0, na.rm = TRUE), ")", sep = ""),
                                paste("<=0.05, - Corr (n = ", sum(spearman_results$adjusted_p_value <= 0.05 & spearman_results$spearman_corr < 0, na.rm = TRUE), ")", sep = ""),
                                paste(">0.05, + Corr (n = ", sum(spearman_results$adjusted_p_value > 0.05 & spearman_results$spearman_corr > 0, na.rm = TRUE), ")", sep = ""),
                                paste(">0.05, - Corr (n = ", sum(spearman_results$adjusted_p_value > 0.05 & spearman_results$spearman_corr < 0, na.rm = TRUE), ")", sep = ""))) +
  labs(x = "Spearman Correlation", y = "-log10(Adjusted p-value)", color = "p-value & Correlation") +
  theme_minimal() +
  ggtitle("LRPs Spearman correlation") +
  geom_text_repel(aes(label = lrp), 
                  data = filtered_results, 
                  min.segment.length = unit(0.1, "lines"), 
                  segment.color = "gray", 
                  max.overlaps = 55, 
                  size = 2.5)+
  scale_y_continuous(breaks = c(0, 5, 10, 15, 50, 70), limits = c(0, 71))+
  geom_hline(yintercept = -log10(0.05), linetype = "dotted", color = "red")
ggsave("/Users/diandra/rlp_meta/results/plots/LRPs_Spearman_correlation_volcano_meta.pdf", width = 8, height = 6, units = "in")


#basic plot
# Create a scatter plot of the filtered results
ggplot(filtered_results, aes(x = lrp, y = spearman_corr)) +
  geom_point(color = "red") +
  labs(x = "LRPs", y = "Spearman Correlation") +
  ggtitle("LRPs Spearman correlation with adjusted p-value <=0.05")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("/Users/diandra/rlp_meta/results/plots/LRPs_Spearman_correlation_adjusted_pvalue0.05_meta.pdf", width = 8, height = 6, units = "in")



#babacic=25
#+ahern=25
#+byeon=27 -> tln1_itgb3
#+ciccosanti=30
#+di_2020=30
#+feng=29 -LBP_CD14
#+geyer=34 +LBP_CD14
#+messd=35
#+messv=36
#+moha=37
#+over=39
#+sahin=39
#+sahme=39
#+shen=45
#+shu_D=49
#+shuv=52
#+spick=57
#+sullivan=64
#+tep=64
#+zhang=65
#+suvarna=63