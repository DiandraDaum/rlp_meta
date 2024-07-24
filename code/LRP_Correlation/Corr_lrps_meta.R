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
database <- read_xlsx("~/rlp_meta/results/alldb_at_least_2_counts_ligand.xlsx")

# Extracted lrps lists from database
l <- as.list(database$ligand) # ligands list
r <- as.list(database$`receptor(s)`) # receptors list

# Create lists to store results
spearman_results <- data.frame(rlp = character(), spearman_corr = character(), 
                               n_li = character(), n_ri = character(), na_li = character(), na_ri = character())
pearson_results <- data.frame(rlp = character(), pearson_corr = character(), 
                              n_li = character(), n_ri = character(), na_li = character(), na_ri = character())

# Get a list of files in the folder
m_folder_path <- "~/covid_data/ms_covid19_and_controls/Clean_matrix"
files <- dir(m_folder_path, pattern = "*.csv|*.txt|*.tsv|*.xlsx")

# Initialize lists to store lim and rim values
# Initialize empty lists to store lim and rim values
all_lim_values <- list()
all_rim_values <- list()

# Loop over each file in the folder
for (m in dir(m_folder_path, pattern = "*.csv")) {
  m_file_path <- file.path(m_folder_path, m)
  
  # Read the file
  m <- read_csv(m_file_path, show_col_types = FALSE)
  
  # Remove rows with NA values in the Protein column
  m <- m[!is.na(m$Protein), ]
  
  # Loop over each ligand-receptor pair
  for (i in 1:length(l)) {
    lii = l[i]
    rii = r[i]
    
    # Initialize empty lists to store lim and rim
    lim <- list()
    rim <- list()
    
    # Test li and ri in m
    if (lii %in% m$Protein & rii %in% m$Protein) {
      li_row <- m[m$Protein == lii, ]
      ri_row <- m[m$Protein == rii, ]
      lim <- c(lim, list(li_row))
      rim <- c(rim, list(ri_row))
    }
    
    # Check if lim or rim is empty
    if (length(lim) > 0) {
      # Create lim matrix
      lim_values <- do.call(rbind, lapply(lim, function(x) as.matrix(x[, -1])))
      rownames(lim_values) <- sapply(lim, function(x) x$Protein)
      colnames(lim_values) <- 1:ncol(lim_values)
      
      # Add lim values to the list with protein name
      all_lim_values <- c(all_lim_values, list(setNames(list(lim_values), lii)))
    }
    
    if (length(rim) > 0) {
      # Create rim matrix
      rim_values <- do.call(rbind, lapply(rim, function(x) as.matrix(x[, -1])))
      rownames(rim_values) <- sapply(rim, function(x) x$Protein)
      colnames(rim_values) <- 1:ncol(rim_values)
      
      # Add rim values to the list with protein name
      all_rim_values <- c(all_rim_values, list(setNames(list(rim_values), rii)))
    }
  }
}
# Combine all lim and rim values into single matrices
lim_values <- do.call(rbind, all_lim_values)
rim_values <- do.call(rbind, all_rim_values)

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
spearman_corrs <- matrix(NA, nrow(lim_values), 7)
rownames(spearman_corrs) <- paste(rownames(lim_values), rownames(rim_values), sep = "_")
colnames(spearman_corrs) <- c("Tot_correlation", "p-value", "CI_lower", "CI_upper", "Tot_sample_size", "X_correlation", "H_correlation")

for (i in 1:nrow(lim_values)) {
  li_values <- lim_values[i,, drop = FALSE]
  ri_values <- rim_values[i,, drop = FALSE]
  
  # Calculate total correlation
  ok_tot_li <-!is.na(li_values)
  ok_tot_ri <-!is.na(ri_values)
  ok_tot <- ok_tot_li & ok_tot_ri
  if (any(ok_tot)) {
    corr_test <- cor.test(li_values[ok_tot], ri_values[ok_tot], method = "spearman")
    corr_test2 <- spearman.ci.sas(li_values[ok_tot], ri_values[ok_tot], adj.bias=TRUE, conf.level=0.95)
    spearman_corr <- corr_test$estimate
    p_value <- corr_test$p.value
    tot_sample_size <- sum(ok_tot)
    ci_lower <- corr_test2$rho.ll
    ci_upper <- corr_test2$rho.ul
  } else {
    spearman_corr <- NA
    p_value <- NA
    tot_sample_size <- 0
    ci_lower <- NA
    ci_upper <- NA
  }
  
  # Calculate correlation for columns starting with "X"
  x_li_values <- li_values[, grep("^X", colnames(li_values))]
  x_ri_values <- ri_values[, grep("^X", colnames(ri_values))]
  ok_x_li <-!is.na(x_li_values)
  ok_x_ri <-!is.na(x_ri_values)
  ok_x <- ok_x_li & ok_x_ri
  if (any(ok_x)) {
    x_corr <- cor(x_li_values[ok_x], x_ri_values[ok_x], method = "spearman")
  } else {
    x_corr <- NA
  }
  
  # Calculate correlation for columns starting with "H"
  h_li_values <- li_values[, grep("^H", colnames(li_values))]
  h_ri_values <- ri_values[, grep("^H", colnames(ri_values))]
  ok_h_li <-!is.na(h_li_values)
  ok_h_ri <-!is.na(h_ri_values)
  ok_h <- ok_h_li & ok_h_ri
  if (any(ok_h)) {
    h_corr <- cor(h_li_values[ok_h], h_ri_values[ok_h], method = "spearman")
  } else {
    h_corr <- NA
  }
  
  # Store the results
  spearman_corrs[i, 1] <- spearman_corr
  spearman_corrs[i, 2] <- p_value
  spearman_corrs[i, 3] <- ci_lower
  spearman_corrs[i, 4] <- ci_upper
  spearman_corrs[i, 5] <- tot_sample_size
  spearman_corrs[i, 6] <- x_corr
  spearman_corrs[i, 7] <- h_corr
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
                               Tot_sample_size = spearman_corrs[, 5])
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


#spearman plots--------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
#BiocManager::install("ggrepel")
library(ggrepel)

#plot only spearman significant adjusted p-values 
filtered_results <- subset(spearman_results, adjusted_p_value <= 0.05)

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
                  data = spearman_results %>% filter(adjusted_p_value <= 0.05), 
                  min.segment.length = unit(0.1, "lines"), 
                  segment.color = "gray", 
                  max.overlaps = 16, 
                  size = 2.5)
ggsave("/Users/diandra/rlp_meta/results/plots/LRPs_Spearman_correlation_volcano_covid19.pdf", width = 8, height = 6, units = "in")




