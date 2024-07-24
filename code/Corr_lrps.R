#code to calculate correlation between lrps

#load useful libraries
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(writexl)
#for CI for spearman: install.packages("DescTools") library(DescTools)
#install.packages("fmsb")
library(fmsb)
library(ggplot2)
library(stats)

#read database
database <- read_xlsx("~/rlp_meta/results/alldb_at_least_2_counts_ligand.xlsx")

#read proteomics matrix
#dataset m = proteomics matrix: proteins rows, sample s columns. 
m <- read.csv("~/covid_data/ms_covid19_and_controls/babacic_2023_nat_comms_matrix.csv")
colnames(m)[1] <- "Protein"

#extracted lrps lists from database
l <- as.list(database$ligand) #ligands list
r <- as.list(database$`receptor(s)`) #receptors list
#lrps <- as.list(database$interaction) #lrps list

#results lists/vectors = 6 parameters (pair name A_B, correlation, n_li, n_ri, na_li, na_ri)
spearman_results <- data.frame(rlp = character(), spearman_corr = character(), 
                               n_li = character(), n_ri = character(), na_li = character(), na_ri = character())
pearson_results <- data.frame(rlp = character(), pearson_corr = character(), 
                              n_li = character(), n_ri = character(), na_li = character(), na_ri = character())

#extract lrps 
lim <- list() # found ligand list plus matrix values
rim <- list() # found receptor list plus matrix values
for (i in 1:length(l)) {
  lii = l[i]
  rii = r[i]
  # test li and ri in m
  # add warning if receptor/ligand only was found but not the other one
  if (lii %in% m$Protein & rii %in% m$Protein) {
    li_row <- m[m$Protein == lii, ]
    ri_row <- m[m$Protein == rii, ]
    lim <- c(lim, list(li_row))
    rim <- c(rim, list(ri_row))
  } else if (lii %in% m$Protein &!(rii %in% m$Protein)) {
    print(paste("ligand", lii, "in m but receptor", rii, "not in matrix"))
  } else if (rii %in% m$Protein &!(lii %in% m$Protein)) {
    print(paste("receptor", rii, "in m but ligand", lii, "not in matrix"))
  } else {
    print(paste("ligand", lii, "and receptor", rii, "not in matrix"))
  }
}


# Extract the numeric elements from each inner list
lim_values <- do.call(rbind, lapply(lim, function(x) as.numeric(x[-1])))
rim_values <- do.call(rbind, lapply(rim, function(x) as.numeric(x[-1])))

# Set the row names to the protein names
rownames(lim_values) <- sapply(lim, function(x) x$Protein)
rownames(rim_values) <- sapply(rim, function(x) x$Protein)

# Set the column names to the sample names
colnames(lim_values) <- colnames(m)[2:ncol(m)]
colnames(rim_values) <- colnames(m)[2:ncol(m)]

#n length = sample size
#Minimal optimal sample size  (6?)
n_li=length(lim)
n_ri=length(rim)


#n length = sample size
#Minimal optimal sample size  (6?)
na_li=length(lim_values)
na_ri=length(rim_values)

#correlation tests:
#r=0; there is no relation between the variable. r=+1; perfectly positively correlated.
#r=-1; perfectly negatively correlated. r= 0 to 0.30; negligible correlation.
#r=0.30 to 0.50; moderate correlation. r=0.50 to 1 highly correlated.
#SPEARMAN correlation: cor.test(x, y, method = “spearman”)
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
#PEARSON correlation
#example: corr.one <- cor(data$x[data$category=="One"], data$y[data$category=="One"], method = 'pearson')
#pearson_corr <- cor.test(lim, rim, method = 'pearson')
pearson_corrs <- matrix(NA, nrow(lim_values), 8)
rownames(pearson_corrs) <- paste(rownames(lim_values), rownames(rim_values), sep = "_")
colnames(pearson_corrs) <- c("Tot_correlation", "p-value", "CI_lower", "CI_upper", "Degree of freedom", "Tot_sample_size", "X_correlation", "H_correlation")

for (i in 1:nrow(lim_values)) {
  li_values <- lim_values[i,, drop = FALSE]
  ri_values <- rim_values[i,, drop = FALSE]
  
  # Calculate total correlation
  ok_tot_li <- !is.na(li_values)
  ok_tot_ri <- !is.na(ri_values)
  ok_tot <- ok_tot_li & ok_tot_ri
  if (any(ok_tot)) {
    corr_test <- cor.test(li_values[ok_tot], ri_values[ok_tot], method = "pearson")
    pearson_corr <- corr_test$estimate
    p_value <- corr_test$p.value
    tot_sample_size <- sum(ok_tot)
    ci <- corr_test$conf.int
    ci_lower <- ci[1]
    ci_upper <- ci[2]
    df <- corr_test$parameter
  } else {
    pearson_corr <- NA
    p_value <- NA
    tot_sample_size <- 0
    ci_lower <- NA
    ci_upper <- NA
    df <- NA
  }
  
  # Calculate correlation for columns starting with "X"
  x_li_values <- li_values[, grep("^X", colnames(li_values))]
  x_ri_values <- ri_values[, grep("^X", colnames(ri_values))]
  ok_x_li <- !is.na(x_li_values)
  ok_x_ri <- !is.na(x_ri_values)
  ok_x <- ok_x_li & ok_x_ri
  if (any(ok_x)) {
    x_corr <- cor(x_li_values[ok_x], x_ri_values[ok_x], method = "pearson")
  } else {
    x_corr <- NA
  }
  
  # Calculate correlation for columns starting with "H"
  h_li_values <- li_values[, grep("^H", colnames(li_values))]
  h_ri_values <- ri_values[, grep("^H", colnames(ri_values))]
  ok_h_li <- !is.na(h_li_values)
  ok_h_ri <- !is.na(h_ri_values)
  ok_h <- ok_h_li & ok_h_ri
  if (any(ok_h)) {
    h_corr <- cor(h_li_values[ok_h], h_ri_values[ok_h], method = "pearson")
  } else {
    h_corr <- NA
  }
  
  # Store the results
  pearson_corrs[i, 1] <- pearson_corr
  pearson_corrs[i, 2] <- p_value
  pearson_corrs[i, 3] <- ci_lower
  pearson_corrs[i, 4] <- ci_upper
  pearson_corrs[i, 5] <- df
  pearson_corrs[i, 6] <- tot_sample_size
  pearson_corrs[i, 7] <- x_corr
  pearson_corrs[i, 8] <- h_corr
}

  
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

# Create pearson_results
pearson_results <- data.frame(lrp = rownames(pearson_corrs), 
                              pearson_corr = pearson_corrs[, 1], 
                              p_value = pearson_corrs[, 2],
                              adjusted_p_value= p.adjust(pearson_corrs[, 2], method = "fdr", n = length(pearson_corrs[, 2])),
                              CI_lower = pearson_corrs[, 3], 
                              CI_upper = pearson_corrs[, 4], 
                              Degree_of_freedom = pearson_corrs[, 5], 
                              Tot_sample_size = pearson_corrs[, 6])
rownames(pearson_results) <- 1:nrow(pearson_results)



#plot only spearman significant adjusted p-values 
filtered_results <- subset(spearman_results, adjusted_p_value <= 0.05)
# Create a scatter plot of the filtered results
ggplot(filtered_results, aes(x = lrp, y = spearman_corr, color = factor(adjusted_p_value <= 0.05))) +
  geom_point() +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray")) +
  labs(x = "LRP", y = "Spearman Correlation", color = "Significant P-Value") +
  theme_minimal()

#plot spearman results with both significant and non adjusted p-values
library(tidyverse)
library(ggplot2)
spearman_results <- spearman_results %>% drop_na(lrp, spearman_corr, adjusted_p_value)
n_significant <- sum(spearman_results$adjusted_p_value <= 0.05, na.rm = TRUE)
n_not_significant <- sum(spearman_results$adjusted_p_value > 0.05, na.rm = TRUE)
ggplot(spearman_results, aes(x = lrp, y = spearman_corr, color = factor(adjusted_p_value <= 0.05))) +
  geom_point() +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray"),
                     breaks = c("TRUE", "FALSE"),
                     labels = c(paste("Significant (n =", n_significant, ")", sep = ""),
                                paste("Not Significant (n =", n_not_significant, ")", sep = ""))) +
  labs(x = "LRP", y = "Spearman Correlation", color = "Significant P-Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(labels = ifelse(spearman_results$adjusted_p_value <= 0.05, spearman_results$lrp, ""))

