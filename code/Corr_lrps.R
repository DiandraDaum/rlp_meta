#code to calculate correlation between lrps

#load useful libraries
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(writexl)

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
spearman_corrs <- matrix(NA, nrow(lim_values), 5)
rownames(spearman_corrs) <- paste(rownames(lim_values), rownames(rim_values), sep = "_")
colnames(spearman_corrs) <- c("Tot_correlation", "p-value", "Tot_sample_size", "X_correlation", "H_correlation")

for (i in 1:nrow(lim_values)) {
  li_values <- lim_values[i,, drop = FALSE]
  ri_values <- rim_values[i,, drop = FALSE]
  
  # Calculate total correlation
  ok_tot_li <- !is.na(li_values)
  ok_tot_ri <- !is.na(ri_values)
  ok_tot <- ok_tot_li & ok_tot_ri
  if (any(ok_tot)) {
    corr_test <- cor.test(li_values[ok_tot], ri_values[ok_tot], method = "spearman")
    spearman_corr <- corr_test$estimate
    p_value <- corr_test$p.value
    tot_sample_size <- sum(ok_tot)
  } else {
    spearman_corr <- NA
    p_value <- NA
    tot_sample_size <- 0
  }
  
  # Calculate correlation for columns starting with "X"
  x_li_values <- li_values[, grep("^X", colnames(li_values))]
  x_ri_values <- ri_values[, grep("^X", colnames(ri_values))]
  ok_x_li <- !is.na(x_li_values)
  ok_x_ri <- !is.na(x_ri_values)
  ok_x <- ok_x_li & ok_x_ri
  if (any(ok_x)) {
    x_corr <- cor(x_li_values[ok_x], x_ri_values[ok_x], method = "spearman")
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
    h_corr <- cor(h_li_values[ok_h], h_ri_values[ok_h], method = "spearman")
  } else {
    h_corr <- NA
  }
  
  # Store the results
  spearman_corrs[i, 1] <- spearman_corr
  spearman_corrs[i, 2] <- p_value
  spearman_corrs[i, 3] <- tot_sample_size
  spearman_corrs[i, 4] <- x_corr
  spearman_corrs[i, 5] <- h_corr
}
#PEARSON correlation
#example: corr.one <- cor(data$x[data$category=="One"], data$y[data$category=="One"], method = 'pearson')
#pearson_corr <- cor.test(lim, rim, method = 'pearson')
pearson_corrs <- matrix(NA, nrow(lim_values), 2)
rownames(pearson_corrs) <- paste(rownames(lim_values), rownames(rim_values), sep = "_")
colnames(pearson_corrs) <- c("X_correlation", "H_correlation")

for (i in 1:nrow(lim_values)) {
  for (j in 1:nrow(rim_values)) {
    li_values <- lim_values[i, , drop = FALSE]
    ri_values <- rim_values[j, , drop = FALSE]
    ok_x <- complete.cases(li_values[, grep("^X", colnames(li_values))]) & complete.cases(ri_values[, grep("^X", colnames(ri_values))])
    ok_h <- complete.cases(li_values[, grep("^H", colnames(li_values))]) & complete.cases(ri_values[, grep("^H", colnames(ri_values))])
    if (any(ok_x)) {
      pearson_corrs[i, 1] <- cor(li_values[, grep("^X", colnames(li_values))][ok_x], ri_values[, grep("^X", colnames(ri_values))][ok_x], method = "pearson")
    } else {
      pearson_corrs[i, 1] <- NA
    }
    if (any(ok_h)) {
      pearson_corrs[i, 2] <- cor(li_values[, grep("^H", colnames(li_values))][ok_h], ri_values[, grep("^H", colnames(ri_values))][ok_h], method = "pearson")
    } else {
      pearson_corrs[i, 2] <- NA
    }
  }
}


#extract results
#cor.res$estimate|pi value|confidence intervals|degrees of freedom


  
#Store as result list in the beginning
spearman_results <- 
pearson_results <- 
  

