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
lrps <- as.list(database$interaction) #lrps list

#results lists/vectors = 6 parameters (pair name A_B, correlation, n_li, n_ri, na_li, na_ri)
spearman_results <- data.frame(rlp = character(), spearman_corr = character(), 
                               n_li = character(), n_ri = character(), na_li = character(), na_ri = character())
pearson_results <- data.frame(rlp = character(), pearson_corr = character(), 
                              n_li = character(), n_ri = character(), na_li = character(), na_ri = character())

#extract lrps 
ri <- list() # found receptor list
li <- list() # found ligand list
for (i in 1:length(l)) {
  lii = l[i]
  rii = r[i]
  # test li and ri in m
  # add warning if receptor/ligand only was found but not the other one
  if (lii %in% m$Protein & rii %in% m$Protein) {
    ri <- c(ri, list(rii))
    li <- c(li, list(lii))
  } else if (lii %in% m$Protein & !(rii %in% m$Protein)) {
    print(paste("ligand", lii, "in m but receptor", rii, "not in m"))
  } else if (rii %in% m$Protein & !(lii %in% m$Protein)) {
    print(paste("receptor", rii, "in m but ligand", lii, "not in m"))
  } else {
    print(paste("ligand", lii, "and receptor", rii, "not in m"))
  }
}


#n length = sample size
#Minimal optimal sample size  (6?)
n_li=length(li)
n_ri=length(ri)

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
#It can be used when association is nonlinear.
#It can be applied for ordinal variables.
spearman <- cor.test(li, ri, method = 'spearman')
  
  
#PEARSON correlation
#example: corr.one <- cor(data$x[data$category=="One"], data$y[data$category=="One"], method = 'pearson')
pearson <- cor.test(x, y, method = 'pearson')



#extract results
#cor.res$estimate|pi value|confidence intervals|degrees of freedom


  
#Store as result list in the beginning
spearman_results <- 
pearson_results <- 
  

