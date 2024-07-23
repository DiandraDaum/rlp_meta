#code to calculate correlation between lrps

#load useful libraries
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(writexl)

#read database
database <- read_xlsx("~/rlp_meta/results/alldb_top3_ligand.xlsx")

#read proteomics matrix
#dataset m = proteomics matrix: proteins rows, sample s columns. 
m <- read.csv("~/rlp_meta/data/covid_data/ms_covid19_and_controls/babacic_2023_nat_comms_matrix.csv")

#extracted lrps from database
ligand=()
receptors=()
lrp_list=()

#results lists/vectors = 6 parameters (pair name A_B, correlation, n_li, n_ri, na_li, na_ri)
spearman_results <- data.frame(rlp = character(), spearman_corr = character(), 
                               n_li = character(), n_ri = character(), na_li = character(), na_ri = character())
pearson_results <- data.frame(rlp = character(), pearson_corr = character(), 
                              n_li = character(), n_ri = character(), na_li = character(), na_ri = character())

#extracted lrps from database fo lrp_list
for i in 1:length(database): 
  li=l[i, ]
  ri=r[ ,i]
  

#test li and ri in m 
#add warning if receptor/ligand only was found but not the other one

  

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
spearman <- cor.test(x, y, method = “spearman”)
  
  
#PEARSON correlation
#example: corr.one <- cor(data$x[data$category=="One"], data$y[data$category=="One"], method = 'pearson')
pearson <- cor.test(x, y, method = “pearson”)



#extract results
#cor.res$estimate|pi value|confidence intervals|degrees of freedom


#n length = sample size
#Minimal optimal sample size  (6?)
n_li=
n_ri=

  
#Store as result list in the beginning
spearman_results <- 
pearson_results <- 
  

