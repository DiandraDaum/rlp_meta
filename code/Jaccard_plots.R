#Jaccard plots with results data
#see overlap of the used datasets

## Script to generate the comparison data presented in the Manuscript
library(tidyverse)
library(magrittr)
#library(liana)
library(RColorBrewer)
library(pheatmap)
library(proxy)
library(UpSetR)
library(grid)
#BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)
library(patchwork)
library(ggplotify)
