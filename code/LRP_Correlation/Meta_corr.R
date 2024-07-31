#meta analysis of correlation
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(writexl)
library(DescTools)
library(fmsb)
library(ggplot2)
library(stats)

library(meta)

spearman_results <- read.csv("~/rlp_meta/results/spearman_results_meta.csv")

meta_analysis <- metacor(spearman_results$spearman_corr, spearman_results$Tot_sample_size, title = gs("Meta analysis trial of spearman_results"))

metaanalysis_results <- data.frame(lrp = spearman_results$lrp, 
                                   Tot_sample_size = meta_analysis$n,
                                   TE=meta_analysis$TE,
                                   seTE= meta_analysis$seTE,
                                   statistic=meta_analysis$statistic, 
                                   pval=meta_analysis$pval,
                                   CI_lower_95=meta_analysis$lower,
                                   CI_upper_95 =meta_analysis$upper,
                                   file=spearman_results$file)

#group by lrp-------------------------------------------------------------------

# Split the data into a list of data frames, each containing a group of identical "lrp" values
lrp_groups <- split(spearman_results, spearman_results$lrp)
# Apply the metacor function to each group
meta_analysis_results <- lapply(lrp_groups, function(x) {
  meta_analysis <- metacor(x$spearman_corr, x$Tot_sample_size)
  data.frame(
    lrp = x$lrp[1],
    spearman_corr = meta_analysis$cor,
    Tot_sample_size = meta_analysis$n,
    TE = meta_analysis$TE,
    seTE = meta_analysis$seTE,
    statistic = meta_analysis$statistic,
    pval = meta_analysis$pval,
    CI_lower_95 = meta_analysis$lower,
    CI_upper_95 = meta_analysis$upper
  )
})
# Combine the results into a single data frame
meta_analysis_results <- do.call(rbind, meta_analysis_results)

#trial forest plot for A2M_LRP1-------------------------------------------------------
MA_A2M_LRP1 <- metacor(spearman_results$spearman_corr[grep("A2M_LRP1", metaanalysis_results$lrp)], 
                       spearman_results$Tot_sample_size[grep("A2M_LRP1", metaanalysis_results$lrp)], 
                        studlab =sub("([0-9]{4}).*$", "\\1", spearman_results$file[grep("A2M_LRP1", spearman_results$lrp)]))
forest(MA_A2M_LRP1,
       sortvar=TE,
       smlab="A2M_LRP1 Correlation",
       leftlabs = c("Study", "TE", "seTE"),
       prediction = TRUE, 
       print.tau2 = FALSE,
       xlim = c(-1, 1),
       )

pdf(file = "~/rlp_meta/results/plots/forestplot_A2M_LRP1.pdf", width = 8, height = 7)
dev.off()

A2M_LRP1_results <- data.frame(lrp = "A2M_LRP1", 
                                   Tot_sample_size = MA_A2M_LRP1$n,
                                   TE=MA_A2M_LRP1$TE,
                                   seTE= MA_A2M_LRP1$seTE,
                                   statistic=MA_A2M_LRP1$statistic, 
                                   pval=MA_A2M_LRP1$pval,
                                   CI_lower_95=MA_A2M_LRP1$lower,
                                   CI_upper_95 =MA_A2M_LRP1$upper,
                                   file=spearman_results$file[grep("A2M_LRP1", spearman_results$lrp)])

#loop
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(writexl)
library(DescTools)
library(fmsb)
library(ggplot2)
library(stats)
library(meta)

spearman_results <- read.csv("~/rlp_meta/results/spearman_results_meta.csv")

# Filter ligand-receptor pairs with at least 2 occurrences
filtered_results <- spearman_results %>%
  group_by(lrp) %>%
  filter(n() >= 4)

# Create a list to store the meta-analysis results
meta_results <- list()

# Loop through each ligand-receptor pair
for (lrp in unique(filtered_results$lrp)) {
  # Filter the data for the current ligand-receptor pair
  lrp_data <- filtered_results %>% filter(lrp == !!lrp)
  
  lrp_data$study_label <- sub("([0-9]{4}).*$", "\\1", lrp_data$file)
  MA <- metacor(lrp_data$spearman_corr, 
                lrp_data$Tot_sample_size, 
                studlab = lrp_data$study_label)
  
  # Store the result in the list
  meta_results[[lrp]] <- MA
}

# Create a directory to store the forest plots
dir.create("~/rlp_meta/results/plots/forest", showWarnings = FALSE)

# Loop through each meta-analysis result and create a forest plot
for (lrp in names(meta_results)) {
  # Check if at least 2 correlations are significant
  if (sum(meta_results[[lrp]]$pval < 0.05) >= 2) {
    # Create the forest plot
    forest(meta_results[[lrp]],
           sortvar = TE,
           smlab = paste0(lrp, " Correlation"),
           leftlabs = c("Study", "TE", "seTE"),
           prediction = TRUE, 
           print.tau2 = FALSE,
           xlim = c(-1, 1),
    )
    
    # Save the plot as a PDF
    pdf(file = paste0("~/rlp_meta/results/plots/forest/forestplot_", lrp, ".pdf"), width = 8, height = 7)
    dev.off()
  }
}

