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
#metacor of A2M_LRP1 lrp correlations and sample sizes
MA_A2M_LRP1 <- metacor(cor = spearman_results$spearman_corr[grep("A2M_LRP1", spearman_results$lrp)], 
                       n = spearman_results$Tot_sample_size[grep("A2M_LRP1",spearman_results$lrp)], 
                       studlab = sub("([0-9]{4}).*$", "\\1", spearman_results$file[grep("A2M_LRP1", spearman_results$lrp)]), 
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML",
                       method.random.ci = "HK",
                       title = "A2M_LRP1 correlation meta_analysis")
summary(MA_A2M_LRP1)

#forest plot
pdf(file = "~/rlp_meta/results/plots/forestplot_A2M_LRP1.pdf", width = 15, height = 6)
forest(MA_A2M_LRP1,
       sortvar=TE,
       smlab="A2M_LRP1 Correlation",
       leftlabs = c("Study", "Observations"),
       prediction = TRUE, 
       print.tau2 = FALSE,
       xlim = c(-1, 1),
       )
dev.off()


#funnel plot
# Define fill colors for contour
col.contour = c("steelblue1", "lightskyblue1", "lightcyan1")

# Generate funnel plot (to not include study labels #studlab = TRUE)
meta::funnel(MA_A2M_LRP1,
             xlim = c(-0.5, 2),
             studlab = TRUE,
             contour = c(0.9, 0.95, 0.99),
             col.contour = col.contour)
# Add a legend
legend(x = 1.6, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)
# Add a title
title("Funnel Plot (A2M_LRP1 correlation meta_analysis)")

#try update: cannot find update
MA_A2M_LRP <- update.meta(MA_A2M_LRP, prediction = TRUE)
summary(MA_A2M_LRP)

#store resutls: different CI values?
A2M_LRP1_results <- data.frame(lrp = "A2M_LRP1", 
                                   Tot_sample_size = MA_A2M_LRP1$n,
                                   TE=MA_A2M_LRP1$TE,
                                   seTE= MA_A2M_LRP1$seTE,
                                   statistic=MA_A2M_LRP1$statistic, 
                                   pval=MA_A2M_LRP1$pval,
                                   CI_lower_95=MA_A2M_LRP1$lower,
                                   CI_upper_95 =MA_A2M_LRP1$upper,
                                   file=spearman_results$file[grep("A2M_LRP1", spearman_results$lrp)])

#loop---------------------------------------------------------------------------
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

# Filter ligand-receptor pairs with at least 4 occurrences
filtered_results <- spearman_results %>%
  group_by(lrp) %>%
  filter(n() >= 2)

# Create a list to store the meta-analysis results
meta_results <- list()

# Loop through each ligand-receptor pair
for (lrp in unique(filtered_results$lrp)) {
  # Filter the data for the current ligand-receptor pair
  lrp_data <- filtered_results %>% 
                  filter(lrp == !!lrp)
  
  lrp_data$study_label <- sub("([0-9]{4}).*$", "\\1", lrp_data$file)
  MA <- metacor(lrp_data$spearman_corr, 
                lrp_data$Tot_sample_size, 
                studlab = lrp_data$study_label,
                fixed = FALSE,
                random = TRUE,
                method.tau = "REML",
                method.random.ci = "HK",
                title = paste0(lrp," correlation meta_analysis"))
  
  # Store the result in the list
  meta_results[[lrp]] <- MA
}

# Create a directory to store the forest plots
dir.create("~/rlp_meta/results/plots/forest", showWarnings = FALSE)

# Loop through each meta-analysis result and create a forest plot
for (lrp in names(meta_results)) {
  # Check if at least 2 correlations are significant
  #if ((meta_results[[lrp]]$pval.Q < 0.05) && ((meta_results[[lrp]]$TE.random >= 0.5) || (meta_results[[lrp]]$TE.random <= 0.5))) {
  #if ((meta_results[[lrp]]$pval.Q < 0.05) & ((meta_results[[lrp]]$upper.random - meta_results[[lrp]]$lower.random) > 0)) {
  if ((meta_results[[lrp]]$pval.random < 0.05) & !((meta_results[[lrp]]$lower.random <= 0) & (meta_results[[lrp]]$upper.random >= 0))) {
    # Create the forest plot
    pdf(file = paste0("~/rlp_meta/results/plots/forest/forestplot_", lrp, ".pdf"), width = 10, height = 6)
    forest(meta_results[[lrp]],
           sortvar = TE,
           smlab = paste0(lrp, " Correlation"),
           leftlabs = c("Study", "Observations"),
           prediction = TRUE, 
           print.tau2 = FALSE,
           xlim = c(-1, 1),
           col.diamond = "steelblue1"
    )
    # Save the plot as a PDF
    dev.off()
    
    #funnel plot
    pdf(file = paste0("~/rlp_meta/results/plots/forest/funnelplot_", lrp, ".pdf"), width = 10, height = 10)
    # Define fill colors for contour
    col.contour = c("steelblue1", "lightskyblue1", "lightcyan1")
    # Generate funnel plot (to not include study labels #studlab = TRUE)
    meta::funnel(meta_results[[lrp]],
                 xlim = c(-0.5, 2),
                 studlab = TRUE,
                 contour = c(0.9, 0.95, 0.99),
                 col.contour = col.contour)
    # Add a legend
    legend(x = 1.6, y = 0.01, 
           legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
           fill = col.contour)
    # Add a title
    title(paste0("Funnel Plot: correlation meta_analysis ", lrp))
    # Save the plot as a PDF
    dev.off()  
    
    if ((meta_results[[lrp]]$TE.random >= 0.5) | (meta_results[[lrp]]$TE.random <= -0.5)){
      cat("lrp: ", lrp, ", TE.random: ", meta_results[[lrp]]$TE.random, "\n")
    }
  }
}
#15 significant, 75 total
