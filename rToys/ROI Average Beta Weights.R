# Comparisions at 7T: ROI Average Beta Weights

library(tidyverse)
library(cowplot)
library(corrplot)
library(scales) # to access break formatting functions
library(RColorBrewer)
library(modelr)

stim_min_kHz = 0.1000
stim_max_kHz = 8.0000
stim_middle_kHz = 2.2839

stim_min_nERB = 3.3589
stim_max_nERB = 33.1892
stim_middle_nERB = 18.2740

# set working directory to correct folder
setwd("E:/OneDrive - The University of Nottingham/data/CorticalMagnification/groupAnalysis")

# ROI avearge beta weights

bw_dataset = read.csv('Comparisions_beta_weights.csv')

bw_dataset %>% filter(beta_averaging == 'Moving Average')   %>%
  filter(roi != '* Anterior' || roi != '* Posterior')  ->  bw_dataset_2plot

summarySE(bw_dataset_2plot,measurevar="beta_weight",groupvars=c("beta_freq_NERB","acquistion"))

ggplot(data = bw_dataset_2plot, mapping = aes(x = beta_freq_NERB, y = beta_weight, color = acquistion)) + 
  geom_smooth()

bw_plot <- ggplot(data = bw_dataset_2plot, mapping = aes(x = beta_freq_NERB, y = beta_weight_normalised, color = acquistion)) + 
  geom_smooth()

ggsave("bw_plot.pdf", bw_plot, device = pdf, path = NULL,
       scale = 1, width = NA, height = NA, units = "mm",
       dpi = 300, limitsize = TRUE)
# ggsave(filename, plot = ggplot2::last_plot(), device = NULL, path = NULL,
#        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#        dpi = 300, limitsize = TRUE, ...)

ggplot(data = bw_dataset_2plot, mapping = aes(x = beta_freq_NERB, y = beta_weight_normalised, color = acquistion)) + 
  geom_line() +
  facet_wrap(~ subjectID, nrow = 2)

ggplot(data = bw_dataset_2plot, mapping = aes(x = beta_freq_NERB, y = beta_weight, color = acquistion)) + 
  geom_smooth() +
  facet_wrap(~ subjectID, nrow = 2)

ggplot(data = bw_dataset, mapping = aes(x = beta_freq_NERB, y = beta_weight_normalised, color = as.character(subjectID))) + 
  geom_smooth() +
  facet_wrap(~ acquistion, nrow = 2)