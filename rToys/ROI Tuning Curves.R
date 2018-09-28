# Comparisions at 7T: ROI Tuning Curves

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

# ROI average split tuning curves
tc_dataset = read.csv('Comparisions_tuning_curves.csv')

# Tuning curves - beta weigths
plot1 <- ggplot(data = filter(tc_dataset, roi == "Left" | roi == "Right"), mapping = aes(x = beta_freq_NERB, y = beta_weight_A_True, color = acquistion)) + 
  geom_smooth() +
  facet_wrap(~ beta_bin_NERB, nrow = 2)

plot1

ggsave("betaTC.pdf",plot1)

# Tuning Curves - pRF average
ggplot(data = filter(tc_dataset, roi == "Left" | roi == "Right"), mapping = aes(x = beta_freq_NERB, y = pRF_tuning_curve, color = acquistion)) + 
  geom_smooth() +
  facet_wrap(~ beta_bin_NERB, nrow = 2)

ggplot(data = filter(tc_dataset, roi == "Left" | roi == "Right"), mapping = aes(x = beta_freq_NERB, y = beta_weight_A_True, color = acquistion, group = acquistion)) + 
  facet_wrap(~ beta_bin_NERB, nrow = 2) + 
  stat_summary(aes(y = beta_weight_A_True), fun.y=mean, geom="line")
