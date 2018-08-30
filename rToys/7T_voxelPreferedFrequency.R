# Comparisions at 7T: Voxel Prefered Frequency

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

# Importing the dataset
# filenames <- 'Comparisions_tuning_curves.csv','Comparisions_beta_weights.csv'
ve_dataset = read.csv('Comparisions_voxel_estimates.csv')

# compare methods
# sparse data
ve_dataset %>% filter(hrf != "Box Car") %>%
  filter(acquistion == 'Sparse' & concatenation == 'Sparse') -> ve_dataset_histo

ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, fill = estimation)) + 
  geom_density(alpha=.3) +
  geom_vline(xintercept=stim_min_nERB, linetype="dashed", color = "red") +
  geom_vline(xintercept=stim_max_nERB, linetype="dashed", color = "red") +
  geom_vline(xintercept=stim_middle_nERB, linetype="dashed", color = "red")

ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, fill = estimation)) + 
  geom_histogram(alpha=.3) +
  facet_wrap(~ subjectID, nrow = 2) +
  theme(aspect.ratio = 1) +
  theme(strip.background =element_rect(fill="white")) +
  theme(strip.text = element_text(colour = 'black')) +
  geom_vline(xintercept=stim_min_nERB, linetype="dashed", color = "red") +
  geom_vline(xintercept=stim_max_nERB, linetype="dashed", color = "red") +
  geom_vline(xintercept=stim_middle_nERB, linetype="dashed", color = "red")

# Correlations

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left") %>%
  filter(acquistion == 'Sparse' & concatenation == 'Sparse')  %>%
  select(voxelID, subjectID, frequency_nERB, estimation) %>% 
  spread(key=estimation, value = frequency_nERB)   %>%
  select(-voxelID, -subjectID) %>% cor() -> ve_dataset_correlations

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left" || roi == "Right") %>%
  filter(acquistion == 'Sparse' & concatenation == 'Sparse')  %>%
  select(voxelID, subjectID, roi, r2ATrue, frequency_nERB, estimation) %>% 
  spread(key=estimation, value = frequency_nERB)   %>%
  select(-voxelID, -subjectID, -roi) -> ve_dataset_analysis_correlations

row.has.na <- apply(ve_dataset_analysis_correlations, 1, function(x){any(is.na(x))})
sum(row.has.na)
ve_dataset_analysis_correlations <- ve_dataset_analysis_correlations[!row.has.na,]

ve_dataset_analysis_correlations %>% cor() -> ve_corr_analysis_matrix

corrplot(ve_corr_analysis_matrix, type="upper")

ggplot(data = ve_dataset_analysis_correlations, mapping = aes(x = `Debiased Centriod`, y = `population Centre Frequency`, alpha = r2ATrue)) + 
  geom_point()

#corrplot(ve_corr_matrix, type="upper", order="hclust",
#        col=brewer.pal(n=8, name="PuOr"))

# compare acquistions

ve_dataset %>% filter(hrf != "Box Car") %>%
  filter(estimation == 'population Centre Frequency')  %>%
  filter(roi == 'Left' || roi == 'Right') -> ve_dataset_histo_acq
# compare acquisitons - nERB scale
ggplot(data = ve_dataset_histo_acq, mapping = aes(x = frequency_nERB, fill = acquistion)) + 
  geom_density(alpha=.3) +
  geom_vline(xintercept=stim_min_nERB, linetype="dashed", color = "red") +
  geom_vline(xintercept=stim_max_nERB, linetype="dashed", color = "red") +
  geom_vline(xintercept=stim_middle_nERB, linetype="dashed", color = "red")

# compare acquisitons - kHz log10 scale
ggplot(data = ve_dataset_histo_acq, mapping = aes(x = frequency_kHz, fill = acquistion)) + 
  geom_density(alpha=.3) +
  geom_vline(xintercept=stim_min_kHz, linetype="dashed", color = "red") +
  geom_vline(xintercept=stim_max_kHz, linetype="dashed", color = "red") +
  geom_vline(xintercept=stim_middle_kHz, linetype="dashed", color = "red") +
  scale_x_continuous(trans='log10')

# compare acquisitons - nERB scale - subjects
ggplot(data = ve_dataset_histo_acq, mapping = aes(x = frequency_nERB, fill = acquistion)) + 
  geom_density(alpha=.3) +
  facet_wrap(~ subjectID, nrow = 2) +
  geom_vline(xintercept=stim_min_nERB, linetype="dashed", color = "red") +
  geom_vline(xintercept=stim_max_nERB, linetype="dashed", color = "red") +
  geom_vline(xintercept=stim_middle_nERB, linetype="dashed", color = "red")

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left" || roi == "Right") %>%
  filter(estimation == 'population Centre Frequency')  %>%
  filter(acquistion == 'Sparse')  %>%
  filter(concatenation != 'Sparse')  %>%
  select(voxelID, subjectID, roi, r2ATrue, frequency_nERB, concatenation) %>% 
  spread(key=concatenation, value = frequency_nERB)   %>%
  select(-voxelID, -subjectID, -roi, -r2ATrue) -> ve_dataset_sparse_correlations

row.has.na <- apply(ve_dataset_sparse_correlations, 1, function(x){any(is.na(x))})
sum(row.has.na)
ve_dataset_sparse_correlations <- ve_dataset_sparse_correlations[!row.has.na,]

ve_dataset_sparse_correlations %>% cor() -> ve_corr_sprase_matrix

corrplot(ve_corr_sprase_matrix, type="upper")

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left" || roi == "Right") %>%
  filter(estimation == 'population Centre Frequency')  %>%
  filter(acquistion == 'Continuous')  %>%
  filter(concatenation != 'Continuous')  %>%
  select(voxelID, subjectID, roi, r2ATrue, frequency_nERB, concatenation) %>% 
  spread(key=concatenation, value = frequency_nERB)   %>%
  select(-voxelID, -subjectID, -roi, -r2ATrue) -> ve_dataset_continuous_correlations

row.has.na <- apply(ve_dataset_continuous_correlations, 1, function(x){any(is.na(x))})
sum(row.has.na)
ve_dataset_continuous_correlations <- ve_dataset_continuous_correlations[!row.has.na,]

ve_dataset_continuous_correlations %>% cor() -> ve_corr_continuous_matrix

corrplot(ve_corr_continuous_matrix, type="upper")
