# Comparisions at 7T

# load tidyverse library
# install.packages("tidyverse")
library(tidyverse)
library(scales) # to access break formatting functions

# set working directory to correct folder
setwd("E:/OneDrive - The University of Nottingham/data/CorticalMagnification/groupAnalysis")

# Importing the dataset
# filenames <- 'Comparisions_tuning_curves.csv','Comparisions_beta_weights.csv'
ve_dataset = read.csv('Comparisions_voxel_estimates.csv')

# test <- filter(ve_dataset, hrf != "Box Car" & roi == "Left" | roi == "Right" & concatenation == "Sparse" | concatenation == "Continuous")
ve_dataset_histo <- filter(ve_dataset, hrf != "Box Car")

ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, color = acquistion)) + 
  geom_histogram() +
  facet_wrap(~ estimation, nrow = 2) +
  theme_minimal()

ggplot(data = filter(ve_dataset_histo, acquistion != "Continuous"), mapping = aes(x = frequency_nERB, color = roi)) + 
  geom_histogram() +
  facet_wrap(~ estimation, nrow = 2) +
  theme_minimal()

ggplot(data = filter(ve_dataset_histo, acquistion != "Continuous", roi == "Left" | roi == "Right"), mapping = aes(x = frequency_nERB, color = roi)) + 
  geom_histogram() +
  theme_minimal()

ggplot(data = filter(ve_dataset_histo, acquistion != "Continuous", roi == "Left" | roi == "Right"), mapping = aes(x = frequency_kHz)) + 
  geom_histogram() +
  facet_wrap(~ estimation, nrow = 2) +
  theme_minimal() + scale_x_continuous(trans='log10')

ggplot(data = ve_dataset, mapping = aes(x = frequency_kHz, color = acquistion)) + 
  geom_histogram() +
  facet_wrap(~ estimation, nrow = 2) +
  theme_minimal() + scale_x_continuous(trans='log10')



tc_dataset = read.csv('Comparisions_tuning_curves.csv')

ggplot(data = filter(tc_dataset, roi == "Left" | roi == "Right"), mapping = aes(x = beta_freq, y = beta_weight, color = acquistion)) + 
  geom_smooth() +
  facet_wrap(~ beta_bin, nrow = 2) +
  theme_minimal()

bw_dataset = read.csv('Comparisions_beta_weights.csv')

ggplot(data = tc_dataset, mapping = aes(x = beta_freq, y = beta_weight, color = acquistion)) + 
  geom_smooth() +
  theme_minimal()

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  select(frequency_nERB,estimation) %>% 
  spread(key=estimation,value = frequency_nERB) -> M

test <- filter(ve_dataset, hrf != "Box Car" & roi == "Left" | roi == "Right" & concatenation == "Sparse" | concatenation == "Continuous")
test2 <- select(test,frequency_nERB,estimation)
test3 <- spread(test2,key=estimation,value = frequency_nERB)


# CDCBirths %>% 
#   filter(!is.na(GenderCode)) %>% 
#   select(State,Year,GenderCode,Births) -> Births
# 
# library(corrplot)
# 
# glimpse(ve_dataset)
#   ve_dataset %>% 
#   select(frequency_nERB,estimation) %>% 
#   spread(frequency_nERB,estimation) -> estimates
# glimpse(estimates)
# 
# ve_dataset %>% select(frequency_nERB,estimation) -> M
# M %>% spread(key=estimation,value = frequency_nERB, drop = TRUE)
# 
# M %>% mutate(i = row_number()) %>% spread(estimation, frequency_nERB)
# %>% cor() 
# cor(M) 
# corrplot(M, method="circle")
