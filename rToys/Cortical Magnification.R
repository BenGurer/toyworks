# Cortical Magnification

library(tidyverse)
library(cowplot)
library(corrplot)
library(scales) # to access break formatting functions
library(RColorBrewer)
library(modelr)

# set working directory to correct folder
setwd("E:/OneDrive - The University of Nottingham/data/CorticalMagnification/groupAnalysis")

cm_dataset = read.csv('CorticalMagnification.csv')

cm_dataset %>% filter(Analysis == 'pRF')   %>%
  filter(r2 > 0.1) ->  cm_dataset_2plot

ggplot(data = cm_dataset_2plot, mapping = aes(x = CorticalDistance, y = Frequency, color = r2)) + 
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + 
  facet_wrap(~ Analysis, nrow = 2)

# nERB
a = 24.7/1000
b = 4.37
x=1/((a*b) * (log(b*f+1)))

# nDLM
# x = -2^(1-b-a f 5-b-a f) (1+a flog 10 )(a2 log2(10))


ggsave("cm.pdf",last_plot())