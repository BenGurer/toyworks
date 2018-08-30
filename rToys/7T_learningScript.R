# Comparisions at 7T

save_plot(filename, plot, ncol = 1, nrow = 1, base_height = 4,
          base_aspect_ratio = 1.1, base_width = NULL, ..., cols = NULL,
          rows = NULL)

# load tidyverse library
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("cowplot")
install.packages("RColorBrewer")
install.packages("modelr")
options(na.action = na.warn)

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

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

#  stat_summary(aes(y = beta_weight_A_True_normalised), fun.y=mean, colour="red", geom="line")

####
# ROI average split tuning curves
ggplot(data = filter(tc_dataset, roi == "Left" | roi == "Right"), mapping = aes(x = beta_freq_NERB, y = beta_weight_A_True_normalised, color = as.character(beta_bin_NERB))) + 
  geom_smooth() +
  facet_wrap(~ subjectID, nrow = 2)

ggplot(data = filter(tc_dataset, roi == "Left" | roi == "Right"), mapping = aes(x = beta_freq_NERB, y = beta_weight_A_True_normalised, color = acquistion)) + 
  geom_smooth(span = 0.2, se = FALSE) +
  facet_wrap(~ beta_bin_NERB, nrow = 2)

ggplot(data = filter(tc_dataset, roi == "Left" | roi == "Right"), mapping = aes(x = beta_freq_NERB, y = beta_weight_A_True_normalised, color = acquistion)) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) +
  facet_wrap(~ beta_bin_NERB, nrow = 2)

ggplot(data = filter(tc_dataset, roi == "Left" | roi == "Right"), mapping = aes(x = beta_freq_kHz, y = beta_weight_A_True_normalised, color = acquistion)) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) +
  facet_wrap(~ beta_bin_kHz, nrow = 2)

ggplot(data = filter(tc_dataset, roi == "Left" | roi == "Right"), mapping = aes(x = beta_freq_kHz, y = beta_weight_A_True_normalised, color = acquistion)) + 
  geom_smooth(span = 0.2) +
  facet_wrap(~ beta_bin_kHz, nrow = 2)
# to add point at index frequency - add data with nans are other frequencies - use acquisiton varible to call it index or something
#####

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

# Cortical Magnification

cm_dataset = read.csv('CorticalMagnification.csv')

cm_dataset %>% filter(Analysis == 'pRF')   %>%
  filter(r2 > 0.1) ->  cm_dataset_2plot

ggplot(data = cm_dataset_2plot, mapping = aes(x = CorticalDistance, y = Frequency, color = r2)) + 
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + 
  facet_wrap(~ Analysis, nrow = 2)


ggsave("cm.pdf",last_plot())

####
mod <- lm(y ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(mtcars, aes(mpg, disp, colour = factor(cyl))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

mtcars %>% 
  group_by(cyl) %>% 
  do({
    mod = lm(disp ~ mpg, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })
# Make the plot
ggplot(aes(x = speed, y = dist), data = cars) + geom_point() +
  stat_smooth(method = "loess")
# Get the values
smooth_vals = predict(loess(dist~speed,cars), cars$speed)

###

ggplot(data = cm_dataset, mapping = aes(x = CorticalDistance, y = Frequency, color = r2)) + 
  geom_bin2d(bins = 50) +
  geom_smooth() + 
  facet_wrap(~ Analysis, nrow = 2)

# geom_hex - i think its like geom_bin2d

# CM - subject

ggplot(data = filter(cm_dataset, Analysis == "pRF"), mapping = aes(x = CorticalDistance, y = Frequency, color = as.character(SubejctID), alpha = r2)) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + 
  facet_wrap(~ ROI, nrow = 2)

ggplot(data = filter(cm_dataset, Analysis == "pRF"), mapping = aes(x = CorticalDistance, y = TuningWidth, color = as.character(SubejctID), alpha = r2)) + 
  geom_smooth() + 
  facet_wrap(~ ROI, nrow = 2)

ggplot(data = cm_dataset, mapping = aes(x = CorticalDistance, y = Frequency, color = as.character(Analysis), alpha = r2)) + 
  geom_point() +
  geom_smooth() + 
  facet_wrap(~ SubejctID, nrow = 2)

# point image
ggplot(data = cm_dataset, mapping = aes(x = Frequency, y = TuningWidth, alpha = r2)) + 
  geom_point() +
  geom_smooth() + 
  facet_wrap(~ Analysis, nrow = 2)

# HRF

hrf_dataset = read.csv('HRF_est.csv')

ggplot(data = hrf_dataset, mapping = aes(x = hrf_Time, y = hrf_EstNorm, color = as.character(Subject))) + 
  geom_point() +
  geom_smooth()

ggplot(data = hrf_dataset, mapping = aes(x = hrf_Time, y = hrf_EstNorm)) + 
  geom_point() +
  geom_smooth()

ggplot(data = hrf_dataset, mapping = aes(x = hrf_Time, y = hrf_EstNorm)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~ Subject, nrow = 2)

# hrf tw

hrftw_dataset = read.csv('HRFTW_est.csv')

ggplot(data = hrftw_dataset, mapping = aes(x = hrf_Time, y = hrf_Est, color = as.character(Subject))) + 
  geom_line() +
  facet_wrap(~ hrf_freq)

ggplot(data = hrftw_dataset, mapping = aes(x = hrf_Time, y = hrf_EstNorm)) + 
  geom_smooth() +
  facet_wrap(~ hrf_freq)

# hrf params av
hrfparamsav_dataset = read.csv('HRFparams_av.csv')

ggplot(data = filter(hrfparamsav_dataset,Function_name != "diffofGamma"), mapping = aes(x = Time, y = Data, color = Function_name)) + 
  geom_line() + 
  stat_summary(data = hrf_dataset, aes(x=hrf_Time, y = hrf_EstNorm, color ="Deconvolution"), fun.y=mean, geom="line")

# test code below

# fitting a function to the data

p <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()
print(p)

p + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

ggplot(data=data, aes(x=Mean, y=CV2)) +
  geom_point() +
  stat_smooth(method='nls',formula=y~(a/(b+x))+c, method.args = list(start = c(a=1, b=1,c=1))) +
  labs(title = "RA vs CV (All analytes)") +
  labs(x = "Mean [%]") +
  labs(y = "CV [%]") +
  theme(plot.title = element_text(hjust = 0.5))

# test <- filter(ve_dataset, hrf != "Box Car" & roi == "Left" | roi == "Right" & concatenation == "Sparse" | concatenation == "Continuous")
ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, fill = acquistion)) + 
  geom_histogram(binwidth=3, alpha=.5, position="identity") +
  facet_wrap(~ estimation, nrow = 2) +
  geom_vline(xintercept=18, linetype="dashed", color = "red") +
  geom_vline(xintercept=3, linetype="dashed", color = "red") +
  geom_vline(xintercept=33, linetype="dashed", color = "red")

ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_kHz, fill = acquistion)) + 
  geom_histogram(alpha=.5, position="dodge") +
  facet_wrap(~ estimation, nrow = 2) +
  theme_minimal() + scale_x_continuous(trans='log10') +
  geom_hline(yintercept=15, linetype="dashed", color = "red")

# calculate centre of stimulus range

sp + geom_line(aes(y = noiseData)) # need to add noise data to data structure then filter it here
# or add as its own data frame

ggplot(data = filter(ve_dataset_histo, acquistion != "Continuous"), mapping = aes(x = frequency_nERB,fill = roi)) + 
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

ggplot(data = filter(ve_dataset, acquistion != "Continuous", roi == "Left" | roi == "Right"), mapping = aes(x = frequency_kHz, y = selectivity_kHz, color = estimation)) + 
  geom_point() +
  theme_minimal() + scale_x_continuous(trans='log10')



tc_dataset = read.csv('Comparisions_tuning_curves.csv')

ggplot(data = filter(tc_dataset, roi == "Left" | roi == "Right"), mapping = aes(x = beta_freq, y = beta_weight_A_True_normalised, color = acquistion,line = subjectID)) + 
  geom_smooth() +
  facet_wrap(~ beta_bin, nrow = 2) +
  theme_minimal()

ggplot(data = filter(tc_dataset, roi == "Left" | roi == "Right"), mapping = aes(x = beta_freq, y = beta_weight_A_True, color = acquistion)) + 
  geom_smooth() +
  facet_wrap(~ beta_bin, nrow = 2) +
  theme_minimal()

bw_dataset = read.csv('Comparisions_beta_weights.csv')

ggplot(data = bw_dataset, mapping = aes(x = beta_freq, y = beta_weight, color = acquistion)) + 
  geom_smooth() +
  theme_minimal()

ggplot(data = bw_dataset, mapping = aes(x = beta_freq, y = beta_weight, color = acquistion)) + 
  geom_smooth() +
  facet_wrap(~ subjectID, nrow = 2) +
  theme_minimal

ggplot(data = bw_dataset, mapping = aes(x = beta_freq, y = beta_weight_normalised, color = as.character(subjectID))) + 
  geom_smooth() +
  facet_wrap(~ acquistion, nrow = 2) +
  theme_minimal()

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left" | roi == "Right") %>%
  select(subjectID, voxelID,frequency_nERB,estimation) %>% 
  
  spread(key=estimation,value = frequency_nERB) -> M

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left" | roi == "Right") %>%
  select(voxelID,subjectID, frequency_nERB, estimation) %>% 
  group_by(voxelID) %>% 
  spread(key=estimation,value = frequency_nERB, drop = TRUE) -> M

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left" | roi == "Right") %>%
  filter(subjectID == 1) %>% 
  select(voxelID, subjectID, frequency_nERB, estimation) %>% 
  group_by(voxelID) %>% 
  mutate(i = row_number()) %>% 
  spread(key=estimation,value = frequency_nERB, drop = TRUE) -> M

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left" | roi == "Right") %>%
  select(voxelID, subjectID, frequency_nERB, estimation) %>% 
  filter(subjectID == 1) -> m

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left" | roi == "Right") %>%
  filter(acquistion == 'Sparse' & concatenation == 'Sparse')  %>%
  filter(subjectID == 1) -> m

######
# hopefully when voxelID is sort we can keep other variables - ie estiamtion type
ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left") %>%
  filter(acquistion == 'Sparse' & concatenation == 'Sparse_1' | concatenation == 'Sparse_2')  %>%
  filter(analysis == 'GLM') %>%
  filter(subjectID == 1) %>%
  filter(estimation == 'Debiased Centriod') %>%
  select(voxelID, frequency_nERB, concatenation) %>% 
  spread(key=concatenation, value = frequency_nERB)   %>%
  select(-voxelID) %>% cor() -> M

corrplot(M, method = "ellipse")

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left") %>%
  filter(acquistion == 'Sparse' & concatenation == 'Sparse_1' | concatenation == 'Sparse_2')  %>%
  filter(analysis == 'GLM') %>%
  filter(estimation == 'Max') %>%
  select(voxelID, subjectID, frequency_nERB, concatenation) %>% 
  group_by(voxelID) %>% 
  spread(key=concatenation, value = frequency_nERB) -> scatterData


ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left") %>%
  filter(concatenation == 'Continuous_3' | concatenation == 'Continuous_4')  %>%
  filter(analysis == 'GLM') %>%
  filter(estimation == 'Max') %>%
  select(voxelID, subjectID, frequency_nERB, concatenation) %>% 
  group_by(voxelID) %>% 
  spread(key=concatenation, value = frequency_nERB) -> scatterData

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left") %>%
  filter(acquistion == 'Sparse' & concatenation == 'Sparse_1' | concatenation == 'Sparse_2')  %>%
  filter(analysis == 'pRF') %>%
  select(voxelID, subjectID, frequency_nERB, concatenation) %>% 
  group_by(voxelID) %>% 
  spread(key=concatenation, value = frequency_nERB) -> scatterData


ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left") %>%
  filter(concatenation == 'Sparse')  %>%
  filter(analysis == 'Max') %>%
  select(voxelID, subjectID, frequency_nERB, concatenation) %>% 
  group_by(voxelID) %>% 
  spread(key=concatenation, value = frequency_nERB) -> scatterData

glimpse(ve_dataset)
summarise(scatterData)

ggplot(data = scatterData, mapping = aes(x= Sparse_1,y= Sparse_2, color = as.character(subjectID))) +
  geom_point() +
  geom_smooth()

ggplot(data = scatterData, mapping = aes(x= Sparse_1,y= Sparse_2)) +
  geom_point() +
  geom_smooth() + 
  scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

corrplot(M, method = "ellipse")

ve_dataset %>%
  filter(hrf != "Box Car") %>%
  filter(roi == "Left") %>%
  filter(acquistion == 'Sparse' & concatenation == 'Sparse')  %>%
  filter(analysis == 'GLM') %>%
  filter(subjectID == 1) %>%
  select(voxelID, frequency_nERB, estimation) %>% 
  spread(key=estimation, value = frequency_nERB)   %>%
  select(-voxelID) %>% cor() -> M

corrplot(M, method = "ellipse")

filter(roi == "Left" | roi == "Right") %>%
  
  select(-voxelID, -subjectID) %>% cor() -> M
######

M <- m %>% mutate(i = row_number())

filter(subjectID == 1) %>% 
  gather(voxelID,subjectID) %>% 
  
  ve_dataset %>%
  filter(hrf != "Box Car") %>%
  select(frequency_nERB,estimation) %>% 
  mutate(i = row_number()) %>% 
  spread(key=estimation, value = frequency_nERB) -> M

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
# ve_dataset %>% select(subjectID, voxelID, frequency_nERB, estimation) -> M
# M %>% spread(key=estimation,value = frequency_nERB)
# 
# M %>% mutate(i = row_number()) %>% spread(estimation, frequency_nERB)
# %>% cor() 
# cor(M) 
# corrplot(M, method="circle")
