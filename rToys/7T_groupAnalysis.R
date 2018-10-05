# set working directory to correct folder
# setwd("E:/OneDrive - The University of Nottingham/data/hearingLossSimulation/groupAnalysis")

setwd("C:/Users/bengu/Google Drive/data/CorticalMagnification/groupAnalysis")

#### Noise ####
# load noise
noise_dataset = read.csv('scanner_noise.csv')

noise_dataset %>% filter(Averaging %in% c('None')) ->  noise_dataset_2plot
noise_dataset %>% filter(Averaging %in% c('Moving Average')) ->  noise_dataset_2plot_mv

###### Average Beta weights ######

# load and filter data
bw_dataset = read.csv('Comparisions_beta_weights.csv')

bw_dataset %>% filter(beta_averaging %in% c('Moving Average')) %>% 
  filter(roi %in% c("Left", "Right")) ->  bw_dataset_2plot

legend_location <- c(0.1,0.9)
bw_plot <- plot_av_beta_weights(bw_dataset_2plot, legend_location)

bw_plot

# save plot
save_plot("cm_av_beta_weights.png", bw_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.1, base_width =  3.34646,dpi = 450)


# difference and noise plots
d_difference  <-  bw_dataset_2plot_sum %>% 
  select(c("acquistion", "beta_weight", "beta_freq_NERB")) %>% 
  spread(acquistion, beta_weight) %>% 
  mutate(condition_difference = Continuous - Sparse) %>% 
  select(c("beta_freq_NERB", "condition_difference"))

d_diff_se  <-  bw_dataset_2plot_sum %>% 
  select(c("acquistion", "se", "beta_freq_NERB")) %>% 
  spread(acquistion, se) %>% 
  mutate(condition_se = Sparse + Continuous) %>% 
  select(c("beta_freq_NERB", "condition_se"))

d <- bind_cols(d_difference,d_diff_se) %>% 
  select(-beta_freq_NERB1)

secylabel <- "Noise level (dB SPL normalised)"
diff_plot <- plot_av_beta_difference(d,noise_dataset_2plot,secylabel)

diff_plot

# save plot
save_plot("cm_av_beta_weights_difference.png", dif_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.6, base_width =  3.34646,dpi = 450)

###### Tuning Curves ######

# load and filter data
tc_dataset = read.csv('Comparisions_tuning_curves.csv')

tc_dataset %>% filter(roi %in% c("Left", "Right")) ->  tc_dataset_2plot

tc_plot <- plot_tuning_curves(tc_dataset)

tc_plot

# save plot
save_plot("sHL_tuning_curves.png", tc_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.1, base_width =  6.9291339,dpi = 450)

### Frequency distribution

# Importing the dataset
ve_dataset = read.csv('Comparisions_voxel_estimates.csv')

ve_dataset %>% 
  filter(estimation == 'population Centre Frequency')  %>%
  filter(roi %in% c("Left", "Right")) ->  ve_dataset_acquistion

legend_location <- c(0.8,0.8)
fhist_plot_acquistion <- plot_hist_density_acquistion(ve_dataset_acquistion,legend_location)

fhist_plot_acquistion

# save plot
save_plot("7T_freq_acquistion.png", fhist_plot_acquistion, device = "png", base_height = NULL,
          base_aspect_ratio = 1.5, base_width =  3.34646,dpi = 450)
##

ve_dataset %>% 
  filter(acquistion == 'Sparse')  %>%
  filter(location %in% c("Both")) ->  ve_dataset_estimate

fhist_plot_estimate <- plot_hist_density_estimate(ve_dataset_estimate,legend_location)

fhist_plot_estimate

# save plot
save_plot("7T_freq_estimate.png", fhist_plot_estimate, device = "png", base_height = NULL,
          base_aspect_ratio = 1.5, base_width =  3.34646,dpi = 450)

ve_dataset %>% 
  filter(acquistion == 'Sparse')  %>%
  filter(estimation %in% c("Debiased Centriod", "population Centre Frequency")) %>%
  filter(location %in% c("Both")) -> ve_dataset_analysis

fhist_plot_analysis <- plot_hist_density_analysis(ve_dataset_analysis,legend_location)

fhist_plot_analysis

# save plot
save_plot("7T_freq_analysis.png", fhist_plot_analysis, device = "png", base_height = NULL,
          base_aspect_ratio = 1.5, base_width =  3.34646,dpi = 450)


#### correlations

# between runs
# between conditions
# between glm estimation
# between analysis



ve_dataset %>%
  filter(location == "Both") %>%
  filter(hemisphere == "Left" | roi == "Right") %>%
  filter(concatenation != 'Sparse')  %>%
  filter(concatenation != 'Continuous')  %>%
  filter(analysis != 'GLM')  %>%
  select(voxelID, subjectID, concatenation, frequency_nERB) %>%
  spread(key=concatenation, value = frequency_nERB) %>%
  select(-voxelID, -subjectID) -> ve_dataset_analysis_correlations

row.has.na <- apply(ve_dataset_analysis_correlations, 1, function(x){any(is.na(x))})
sum(row.has.na)
ve_dataset_analysis_correlations <- ve_dataset_analysis_correlations[!row.has.na,]

ve_dataset_analysis_correlations %>% cor() -> ve_corr_analysis_matrix

correlations_plot <- plot_correlations(ve_corr_analysis_matrix)

correlations_plot

### scatter plots - NO BEN

dat$density <- get_density(dat$x, dat$y)
ggplot(dat) + geom_point(aes(x, y, color = density)) + scale_color_viridis()

# Importing the dataset
# filenames <- 'Comparisions_tuning_curves.csv','Comparisions_beta_weights.csv'
ve_dataset = read.csv('Comparisions_voxel_estimates.csv')

ve_dataset %>% 
  filter(estimation == 'population Centre Frequency')  %>%
  filter(roi %in% c("Left", "Right")) ->  ve_dataset_histo


stim_min_nERB = 3.3589
stim_max_nERB = 33.1892
stim_middle_nERB = 18.2740

ticks_values <- c(0, 10, 20, 30, 40)
ticks_labels <- nERB2kHz(ticks_values)
ticks_labels <- round(ticks_labels,digits=2)

ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, fill = acquistion, colour = NULL)) + 
  geom_density(alpha=0.3) +
  labs(title = "Distribution of preferred frequency", x = "Frequency (kHz)", y = "Voxel Count") +
  guides(fill=guide_legend(title=NULL))+
  labs(fill="Condition") + 
  scale_x_continuous(breaks = ticks_values, label = ticks_labels) +
  theme(legend.justification=c(1,0), 
        legend.position=c(1, 0.75),  
        legend.background = element_blank(),
        legend.key = element_blank()) 


# Tuning Curves

tc_dataset = read.csv('Comparisions_tuning_curves.csv')

tc_dataset %>% filter(roi %in% c("Left", "Right")) ->  tc_dataset_2plot

row.has.na <- apply(tc_dataset, 1, function(x){any(is.na(x))})
sum(row.has.na)
tc_dataset <- tc_dataset[!row.has.na,]

tc_dataset_2plot_sum <-summarySE(tc_dataset,measurevar="beta_weight_A_True",groupvars=c("beta_freq_NERB","beta_bin_kHz","acquistion"))

ticks_values <- c(stim_min_nERB, stim_middle_nERB, stim_max_nERB)
ticks_labels <- nERB2kHz(ticks_values)
ticks_labels <- round(ticks_labels,digits=2)

ggplot(tc_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight_A_True, colour=acquistion)) + 
  geom_errorbar(aes(ymin=beta_weight_A_True-se, ymax=beta_weight_A_True+se), width=1)+
  geom_line() +
  geom_point() +
  facet_wrap(~ beta_bin_kHz, nrow = 2) +
  scale_x_continuous(breaks = ticks_values, label = ticks_labels) +
  labs(title = "Average Tuning Curves within ROI", x = "Frequency (kHz)", y = "Average Beta Weight (arb. units)") +
  guides(colour=guide_legend(title=NULL)) +
  theme(strip.background =element_rect(fill="white"))

# noise

noise_dataset = read.csv('scanner_noise.csv')

noise_dataset %>% filter(Averaging %in% c('None')) ->  noise_dataset_2plot

# %>% mutate(noise_SPLnorm_ex = noise_SPLnorm * 2)

ggplot(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SPLnorm)) +
  geom_line(linetype="dashed")

# subtract each mean curve to get difference
bw_dataset_2plot_sum %>% mutate()
  
# ROI avearge beta weights

bw_dataset = read.csv('Comparisions_beta_weights.csv')

bw_dataset %>% filter(beta_averaging %in% c('Moving Average')) %>% 
  filter(roi %in% c("Left", "Right")) ->  bw_dataset_2plot

row.has.na <- apply(bw_dataset_2plot, 1, function(x){any(is.na(x))})
sum(row.has.na)
bw_dataset_2plot <- bw_dataset_2plot[!row.has.na,]

bw_dataset_2plot_sum <-summarySE(bw_dataset_2plot,measurevar="beta_weight",groupvars=c("beta_freq_NERB","beta_freq_kHz","acquistion"))

pd <- position_dodge(1) # move them .05 to the left and right
#ticks_values <- c(4.802, 10.576, 17.312, 24.048, 31.746)

stim_min_nERB_mv = 4.802
stim_max_nERB_mv = 31.746
stim_middle_nERB_mv = ((stim_max_nERB_mv - stim_min_nERB_mv) / 2) + stim_min_nERB_mv
stim_lower_nERB_mv = ((stim_middle_nERB_mv - stim_min_nERB_mv) / 2) + stim_min_nERB_mv
stim_upper_nERB_mv = stim_middle_nERB_mv + ((stim_middle_nERB_mv - stim_min_nERB_mv) / 2)
#ticks_values <- c(4.802, 7.9365, 15.873, 23.8095, 31.746)
ticks_values <- c(stim_min_nERB_mv, stim_lower_nERB_mv, stim_middle_nERB_mv, stim_upper_nERB_mv, stim_max_nERB_mv)
ticks_labels <- nERB2kHz(ticks_values)
ticks_labels <- round(ticks_labels,digits=2)

ggplot(bw_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight, colour=acquistion)) + 
  geom_errorbar(aes(ymin=beta_weight-se, ymax=beta_weight+se)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = ticks_values, label = ticks_labels) +
  labs(title = "Average Beta Weight within ROI", x = "Frequency (kHz)", y = "Average Beta Weight (arb. units)") +
  guides(colour=guide_legend(title=NULL)) + 
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=1/noise_SPLnorm, colour=NULL),linetype="dashed")


ggplot(bw_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight, colour=acquistion)) + 
  geom_ribbon(aes(ymin=beta_weight-se, ymax=beta_weight+se)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = ticks_values, label = ticks_labels) +
  labs(title = "Average Beta Weight within ROI", x = "Frequency (kHz)", y = "Average Beta Weight (arb. units)") +
  guides(colour=guide_legend(title=NULL)) + 
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=1/noise_SPLnorm, colour=NULL),linetype="dashed")

ggplot(bw_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight, colour=acquistion, fill=acquistion)) + 
  geom_ribbon(aes(ymin=beta_weight-se, ymax=beta_weight+se), alpha=0.1, linetype="blank") +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = ticks_values, label = ticks_labels) + 
  labs(title = "Average Beta Weight within ROI", x = "Frequency (kHz)", y = "Average Beta Weight (arb. units)") +
  guides(colour=guide_legend(title=NULL)) + 
  scale_colour_manual(name="Condition", 
                      labels = c("Normal Hearing", 
                                 "Hearing Loss Simulation"), 
                      values = c("NH"="green", 
                                 "sHL"="red")) +
  scale_colour_brewer(palette = "Set1") +
  guides(fill=guide_legend(title=NULL)) + 
  scale_fill_brewer(palette = "Set1") +
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=1/noise_SPLnorm, colour=NULL, fill=NULL),linetype="dashed")

d_difference  <-  bw_dataset_2plot_sum %>% 
  select(c("acquistion", "beta_weight", "beta_freq_NERB")) %>% 
  spread(acquistion, beta_weight) %>% 
  mutate(condition_difference = Sparse - Continuous) %>% 
  select(c("beta_freq_NERB", "condition_difference"))

d_diff_se  <-  bw_dataset_2plot_sum %>% 
  select(c("acquistion", "se", "beta_freq_NERB")) %>% 
  spread(acquistion, se) %>% 
  mutate(condition_se = Sparse + Continuous) %>% 
  select(c("beta_freq_NERB", "condition_se"))

d <- bind_cols(d_difference,d_diff_se) %>% 
  select(-beta_freq_NERB1)

ggplot(data=d, aes(x = beta_freq_NERB, y = condition_difference)) +
  geom_ribbon(aes(ymin=condition_difference-condition_se, ymax=condition_difference+condition_se), alpha=0.1) +
  geom_line() +
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SPLnorm*4-3, colour=NULL),linetype="dashed") + scale_y_continuous(sec.axis = sec_axis(~.*1,name = "Relative Sensation Level"))



# use this: http://bioinfo.iric.ca/introduction-to-cowplot/ - cool multi-plot

# to save use save_plot(filename, plot, ncol = 1, nrow = 1, base_height = 4,
# base_aspect_ratio = 1.1, base_width = NULL, ..., cols = NULL,
# rows = NULL)

bw_plot <- ggplot(data = bw_dataset_2plot, mapping = aes(x = beta_freq_NERB, y = beta_weight_normalised, color = acquistion)) + 
  geom_smooth()

ggsave("bw_plot.pdf", bw_plot, device = pdf, path = NULL,
       scale = 1, width = NA, height = NA, units = "mm",
       dpi = 300, limitsize = TRUE)


# figuring out how to do percentages
nVoxels <- nrow(ve_dataset_histo[ve_dataset_histo$comparision == "sHL_pRF",])

# divide by 10 bins

sum(ve_dataset_histo$frequency_nERB > stim_min_nERB & ve_dataset_histo$frequency_nERB <= stim_middle_nERB, na.rm=TRUE)

nrow(ve_dataset_histo[ve_dataset_histo$frequency_nERB <= stim_min_nERB,]) / nVoxels

nrow(ve_dataset_histo[ve_dataset_histo$frequency_nERB > stim_min_nERB & ve_dataset_histo$frequency_nERB <= stim_middle_nERB,]) / nVoxels

(nrow(ve_dataset_histo[ve_dataset_histo$frequency_nERB > stim_middle_nERB & ve_dataset_histo$frequency_nERB <= stim_max_nERB,])) / nVoxels

nrow(ve_dataset_histo[ve_dataset_histo$frequency_nERB >= stim_max_nERB,]) / nVoxels