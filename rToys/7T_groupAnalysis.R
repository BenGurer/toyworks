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

row.has.na <- apply(bw_dataset_2plot, 1, function(x){any(is.na(x))})
sum(row.has.na)
bw_dataset_2plot <- bw_dataset_2plot[!row.has.na,]

bw_dataset_2plot_sum <-summarySE(bw_dataset_2plot,measurevar="beta_weight",groupvars=c("beta_freq_NERB","acquistion"))

# set label stuff
stim_min_nERB_mv = 4.802
stim_max_nERB_mv = 31.746
stim_middle_nERB_mv = ((stim_max_nERB_mv - stim_min_nERB_mv) / 2) + stim_min_nERB_mv
stim_lower_nERB_mv = ((stim_middle_nERB_mv - stim_min_nERB_mv) / 2) + stim_min_nERB_mv
stim_upper_nERB_mv = stim_middle_nERB_mv + ((stim_middle_nERB_mv - stim_min_nERB_mv) / 2)
#ticks_values <- c(4.802, 7.9365, 15.873, 23.8095, 31.746)
bw_ticks_values <- c(stim_min_nERB_mv, stim_lower_nERB_mv, stim_middle_nERB_mv, stim_upper_nERB_mv, stim_max_nERB_mv)
bw_ticks_labels <- nERB2kHz(bw_ticks_values)
bw_ticks_labels <- round(bw_ticks_labels,digits=2)
text_size = 9

# plot data
bw_plot <- ggplot(bw_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight, colour=acquistion, fill=acquistion)) + 
  geom_ribbon(aes(ymin=beta_weight-se, ymax=beta_weight+se), alpha=0.1, linetype="blank") +
  geom_line() +
  geom_point() +
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SPLnorm, colour=NULL, fill=NULL),linetype="dashed")

# add labels
bw_plot <- bw_plot +
  scale_x_continuous(breaks = bw_ticks_values, label = bw_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Average Beta Weight (arb. units)") +
  theme(legend.position="none") +
  scale_colour_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1")

# set sizes
bw_plot <- bw_plot +
  theme(axis.text.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=text_size,angle=0,hjust=0,vjust=0.5,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),
        axis.title.y = element_text(colour="grey20",size=text_size,angle=90,hjust=0.5,vjust=0.5,face="plain"))

bw_plot

# save plot
save_plot("cm_av_beta_weights.png", bw_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.1, base_width =  3.34646,dpi = 450)


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

data_max <- max(d$condition_difference, na.rm = TRUE) 
data_min <- min(d$condition_difference, na.rm = TRUE) 
scale_data <- data_max - data_min
noise_max <- max(noise_dataset_2plot$noise_SPLnorm, na.rm = TRUE)
noise_min <- min(noise_dataset_2plot$noise_SPLnorm, na.rm = TRUE)
scale_noise <- noise_max - noise_min
scale_plot <- scale_data / scale_noise
# +noise_max-scale_noise

dif_plot <- ggplot(data=d, aes(x = beta_freq_NERB, y = condition_difference)) +
  geom_ribbon(aes(ymin=condition_difference-condition_se, ymax=condition_difference+condition_se), alpha=0.1) +
  geom_line() +
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=(noise_SPLnorm*scale_plot)+data_max-scale_plot, colour=NULL),linetype="dashed") + 
  scale_y_continuous(sec.axis = sec_axis(~.*(scale_noise) + scale_noise, name = "Noise level (dB SPL (normalised))"))

# add labels
dif_plot <- dif_plot +
  scale_x_continuous(breaks = bw_ticks_values, label = bw_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Average Beta Weight Difference (arb. units)") +
  theme(legend.position="none") +
  scale_colour_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1")

# set sizes
dif_plot <- dif_plot +
  theme(axis.text.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=text_size,angle=0,hjust=0,vjust=0.5,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),
        axis.title.y = element_text(colour="grey20",size=text_size,angle=90,hjust=0.5,vjust=0.5,face="plain"))

dif_plot
# save plot
save_plot("cm_av_beta_weights_difference.png", dif_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.6, base_width =  3.34646,dpi = 450)

###### Tuning Curves ######

# select just beta_bin_kHz and use for labels - both axis and plot a black point
# get rid of titles

# load and filter data
tc_dataset = read.csv('Comparisions_tuning_curves.csv')

tc_dataset %>% filter(roi %in% c("Left", "Right")) ->  tc_dataset_2plot

# labels
tc_bin_values <- select(tc_dataset,beta_bin_NERB) %>% distinct(beta_bin_NERB)
tc_stim_min_nERB <- min(tc_tick_values)
tc_stim_max_nERB <- max(tc_tick_values)

tc_stim_middle_nERB <- (tc_stim_max_nERB/2)+(tc_stim_min_nERB/2)
tc_ticks_values <- c(tc_stim_min_nERB, tc_stim_middle_nERB, tc_stim_max_nERB)
tc_ticks_labels <- c(nERB2kHz(stim_min_nERB),nERB2kHz(stim_middle_nERB),nERB2kHz(stim_max_nERB))
tc_ticks_labels <- round(tc_ticks_labels,digits=2)

row.has.na <- apply(tc_dataset, 1, function(x){any(is.na(x))})
sum(row.has.na)
tc_dataset <- tc_dataset[!row.has.na,]

tc_dataset_2plot_sum <-summarySE(tc_dataset,measurevar="beta_weight_A_True",groupvars=c("beta_freq_NERB","beta_bin_NERB","acquistion"))


tc_plot <- ggplot(tc_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight_A_True, colour=acquistion)) + 
  geom_ribbon(aes(ymin=beta_weight_A_True-se, ymax=beta_weight_A_True+se, fill=acquistion), alpha=0.1, linetype="blank") +
  geom_line() +
  geom_point() +
  facet_wrap(~ beta_bin_NERB, nrow = 2) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_segment(data=tc_bin_values, aes(x=as.numeric(beta_bin_NERB), y = 0, xend = as.numeric(beta_bin_NERB), yend = 1.6, colour=NULL), linetype="dashed")

tc_plot <- tc_plot +
  scale_x_continuous(breaks = tc_ticks_values, label = tc_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Average Beta Weight (arb. units)") +
  theme(legend.position="none") +
  theme(strip.background =element_rect(fill="white")) + 
  scale_colour_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1")

# set sizes
tc_plot <- tc_plot +
  theme(axis.text.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=text_size,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=text_size,angle=90,hjust=0.5,vjust=0.5,face="plain"))

tc_plot

# save plot
save_plot("sHL_tuning_curves.png", tc_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.1, base_width =  6.9291339,dpi = 450)

### Frequency distribution

# Importing the dataset
ve_dataset = read.csv('Comparisions_voxel_estimates.csv')

# set plot labels
fd_ticks_values <- c(0, 10, 20, 30, 40)
fd_ticks_labels <- nERB2kHz(fd_ticks_values)
fd_ticks_labels <- round(fd_ticks_labels,digits=2)

ve_dataset %>% 
  filter(estimation == 'population Centre Frequency')  %>%
  filter(roi %in% c("Left", "Right")) ->  ve_dataset_histo

fhist_plot <- ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, fill = acquistion, colour = NULL)) + 
  geom_density(alpha=0.3)+
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SPLnorm*0.05, colour=NULL, fill=NULL),linetype="dashed") 

fhist_plot <- fhist_plot +
  scale_x_continuous(breaks = fd_ticks_values, label = fd_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Voxel Count") +
  theme(legend.position="none") +
  theme(strip.background =element_rect(fill="white")) + 
  scale_colour_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1")

# set sizes
fhist_plot <- fhist_plot +
  theme(axis.text.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=text_size,angle=90,hjust=0.5,vjust=0.5,face="plain"))


fhist_plot

# save plot
save_plot("7T_freq_dist.png", fhist_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.5, base_width =  3.34646,dpi = 450)
#########





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