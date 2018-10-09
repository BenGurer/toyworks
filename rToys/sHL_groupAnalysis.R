# Group analysis: Simulated hearing loss

# set working directory to correct folder
# setwd("E:/OneDrive - The University of Nottingham/data/hearingLossSimulation/groupAnalysis")

setwd("C:/Users/bengu/Google Drive/data/hearingLossSimulation/groupAnalysis")

###### noise ######

# load and filter data
noise_dataset = read.csv('Sensation_Level.csv')

noise_dataset %>% filter(Averaging %in% c('None')) %>% dplyr::rename(noise = noise_SLnorm) ->  noise_dataset_2plot
noise_dataset %>% filter(Averaging %in% c('Moving Average')) %>% dplyr::rename(noise = noise_SLnorm) ->  noise_dataset_2plot_mv


###### Average Beta weights ######

# load and filter data
bw_dataset = read.csv('sHL_beta_weights.csv')

bw_dataset %>% filter(beta_averaging %in% c('Moving Average')) %>% 
  filter(roi %in% c("Left", "Right")) ->  bw_dataset_2plot

legend_location <- c(0.05,0.9)
bw_plot <- plot_av_beta_weights(bw_dataset_2plot, legend_location)

bw_plot

# save plot
save_plot("sHL_av_beta_weights.png", bw_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.1, base_width =  3.34646,dpi = 450)


# difference and noise plots
bw_dataset_2plot_sum <-summarySE(bw_dataset_2plot,measurevar="beta_weight",groupvars=c("beta_freq_NERB","acquistion"), na.rm=TRUE)

d_difference  <-  bw_dataset_2plot_sum %>% 
  select(c("acquistion", "beta_weight", "beta_freq_NERB")) %>% 
  spread(acquistion, beta_weight) %>% 
  mutate(condition_difference = NH - sHL) %>% 
  select(c("beta_freq_NERB", "condition_difference"))

d_diff_se  <-  bw_dataset_2plot_sum %>% 
  select(c("acquistion", "se", "beta_freq_NERB")) %>% 
  spread(acquistion, se) %>% 
  mutate(condition_se = NH + sHL) %>% 
  select(c("beta_freq_NERB", "condition_se"))

d <- bind_cols(d_difference,d_diff_se) %>% 
  select(-beta_freq_NERB1)

secylabel <- "Noise level (dB SL normalised)"
legend_location <- c(0.6,0.2)
diff_plot <- plot_av_beta_difference(d,noise_dataset_2plot_mv,secylabel,legend_location)

diff_plot

# save plot
save_plot("sHL_av_beta_weights_difference.png", diff_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.1, base_width =  3.84646,dpi = 450)

###### Tuning Curves ######

# load and filter data
tc_dataset = read.csv('sHL_tuning_curves.csv')

tc_dataset %>% filter(roi %in% c("Left", "Right")) ->  tc_dataset_2plot

tc_plot <- plot_tuning_curves(tc_dataset_2plot)

tc_plot

# save plot
save_plot("sHL_tuning_curves.png", tc_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.6, base_width =  6.9291339,dpi = 450)

### Frequency distribution

# Importing the dataset
# filenames <- 'Comparisions_tuning_curves.csv','Comparisions_beta_weights.csv'
ve_dataset = read.csv('sHL_voxel_estimates.csv')

ve_dataset %>% 
  filter(estimation == 'population Centre Frequency')  %>%
  filter(roi %in% c("Left", "Right")) ->  ve_dataset_histo

legend_location <- c(0.65,0.9)
fhist_plot <- plot_hist_freqhist_analysis(ve_dataset_histo,legend_location)

fhist_plot

# save plot
save_plot("sHL_freq_dist.png", fhist_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.2, base_width =  3.34646,dpi = 450)
######

###### Average Beta weights ######

# load and filter data
bw_dataset = read.csv('sHL_beta_weights.csv')

bw_dataset %>% filter(beta_averaging %in% c('Moving Average')) %>% 
  filter(roi %in% c("Left", "Right")) ->  bw_dataset_2plot

row.has.na <- apply(bw_dataset_2plot, 1, function(x){any(is.na(x))})
sum(row.has.na)
bw_dataset_2plot <- bw_dataset_2plot[!row.has.na,]

bw_dataset_2plot_sum <-summarySE(bw_dataset_2plot,measurevar="beta_weight",groupvars=c("beta_freq_NERB","beta_freq_kHz","acquistion"))

# by subject - or could do by ROI
bw_dataset_2plot_sum <-summarySE(bw_dataset_2plot,measurevar="beta_weight",groupvars=c("beta_freq_NERB","acquistion", "subjectID"))


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
  geom_line(data=noise_dataset_2plot_mv, aes(x=frequency_nERB, y=noise_SLnorm, colour=NULL, fill=NULL),linetype="dashed")

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
save_plot("sHL_av_beta_weights.png", bw_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.1, base_width =  3.34646,dpi = 450)


d_difference  <-  bw_dataset_2plot_sum %>% 
  select(c("acquistion", "beta_weight", "beta_freq_NERB")) %>% 
  spread(acquistion, beta_weight) %>% 
  mutate(condition_difference = NH - sHL) %>% 
  select(c("beta_freq_NERB", "condition_difference"))

d_diff_se  <-  bw_dataset_2plot_sum %>% 
  select(c("acquistion", "se", "beta_freq_NERB")) %>% 
  spread(acquistion, se) %>% 
  mutate(condition_se = NH + sHL) %>% 
  select(c("beta_freq_NERB", "condition_se"))

d <- bind_cols(d_difference,d_diff_se) %>% 
  select(-beta_freq_NERB1)

dif_plot <- ggplot(data=d, aes(x = beta_freq_NERB, y = condition_difference)) +
  geom_ribbon(aes(ymin=condition_difference-condition_se, ymax=condition_difference+condition_se), alpha=0.1) +
  geom_line() +
  geom_line(data=noise_dataset_2plot_mv, aes(x=frequency_nERB, y=noise_SLnorm-1, colour=NULL),linetype="dashed") + 
  scale_y_continuous(sec.axis = sec_axis(~.+1,name = "Relative Sensation Level"))

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
save_plot("sHL_av_beta_weights_difference.png", dif_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.6, base_width =  3.34646,dpi = 450)

###### Tuning Curves ######

# select just beta_bin_kHz and use for labels - both axis and plot a black point
# get rid of titles

# load and filter data
tc_dataset = read.csv('sHL_tuning_curves.csv')

tc_dataset %>% filter(roi %in% c("Left", "Right")) ->  tc_dataset_2plot


# cbind(first,third)
# labels
tc_bin_values <- select(tc_dataset,beta_bin_NERB) %>% distinct(beta_bin_NERB)
tc_stim_min_nERB <- min(tc_bin_values)
tc_stim_max_nERB <- max(tc_bin_values)

tc_stim_middle_nERB <- (tc_stim_max_nERB/2)+(tc_stim_min_nERB/2)
tc_ticks_values <- c(tc_stim_min_nERB, tc_stim_middle_nERB, tc_stim_max_nERB)
tc_ticks_labels <- round(tc_ticks_values,digits=2)

row.has.na <- apply(tc_dataset, 1, function(x){any(is.na(x))})
sum(row.has.na)
tc_dataset <- tc_dataset[!row.has.na,]

tc_dataset_2plot_sum <-summarySE(tc_dataset,measurevar="beta_weight_A_True",groupvars=c("beta_freq_NERB","beta_bin_NERB","acquistion"))


tc_plot <- ggplot(tc_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight_A_True, colour=acquistion)) + 
  geom_ribbon(aes(ymin=beta_weight_A_True-se, ymax=beta_weight_A_True+se, fill=acquistion), alpha=0.1, linetype="blank") +
  geom_line() +
  geom_point() +
  facet_wrap(~ beta_bin_NERB, ncol = 2) +
  theme(strip.background = element_blank(),
    strip.text.x = element_blank()) +
  geom_segment(data=tc_bin_values, aes(x=as.numeric(beta_bin_NERB), y = 0, xend = as.numeric(beta_bin_NERB), yend = 1.6, colour=NULL), linetype="dashed")

# geom_vline(data=tc_tick_values, aes(xintercept=as.numeric(beta_bin_NERB)), linetype="dashed")

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
          base_aspect_ratio = 0.4, base_width =  6.9291339,dpi = 450)

### Frequency distribution

# Importing the dataset
# filenames <- 'Comparisions_tuning_curves.csv','Comparisions_beta_weights.csv'
ve_dataset = read.csv('sHL_voxel_estimates.csv')

# set plot labels
fd_ticks_values <- c(0, 10, 20, 30, 40)
fd_ticks_labels <- nERB2kHz(fd_ticks_values)
fd_ticks_labels <- round(fd_ticks_labels,digits=2)

ve_dataset %>% 
  filter(estimation == 'population Centre Frequency')  %>%
  filter(roi %in% c("Left", "Right")) ->  ve_dataset_histo

fhist_plot <- ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, colour = comparision)) + 
  geom_freqpoly(binwidth = 2) +
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SLnorm*20000, colour=NULL),linetype="dashed") 
  
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
save_plot("sHL_freq_dist.png", fhist_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.5, base_width =  3.34646,dpi = 450)
######













stim_min_nERB = 3.3589
stim_max_nERB = 33.1892
stim_middle_nERB = 18.2740

ticks_values <- c(0, 10, 20, 30, 40)
ticks_labels <- nERB2kHz(ticks_values)
ticks_labels <- round(ticks_labels,digits=2)

# Importing the dataset
# filenames <- 'Comparisions_tuning_curves.csv','Comparisions_beta_weights.csv'
ve_dataset = read.csv('sHL_voxel_estimates.csv')

ve_dataset %>% 
  filter(estimation == 'population Centre Frequency')  %>%
  filter(roi %in% c("Left", "Right")) ->  ve_dataset_histo

ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, fill = comparision)) + 
  geom_histogram(position = "dodge", bins = 10) +
  labs(title = "Distribution of preferred frequency", x = "Frequency (kHz)", y = "Voxel Count") +
  guides(fill=guide_legend(title=NULL))+
  labs(fill="Condition") + 
  scale_x_continuous(breaks = ticks_values, label = ticks_labels) +
  theme(legend.justification=c(1,0), 
        legend.position=c(1, 0.75),  
        legend.background = element_blank(),
        legend.key = element_blank()) + 
  scale_fill_manual(name="Condition", 
                    labels = c("Normal Hearing\n(pRF)", 
                               "Hearing Loss Simulation\n(pRF)", 
                               "Hearing Loss Simulation\n(pRF de-biased)"), 
                    values = c("NH_pRF"="green", 
                               "sHL_pRF"="red", 
                               "sHL_pRF_SL_level"="blue")) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

fhist_plot <- ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, colour = comparision)) + 
  geom_freqpoly(binwidth = 2, alpha = 0.8, size=1.5) +
  labs(title = "Distribution of preferred frequency", x = "Frequency (kHz)", y = "Voxel Count") +
  guides(fill=guide_legend(title=NULL))+
  labs(fill="Condition") + 
  scale_x_continuous(breaks = ticks_values, label = ticks_labels) +
  theme(legend.justification=c(1,0), 
        legend.position=c(1, 0.75),  
        legend.background = element_blank(),
        legend.key = element_blank()) + 
  scale_colour_manual(name="Condition", 
                    labels = c("Normal Hearing", 
                               "Hearing Loss Sim", 
                               "Hearing Loss Sim (de-biased)"), 
                    values = c("NH_pRF"="green", 
                               "sHL_pRF"="red", 
                               "sHL_pRF_SL_level"="blue")) +
  scale_colour_brewer(palette = "Set1")

 fhist_plot +
geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SLnorm*20000, colour=NULL),linetype="dashed",size=1.5) 
  
ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, colour = comparision, fill = comparision)) + 
  geom_density(alpha = 0.1, size=1.5) +
  labs(title = "Distribution of preferred frequency", x = "Frequency (kHz)", y = "Voxel Count") +
  guides(fill=guide_legend(title=NULL))+
  labs(fill="Condition") + 
  scale_x_continuous(breaks = ticks_values, label = ticks_labels) +
  theme(legend.justification=c(1,0), 
        legend.position=c(1, 0.75),  
        legend.background = element_blank(),
        legend.key = element_blank()) + 
  scale_colour_manual(name="Condition", 
                      labels = c("Normal Hearing", 
                                 "Hearing Loss Sim", 
                                 "Hearing Loss Sim (de-biased)"), 
                      values = c("NH_pRF"="green", 
                                 "sHL_pRF"="red", 
                                 "sHL_pRF_SL_level"="blue")) +
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+ 
  guides(fill=FALSE)

# + guides(fill=FALSE, color=FALSE)

# correlation

ve_dataset %>%
  filter(location == "Both") %>%
  filter(hemisphere == "Left" | roi == "Right") %>%
  filter(concatenation == 'NH' | concatenation == 'sHL')  %>%
  filter(analysis != 'GLM')  %>%
  select(voxelID, subjectID, comparision, frequency_nERB) %>%
  spread(key=comparision, value = frequency_nERB) %>%
  select(-voxelID, -subjectID) -> ve_dataset_analysis_correlations

row.has.na <- apply(ve_dataset_analysis_correlations, 1, function(x){any(is.na(x))})
sum(row.has.na)
ve_dataset_analysis_correlations <- ve_dataset_analysis_correlations[!row.has.na,]

ve_dataset_analysis_correlations %>% cor() -> ve_corr_analysis_matrix

melted_ve_corr_analysis_matrix <- melt(ve_corr_analysis_matrix)

ggplot(data = melted_ve_corr_analysis_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

corrplot(ve_corr_analysis_matrix, type="upper")

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(ve_corr_analysis_matrix)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

ve_dataset %>%
  filter(location == "Both") %>%
  filter(hemisphere == "Left" | roi == "Right") %>%
  filter(concatenation == 'NH' | concatenation == 'sHL')  %>%
  filter(analysis != 'GLM')  %>%
  select(voxelID, subjectID, comparision, r2ATrue, frequency_nERB) %>%
  spread(key=comparision, value = frequency_nERB)-> ve_dataset_analysis_spread

gg_scatter <- ggplot(data = filter(ve_dataset_analysis_spread,r2ATrue>0.2), mapping = aes(x = `NH_pRF`, y = `sHL_pRF_SL_level`, alpha = r2ATrue)) + 
  geom_point(alpha=0.2) + 
  geom_density_2d() + 
  theme(plot.margin = unit(c(0, 0, 0.5, 0.5), "cm"))

gg_dist_g1 <- ggplot(data = filter(ve_dataset_analysis_spread,r2ATrue>0.2), mapping = aes(x = `NH_pRF`, alpha = r2ATrue)) + 
  geom_density(alpha=0.2) + theme(axis.title.x=element_blank(),
                                  axis.text=element_blank(),
                                  axis.line=element_blank(),
                                  axis.ticks=element_blank()) + 
  theme(plot.margin = unit(c(0.5, 0, 0, 0.7), "cm"))


gg_dist_g2 <- ggplot(data = filter(ve_dataset_analysis_spread,r2ATrue>0.2), mapping = aes(x = `sHL_pRF_SL_level`, alpha = r2ATrue)) + 
  geom_density(alpha=0.05) + coord_flip() + theme(axis.title.y=element_blank(),
                                                 axis.text=element_blank(),
                                                 axis.line=element_blank(),
                                                 axis.ticks=element_blank()) + 
  theme(plot.margin = unit(c(0, 0.5, 0.5, 0), "cm"))

first_col = plot_grid(gg_dist_g1, gg_scatter, ncol = 1, rel_heights = c(1, 3))
second_col = plot_grid(NULL, gg_dist_g2, ncol = 1, rel_heights = c(1, 3))
perfect = plot_grid(first_col, second_col, ncol = 2, rel_widths = c(3, 1))

perfect
# plot change in freq from NH to sHL x = NH y = change

ticks_values <- c(stim_min_nERB, stim_middle_nERB, stim_max_nERB)
ticks_labels <- c(nERB2kHz(stim_min_nERB),nERB2kHz(stim_middle_nERB),nERB2kHz(stim_max_nERB))
ticks_labels <- round(ticks_labels,digits=2)

tc_dataset = read.csv('sHL_tuning_curves.csv')

tc_dataset %>% filter(roi %in% c("Left", "Right")) ->  tc_dataset_2plot

row.has.na <- apply(tc_dataset, 1, function(x){any(is.na(x))})
sum(row.has.na)
tc_dataset <- tc_dataset[!row.has.na,]

tc_dataset_2plot_sum <-summarySE(tc_dataset,measurevar="beta_weight_A_True",groupvars=c("beta_freq_NERB","beta_bin_kHz","acquistion"))

ticks_values <- c(stim_min_nERB, stim_middle_nERB, stim_max_nERB)
ticks_labels <- c(nERB2kHz(stim_min_nERB),nERB2kHz(stim_middle_nERB),nERB2kHz(stim_max_nERB))
ticks_labels <- round(ticks_labels,digits=2)

ggplot(tc_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight_A_True, colour=acquistion)) + 
  geom_errorbar(aes(ymin=beta_weight_A_True-se, ymax=beta_weight_A_True+se), width=1) +
  geom_line() +
  geom_point() +
  facet_wrap(~ beta_bin_kHz, nrow = 2) +
  scale_x_continuous(breaks = ticks_values, label = ticks_labels) +
  labs(title = "Average Tuning Curves within ROI", x = "Frequency (kHz)", y = "Average Beta Weight (arb. units)") +
  guides(colour=guide_legend(title=NULL)) +
  theme(strip.background =element_rect(fill="white")) + 
  scale_colour_manual(name="Condition", 
                    labels = c("Normal Hearing", 
                               "Hearing Loss Simulation"), 
                    values = c("NH"="green", 
                               "sHL"="red")) +
  scale_colour_brewer(palette = "Set1")


ggplot(tc_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight_A_True, colour=acquistion)) + 
  geom_ribbon(aes(ymin=beta_weight_A_True-se, ymax=beta_weight_A_True+se), alpha=0.1, linetype="blank") +
  geom_line() +
  geom_point() +
  facet_wrap(~ beta_bin_kHz, nrow = 2) +
  scale_x_continuous(breaks = ticks_values, label = ticks_labels) +
  labs(title = "Average Tuning Curves within ROI", x = "Frequency (kHz)", y = "Average Beta Weight (arb. units)") +
  guides(colour=guide_legend(title=NULL)) +
  theme(strip.background =element_rect(fill="white")) + 
  scale_colour_manual(name="Condition", 
                      labels = c("Normal Hearing", 
                                 "Hearing Loss Simulation"), 
                      values = c("NH"="green", 
                                 "sHL"="red")) +
  scale_colour_brewer(palette = "Set1")


ggplot(data=noise_dataset_2plot_mv, aes(x=frequency_nERB, y=noise_SLnorm)) +
  geom_line(linetype="dashed")
  

# ROI avearge beta weights

bw_dataset = read.csv('sHL_beta_weights.csv')

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
line_width <- 2

bw_plot <- ggplot(bw_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight, colour=acquistion, fill=acquistion)) + 
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
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SLnorm, colour=NULL, fill=NULL),linetype="dashed")



+ theme(text = element_text(size = 9))

save_plot(filename, plot, ncol = 1, nrow = 1, base_height = 4,
          base_aspect_ratio = 1.1, base_width = NULL, ..., cols = NULL,
          rows = NULL)

ggsave(filename, plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE, ...)

# 1 column (maximum width 8.5 cm), 1.5 columns (maximum width 11.6 cm) or 2 columns (maximum width 17.6 cm)
# 3.34646
# write_csv(bw_dataset_2plot_sum,"mean_bw")

d_difference  <-  bw_dataset_2plot_sum %>% 
  select(c("acquistion", "beta_weight", "beta_freq_NERB")) %>% 
  spread(acquistion, beta_weight) %>% 
  mutate(condition_difference = NH - sHL) %>% 
  select(c("beta_freq_NERB", "condition_difference"))

d_diff_se  <-  bw_dataset_2plot_sum %>% 
  select(c("acquistion", "se", "beta_freq_NERB")) %>% 
  spread(acquistion, se) %>% 
  mutate(condition_se = NH + sHL) %>% 
  select(c("beta_freq_NERB", "condition_se"))

d <- bind_cols(d_difference,d_diff_se) %>% 
  select(-beta_freq_NERB1)
  
ggplot(d_wide, aes(x = beta_freq_NERB, y = condition_difference)) + 
  geom_line() + theme(aspect.ratio = 1)

bw_plot + 
  geom_line(data=d, aes(x = beta_freq_NERB, y = condition_difference, colour=NULL)) +
   geom_point(data=d, aes(x = beta_freq_NERB, y = condition_difference, colour=NULL)) +
  geom_ribbon(data=d, aes(ymin=condition_difference-condition_se, ymax=condition_difference+condition_se), alpha=0.1) +
geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SLnorm, colour=NULL),linetype="dashed") 


ggplot(data=d, aes(x = beta_freq_NERB, y = condition_difference)) +
  geom_ribbon(aes(ymin=condition_difference-condition_se, ymax=condition_difference+condition_se), alpha=0.1) +
  geom_line() +
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SLnorm*-1, colour=NULL),linetype="dashed") + scale_y_continuous(trans = "reverse",sec.axis = sec_axis(~.*-1, name = "Relative humidity [%]"))


bwdiff_plot <- ggplot(data=d, aes(x = beta_freq_NERB, y = condition_difference/max(condition_difference)*-1+1)) +
  geom_ribbon(aes(ymin=condition_difference/max(condition_difference)*-1+1-condition_se, ymax=condition_difference/max(condition_difference)*-1+1+condition_se), alpha=0.1) +
  geom_line() +
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SLnorm, colour=NULL),linetype="dashed") + scale_y_continuous(trans = "reverse",sec.axis = sec_axis(~.*1,name = "Relative Sensation Level"))



bw_plot + bwdiff_plot

plot2save <- plot_grid(bw_plot, bwdiff_plot, labels = c("A", "B"), align = "h")

save_plot('test.png',plot2save)

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