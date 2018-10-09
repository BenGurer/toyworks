plot_tuning_curves<- function(tc_dataset_2plot){
  
  
text_size = 9

tc_bin_values <- select(tc_dataset_2plot,beta_bin_NERB) %>% distinct(beta_bin_NERB)
tc_stim_min_nERB <- min(tc_bin_values)
tc_stim_max_nERB <- max(tc_bin_values)

tc_stim_middle_nERB <- (tc_stim_max_nERB/2)+(tc_stim_min_nERB/2)
tc_ticks_values <- c(tc_stim_min_nERB, tc_stim_middle_nERB, tc_stim_max_nERB)
tc_ticks_labels <- nERB2kHz(tc_ticks_values)
tc_ticks_labels <- round(tc_ticks_labels,digits=2)

row.has.na <- apply(tc_dataset_2plot, 1, function(x){any(is.na(x))})
sum(row.has.na)
tc_dataset_2plot <- tc_dataset_2plot[!row.has.na,]

# tc_dataset_2plot_sum <-summarySE(tc_dataset,measurevar="beta_weight_A_True",groupvars=c("beta_freq_NERB","beta_bin_NERB","acquistion"))
tc_dataset_2plot_sum <-summarySE(tc_dataset_2plot,measurevar="beta_weight",groupvars=c("beta_freq_NERB","beta_bin_NERB","acquistion"), na.rm=TRUE)

y_max <-max(tc_dataset_2plot_sum$beta_weight)

tc_plot <- ggplot(tc_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight, colour=acquistion)) + 
  geom_ribbon(aes(ymin=beta_weight-se, ymax=beta_weight+se, fill=acquistion), alpha=0.1, linetype="blank") +
  geom_line() +
  geom_point() +
  facet_wrap(~ beta_bin_NERB, nrow = 2) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_segment(data=tc_bin_values, aes(x=as.numeric(beta_bin_NERB), y = 0, xend = as.numeric(beta_bin_NERB), yend = y_max, colour=NULL), linetype="dashed")


# y_max <-max(tc_dataset_2plot_sum$beta_weight_A_True)

# tc_plot <- ggplot(tc_dataset_2plot_sum, aes(x=beta_freq_NERB, y=beta_weight_A_True, colour=acquistion)) + 
#   geom_ribbon(aes(ymin=beta_weight_A_True-se, ymax=beta_weight_A_True+se, fill=acquistion), alpha=0.1, linetype="blank") +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~ beta_bin_NERB, nrow = 2) +
#   theme(strip.background = element_blank(),
#         strip.text.x = element_blank()) +
#   geom_segment(data=tc_bin_values, aes(x=as.numeric(beta_bin_NERB), y = 0, xend = as.numeric(beta_bin_NERB), yend = y_max, colour=NULL), linetype="dashed")

tc_plot <- tc_plot +
  scale_x_continuous(breaks = tc_ticks_values, label = tc_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Average Beta Weight (arb. units)") +
  guides(colour=guide_legend(title=NULL)) + 
  guides(fill=guide_legend(title=NULL)) + 
  theme(legend.position=c(0.75, 0.4),legend.text=element_text(size=text_size)) +
  theme(legend.background = element_rect(colour = 'white', fill = 'white', linetype='solid')) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

# set sizes
tc_plot <- tc_plot +
  theme(axis.text.x = element_text(colour="grey20",size=text_size,angle=45,hjust=0.5,vjust=0.75,face="plain"),
        axis.text.y = element_text(colour="grey20",size=text_size,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=text_size,angle=90,hjust=0.5,vjust=0.5,face="plain"))

return(tc_plot)

}

# # save plot
# save_plot("sHL_tuning_curves.png", tc_plot, device = "png", base_height = NULL,
#           base_aspect_ratio = 1.1, base_width =  6.9291339,dpi = 450)