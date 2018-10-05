plot_av_beta_difference <- function(d,noise_dataset_2plot,legend_location){
  
text_size = 9
  
data_max <- max(d$condition_difference, na.rm = TRUE) 
data_min <- min(d$condition_difference, na.rm = TRUE) 
scale_data <- data_max - data_min
noise_max <- max(noise_dataset_2plot$noise_SPLnorm, na.rm = TRUE)
noise_min <- min(noise_dataset_2plot$noise_SPLnorm, na.rm = TRUE)
scale_noise <- noise_max - noise_min
scale_plot <- scale_data / scale_noise
# +noise_max-scale_noise

dif_plot <- ggplot(data=d, aes(x = beta_freq_NERB, y = condition_difference, linetype="Difference")) +
  geom_ribbon(aes(ymin=condition_difference-condition_se, ymax=condition_difference+condition_se), alpha=0.1) +
  geom_line() +
  geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=(noise_SPLnorm*scale_plot)+data_max-scale_plot, linetype="Noise")) + 
  scale_y_continuous(sec.axis = sec_axis(~.*(scale_noise) + scale_noise, name = secylabel))

# add labels
dif_plot <- dif_plot +
  scale_x_continuous(breaks = bw_ticks_values, label = bw_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Average Beta Weight Difference (arb. units)") +
  scale_colour_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1")

# set sizes
dif_plot <- dif_plot +
  theme(axis.text.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=text_size,angle=0,hjust=0,vjust=0.5,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),
        axis.title.y = element_text(colour="grey20",size=text_size,angle=90,hjust=0.5,vjust=0.5,face="plain"))

return(dif_plot)

}
# # save plot
# save_plot("cm_av_beta_weights_difference.png", dif_plot, device = "png", base_height = NULL,
#           base_aspect_ratio = 1.6, base_width =  3.34646,dpi = 450)
